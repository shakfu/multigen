"""Type parameter extraction for parameterized container code generation.

This module extracts type parameters from container type patterns:
- vec_<T> -> T
- map_<K>_<V> -> K, V
- set_<T> -> T
- vec_vec_<T> -> vec_T, T (nested)
"""

import re
from dataclasses import dataclass
from typing import Optional

from .type_properties import TypeProperties, get_type_properties, is_registered_type


@dataclass
class ContainerTypeInfo:
    """Information about a parameterized container type.

    Attributes:
        family: Container family ("vec", "map", "set")
        full_name: Full container type name (e.g., "vec_int", "map_str_int")
        type_params: List of type parameter names
        type_properties: List of TypeProperties for each parameter
        is_nested: Whether this is a nested container (e.g., vec_vec_int)
    """

    family: str
    full_name: str
    type_params: list[str]
    type_properties: list[TypeProperties]
    is_nested: bool


class TypeParameterExtractor:
    """Extracts type parameters from container type names."""

    # Pattern for vec_<T>
    VEC_PATTERN = re.compile(r"^vec_([a-z_]+)$")

    # Pattern for map_<K>_<V>
    MAP_PATTERN = re.compile(r"^map_([a-z_]+)_([a-z_]+)$")

    # Pattern for set_<T>
    SET_PATTERN = re.compile(r"^set_([a-z_]+)$")

    # Pattern for vec_vec_<T> (nested vector)
    VEC_VEC_PATTERN = re.compile(r"^vec_vec_([a-z_]+)$")

    def extract(self, container_type: str) -> Optional[ContainerTypeInfo]:
        """Extract type parameters from a container type name.

        Args:
            container_type: Container type name (e.g., "vec_int", "map_str_int")

        Returns:
            ContainerTypeInfo if extraction successful, None otherwise

        Examples:
            >>> extractor = TypeParameterExtractor()
            >>> info = extractor.extract("vec_int")
            >>> info.family
            'vec'
            >>> info.type_params
            ['int']
            >>> info = extractor.extract("map_str_int")
            >>> info.family
            'map'
            >>> info.type_params
            ['str', 'int']
        """
        # Try nested vector first (most specific)
        if match := self.VEC_VEC_PATTERN.match(container_type):
            inner_type = match.group(1)
            if not is_registered_type(inner_type):
                return None

            return ContainerTypeInfo(
                family="vec_vec",
                full_name=container_type,
                type_params=["vec_" + inner_type, inner_type],
                type_properties=[get_type_properties(inner_type)],  # Only inner type props
                is_nested=True,
            )

        # Try vector
        if match := self.VEC_PATTERN.match(container_type):
            elem_type = match.group(1)
            if not is_registered_type(elem_type):
                return None

            return ContainerTypeInfo(
                family="vec",
                full_name=container_type,
                type_params=[elem_type],
                type_properties=[get_type_properties(elem_type)],
                is_nested=False,
            )

        # Try map
        if match := self.MAP_PATTERN.match(container_type):
            key_type = match.group(1)
            val_type = match.group(2)

            if not is_registered_type(key_type) or not is_registered_type(val_type):
                return None

            return ContainerTypeInfo(
                family="map",
                full_name=container_type,
                type_params=[key_type, val_type],
                type_properties=[get_type_properties(key_type), get_type_properties(val_type)],
                is_nested=False,
            )

        # Try set
        if match := self.SET_PATTERN.match(container_type):
            elem_type = match.group(1)
            if not is_registered_type(elem_type):
                return None

            return ContainerTypeInfo(
                family="set",
                full_name=container_type,
                type_params=[elem_type],
                type_properties=[get_type_properties(elem_type)],
                is_nested=False,
            )

        return None

    def is_parameterized_container(self, container_type: str) -> bool:
        """Check if a container type can be parameterized.

        Args:
            container_type: Container type name

        Returns:
            True if container can be parameterized, False otherwise
        """
        return self.extract(container_type) is not None

    def get_template_name(self, container_type: str) -> Optional[str]:
        """Get the generic template name for a container type.

        Args:
            container_type: Container type name (e.g., "vec_int")

        Returns:
            Template name (e.g., "vec_T") or None if not parameterized

        Examples:
            >>> extractor = TypeParameterExtractor()
            >>> extractor.get_template_name("vec_int")
            'vec_T'
            >>> extractor.get_template_name("map_str_int")
            'map_K_V'
            >>> extractor.get_template_name("set_int")
            'set_T'
        """
        info = self.extract(container_type)
        if not info:
            return None

        if info.family == "vec":
            return "vec_T"
        elif info.family == "vec_vec":
            return "vec_vec_T"
        elif info.family == "map":
            return "map_K_V"
        elif info.family == "set":
            return "set_T"

        return None


__all__ = ["ContainerTypeInfo", "TypeParameterExtractor"]
