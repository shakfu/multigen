"""Nested Container Support for STC.

This module provides support for complex nested container types like
List[List[int]], Dict[str, List[int]], etc. by managing template dependencies
and generating proper instantiation order.
"""

import re
from dataclasses import dataclass
from typing import Any


@dataclass
class NestedTypeInfo:
    """Information about a nested container type."""

    full_type: str  # e.g., "List[Dict[str, int]]"
    container_type: str  # e.g., "List"
    inner_types: list[str]  # e.g., ["Dict[str, int]"]
    depth: int  # Nesting depth
    dependencies: list[str]  # Types this depends on


class NestedContainerManager:
    """Manages complex nested container type generation."""

    def __init__(self) -> None:
        self.type_registry: dict[str, NestedTypeInfo] = {}
        self.dependency_graph: dict[str, set[str]] = {}
        self.instantiation_order: list[str] = []

    def parse_nested_type(self, type_str: str) -> NestedTypeInfo:
        """Parse a nested container type string into components.

        Args:
            type_str: Type string like "List[Dict[str, int]]"

        Returns:
            NestedTypeInfo object with parsed components
        """
        # Clean the type string
        type_str = type_str.strip()

        # Handle basic types first
        if not any(bracket in type_str for bracket in ["[", "]"]):
            return NestedTypeInfo(full_type=type_str, container_type=type_str, inner_types=[], depth=0, dependencies=[])

        # Parse nested structure
        container_type, inner_types, depth = self._parse_container_structure(type_str)

        # Find dependencies
        dependencies = self._extract_dependencies(inner_types)

        return NestedTypeInfo(
            full_type=type_str,
            container_type=container_type,
            inner_types=inner_types,
            depth=depth,
            dependencies=dependencies,
        )

    def _parse_container_structure(self, type_str: str) -> tuple[str, list[str], int]:
        """Parse the container structure recursively."""
        # Match pattern like "List[...]" or "Dict[..., ...]"
        match = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\[(.*)\]$", type_str)
        if not match:
            return type_str, [], 0

        container_type = match.group(1)
        inner_content = match.group(2)

        # Parse inner types by balancing brackets
        inner_types = self._split_balanced(inner_content)

        # Calculate depth
        max_depth = 0
        for inner_type in inner_types:
            if "[" in inner_type:
                _, _, inner_depth = self._parse_container_structure(inner_type)
                max_depth = max(max_depth, inner_depth + 1)
            else:
                max_depth = max(max_depth, 1)

        return container_type, inner_types, max_depth

    def _split_balanced(self, content: str) -> list[str]:
        """Split content by commas while respecting bracket balance."""
        parts = []
        current = ""
        bracket_count = 0

        for char in content:
            if char == "[":
                bracket_count += 1
            elif char == "]":
                bracket_count -= 1
            elif char == "," and bracket_count == 0:
                parts.append(current.strip())
                current = ""
                continue

            current += char

        if current.strip():
            parts.append(current.strip())

        return parts

    def _extract_dependencies(self, inner_types: list[str]) -> list[str]:
        """Extract type dependencies from inner types."""
        dependencies = []

        for inner_type in inner_types:
            if "[" in inner_type:
                # This is a nested container type
                dependencies.append(inner_type)
            # For basic types, no dependencies needed

        return dependencies

    def register_nested_type(self, type_str: str) -> str:
        """Register a nested type and return its canonical name.

        Args:
            type_str: Type string to register

        Returns:
            Canonical type name for use in template system
        """
        # Parse the type
        type_info = self.parse_nested_type(type_str)

        # Generate canonical name
        canonical_name = self._generate_canonical_name(type_info)

        # Register in type registry
        self.type_registry[canonical_name] = type_info

        # Update dependency graph
        self._update_dependency_graph(canonical_name, type_info)

        return canonical_name

    def _generate_canonical_name(self, type_info: NestedTypeInfo) -> str:
        """Generate a canonical name for the nested type."""
        if type_info.depth == 0:
            return type_info.container_type.lower()

        # Build canonical name recursively
        base_name = type_info.container_type.lower()

        if type_info.inner_types:
            inner_names = []
            for inner_type in type_info.inner_types:
                if "[" in inner_type:
                    # Recursive nested type
                    inner_info = self.parse_nested_type(inner_type)
                    inner_name = self._generate_canonical_name(inner_info)
                else:
                    # Basic type
                    inner_name = inner_type.lower()
                inner_names.append(inner_name)

            return f"{base_name}_{'_'.join(inner_names)}"

        return base_name

    def _update_dependency_graph(self, type_name: str, type_info: NestedTypeInfo) -> None:
        """Update the dependency graph with new type."""
        if type_name not in self.dependency_graph:
            self.dependency_graph[type_name] = set()

        for dep in type_info.dependencies:
            dep_canonical = self.register_nested_type(dep)
            self.dependency_graph[type_name].add(dep_canonical)

    def get_instantiation_order(self) -> list[str]:
        """Get the correct instantiation order for all registered types.

        Returns:
            List of type names in dependency order (dependencies first)
        """
        # Topological sort of dependency graph
        visited = set()
        temp_visited = set()
        order = []

        def dfs(node: str) -> None:
            if node in temp_visited:
                raise ValueError(f"Circular dependency detected involving {node}")
            if node in visited:
                return

            temp_visited.add(node)

            # Visit dependencies first
            for dep in self.dependency_graph.get(node, set()):
                dfs(dep)

            temp_visited.remove(node)
            visited.add(node)
            order.append(node)

        # Process all nodes
        for node in self.dependency_graph:
            if node not in visited:
                dfs(node)

        self.instantiation_order = order
        return order

    def generate_stc_template_definitions(self, template_manager: Any) -> list[str]:
        """Generate STC template definitions for all nested types.

        Args:
            template_manager: STCTemplateManager instance

        Returns:
            List of template definition strings
        """
        definitions = []

        # Get instantiation order
        order = self.get_instantiation_order()

        for type_name in order:
            type_info = self.type_registry[type_name]

            # Generate template definition based on type structure
            template_def = self._generate_template_definition(type_name, type_info, template_manager)
            if template_def:
                definitions.extend(template_def)

        return definitions

    def _generate_template_definition(
        self, type_name: str, type_info: NestedTypeInfo, template_manager: Any
    ) -> list[str]:
        """Generate template definition for a specific nested type."""
        definitions = []

        if type_info.depth == 0:
            # Basic type, no template needed
            return []

        # Map container types to STC types
        stc_mapping = {"List": "vec", "list": "vec", "Dict": "hmap", "dict": "hmap", "Set": "hset", "set": "hset"}

        stc_type = stc_mapping.get(type_info.container_type, type_info.container_type.lower())

        if stc_type == "vec":
            # Vector of something
            if len(type_info.inner_types) != 1:
                raise ValueError(f"Vector must have exactly one element type, got {type_info.inner_types}")

            element_type = type_info.inner_types[0]
            if "[" in element_type:
                # Nested container as element
                element_canonical = self._generate_canonical_name(self.parse_nested_type(element_type))
                c_element_type = element_canonical
            else:
                # Basic type as element
                c_element_type = self._map_basic_type_to_c(element_type)

            # Register with template manager
            instance_name = template_manager.register_container_usage(type_name, "vec", [c_element_type], "stc/vec.h")

            definitions.append(f"// Nested container: {type_info.full_type} -> {instance_name}")

        elif stc_type == "hmap":
            # Map of key -> value
            if len(type_info.inner_types) != 2:
                raise ValueError(f"Map must have exactly two types (key, value), got {type_info.inner_types}")

            key_type, value_type = type_info.inner_types

            # Process key type
            if "[" in key_type:
                key_canonical = self._generate_canonical_name(self.parse_nested_type(key_type))
                c_key_type = key_canonical
            else:
                c_key_type = self._map_basic_type_to_c(key_type)

            # Process value type
            if "[" in value_type:
                value_canonical = self._generate_canonical_name(self.parse_nested_type(value_type))
                c_value_type = value_canonical
            else:
                c_value_type = self._map_basic_type_to_c(value_type)

            # Register with template manager
            instance_name = template_manager.register_container_usage(
                type_name, "hmap", [c_key_type, c_value_type], "stc/hmap.h"
            )

            definitions.append(f"// Nested container: {type_info.full_type} -> {instance_name}")

        elif stc_type == "hset":
            # Set of something
            if len(type_info.inner_types) != 1:
                raise ValueError(f"Set must have exactly one element type, got {type_info.inner_types}")

            element_type = type_info.inner_types[0]
            if "[" in element_type:
                # Nested container as element
                element_canonical = self._generate_canonical_name(self.parse_nested_type(element_type))
                c_element_type = element_canonical
            else:
                # Basic type as element
                c_element_type = self._map_basic_type_to_c(element_type)

            # Register with template manager
            instance_name = template_manager.register_container_usage(type_name, "hset", [c_element_type], "stc/hset.h")

            definitions.append(f"// Nested container: {type_info.full_type} -> {instance_name}")

        return definitions

    def _map_basic_type_to_c(self, python_type: str) -> str:
        """Map basic Python types to C types."""
        mapping = {"int": "int", "float": "double", "str": "cstr", "bool": "bool", "bytes": "uint8_t*"}
        return mapping.get(python_type, python_type)

    def is_nested_container_type(self, type_str: str) -> bool:
        """Check if a type string represents a nested container."""
        # Count bracket depth
        bracket_depth = 0
        max_depth = 0

        for char in type_str:
            if char == "[":
                bracket_depth += 1
                max_depth = max(max_depth, bracket_depth)
            elif char == "]":
                bracket_depth -= 1

        return max_depth > 1 or (
            max_depth == 1
            and any(container in type_str for container in ["List[", "Dict[", "Set[", "list[", "dict[", "set["])
        )

    def get_statistics(self) -> dict[str, Any]:
        """Get statistics about nested container usage."""
        total_types = len(self.type_registry)
        nested_types = len([t for t in self.type_registry.values() if t.depth > 1])
        max_depth = max([t.depth for t in self.type_registry.values()], default=0)

        return {
            "total_types": total_types,
            "nested_types": nested_types,
            "max_nesting_depth": max_depth,
            "dependency_count": sum(len(deps) for deps in self.dependency_graph.values()),
            "instantiation_order_length": len(self.instantiation_order),
        }


__all__ = ["NestedContainerManager", "NestedTypeInfo"]
