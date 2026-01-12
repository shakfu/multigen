"""STC Template Manager.

This module manages STC template instantiation and avoids multiple macro redefinitions.
It provides a centralized system for tracking and generating unique STC template definitions.
"""

import hashlib
from dataclasses import dataclass
from typing import Any, Optional


@dataclass
class STCTemplateInstance:
    """Represents a single STC template instantiation."""

    template_name: str  # e.g., "vec_int"
    stc_type: str  # e.g., "vec"
    element_types: list[str]  # e.g., ["int"] or ["cstr", "int"] for maps
    header_file: str  # e.g., "stc/vec.h"
    macro_definition: str  # The actual #define statement
    instance_name: str  # Unique instance name for this specific usage


class STCTemplateManager:
    """Manages STC template instantiation to avoid macro redefinition conflicts."""

    def __init__(self) -> None:
        # Track generated template instances by signature
        self.template_instances: dict[str, STCTemplateInstance] = {}
        # Track which headers are needed
        self.required_headers: set[str] = set()
        # Track variable to template instance mappings
        self.variable_mappings: dict[str, str] = {}
        # Counter for generating unique names
        self.instance_counter = 0
        # Nested container manager (lazy initialization to avoid circular imports)
        self._nested_container_manager: Optional[Any] = None

    def _generate_signature(self, stc_type: str, element_types: list[str]) -> str:
        """Generate a unique signature for a template instantiation."""
        combined = f"{stc_type}_{'_'.join(element_types)}"
        # Use hash for very long type names
        if len(combined) > 50:
            hash_obj = hashlib.sha256(combined.encode())
            return f"{stc_type}_{hash_obj.hexdigest()[:8]}"
        return combined

    def _normalize_type_name(self, type_name: str) -> str:
        """Normalize C type names for consistent naming."""
        # Handle common type mappings
        type_mappings = {
            "const char*": "cstr",
            "char*": "cstr",
            "string": "cstr",
            "std::string": "cstr",
            "long long": "longlong",
            "long double": "longdouble",
            "unsigned int": "uint",
            "unsigned long": "ulong",
            "unsigned char": "uchar",
            "unsigned short": "ushort",
        }
        return type_mappings.get(type_name, type_name)

    @property
    def nested_container_manager(self) -> Any:
        """Lazy initialization of nested container manager."""
        if self._nested_container_manager is None:
            from .nested_containers import NestedContainerManager

            self._nested_container_manager = NestedContainerManager()
        return self._nested_container_manager

    def register_container_usage(
        self, variable_name: str, stc_type: str, element_types: list[str], header_file: str
    ) -> str:
        """Register a container usage and get the template instance name.
        Enhanced to support nested containers.

        Args:
            variable_name: Name of the variable using this container
            stc_type: STC container type (vec, hmap, hset, etc.)
            element_types: List of element types [value_type] or [key_type, value_type]
            header_file: Header file to include

        Returns:
            Template instance name to use in code generation
        """
        # Check if any element types are nested containers
        nested_dependencies = []
        normalized_types = []

        for element_type in element_types:
            if self.nested_container_manager.is_nested_container_type(element_type):
                # Register nested type and get canonical name
                canonical_name = self.nested_container_manager.register_nested_type(element_type)
                normalized_types.append(canonical_name)
                nested_dependencies.append(canonical_name)
            else:
                # Regular type normalization
                normalized_types.append(self._normalize_type_name(element_type))

        # Generate signature for this template instantiation
        signature = self._generate_signature(stc_type, normalized_types)

        # Check if we already have this template instantiation
        if signature in self.template_instances:
            instance = self.template_instances[signature]
            # Map this variable to the existing instance
            self.variable_mappings[variable_name] = instance.instance_name
            return instance.instance_name

        # Generate unique instance name
        self.instance_counter += 1
        instance_name = f"{signature}_{self.instance_counter}"

        # Create macro definition based on STC type
        if stc_type in ["vec", "deque", "stack", "queue"]:
            # Single type parameter containers
            if len(normalized_types) != 1:
                raise ValueError(f"Container {stc_type} requires exactly 1 type parameter")
            macro_def = f"#define T {instance_name}, {normalized_types[0]}"

        elif stc_type in ["hmap", "smap"]:
            # Key-value containers
            if len(normalized_types) != 2:
                raise ValueError(f"Container {stc_type} requires exactly 2 type parameters")
            macro_def = f"#define T {instance_name}, {normalized_types[0]}, {normalized_types[1]}"

        elif stc_type in ["hset", "sset"]:
            # Key-only containers
            if len(normalized_types) != 1:
                raise ValueError(f"Container {stc_type} requires exactly 1 type parameter")
            macro_def = f"#define T {instance_name}, {normalized_types[0]}"

        elif stc_type == "pqueue":
            # Priority queue
            if len(normalized_types) != 1:
                raise ValueError(f"Container {stc_type} requires exactly 1 type parameter")
            macro_def = f"#define T {instance_name}, {normalized_types[0]}"

        else:
            raise ValueError(f"Unsupported STC container type: {stc_type}")

        # Create template instance
        instance = STCTemplateInstance(
            template_name=signature,
            stc_type=stc_type,
            element_types=normalized_types,
            header_file=header_file,
            macro_definition=macro_def,
            instance_name=instance_name,
        )

        # Register the instance
        self.template_instances[signature] = instance
        self.required_headers.add(header_file)
        self.variable_mappings[variable_name] = instance_name

        return instance_name

    def get_container_type_name(self, variable_name: str) -> Optional[str]:
        """Get the STC container type name for a variable."""
        return self.variable_mappings.get(variable_name)

    def generate_template_definitions(self) -> list[str]:
        """Generate all template definitions in dependency order."""
        definitions = []

        # First generate nested container definitions
        nested_definitions = self.nested_container_manager.generate_stc_template_definitions(self)
        definitions.extend(nested_definitions)

        # Sort instances by dependency (simpler types first)
        sorted_instances = sorted(
            self.template_instances.values(), key=lambda x: (len(x.element_types), x.template_name)
        )

        for instance in sorted_instances:
            definitions.append(instance.macro_definition)
            definitions.append(f"#include <{instance.header_file}>")
            definitions.append("#undef T")
            definitions.append("")  # Blank line for readability

        return definitions

    def generate_include_statements(self) -> list[str]:
        """Generate unique include statements needed."""
        return [f"#include <{header}>" for header in sorted(self.required_headers)]

    def generate_cleanup_statements(self, scope_variables: list[str]) -> list[str]:
        """Generate cleanup statements for variables in the given scope."""
        cleanup_statements = []

        for var_name in scope_variables:
            if var_name in self.variable_mappings:
                container_type = self.variable_mappings[var_name]
                cleanup_statements.append(f"{container_type}_drop(&{var_name});")

        return cleanup_statements

    def generate_initialization_statement(self, variable_name: str) -> Optional[str]:
        """Generate initialization statement for a container variable."""
        if variable_name in self.variable_mappings:
            container_type = self.variable_mappings[variable_name]
            return f"{container_type} {variable_name} = {{0}};"
        return None

    def clear(self) -> None:
        """Clear all registered template instances and mappings."""
        self.template_instances.clear()
        self.required_headers.clear()
        self.variable_mappings.clear()
        self.instance_counter = 0

    def get_statistics(self) -> dict[str, int]:
        """Get statistics about template usage."""
        return {
            "total_instances": len(self.template_instances),
            "unique_headers": len(self.required_headers),
            "variable_mappings": len(self.variable_mappings),
            "next_instance_id": self.instance_counter + 1,
        }


# Global template manager instance
_global_template_manager = STCTemplateManager()


def get_template_manager() -> STCTemplateManager:
    """Get the global template manager instance."""
    return _global_template_manager


def reset_template_manager() -> None:
    """Reset the global template manager."""
    global _global_template_manager
    _global_template_manager = STCTemplateManager()


__all__ = ["STCTemplateInstance", "STCTemplateManager", "get_template_manager", "reset_template_manager"]
