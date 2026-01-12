"""Rust container system for MultiGen."""

from ..base import AbstractContainerSystem


class RustContainerSystem(AbstractContainerSystem):
    """Rust container system using std::collections."""

    def get_list_type(self, element_type: str) -> str:
        """Get Rust Vec type for element type."""
        return f"Vec<{element_type}>"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get Rust HashMap type for key-value storage."""
        return f"HashMap<{key_type}, {value_type}>"

    def get_set_type(self, element_type: str) -> str:
        """Get Rust HashSet type for set storage."""
        return f"HashSet<{element_type}>"

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate Rust container operations."""
        operations_code = []

        for op in operations:
            if op == "append" and "Vec" in container_type:
                operations_code.append("// vec.push(item);")
            elif op == "insert" and "HashMap" in container_type:
                operations_code.append("// map.insert(key, value);")
            elif op == "remove":
                operations_code.append("// container.remove(&item);")

        return "\n".join(operations_code)

    def get_required_imports(self) -> list[str]:
        """Get Rust use statements required for container operations."""
        return [
            "use std::collections::HashMap;",
            "use std::collections::HashSet;",
        ]
