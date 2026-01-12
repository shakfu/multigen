"""Go container system for MultiGen."""

from ..base import AbstractContainerSystem


class GoContainerSystem(AbstractContainerSystem):
    """Go container system using built-in types."""

    def get_list_type(self, element_type: str) -> str:
        """Get Go slice type for element type."""
        return f"[]{element_type}"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get Go map type for key-value storage."""
        return f"map[{key_type}]{value_type}"

    def get_set_type(self, element_type: str) -> str:
        """Get Go map type for set storage (using map[T]bool pattern)."""
        return f"map[{element_type}]bool"

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate Go container operations."""
        operations_code = []

        for op in operations:
            if op == "append" and "[]" in container_type:
                operations_code.append("// slice = append(slice, item)")
            elif op == "insert" and "map[" in container_type:
                operations_code.append("// m[key] = value")
            elif op == "remove":
                operations_code.append("// delete(m, key)")

        return "\n".join(operations_code)

    def get_required_imports(self) -> list[str]:
        """Get Go imports required for container operations."""
        return []  # Go's built-in containers don't require imports
