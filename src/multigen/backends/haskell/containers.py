"""Haskell container system for MultiGen."""

from ..base import AbstractContainerSystem


class HaskellContainerSystem(AbstractContainerSystem):
    """Haskell container system using standard library containers."""

    def get_list_type(self, element_type: str) -> str:
        """Get Haskell list type for element type."""
        return f"[{element_type}]"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get Haskell Map type for key-value storage."""
        return f"Map {key_type} {value_type}"

    def get_set_type(self, element_type: str) -> str:
        """Get Haskell Set type for set storage."""
        return f"Set {element_type}"

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate Haskell container operations."""
        operations_code = []

        for op in operations:
            if op == "append" and "[" in container_type:
                operations_code.append("-- list ++ [item]")
            elif op == "insert" and "Map" in container_type:
                operations_code.append("-- Map.insert key value map")
            elif op == "insert" and "Set" in container_type:
                operations_code.append("-- Set.insert item set")
            elif op == "remove":
                operations_code.append("-- Set.delete item set or Map.delete key map")

        return "\n".join(operations_code)

    def get_required_imports(self) -> list[str]:
        """Get Haskell import statements required for container operations."""
        return [
            "import qualified Data.Map as Map",
            "import qualified Data.Set as Set",
            "import Data.Map (Map)",
            "import Data.Set (Set)",
        ]
