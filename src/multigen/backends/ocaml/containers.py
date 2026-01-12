"""OCaml container system for MultiGen."""

from typing import Any, Optional

from ..base import AbstractContainerSystem
from ..preferences import BackendPreferences, OCamlPreferences


class OCamlContainerSystem(AbstractContainerSystem):
    """Container system for OCaml using standard library collections."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the OCaml container system with preferences."""
        self.preferences = preferences or OCamlPreferences()

    def get_list_type(self, element_type: str) -> str:
        """Get OCaml list type declaration."""
        ocaml_type = self._map_python_type_to_ocaml(element_type)
        return f"{ocaml_type} list"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get OCaml map type declaration."""
        ocaml_key_type = self._map_python_type_to_ocaml(key_type)
        ocaml_value_type = self._map_python_type_to_ocaml(value_type)

        # Use Map module for dictionaries
        if self.preferences.get("hashtables") == "stdlib":
            return f"({ocaml_key_type}, {ocaml_value_type}) Hashtbl.t"
        else:
            # Default to Map for functional approach
            return f"{ocaml_value_type} Map.Make(struct type t = {ocaml_key_type} let compare = compare end).t"

    def get_set_type(self, element_type: str) -> str:
        """Get OCaml set type declaration."""
        ocaml_type = self._map_python_type_to_ocaml(element_type)
        return f"{ocaml_type} Set.Make(struct type t = {ocaml_type} let compare = compare end).t"

    def get_list_operations(self) -> dict[str, str]:
        """Get OCaml list operation mappings."""
        if self.preferences.get("list_operations") == "functional":
            return {
                "append": "List.append",
                "length": "List.length",
                "map": "List.map",
                "filter": "List.filter",
                "fold": "List.fold_left",
                "reverse": "List.rev",
                "concat": "List.concat",
                "head": "List.hd",
                "tail": "List.tl",
            }
        else:
            # Use runtime functions for consistency
            return {
                "append": "@",  # List concatenation operator
                "length": "len_list",
                "map": "list_comprehension",
                "filter": "list_comprehension_with_filter",
                "fold": "List.fold_left",
                "reverse": "List.rev",
                "concat": "List.concat",
                "head": "List.hd",
                "tail": "List.tl",
            }

    def get_dict_operations(self) -> dict[str, str]:
        """Get OCaml dictionary operation mappings."""
        if self.preferences.get("hashtables") == "stdlib":
            return {
                "create": "Hashtbl.create",
                "add": "Hashtbl.add",
                "find": "Hashtbl.find",
                "remove": "Hashtbl.remove",
                "mem": "Hashtbl.mem",
                "length": "Hashtbl.length",
                "iter": "Hashtbl.iter",
                "fold": "Hashtbl.fold",
            }
        else:
            # Map module operations
            return {
                "create": "Map.empty",
                "add": "Map.add",
                "find": "Map.find",
                "remove": "Map.remove",
                "mem": "Map.mem",
                "length": "Map.cardinal",
                "iter": "Map.iter",
                "fold": "Map.fold",
            }

    def get_set_operations(self) -> dict[str, str]:
        """Get OCaml set operation mappings."""
        return {
            "create": "Set.empty",
            "add": "Set.add",
            "remove": "Set.remove",
            "mem": "Set.mem",
            "union": "Set.union",
            "inter": "Set.inter",
            "diff": "Set.diff",
            "subset": "Set.subset",
            "length": "Set.cardinal",
            "iter": "Set.iter",
            "fold": "Set.fold",
        }

    def generate_list_literal(self, elements: list, element_type: str) -> str:
        """Generate OCaml list literal."""
        if not elements:
            return "[]"

        # Convert elements to OCaml syntax
        ocaml_elements = []
        for element in elements:
            if isinstance(element, str) and element_type in ["str", "string"]:
                ocaml_elements.append(f'"{element}"')
            else:
                ocaml_elements.append(str(element))

        return f"[{'; '.join(ocaml_elements)}]"

    def generate_dict_literal(self, items: dict[Any, Any], key_type: str, value_type: str) -> str:
        """Generate OCaml dictionary literal."""
        if not items:
            if self.preferences.get("hashtables") == "stdlib":
                return "Hashtbl.create 16"
            else:
                return "Map.empty"

        # For non-empty dictionaries, we need to build them step by step
        if self.preferences.get("hashtables") == "stdlib":
            # Hashtbl approach
            pairs = []
            for key, value in items.items():
                key_str = f'"{key}"' if key_type in ["str", "string"] else str(key)
                value_str = f'"{value}"' if value_type in ["str", "string"] else str(value)
                pairs.append(f"({key_str}, {value_str})")
            return f"let dict = Hashtbl.create {len(items)} in List.iter (fun (k, v) -> Hashtbl.add dict k v) [{'; '.join(pairs)}]; dict"
        else:
            # Map approach
            pairs = []
            for key, value in items.items():
                key_str = f'"{key}"' if key_type in ["str", "string"] else str(key)
                value_str = f'"{value}"' if value_type in ["str", "string"] else str(value)
                pairs.append(f"Map.add {key_str} {value_str}")
            return f"({' ('.join(pairs)} Map.empty{')' * len(pairs)})"

    def generate_set_literal(self, elements: list, element_type: str) -> str:
        """Generate OCaml set literal."""
        if not elements:
            return "Set.empty"

        # Convert elements and build set
        ocaml_elements = []
        for element in elements:
            if isinstance(element, str) and element_type in ["str", "string"]:
                ocaml_elements.append(f'"{element}"')
            else:
                ocaml_elements.append(str(element))

        adds = " ".join(f"Set.add {elem}" for elem in ocaml_elements)
        return f"({adds} Set.empty{')' * len(ocaml_elements)})"

    def _map_python_type_to_ocaml(self, python_type: str) -> str:
        """Map Python type to OCaml type."""
        type_mapping = {
            "int": "int",
            "float": "float",
            "str": "string",
            "string": "string",
            "bool": "bool",
            "list": "list",
            "dict": "map",
            "set": "set",
            "tuple": "tuple",
            "None": "unit",
            "Any": "any",
        }
        return type_mapping.get(python_type, python_type)

    def get_include_headers(self) -> list:
        """Get required include headers for OCaml."""
        # OCaml doesn't use include headers like C/C++
        # Modules are opened directly in the source
        return []

    def get_container_includes(self, container_types: set) -> list:
        """Get specific container module opens for OCaml."""
        includes = []

        if any("list" in t for t in container_types):
            includes.append("open List")

        if any("map" in t.lower() or "dict" in t.lower() for t in container_types):
            if self.preferences.get("hashtables") == "stdlib":
                includes.append("open Hashtbl")
            else:
                includes.append("module StringMap = Map.Make(String)")
                includes.append("module IntMap = Map.Make(struct type t = int let compare = compare end)")

        if any("set" in t.lower() for t in container_types):
            includes.append("module StringSet = Set.Make(String)")
            includes.append("module IntSet = Set.Make(struct type t = int let compare = compare end)")

        return includes

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate container-specific operations code."""
        if "list" in container_type.lower():
            ops = self.get_list_operations()
        elif "dict" in container_type.lower() or "map" in container_type.lower():
            ops = self.get_dict_operations()
        elif "set" in container_type.lower():
            ops = self.get_set_operations()
        else:
            return ""

        code_lines = []
        for operation in operations:
            if operation in ops:
                code_lines.append(f"(* {operation}: {ops[operation]} *)")

        return "\n".join(code_lines)

    def get_required_imports(self) -> list[str]:
        """Get imports required for container operations."""
        imports = []

        if self.preferences.get("hashtables") == "stdlib":
            imports.append("open Hashtbl")
        else:
            imports.append("module StringMap = Map.Make(String)")

        imports.extend(["open List", "module StringSet = Set.Make(String)"])

        return imports
