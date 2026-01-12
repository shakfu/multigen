"""OCaml backend factory for MultiGen."""

from typing import Optional

from ..base import AbstractFactory
from ..preferences import BackendPreferences, OCamlPreferences


class OCamlFactory(AbstractFactory):
    """Factory for creating OCaml backend components."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the OCaml factory with preferences."""
        self.preferences = preferences or OCamlPreferences()

    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create OCaml variable declaration."""
        ocaml_type = self._map_python_type_to_ocaml(type_name)
        if value:
            return f"let {name} : {ocaml_type} = {value}"
        else:
            return f"let {name} : {ocaml_type}"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create OCaml function signature."""
        ocaml_return_type = self._map_python_type_to_ocaml(return_type)

        if not params:
            return f"let {name} () : {ocaml_return_type}"

        param_types = []
        for _param_name, param_type in params:
            ocaml_param_type = self._map_python_type_to_ocaml(param_type)
            param_types.append(ocaml_param_type)

        type_signature = " -> ".join(param_types + [ocaml_return_type])
        param_names = " ".join(param_name for param_name, _ in params)

        return f"let {name} {param_names} : {type_signature}"

    def create_comment(self, text: str) -> str:
        """Create OCaml comment."""
        return f"(* {text} *)"

    def create_include(self, library: str) -> str:
        """Create OCaml module open statement."""
        return f"open {library}"

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
            "None": "unit",
            "Any": "'a",
        }
        return type_mapping.get(python_type, python_type)

    def get_file_extension(self) -> str:
        """Get the file extension for OCaml files."""
        return ".ml"

    def get_runtime_files(self) -> list:
        """Get the list of runtime files needed for OCaml."""
        return ["multigen_runtime.ml"]
