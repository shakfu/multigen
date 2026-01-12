"""Haskell code element factory."""

from typing import Optional

from ..base import AbstractFactory


class HaskellFactory(AbstractFactory):
    """Factory for creating Haskell code elements."""

    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create Haskell variable declaration."""
        if value is not None:
            return f"{name} :: {type_name}\n{name} = {value}"
        return f"{name} :: {type_name}"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create Haskell function signature."""
        if params:
            param_types = [param_type for _, param_type in params]
            all_types = param_types + [return_type] if return_type and return_type != "void" else param_types + ["()"]
            return f"{name} :: " + " -> ".join(all_types)
        else:
            ret_type = return_type if return_type and return_type != "void" else "()"
            return f"{name} :: {ret_type}"

    def create_comment(self, text: str) -> str:
        """Create Haskell comment."""
        if "\n" in text:
            lines = text.split("\n")
            if len(lines) > 1:
                # Multi-line comment
                return "{-\n" + "\n".join(lines) + "\n-}"
            else:
                return f"-- {text}"
        return f"-- {text}"

    def create_include(self, library: str) -> str:
        """Create Haskell import statement."""
        return f"import {library}"
