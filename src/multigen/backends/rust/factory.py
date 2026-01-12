"""Rust code element factory."""

from typing import Optional

from ..base import AbstractFactory


class RustFactory(AbstractFactory):
    """Factory for creating Rust code elements."""

    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create Rust variable declaration."""
        if value is not None:
            return f"let {name}: {type_name} = {value};"
        return f"let {name}: {type_name};"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create Rust function signature."""
        param_strs = []
        for param_name, param_type in params:
            param_strs.append(f"{param_name}: {param_type}")

        params_str = ", ".join(param_strs)

        if return_type and return_type != "void":
            return f"fn {name}({params_str}) -> {return_type}"
        return f"fn {name}({params_str})"

    def create_comment(self, text: str) -> str:
        """Create Rust comment."""
        if "\n" in text:
            lines = text.split("\n")
            comment_lines = [f"// {line}" for line in lines]
            return "\n".join(comment_lines)
        return f"// {text}"

    def create_include(self, library: str) -> str:
        """Create Rust use statement."""
        return f"use {library};"
