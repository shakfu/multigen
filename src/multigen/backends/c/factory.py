"""C code element factory for generating clean C code."""

from typing import Optional

from ..base import AbstractFactory


class CFactory(AbstractFactory):
    """Factory for creating C code elements."""

    def __init__(self) -> None:
        """Initialize factory."""
        pass

    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create C variable declaration."""
        if value is not None:
            return f"{type_name} {name} = {value};"
        return f"{type_name} {name};"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create C function signature."""
        param_strs = []
        for param_name, param_type in params:
            param_strs.append(f"{param_type} {param_name}")

        params_str = ", ".join(param_strs) if param_strs else "void"
        return f"{return_type} {name}({params_str})"

    def create_comment(self, text: str) -> str:
        """Create C comment."""
        if "\n" in text:
            lines = text.split("\n")
            comment_lines = ["/*"] + [f" * {line}" for line in lines] + [" */"]
            return "\n".join(comment_lines)
        return f"/* {text} */"

    def create_include(self, library: str) -> str:
        """Create C include statement."""
        if library.startswith("<") and library.endswith(">"):
            return f"#include {library}"
        elif library.startswith('"') and library.endswith('"'):
            return f"#include {library}"
        else:
            # Assume system header
            return f"#include <{library}>"

    def create_function(self, name: str, params: list[str], return_type: str, body: str) -> str:
        """Create complete C function."""
        params_str = ", ".join(params) if params else "void"
        return f"{return_type} {name}({params_str}) {{\n{body}\n}}"

    def create_struct(self, name: str, fields: list[tuple]) -> str:
        """Create C struct definition."""
        field_strs = []
        for field_name, field_type in fields:
            field_strs.append(f"    {field_type} {field_name};")

        fields_str = "\n".join(field_strs)
        return f"struct {name} {{\n{fields_str}\n}};"

    def create_array_declaration(self, name: str, element_type: str, size: Optional[int] = None) -> str:
        """Create C array declaration."""
        if size is not None:
            return f"{element_type} {name}[{size}];"
        return f"{element_type} *{name};"
