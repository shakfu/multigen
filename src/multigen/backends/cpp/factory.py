"""C++ code element factory."""

from typing import Any, Optional

from ..base import AbstractFactory


class CppFactory(AbstractFactory):
    """Factory for creating C++ code elements."""

    def create_function(self, name: str, params: list[str], return_type: str, body: str) -> str:
        """Create a C++ function."""
        param_str = ", ".join(params)
        return f"{return_type} {name}({param_str}) {{\n{body}\n}}"

    def create_variable(self, name: str, var_type: str, value: Optional[str] = None) -> str:
        """Create a C++ variable declaration."""
        if value is not None:
            return f"{var_type} {name} = {value};"
        return f"{var_type} {name};"

    def create_parameter(self, name: str, param_type: str) -> str:
        """Create a C++ function parameter."""
        return f"{param_type} {name}"

    def create_return_statement(self, value: str) -> str:
        """Create a C++ return statement."""
        return f"return {value};"

    def create_binary_op(self, left: str, operator: str, right: str) -> str:
        """Create a C++ binary operation."""
        return f"({left} {operator} {right})"

    def create_function_call(self, name: str, args: list[str]) -> str:
        """Create a C++ function call."""
        args_str = ", ".join(args)
        return f"{name}({args_str})"

    def create_if_statement(self, condition: str, then_body: str, else_body: Optional[str] = None) -> str:
        """Create a C++ if statement."""
        result = f"if ({condition}) {{\n{then_body}\n}}"
        if else_body:
            result += f" else {{\n{else_body}\n}}"
        return result

    def create_while_loop(self, condition: str, body: str) -> str:
        """Create a C++ while loop."""
        return f"while ({condition}) {{\n{body}\n}}"

    def create_for_loop(self, init: str, condition: str, update: str, body: str) -> str:
        """Create a C++ for loop."""
        return f"for ({init}; {condition}; {update}) {{\n{body}\n}}"

    def create_assignment(self, target: str, value: str) -> str:
        """Create a C++ assignment."""
        return f"{target} = {value};"

    def create_literal(self, value: Any, literal_type: str) -> str:
        """Create a C++ literal."""
        if literal_type == "string":
            return f'"{value}"'
        elif literal_type == "char":
            return f"'{value}'"
        elif literal_type == "bool":
            return "true" if value else "false"
        elif literal_type == "float":
            return f"{value}f"
        else:
            return str(value)

    def create_comment(self, text: str, block: bool = False) -> str:
        """Create a C++ comment."""
        if block:
            return f"/* {text} */"
        return f"// {text}"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create a C++ function signature."""
        param_strings = []
        for param_name, param_type in params:
            param_strings.append(f"{param_type} {param_name}")
        param_str = ", ".join(param_strings)
        return f"{return_type} {name}({param_str})"

    def create_include(self, header: str, system: bool = True) -> str:
        """Create a C++ include directive."""
        if system:
            return f"#include <{header}>"
        return f'#include "{header}"'

    def create_namespace(self, name: str, body: str) -> str:
        """Create a C++ namespace."""
        return f"namespace {name} {{\n{body}\n}}"

    def create_class(self, name: str, body: str, base_classes: Optional[list[str]] = None) -> str:
        """Create a C++ class."""
        inheritance = ""
        if base_classes:
            inheritance = f" : {', '.join(f'public {base}' for base in base_classes)}"
        return f"class {name}{inheritance} {{\n{body}\n}};"

    def create_struct(self, name: str, body: str) -> str:
        """Create a C++ struct."""
        return f"struct {name} {{\n{body}\n}};"

    def get_type_mapping(self) -> dict:
        """Get Python to C++ type mappings."""
        return {
            "int": "int",
            "float": "double",
            "str": "std::string",
            "bool": "bool",
            "list": "std::vector",
            "dict": "std::map",
            "set": "std::set",
            "tuple": "std::tuple",
            "None": "void",
        }
