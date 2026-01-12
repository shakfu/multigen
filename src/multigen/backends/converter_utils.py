"""Common converter utilities for reducing code duplication across backends.

This module provides utility functions and mixins that can be used by all backend
converters to reduce code duplication while maintaining language-specific flexibility.
"""

import ast
from typing import Any, Optional

# ============================================================================
# Common AST Analysis Utilities
# ============================================================================


def uses_comprehensions(node: ast.Module) -> bool:
    """Check if module uses any comprehensions.

    Args:
        node: AST Module node

    Returns:
        True if module contains list/dict/set comprehensions
    """
    for child in ast.walk(node):
        if isinstance(child, (ast.ListComp, ast.DictComp, ast.SetComp)):
            return True
    return False


def uses_classes(node: ast.Module) -> bool:
    """Check if module defines any classes.

    Args:
        node: AST Module node

    Returns:
        True if module contains class definitions
    """
    for item in node.body:
        if isinstance(item, ast.ClassDef):
            return True
    return False


def uses_string_methods(node: ast.Module) -> set[str]:
    """Detect which string methods are used in the module.

    Args:
        node: AST Module node

    Returns:
        Set of string method names used (upper, lower, strip, etc.)
    """
    methods = set()
    for child in ast.walk(node):
        if isinstance(child, ast.Call) and isinstance(child.func, ast.Attribute):
            method_name = child.func.attr
            if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                methods.add(method_name)
    return methods


def uses_builtin_functions(node: ast.Module) -> set[str]:
    """Detect which built-in functions are used in the module.

    Args:
        node: AST Module node

    Returns:
        Set of built-in function names used
    """
    builtins = set()
    for child in ast.walk(node):
        if isinstance(child, ast.Call) and isinstance(child.func, ast.Name):
            func_name = child.func.id
            if func_name in ["abs", "len", "min", "max", "sum", "bool", "str", "range", "print"]:
                builtins.add(func_name)
    return builtins


def extract_instance_variables(class_node: ast.ClassDef) -> dict[str, Optional[ast.expr]]:
    """Extract instance variables from a class __init__ method.

    Args:
        class_node: AST ClassDef node

    Returns:
        Dictionary mapping variable names to their type annotations (or None if not annotated)
    """
    variables: dict[str, Optional[ast.expr]] = {}

    # Find __init__ method
    for item in class_node.body:
        if isinstance(item, ast.FunctionDef) and item.name == "__init__":
            # Look for self.x assignments
            for stmt in item.body:
                if isinstance(stmt, ast.Assign):
                    for target in stmt.targets:
                        if isinstance(target, ast.Attribute) and isinstance(target.value, ast.Name):
                            if target.value.id == "self":
                                variables[target.attr] = None

                elif isinstance(stmt, ast.AnnAssign):
                    if isinstance(stmt.target, ast.Attribute) and isinstance(stmt.target.value, ast.Name):
                        if stmt.target.value.id == "self":
                            var_name = stmt.target.attr
                            # Store annotation for type inference
                            variables[var_name] = stmt.annotation

    return variables


def extract_methods(class_node: ast.ClassDef) -> list[ast.FunctionDef]:
    """Extract methods from a class (excluding __init__).

    Args:
        class_node: AST ClassDef node

    Returns:
        List of FunctionDef nodes for methods
    """
    methods = []
    for item in class_node.body:
        if isinstance(item, ast.FunctionDef) and item.name != "__init__":
            methods.append(item)
    return methods


# ============================================================================
# Common Type Inference Utilities
# ============================================================================


def infer_basic_type_from_constant(value: Any) -> Optional[str]:
    """Infer Python type from constant value.

    Args:
        value: Python constant value

    Returns:
        Type name string or None if cannot infer
    """
    if isinstance(value, bool):
        return "bool"
    elif isinstance(value, int):
        return "int"
    elif isinstance(value, float):
        return "float"
    elif isinstance(value, str):
        return "str"
    elif value is None:
        return "None"
    return None


def infer_type_from_ast_node(node: ast.expr) -> Optional[str]:
    """Infer Python type from AST expression node.

    Args:
        node: AST expression node

    Returns:
        Type name string or None if cannot infer
    """
    if isinstance(node, ast.Constant):
        return infer_basic_type_from_constant(node.value)
    elif isinstance(node, ast.List):
        return "list"
    elif isinstance(node, ast.Dict):
        return "dict"
    elif isinstance(node, ast.Set):
        return "set"
    elif isinstance(node, ast.ListComp):
        return "list"
    elif isinstance(node, ast.DictComp):
        return "dict"
    elif isinstance(node, ast.SetComp):
        return "set"
    return None


# ============================================================================
# Common Operator Mapping Utilities
# ============================================================================


# Standard operator mappings that are common across C-family languages
STANDARD_BINARY_OPERATORS = {
    ast.Add: "+",
    ast.Sub: "-",
    ast.Mult: "*",
    ast.Div: "/",
    ast.Mod: "%",
    ast.BitOr: "|",
    ast.BitXor: "^",
    ast.BitAnd: "&",
    ast.LShift: "<<",
    ast.RShift: ">>",
}

STANDARD_UNARY_OPERATORS = {
    ast.UAdd: "+",
    ast.USub: "-",
    ast.Not: "!",
    ast.Invert: "~",
}

STANDARD_COMPARISON_OPERATORS = {
    ast.Eq: "==",
    ast.NotEq: "!=",
    ast.Lt: "<",
    ast.LtE: "<=",
    ast.Gt: ">",
    ast.GtE: ">=",
}


def get_standard_binary_operator(op: ast.operator) -> Optional[str]:
    """Get standard binary operator symbol for C-family languages.

    Args:
        op: AST operator node

    Returns:
        Operator symbol or None if not standard
    """
    return STANDARD_BINARY_OPERATORS.get(type(op))


def get_standard_unary_operator(op: ast.unaryop) -> Optional[str]:
    """Get standard unary operator symbol for C-family languages.

    Args:
        op: AST unary operator node

    Returns:
        Operator symbol or None if not standard
    """
    return STANDARD_UNARY_OPERATORS.get(type(op))


def get_standard_comparison_operator(op: ast.cmpop) -> Optional[str]:
    """Get standard comparison operator symbol for C-family languages.

    Args:
        op: AST comparison operator node

    Returns:
        Operator symbol or None if not standard
    """
    return STANDARD_COMPARISON_OPERATORS.get(type(op))


# ============================================================================
# Common String Utilities
# ============================================================================


def escape_string_for_c_family(s: str) -> str:
    """Escape string for C-family language string literals.

    Args:
        s: Input string

    Returns:
        Escaped string suitable for C/C++/Java/Go/Rust string literals
    """
    return s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")


def to_snake_case(camel_str: str) -> str:
    """Convert CamelCase or mixedCase to snake_case.

    Args:
        camel_str: CamelCase or mixedCase string

    Returns:
        snake_case string
    """
    import re

    # Insert underscore before uppercase letters
    result = re.sub("(.)([A-Z][a-z]+)", r"\1_\2", camel_str)
    result = re.sub("([a-z0-9])([A-Z])", r"\1_\2", result)
    return result.lower()


def to_camel_case(snake_str: str) -> str:
    """Convert snake_case to CamelCase.

    Args:
        snake_str: snake_case string

    Returns:
        CamelCase string
    """
    components = snake_str.split("_")
    return "".join(word.capitalize() for word in components)


def to_mixed_case(snake_str: str) -> str:
    """Convert snake_case to mixedCase (camelCase with lowercase first letter).

    Args:
        snake_str: snake_case string

    Returns:
        mixedCase string
    """
    if not snake_str:
        return snake_str

    components = snake_str.split("_")
    if len(components) == 1:
        return snake_str.lower()

    return components[0].lower() + "".join(word.capitalize() for word in components[1:])


# ============================================================================
# Common Default Value Utilities
# ============================================================================


# Default values for common types across languages
COMMON_DEFAULT_VALUES = {
    "int": "0",
    "float": "0.0",
    "bool": "false",
    "str": '""',
}


def get_numeric_default(type_name: str) -> Optional[str]:
    """Get default value for numeric types.

    Args:
        type_name: Type name

    Returns:
        Default value or None if not numeric
    """
    if "int" in type_name.lower():
        return "0"
    elif "float" in type_name.lower() or "double" in type_name.lower():
        return "0.0"
    return None


# ============================================================================
# Common Augmented Assignment Operator Mapping
# ============================================================================


AUGMENTED_ASSIGNMENT_OPERATORS = {
    ast.Add: "+=",
    ast.Sub: "-=",
    ast.Mult: "*=",
    ast.Div: "/=",
    ast.Mod: "%=",
    ast.BitOr: "|=",
    ast.BitXor: "^=",
    ast.BitAnd: "&=",
    ast.LShift: "<<=",
    ast.RShift: ">>=",
}


def get_augmented_assignment_operator(op: ast.operator) -> Optional[str]:
    """Get augmented assignment operator symbol.

    Args:
        op: AST operator node

    Returns:
        Augmented assignment operator (+=, -=, etc.) or None
    """
    return AUGMENTED_ASSIGNMENT_OPERATORS.get(type(op))
