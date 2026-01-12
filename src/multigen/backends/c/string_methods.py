"""String method handling for C backend.

This module contains utilities for converting Python string methods
and f-strings to C code using the multigen runtime library.
"""

from __future__ import annotations

import ast
from typing import TYPE_CHECKING

from ..errors import UnsupportedFeatureError

if TYPE_CHECKING:
    from typing import Callable


class CStringMethodConverter:
    """Handles conversion of Python string methods to C code.

    This class encapsulates string-related conversions including:
    - String methods (upper, lower, strip, find, replace, split)
    - F-string interpolation
    - Type conversions for string formatting

    Usage:
        converter = CStringMethodConverter()
        c_code = converter.convert_string_method("text", "upper", [])
        # Returns: "multigen_str_upper(text)"
    """

    # String methods that return a new string
    STRING_RETURNING_METHODS = {"upper", "lower", "strip", "replace", "lstrip", "rstrip"}

    # String methods that return other types
    STRING_OTHER_METHODS = {"find", "split"}

    # All supported string methods
    SUPPORTED_METHODS = STRING_RETURNING_METHODS | STRING_OTHER_METHODS

    def __init__(self) -> None:
        """Initialize the string method converter."""
        self.includes_needed: set[str] = set()

    def convert_string_method(self, obj: str, method_name: str, args: list[str]) -> str:
        """Convert string method calls to appropriate C code.

        Args:
            obj: The C expression for the string object
            method_name: The Python method name (e.g., "upper", "split")
            args: List of converted argument expressions

        Returns:
            C code that calls the appropriate multigen runtime function

        Raises:
            UnsupportedFeatureError: If the method is not supported or
                                     has incorrect argument count
        """
        self.includes_needed.add('#include "multigen_string_ops.h"')

        if method_name == "upper":
            if args:
                raise UnsupportedFeatureError("str.upper() takes no arguments")
            return f"multigen_str_upper({obj})"

        elif method_name == "lower":
            if args:
                raise UnsupportedFeatureError("str.lower() takes no arguments")
            return f"multigen_str_lower({obj})"

        elif method_name == "strip":
            if len(args) == 0:
                return f"multigen_str_strip({obj})"
            elif len(args) == 1:
                return f"multigen_str_strip_chars({obj}, {args[0]})"
            else:
                raise UnsupportedFeatureError("str.strip() takes at most one argument")

        elif method_name == "lstrip":
            if len(args) == 0:
                return f"multigen_str_lstrip({obj})"
            elif len(args) == 1:
                return f"multigen_str_lstrip_chars({obj}, {args[0]})"
            else:
                raise UnsupportedFeatureError("str.lstrip() takes at most one argument")

        elif method_name == "rstrip":
            if len(args) == 0:
                return f"multigen_str_rstrip({obj})"
            elif len(args) == 1:
                return f"multigen_str_rstrip_chars({obj}, {args[0]})"
            else:
                raise UnsupportedFeatureError("str.rstrip() takes at most one argument")

        elif method_name == "find":
            if len(args) != 1:
                raise UnsupportedFeatureError("str.find() requires exactly one argument")
            return f"multigen_str_find({obj}, {args[0]})"

        elif method_name == "replace":
            if len(args) != 2:
                raise UnsupportedFeatureError("str.replace() requires exactly two arguments")
            return f"multigen_str_replace({obj}, {args[0]}, {args[1]})"

        elif method_name == "split":
            if len(args) == 0:
                return f"multigen_str_split({obj}, NULL)"
            elif len(args) == 1:
                return f"multigen_str_split({obj}, {args[0]})"
            else:
                raise UnsupportedFeatureError("str.split() takes at most one argument")

        else:
            raise UnsupportedFeatureError(f"Unsupported string method: {method_name}")

    def is_string_returning_method(self, method_name: str) -> bool:
        """Check if a string method returns a string.

        Args:
            method_name: The method name to check

        Returns:
            True if the method returns a string, False otherwise
        """
        return method_name in self.STRING_RETURNING_METHODS

    def is_supported_method(self, method_name: str) -> bool:
        """Check if a string method is supported.

        Args:
            method_name: The method name to check

        Returns:
            True if the method is supported, False otherwise
        """
        return method_name in self.SUPPORTED_METHODS


class CFStringConverter:
    """Handles conversion of Python f-strings to C code.

    Converts Python f-string expressions like f"Value: {x}" to C code
    using multigen_sprintf_string and related runtime functions.
    """

    def __init__(self, expression_converter: Callable[[ast.expr], str]) -> None:
        """Initialize the f-string converter.

        Args:
            expression_converter: A callable that converts AST expressions to C code
        """
        self._convert_expression = expression_converter
        self.includes_needed: set[str] = set()

    def convert_f_string(self, expr: ast.JoinedStr) -> str:
        """Convert f-string to C string concatenation using multigen_string_concat.

        Example:
            f"Result: {x}" -> multigen_sprintf_string("Result: %s", multigen_int_to_string(x))
            f"Count: {len(items)}" -> multigen_sprintf_string("Count: %s", ...)

        Args:
            expr: The AST JoinedStr node representing the f-string

        Returns:
            C code that produces the interpolated string
        """
        self.includes_needed.add('#include "multigen_string_ops.h"')

        # Count format string placeholders and collect arguments
        format_parts: list[str] = []
        args: list[str] = []

        for value in expr.values:
            if isinstance(value, ast.Constant):
                # Literal string part
                if isinstance(value.value, str):
                    format_parts.append(value.value)
            elif isinstance(value, ast.FormattedValue):
                # Expression - add placeholder
                format_parts.append("%s")
                expr_code = self._convert_expression(value.value)
                # Wrap in appropriate conversion based on type
                args.append(self._to_c_string_expr(expr_code, value.value))

        format_string = "".join(format_parts)

        if len(args) == 0:
            # No interpolation, just return the string
            return f'"{format_string}"'
        else:
            # Use multigen_sprintf_string helper (needs to be in runtime)
            args_str = ", ".join(args)
            return f'multigen_sprintf_string("{format_string}", {args_str})'

    def _to_c_string_expr(self, expr_code: str, node: ast.expr) -> str:
        """Wrap an expression for string formatting in C.

        Returns an expression that can be used as a %s argument to sprintf.
        For non-strings, we need to convert them first.

        Args:
            expr_code: The C code for the expression
            node: The AST node for type inference

        Returns:
            C expression suitable for use with %s format specifier
        """
        # Check if it's a string literal or variable
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return expr_code  # Already a string

        if isinstance(node, ast.Name):
            var_name = node.id.lower()
            # Heuristic: check if variable name suggests it's a string
            string_hints = ["name", "text", "str", "msg", "message", "path", "file", "word", "line"]
            if any(substr in var_name for substr in string_hints):
                return expr_code  # Assume string type

        # For other types, we need type conversion
        # Default to integer conversion
        return f"multigen_int_to_string({expr_code})"


def get_string_method_result_type(method_name: str) -> str:
    """Get the C type for the result of a string method.

    Args:
        method_name: The string method name

    Returns:
        The C type name for the method's return value
    """
    if method_name == "split":
        return "multigen_string_array_t*"
    elif method_name == "find":
        return "int"
    elif method_name in CStringMethodConverter.STRING_RETURNING_METHODS:
        return "char*"
    else:
        return "char*"  # Default


def is_string_method(method_name: str) -> bool:
    """Check if a method name is a known string method.

    Args:
        method_name: The method name to check

    Returns:
        True if this is a known string method
    """
    return method_name in CStringMethodConverter.SUPPORTED_METHODS
