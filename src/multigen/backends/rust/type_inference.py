"""Rust-specific type inference strategies.

Extends the base type inference system with Rust-specific type formatting,
ownership analysis, and special handling for Rust idioms.
"""

import ast
from typing import TYPE_CHECKING, Callable, Optional

from ..type_inference_strategies import (
    CallInferenceStrategy,
    ComprehensionInferenceStrategy,
    DictInferenceStrategy,
    InferenceContext,
    ListInferenceStrategy,
    NameInferenceStrategy,
    SetInferenceStrategy,
    TypeInferenceStrategy,
)

if TYPE_CHECKING:
    from ..type_inference_strategies import TypeInferenceEngine


class RustNameInferenceStrategy(NameInferenceStrategy):
    """Rust-specific variable name inference with AST fallback.

    Extends base NameInferenceStrategy to support Rust converter's
    _infer_variable_type_from_ast for advanced variable type inference.
    """

    def __init__(
        self,
        ast_inference_func: Optional[Callable[[str, ast.FunctionDef], Optional[str]]] = None,
        current_function_node: Optional[Callable[[], Optional[ast.FunctionDef]]] = None,
    ) -> None:
        """Initialize with optional AST inference support.

        Args:
            ast_inference_func: Function for inferring variable type from AST context
            current_function_node: Callable that returns current function AST node
        """
        self.ast_inference_func = ast_inference_func
        self.current_function_node = current_function_node

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Name), "Expected ast.Name"

        # First try standard variable types lookup
        var_type = context.variable_types.get(value.id)
        if var_type:
            return var_type

        # If not found and AST inference is available, try that
        if self.ast_inference_func and self.current_function_node:
            func_node = self.current_function_node()
            if func_node:
                inferred = self.ast_inference_func(value.id, func_node)
                if inferred:
                    return inferred

        # Fallback to generic type
        return context.type_mapper("int")  # Rust defaults to i32


class RustListInferenceStrategy(ListInferenceStrategy):
    """Rust-specific list type inference with Vec<T> formatting."""

    def _format_list_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as Vec<T>."""
        return f"Vec<{element_type}>"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.List), "Expected ast.List"

        if not value.elts:
            # Empty list - use default i32
            return "Vec<i32>"

        # Use parent implementation for element type inference
        result = super().infer(value, context)

        # If parent returns generic "list", convert to Vec<i32> default
        if result == context.type_mapper("list"):
            return "Vec<i32>"

        return result


class RustDictInferenceStrategy(DictInferenceStrategy):
    """Rust-specific dict type inference with HashMap<K,V> formatting."""

    def _format_dict_type(self, key_type: str, value_type: str, context: InferenceContext) -> str:
        """Format as std::collections::HashMap<K,V>."""
        return f"std::collections::HashMap<{key_type}, {value_type}>"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Dict), "Expected ast.Dict"

        if not value.keys or not value.values:
            # Empty dict - use default i32 keys/values
            return "std::collections::HashMap<i32, i32>"

        # Use parent implementation
        result = super().infer(value, context)

        # If parent returns generic "dict", convert to HashMap<i32, i32> default
        if result == context.type_mapper("dict"):
            return "std::collections::HashMap<i32, i32>"

        return result


class RustSetInferenceStrategy(SetInferenceStrategy):
    """Rust-specific set type inference with HashSet<T> formatting."""

    def _format_set_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as std::collections::HashSet<T>."""
        return f"std::collections::HashSet<{element_type}>"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Set), "Expected ast.Set"

        if not value.elts:
            # Empty set - use default i32
            return "std::collections::HashSet<i32>"

        # Use parent implementation
        result = super().infer(value, context)

        # If parent returns generic "set", convert to HashSet<i32> default
        if result == context.type_mapper("set"):
            return "std::collections::HashSet<i32>"

        return result


class RustComprehensionInferenceStrategy(ComprehensionInferenceStrategy):
    """Rust-specific comprehension type inference with explicit element type extraction."""

    def __init__(self, element_type_inferrer: Optional[Callable[[ast.expr], str]] = None) -> None:
        """Initialize with optional element type inference function.

        Args:
            element_type_inferrer: Function for inferring comprehension element types
        """
        self.element_type_inferrer = element_type_inferrer

    def _infer_list_comp(self, value: ast.ListComp, context: InferenceContext) -> str:
        """Infer type from list comprehension."""
        if self.element_type_inferrer:
            element_type = self.element_type_inferrer(value.elt)
            return f"Vec<{element_type}>"
        elif context.infer_recursively:
            element_type = context.infer_recursively(value.elt)
            return f"Vec<{element_type}>"
        return "Vec<i32>"

    def _infer_dict_comp(self, value: ast.DictComp, context: InferenceContext) -> str:
        """Infer type from dict comprehension."""
        if self.element_type_inferrer:
            key_type = self.element_type_inferrer(value.key)
            value_type = self.element_type_inferrer(value.value)
            return f"std::collections::HashMap<{key_type}, {value_type}>"
        return "std::collections::HashMap<i32, i32>"

    def _infer_set_comp(self, value: ast.SetComp, context: InferenceContext) -> str:
        """Infer type from set comprehension."""
        if self.element_type_inferrer:
            element_type = self.element_type_inferrer(value.elt)
            return f"std::collections::HashSet<{element_type}>"
        return "std::collections::HashSet<i32>"


class RustCallInferenceStrategy(CallInferenceStrategy):
    """Rust-specific call type inference with function return types and struct info."""

    def __init__(
        self,
        function_return_types: Optional[dict[str, str]] = None,
        struct_info: Optional[dict[str, dict]] = None,
    ) -> None:
        """Initialize with Rust converter context.

        Args:
            function_return_types: Mapping of function names to return types
            struct_info: Struct definitions for class types
        """
        self.function_return_types = function_return_types or {}
        self.struct_info = struct_info or {}

    def _infer_from_function(self, func_name: str, context: InferenceContext) -> str:
        """Infer return type from function name (Rust specific)."""
        # Check user-defined function return types
        if func_name in self.function_return_types:
            return self.function_return_types[func_name]

        # Check struct constructors
        if func_name in self.struct_info:
            return func_name

        # Standard built-ins
        if func_name in ["abs", "len", "sum", "min", "max"]:
            return "i32"
        elif func_name == "set":
            return "std::collections::HashSet<i32>"
        elif func_name == "dict":
            return "std::collections::HashMap<i32, i32>"
        else:
            return "i32"  # Default for unknown functions

    def _infer_from_method(self, method_name: str, context: InferenceContext) -> str:
        """Infer return type from method name (Rust specific)."""
        # String methods
        if method_name in ["upper", "lower", "strip", "replace"]:
            return "String"
        # Search methods
        elif method_name == "find":
            return "i32"
        # String split
        elif method_name == "split":
            return "Vec<String>"
        else:
            return "i32"  # Default


class RustBinOpInferenceStrategy(TypeInferenceStrategy):
    """Rust-specific binary operation type inference with type promotion."""

    def can_infer(self, value: ast.expr) -> bool:
        return isinstance(value, ast.BinOp)

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.BinOp), "Expected ast.BinOp"

        if not context.infer_recursively:
            return "i32"

        # Infer types from both operands
        left_type = context.infer_recursively(value.left)
        right_type = context.infer_recursively(value.right)

        # If both are same type, return that type
        if left_type == right_type:
            return left_type

        # Type promotion: if one is float, result is float
        if left_type == "f64" or right_type == "f64":
            return "f64"

        # Default to i32 for mixed int operations
        return "i32"


class RustSubscriptInferenceStrategy(TypeInferenceStrategy):
    """Rust-specific subscript type inference for extracting element types."""

    def can_infer(self, value: ast.expr) -> bool:
        return isinstance(value, ast.Subscript)

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Subscript), "Expected ast.Subscript"

        # Try to infer element/value type from container
        if isinstance(value.value, ast.Name):
            container_name = value.value.id
            if container_name in context.variable_types:
                container_type = context.variable_types[container_name]

                # Extract element type from Vec<T>
                if container_type.startswith("Vec<"):
                    return container_type[4:-1]  # Extract T

                # Extract value type from HashMap<K, V>
                elif container_type.startswith("std::collections::HashMap<"):
                    inner = container_type[26:-1]  # Remove prefix and ">"
                    parts = inner.split(", ", 1)
                    if len(parts) == 2:
                        return parts[1]  # Return value type

        # Default to i32 - can't infer from unknown container
        return "i32"


def create_rust_type_inference_engine(
    converter: "MultiGenPythonToRustConverter",  # type: ignore[name-defined]
) -> "TypeInferenceEngine":
    """Create TypeInferenceEngine configured for Rust.

    Args:
        converter: Reference to MultiGenPythonToRustConverter for context access

    Returns:
        TypeInferenceEngine with Rust-specific strategies
    """
    from ..type_inference_strategies import ConstantInferenceStrategy, TypeInferenceEngine

    strategies = [
        ConstantInferenceStrategy(),
        RustNameInferenceStrategy(
            ast_inference_func=converter._infer_variable_type_from_ast,
            current_function_node=lambda: converter.current_function_node,
        ),
        RustListInferenceStrategy(),
        RustDictInferenceStrategy(),
        RustSetInferenceStrategy(),
        RustSubscriptInferenceStrategy(),
        RustBinOpInferenceStrategy(),
        RustComprehensionInferenceStrategy(element_type_inferrer=converter._infer_comprehension_element_type),
        RustCallInferenceStrategy(
            function_return_types=converter.function_return_types,
            struct_info=converter.struct_info,
        ),
    ]

    return TypeInferenceEngine(strategies)


__all__ = [
    "RustNameInferenceStrategy",
    "RustListInferenceStrategy",
    "RustDictInferenceStrategy",
    "RustSetInferenceStrategy",
    "RustComprehensionInferenceStrategy",
    "RustCallInferenceStrategy",
    "RustBinOpInferenceStrategy",
    "RustSubscriptInferenceStrategy",
    "create_rust_type_inference_engine",
]
