"""Go-specific type inference strategies.

Extends the base type inference system with Go-specific type formatting
and special handling for Go idioms like interface{}.
"""

import ast
from typing import TYPE_CHECKING, Callable, Optional

from ..type_inference_strategies import (
    CallInferenceStrategy,
    ComprehensionInferenceStrategy,
    DictInferenceStrategy,
    InferenceContext,
    ListInferenceStrategy,
    SetInferenceStrategy,
)

if TYPE_CHECKING:
    from ..type_inference_strategies import TypeInferenceEngine


class GoListInferenceStrategy(ListInferenceStrategy):
    """Go-specific list type inference with []T formatting."""

    def _format_list_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as []T."""
        return f"[]{element_type}"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.List), "Expected ast.List"

        if not value.elts:
            # Empty list - use default int
            return "[]int"

        # Use parent implementation for element type inference
        result = super().infer(value, context)

        # If parent returns generic "list", convert to []int default
        if result == context.type_mapper("list"):
            return "[]int"

        return result


class GoDictInferenceStrategy(DictInferenceStrategy):
    """Go-specific dict type inference with map[K]V formatting."""

    def _format_dict_type(self, key_type: str, value_type: str, context: InferenceContext) -> str:
        """Format as map[K]V."""
        return f"map[{key_type}]{value_type}"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Dict), "Expected ast.Dict"

        if not value.keys or not value.values:
            # Empty dict - use default int keys/values
            return "map[int]int"

        # Use parent implementation
        result = super().infer(value, context)

        # If parent returns generic "dict", convert to map[int]int default
        if result == context.type_mapper("dict"):
            return "map[int]int"

        return result


class GoSetInferenceStrategy(SetInferenceStrategy):
    """Go-specific set type inference with map[T]bool formatting."""

    def _format_set_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as map[T]bool (Go doesn't have native sets)."""
        return f"map[{element_type}]bool"

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.Set), "Expected ast.Set"

        if not value.elts:
            # Empty set - use default int
            return "map[int]bool"

        # Use parent implementation
        result = super().infer(value, context)

        # If parent returns generic "set", convert to map[int]bool default
        if result == context.type_mapper("set"):
            return "map[int]bool"

        return result


class GoComprehensionInferenceStrategy(ComprehensionInferenceStrategy):
    """Go-specific comprehension type inference with loop variable type inference."""

    def __init__(
        self,
        loop_var_type_inferrer: Optional[Callable[[ast.comprehension], dict[str, str]]] = None,
        element_type_inferrer: Optional[Callable[[ast.expr, dict[str, str]], str]] = None,
    ) -> None:
        """Initialize with Go-specific inference functions.

        Args:
            loop_var_type_inferrer: Function for inferring loop variable types
            element_type_inferrer: Function for inferring element types with loop var context
        """
        self.loop_var_type_inferrer = loop_var_type_inferrer
        self.element_type_inferrer = element_type_inferrer

    def _infer_list_comp(self, value: ast.ListComp, context: InferenceContext) -> str:
        """Infer type from list comprehension."""
        if self.loop_var_type_inferrer and self.element_type_inferrer:
            loop_var_type = self.loop_var_type_inferrer(value.generators[0])
            element_type = self.element_type_inferrer(value.elt, loop_var_type)
            return f"[]{element_type}"
        return "[]int"

    def _infer_dict_comp(self, value: ast.DictComp, context: InferenceContext) -> str:
        """Infer type from dict comprehension."""
        if self.loop_var_type_inferrer and self.element_type_inferrer:
            loop_var_type = self.loop_var_type_inferrer(value.generators[0])
            key_type = self.element_type_inferrer(value.key, loop_var_type)
            value_type = self.element_type_inferrer(value.value, loop_var_type)
            return f"map[{key_type}]{value_type}"
        return "map[int]int"

    def _infer_set_comp(self, value: ast.SetComp, context: InferenceContext) -> str:
        """Infer type from set comprehension."""
        if self.loop_var_type_inferrer and self.element_type_inferrer:
            loop_var_type = self.loop_var_type_inferrer(value.generators[0])
            element_type = self.element_type_inferrer(value.elt, loop_var_type)
            return f"map[{element_type}]bool"
        return "map[int]bool"


class GoCallInferenceStrategy(CallInferenceStrategy):
    """Go-specific call type inference with function return types and struct info."""

    def __init__(
        self,
        function_return_types: Optional[dict[str, str]] = None,
        struct_info: Optional[dict[str, dict]] = None,
    ) -> None:
        """Initialize with Go converter context.

        Args:
            function_return_types: Mapping of function names to return types
            struct_info: Struct definitions for class types
        """
        self.function_return_types = function_return_types or {}
        self.struct_info = struct_info or {}

    def _infer_from_function(self, func_name: str, context: InferenceContext) -> str:
        """Infer return type from function name (Go specific)."""
        # Check user-defined function return types
        if func_name in self.function_return_types:
            return self.function_return_types[func_name]

        # Check struct constructors
        if func_name in self.struct_info:
            return func_name

        # Standard built-ins
        if func_name == "sum":
            return "int"
        else:
            return "interface{}"  # Go's default for unknown types

    def _infer_from_method(self, method_name: str, context: InferenceContext) -> str:
        """Infer return type from method name (Go specific)."""
        # String methods
        if method_name in ["upper", "lower", "strip", "replace", "join"]:
            return "string"
        # String split
        elif method_name == "split":
            return "[]string"
        # Search methods
        elif method_name == "find":
            return "int"
        else:
            return "interface{}"  # Go's default


def create_go_type_inference_engine(
    converter: "MultiGenPythonToGoConverter",  # type: ignore[name-defined]
) -> "TypeInferenceEngine":
    """Create TypeInferenceEngine configured for Go.

    Args:
        converter: Reference to MultiGenPythonToGoConverter for context access

    Returns:
        TypeInferenceEngine with Go-specific strategies
    """
    from ..type_inference_strategies import ConstantInferenceStrategy, NameInferenceStrategy, TypeInferenceEngine

    strategies = [
        ConstantInferenceStrategy(),
        NameInferenceStrategy(),
        GoListInferenceStrategy(),
        GoDictInferenceStrategy(),
        GoSetInferenceStrategy(),
        GoComprehensionInferenceStrategy(
            loop_var_type_inferrer=converter._infer_loop_variable_type,
            element_type_inferrer=converter._infer_comprehension_element_type,
        ),
        GoCallInferenceStrategy(
            function_return_types=converter.function_return_types,
            struct_info=converter.struct_info,
        ),
    ]

    return TypeInferenceEngine(strategies)


__all__ = [
    "GoListInferenceStrategy",
    "GoDictInferenceStrategy",
    "GoSetInferenceStrategy",
    "GoComprehensionInferenceStrategy",
    "GoCallInferenceStrategy",
    "create_go_type_inference_engine",
]
