"""C++-specific type inference strategies.

Extends the base type inference system with C++-specific type formatting
and special handling for auto types, concrete types, and binary operations.
"""

import ast
from typing import TYPE_CHECKING

from ..type_inference_strategies import (
    CallInferenceStrategy,
    DictInferenceStrategy,
    InferenceContext,
    ListInferenceStrategy,
    SetInferenceStrategy,
    TypeInferenceStrategy,
)

if TYPE_CHECKING:
    from ..type_inference_strategies import TypeInferenceEngine


class CppListInferenceStrategy(ListInferenceStrategy):
    """C++-specific list type inference with std::vector<T> formatting."""

    def _format_list_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as std::vector<T> if element type is concrete."""
        # C++ specific: avoid auto in element types
        if element_type and element_type not in ["auto", ""]:
            return f"std::vector<{element_type}>"
        return "auto"


class CppDictInferenceStrategy(DictInferenceStrategy):
    """C++-specific dict type inference with std::unordered_map<K,V> formatting."""

    def _format_dict_type(self, key_type: str, value_type: str, context: InferenceContext) -> str:
        """Format as std::unordered_map<K,V> if types are concrete."""
        # C++ specific: avoid auto in key/value types
        if key_type and key_type not in ["auto", ""] and value_type and value_type not in ["auto", ""]:
            return f"std::unordered_map<{key_type}, {value_type}>"
        return "auto"


class CppSetInferenceStrategy(SetInferenceStrategy):
    """C++-specific set type inference with std::unordered_set<T> formatting."""

    def _format_set_type(self, element_type: str, context: InferenceContext) -> str:
        """Format as std::unordered_set<T> if element type is concrete."""
        # C++ specific: avoid auto in element types
        if element_type and element_type not in ["auto", ""]:
            return f"std::unordered_set<{element_type}>"
        return "auto"


class CppCallInferenceStrategy(CallInferenceStrategy):
    """C++-specific call type inference with extended function/method mappings."""

    def _infer_from_function(self, func_name: str, context: InferenceContext) -> str:
        """Infer return type from function name (C++ specific)."""
        # Standard built-ins
        if func_name in ["abs", "len", "sum", "min", "max"]:
            return "int"
        elif func_name == "bool":
            return "bool"
        elif func_name in ["str", "upper", "lower", "strip", "replace"]:
            return "std::string"
        elif func_name == "range":
            return "Range"
        # C++ specific: pattern-based inference
        elif func_name.endswith("_int") or func_name in [
            "factorial",
            "calculate",
            "compute",
            "add",
            "subtract",
            "multiply",
        ]:
            return "int"
        elif func_name.endswith("_str") or func_name in ["format", "get_name", "to_string"]:
            return "std::string"
        elif func_name.endswith("_float") or func_name in ["average", "mean"]:
            return "double"
        elif func_name.endswith("_bool") or func_name in ["is_valid", "check"]:
            return "bool"
        else:
            return "auto"

    def _infer_from_method(self, method_name: str, context: InferenceContext) -> str:
        """Infer return type from method name (C++ specific)."""
        # String methods
        if method_name in ["upper", "lower", "strip", "replace"]:
            return "std::string"
        # Search methods
        elif method_name == "find":
            return "int"
        # String split
        elif method_name == "split":
            return "std::vector<std::string>"
        else:
            return "auto"


class CppBinOpInferenceStrategy(TypeInferenceStrategy):
    """C++-specific binary operation type inference.

    Handles type promotion (int + float = float) and string concatenation.
    """

    def can_infer(self, value: ast.expr) -> bool:
        return isinstance(value, ast.BinOp)

    def infer(self, value: ast.expr, context: InferenceContext) -> str:
        assert isinstance(value, ast.BinOp), "Expected ast.BinOp"

        if not context.infer_recursively:
            return "auto"

        # Infer types from both operands
        left_type = context.infer_recursively(value.left)
        right_type = context.infer_recursively(value.right)

        # If both sides are the same concrete type, return that type
        if left_type != "auto" and left_type == right_type:
            return left_type

        # Type promotion: int + float = float
        if (left_type == "int" and right_type == "double") or (left_type == "double" and right_type == "int"):
            return "double"

        # String concatenation
        if isinstance(value.op, ast.Add) and (left_type == "std::string" or right_type == "std::string"):
            return "std::string"

        # Default to auto for complex cases
        return "auto"


def create_cpp_type_inference_engine() -> "TypeInferenceEngine":
    """Create TypeInferenceEngine configured for C++.

    Returns:
        TypeInferenceEngine with C++-specific strategies
    """
    from ..type_inference_strategies import (
        ComprehensionInferenceStrategy,
        ConstantInferenceStrategy,
        NameInferenceStrategy,
        TypeInferenceEngine,
    )

    strategies = [
        ConstantInferenceStrategy(),
        NameInferenceStrategy(),
        CppListInferenceStrategy(),
        CppDictInferenceStrategy(),
        CppSetInferenceStrategy(),
        CppBinOpInferenceStrategy(),
        ComprehensionInferenceStrategy(),
        CppCallInferenceStrategy(),
    ]

    return TypeInferenceEngine(strategies)


__all__ = [
    "CppListInferenceStrategy",
    "CppDictInferenceStrategy",
    "CppSetInferenceStrategy",
    "CppCallInferenceStrategy",
    "CppBinOpInferenceStrategy",
    "create_cpp_type_inference_engine",
]
