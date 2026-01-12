"""Type Inference System for Static Python Analysis.

This module provides advanced type inference capabilities that go beyond
simple type annotations to infer types from context, usage patterns,
and static analysis.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any, Optional

if TYPE_CHECKING:
    from .flow_sensitive_inference import FlowSensitiveInferencer

from ..common import log
from .ast_analyzer import TypeInfo


class InferenceMethod(Enum):
    """Methods used for type inference."""

    ANNOTATION = "annotation"  # Explicit type annotation
    LITERAL = "literal"  # Inferred from literal value
    ASSIGNMENT = "assignment"  # Inferred from assignment
    OPERATION = "operation"  # Inferred from operations
    FUNCTION_RETURN = "function_return"  # Inferred from function return
    PARAMETER_USAGE = "parameter_usage"  # Inferred from parameter usage
    CONTEXT = "context"  # Inferred from surrounding context


@dataclass
class InferenceResult:
    """Result of type inference for a single expression."""

    type_info: TypeInfo
    confidence: float  # 0.0 to 1.0
    method: InferenceMethod
    evidence: list[str] = field(default_factory=list)
    alternatives: list[TypeInfo] = field(default_factory=list)

    @property
    def c_type(self) -> str:
        """Get C type equivalent from type_info."""
        return self.type_info.c_equivalent or "void*"

    @property
    def python_type(self) -> str:
        """Get Python type name from type_info."""
        return self.type_info.name


@dataclass
class TypeConstraint:
    """A constraint on a type based on usage."""

    variable_name: str
    constraint_type: str  # "must_be", "cannot_be", "must_support"
    constraint_value: Any
    source_line: int
    evidence: str


class TypeInferenceEngine:
    """Advanced type inference engine for static Python code."""

    def __init__(self, enable_flow_sensitive: bool = True):
        self.log = log.config(self.__class__.__name__)
        self.inferred_types: dict[str, InferenceResult] = {}
        self.constraints: list[TypeConstraint] = []
        self.enable_flow_sensitive = enable_flow_sensitive

        # Lazy import to avoid circular dependency
        self._flow_sensitive_inferencer: Optional[FlowSensitiveInferencer] = None
        self.binary_op_result_types = {
            # (left_type, operator, right_type) -> result_type
            ("int", ast.Add, "int"): "int",
            ("int", ast.Sub, "int"): "int",
            ("int", ast.Mult, "int"): "int",
            ("int", ast.Div, "int"): "double",  # Division always returns float in Python 3
            ("int", ast.FloorDiv, "int"): "int",
            ("int", ast.Mod, "int"): "int",
            ("int", ast.Pow, "int"): "int",
            ("double", ast.Add, "double"): "double",
            ("double", ast.Sub, "double"): "double",
            ("double", ast.Mult, "double"): "double",
            ("double", ast.Div, "double"): "double",
            ("double", ast.FloorDiv, "double"): "double",
            ("double", ast.Mod, "double"): "double",
            ("double", ast.Pow, "double"): "double",
            # Mixed int/float operations
            ("int", ast.Add, "double"): "double",
            ("double", ast.Add, "int"): "double",
            ("int", ast.Mult, "double"): "double",
            ("double", ast.Mult, "int"): "double",
            # String operations
            ("char*", ast.Add, "char*"): "char*",
            ("char*", ast.Mult, "int"): "char*",
            # Comparison operations
            ("int", ast.Lt, "int"): "bool",
            ("int", ast.Gt, "int"): "bool",
            ("int", ast.Eq, "int"): "bool",
            ("double", ast.Lt, "double"): "bool",
            ("char*", ast.Eq, "char*"): "bool",
        }

    def infer_expression_type(self, node: ast.expr, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer the type of an expression."""
        if isinstance(node, ast.Constant):
            return self._infer_constant_type(node)
        elif isinstance(node, ast.Name):
            return self._infer_name_type(node, context)
        elif isinstance(node, ast.BinOp):
            return self._infer_binop_type(node, context)
        elif isinstance(node, ast.UnaryOp):
            return self._infer_unaryop_type(node, context)
        elif isinstance(node, ast.Call):
            return self._infer_call_type(node, context)
        elif isinstance(node, ast.Compare):
            return self._infer_compare_type(node, context)
        elif isinstance(node, ast.List):
            return self._infer_list_type(node, context)
        elif isinstance(node, ast.Tuple):
            return self._infer_tuple_type(node, context)
        else:
            # Unknown expression type
            return InferenceResult(
                type_info=TypeInfo("unknown"),
                confidence=0.0,
                method=InferenceMethod.CONTEXT,
                evidence=[f"Unknown expression type: {type(node).__name__}"],
            )

    def _infer_constant_type(self, node: ast.Constant) -> InferenceResult:
        """Infer type from constant literals."""
        value = node.value
        if isinstance(value, bool):
            type_info = TypeInfo("bool")
        elif isinstance(value, int):
            type_info = TypeInfo("int")
        elif isinstance(value, float):
            type_info = TypeInfo("float")
        elif isinstance(value, str):
            type_info = TypeInfo("str")
        elif value is None:
            type_info = TypeInfo("NoneType")
        else:
            type_info = TypeInfo("unknown")

        return InferenceResult(
            type_info=type_info,
            confidence=1.0,
            method=InferenceMethod.LITERAL,
            evidence=[f"Literal value: {repr(value)}"],
        )

    def _infer_name_type(self, node: ast.Name, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from variable name."""
        var_name = node.id

        if var_name in context:
            return InferenceResult(
                type_info=context[var_name],
                confidence=1.0,
                method=InferenceMethod.ANNOTATION,
                evidence=[f"Variable '{var_name}' has explicit type annotation"],
            )

        # Try to infer from previous assignments or usage
        if var_name in self.inferred_types:
            prev_result = self.inferred_types[var_name]
            return InferenceResult(
                type_info=prev_result.type_info,
                confidence=prev_result.confidence * 0.8,  # Slightly lower confidence
                method=InferenceMethod.CONTEXT,
                evidence=[f"Previously inferred type for '{var_name}'"],
            )

        # Unknown variable
        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.CONTEXT,
            evidence=[f"Unknown variable: {var_name}"],
        )

    def _infer_binop_type(self, node: ast.BinOp, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from binary operations."""
        left_result = self.infer_expression_type(node.left, context)
        right_result = self.infer_expression_type(node.right, context)

        # Look up result type in our mapping
        op_type = type(node.op)
        left_c = left_result.type_info.c_equivalent if left_result.type_info.c_equivalent else "unknown"
        right_c = right_result.type_info.c_equivalent if right_result.type_info.c_equivalent else "unknown"
        key = (left_c, op_type, right_c)

        if key in self.binary_op_result_types:
            result_c_type = self.binary_op_result_types[key]
            result_type = TypeInfo("inferred", c_equivalent=result_c_type)
            confidence = min(left_result.confidence, right_result.confidence) * 0.9

            return InferenceResult(
                type_info=result_type,
                confidence=confidence,
                method=InferenceMethod.OPERATION,
                evidence=[
                    f"Binary operation: {left_result.type_info.c_equivalent} "
                    f"{op_type.__name__} {right_result.type_info.c_equivalent}"
                ],
            )

        # Special handling for unknown operations
        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.OPERATION,
            evidence=[f"Unknown binary operation: {op_type.__name__}"],
        )

    def _infer_unaryop_type(self, node: ast.UnaryOp, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from unary operations."""
        operand_result = self.infer_expression_type(node.operand, context)

        # Most unary operations preserve the operand type
        if isinstance(node.op, ast.UAdd) or isinstance(node.op, ast.USub):
            # +x or -x preserves numeric type
            if operand_result.type_info.c_equivalent in ["int", "double"]:
                return InferenceResult(
                    type_info=operand_result.type_info,
                    confidence=operand_result.confidence,
                    method=InferenceMethod.OPERATION,
                    evidence=[f"Unary {type(node.op).__name__} preserves type"],
                )
        elif isinstance(node.op, ast.Not):
            # not x always returns bool
            return InferenceResult(
                type_info=TypeInfo("bool"),
                confidence=1.0,
                method=InferenceMethod.OPERATION,
                evidence=["Unary 'not' always returns bool"],
            )

        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.OPERATION,
            evidence=[f"Unknown unary operation: {type(node.op).__name__}"],
        )

    def _infer_call_type(self, node: ast.Call, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from function calls."""
        if isinstance(node.func, ast.Name):
            func_name = node.func.id

            # Built-in function type mappings
            builtin_returns = {
                "len": TypeInfo("int"),
                "str": TypeInfo("str"),
                "int": TypeInfo("int"),
                "float": TypeInfo("float"),
                "bool": TypeInfo("bool"),
                "abs": None,  # Depends on argument
                "max": None,  # Depends on arguments
                "min": None,  # Depends on arguments
                "sum": None,  # Depends on arguments
            }

            if func_name in builtin_returns:
                return_type = builtin_returns[func_name]
                if return_type:
                    return InferenceResult(
                        type_info=return_type,
                        confidence=1.0,
                        method=InferenceMethod.FUNCTION_RETURN,
                        evidence=[f"Built-in function '{func_name}' return type"],
                    )

            # For functions that return the type of their argument
            if func_name in ["abs", "max", "min"] and node.args:
                arg_result = self.infer_expression_type(node.args[0], context)
                return InferenceResult(
                    type_info=arg_result.type_info,
                    confidence=arg_result.confidence * 0.9,
                    method=InferenceMethod.FUNCTION_RETURN,
                    evidence=[f"Function '{func_name}' returns argument type"],
                )

        # Unknown function call
        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.FUNCTION_RETURN,
            evidence=["Unknown function call"],
        )

    def _infer_compare_type(self, node: ast.Compare, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from comparison operations."""
        # All comparisons return bool
        return InferenceResult(
            type_info=TypeInfo("bool"),
            confidence=1.0,
            method=InferenceMethod.OPERATION,
            evidence=["Comparison operations always return bool"],
        )

    def _infer_list_type(self, node: ast.List, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from list literals."""
        if not node.elts:
            # Empty list - can't infer element type
            return InferenceResult(
                type_info=TypeInfo("list", c_equivalent="void*"),
                confidence=0.5,
                method=InferenceMethod.LITERAL,
                evidence=["Empty list - unknown element type"],
            )

        # Infer element type from first element
        first_elem_result = self.infer_expression_type(node.elts[0], context)
        return InferenceResult(
            type_info=TypeInfo("list", c_equivalent=f"{first_elem_result.type_info.c_equivalent}*"),
            confidence=first_elem_result.confidence * 0.8,
            method=InferenceMethod.LITERAL,
            evidence=[f"List with {first_elem_result.type_info.c_equivalent} elements"],
        )

    def _infer_tuple_type(self, node: ast.Tuple, context: dict[str, TypeInfo]) -> InferenceResult:
        """Infer type from tuple literals."""
        element_types = []
        min_confidence = 1.0

        for elem in node.elts:
            elem_result = self.infer_expression_type(elem, context)
            c_equiv = elem_result.type_info.c_equivalent if elem_result.type_info.c_equivalent else "unknown"
            element_types.append(c_equiv)
            min_confidence = min(min_confidence, elem_result.confidence)

        return InferenceResult(
            type_info=TypeInfo("tuple", c_equivalent="struct"),
            confidence=min_confidence * 0.9,
            method=InferenceMethod.LITERAL,
            evidence=[f"Tuple with elements: {', '.join(element_types)}"],
        )

    def analyze_function_signature(self, func_node: ast.FunctionDef) -> dict[str, InferenceResult]:
        """Analyze and infer types for a complete function signature."""
        results = {}

        # Analyze parameters
        for arg in func_node.args.args:
            if arg.annotation:
                # Explicit annotation
                type_info = self._extract_type_from_annotation(arg.annotation)
                results[arg.arg] = InferenceResult(
                    type_info=type_info,
                    confidence=1.0,
                    method=InferenceMethod.ANNOTATION,
                    evidence=["Explicit type annotation"],
                )
            else:
                # Try to infer from usage
                usage_result = self._infer_parameter_from_usage(arg.arg, func_node)
                results[arg.arg] = usage_result

        # Analyze return type
        if func_node.returns:
            return_type_info = self._extract_type_from_annotation(func_node.returns)
            results["__return__"] = InferenceResult(
                type_info=return_type_info,
                confidence=1.0,
                method=InferenceMethod.ANNOTATION,
                evidence=["Explicit return type annotation"],
            )
        else:
            # Infer from return statements
            return_result = self._infer_return_type_from_statements(func_node)
            results["__return__"] = return_result

        return results

    def analyze_function_signature_enhanced(self, func_node: ast.FunctionDef) -> dict[str, InferenceResult]:
        """Enhanced function analysis with optional flow-sensitive inference."""
        if self.enable_flow_sensitive:
            return self._get_flow_sensitive_results(func_node)
        else:
            return self.analyze_function_signature(func_node)

    def _get_flow_sensitive_results(self, func_node: ast.FunctionDef) -> dict[str, InferenceResult]:
        """Get results from flow-sensitive analysis."""
        if self._flow_sensitive_inferencer is None:
            # Lazy import to avoid circular dependency
            from .flow_sensitive_inference import FlowSensitiveInferencer

            self._flow_sensitive_inferencer = FlowSensitiveInferencer(self)

        try:
            # Try flow-sensitive analysis first
            flow_results = self._flow_sensitive_inferencer.analyze_function_flow(func_node)
            self.log.debug(f"Flow-sensitive analysis completed for {func_node.name}")
            return flow_results
        except Exception as e:
            # Fall back to regular analysis if flow-sensitive fails
            self.log.warning(f"Flow-sensitive analysis failed for {func_node.name}: {e}")
            return self.analyze_function_signature(func_node)

    def _extract_type_from_annotation(self, annotation: ast.expr) -> TypeInfo:
        """Extract TypeInfo from AST annotation node."""
        if isinstance(annotation, ast.Name):
            return TypeInfo(annotation.id)
        elif isinstance(annotation, ast.Constant):
            return TypeInfo(str(annotation.value))
        elif isinstance(annotation, ast.Subscript):
            # Handle generic types
            if isinstance(annotation.value, ast.Name):
                base_type = annotation.value.id
                if base_type == "list":
                    return TypeInfo("list", c_equivalent="*")
                elif base_type == "dict":
                    return TypeInfo("dict", c_equivalent="*")
                elif base_type == "Optional":
                    inner_type = self._extract_type_from_annotation(annotation.slice)
                    inner_type.is_nullable = True
                    return inner_type

        return TypeInfo("unknown")

    def _infer_parameter_from_usage(self, param_name: str, func_node: ast.FunctionDef) -> InferenceResult:
        """Infer parameter type from its usage within the function."""
        # Analyze how the parameter is used
        usage_patterns = []

        for node in ast.walk(func_node):
            if isinstance(node, ast.BinOp):
                if isinstance(node.left, ast.Name) and node.left.id == param_name:
                    usage_patterns.append(f"Used in binary operation: {type(node.op).__name__}")
                elif isinstance(node.right, ast.Name) and node.right.id == param_name:
                    usage_patterns.append(f"Used in binary operation: {type(node.op).__name__}")

        # Simple heuristics for type inference
        if any("Add" in pattern or "Sub" in pattern for pattern in usage_patterns):
            return InferenceResult(
                type_info=TypeInfo("int"),  # Assume numeric
                confidence=0.7,
                method=InferenceMethod.PARAMETER_USAGE,
                evidence=usage_patterns,
            )

        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.PARAMETER_USAGE,
            evidence=["No clear usage pattern found"],
        )

    def _infer_return_type_from_statements(self, func_node: ast.FunctionDef) -> InferenceResult:
        """Infer return type from return statements in the function."""
        return_types = []

        for node in ast.walk(func_node):
            if isinstance(node, ast.Return) and node.value:
                # Found a return statement with a value
                return_result = self.infer_expression_type(node.value, {})
                return_types.append(return_result)

        if not return_types:
            # No return statements found
            return InferenceResult(
                type_info=TypeInfo("void"),
                confidence=1.0,
                method=InferenceMethod.FUNCTION_RETURN,
                evidence=["No return statements found"],
            )

        # If all return types are the same, use that
        if len(set(rt.type_info.name for rt in return_types)) == 1:
            first_result = return_types[0]
            avg_confidence = sum(rt.confidence for rt in return_types) / len(return_types)
            return InferenceResult(
                type_info=first_result.type_info,
                confidence=avg_confidence,
                method=InferenceMethod.FUNCTION_RETURN,
                evidence=[f"Consistent return type across {len(return_types)} return statements"],
            )

        # Multiple different return types - use the most common or most confident
        return InferenceResult(
            type_info=TypeInfo("unknown"),
            confidence=0.0,
            method=InferenceMethod.FUNCTION_RETURN,
            evidence=["Inconsistent return types found"],
        )
