"""Compile-time Computation Engine for the Intelligence Layer.

This module provides compile-time evaluation, constant folding, and expression
simplification capabilities for static Python code optimization.
"""

import ast
import operator as op
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional, cast

from ..base import AnalysisContext, BaseOptimizer, OptimizationLevel, OptimizationResult


class EvaluationResult(Enum):
    """Results of compile-time evaluation attempts."""

    SUCCESS = "success"  # Successfully evaluated to constant
    PARTIAL = "partial"  # Partially evaluated (some simplification)
    FAILED = "failed"  # Cannot evaluate at compile time
    UNSAFE = "unsafe"  # Evaluation would be unsafe


class ExpressionType(Enum):
    """Types of expressions for optimization categorization."""

    CONSTANT = "constant"  # Literal values
    ARITHMETIC = "arithmetic"  # Mathematical operations
    LOGICAL = "logical"  # Boolean operations
    COMPARISON = "comparison"  # Comparison operations
    VARIABLE = "variable"  # Variable references
    FUNCTION_CALL = "function_call"  # Function calls
    COMPLEX = "complex"  # Complex expressions


@dataclass
class ConstantValue:
    """Represents a compile-time constant value."""

    value: Any
    type_name: str
    is_safe: bool = True
    confidence: float = 1.0
    source_expression: Optional[str] = None

    def __post_init__(self) -> None:
        """Validate and normalize the constant value."""
        if self.type_name is None:
            self.type_name = type(self.value).__name__


@dataclass
class OptimizationCandidate:
    """Represents an expression that can be optimized."""

    node: ast.AST
    original_code: str
    optimized_code: str
    optimization_type: str
    confidence: float
    estimated_speedup: float = 1.0
    memory_impact: int = 0  # Bytes saved/used
    safety_verified: bool = True


@dataclass
class CompileTimeReport:
    """Report from compile-time evaluation and optimization."""

    constants_found: dict[str, ConstantValue] = field(default_factory=dict)
    optimizations: list[OptimizationCandidate] = field(default_factory=list)
    expressions_evaluated: int = 0
    expressions_optimized: int = 0
    total_estimated_speedup: float = 1.0
    memory_saved: int = 0
    warnings: list[str] = field(default_factory=list)
    limitations: list[str] = field(default_factory=list)


class CompileTimeEvaluator(BaseOptimizer):
    """Optimizer for compile-time computation and constant folding."""

    def __init__(self, optimization_level: OptimizationLevel = OptimizationLevel.BASIC):
        super().__init__("CompileTimeEvaluator", optimization_level)
        self._constants: dict[str, ConstantValue] = {}
        self._safe_functions = {"abs", "min", "max", "len", "round", "int", "float", "bool", "str"}
        self._binary_operators = {
            ast.Add: op.add,
            ast.Sub: op.sub,
            ast.Mult: op.mul,
            ast.Div: op.truediv,
            ast.FloorDiv: op.floordiv,
            ast.Mod: op.mod,
            ast.Pow: op.pow,
            ast.LShift: op.lshift,
            ast.RShift: op.rshift,
            ast.BitOr: op.or_,
            ast.BitXor: op.xor,
            ast.BitAnd: op.and_,
        }
        self._unary_operators = {
            ast.UAdd: op.pos,
            ast.USub: op.neg,
            ast.Not: op.not_,
            ast.Invert: op.invert,
        }
        self._comparison_operators = {
            ast.Eq: op.eq,
            ast.NotEq: op.ne,
            ast.Lt: op.lt,
            ast.LtE: op.le,
            ast.Gt: op.gt,
            ast.GtE: op.ge,
        }

    def optimize(self, context: AnalysisContext) -> OptimizationResult:
        """Perform compile-time optimization on the given context."""
        try:
            # Reset state
            self._constants.clear()
            report = CompileTimeReport()

            # Create optimized AST
            optimized_ast = self._optimize_ast(context.ast_node, report)

            # Calculate transformations and performance estimates
            transformations = self._generate_transformations(report)
            performance_gain = self._estimate_performance_gain(report)

            # Perform safety analysis
            safety_analysis = self._analyze_safety(report)

            return OptimizationResult(
                optimizer_name=self.name,
                success=True,
                optimized_ast=optimized_ast,
                transformations=transformations,
                performance_gain_estimate=performance_gain,
                safety_analysis=safety_analysis,
                metadata={
                    "optimization_level": self.optimization_level.value,
                    "constants_found": len(report.constants_found),
                    "expressions_optimized": report.expressions_optimized,
                    "total_speedup": report.total_estimated_speedup,
                    "memory_saved": report.memory_saved,
                    "report": report,
                },
            )

        except Exception as e:
            return OptimizationResult(
                optimizer_name=self.name,
                success=False,
                optimized_ast=None,
                transformations=[f"Optimization failed: {str(e)}"],
                performance_gain_estimate=1.0,
                safety_analysis={"compile_time_evaluation": False},
                metadata={"error": str(e), "error_type": type(e).__name__},
            )

    def _optimize_ast(self, node: ast.AST, report: CompileTimeReport) -> ast.AST:
        """Optimize the AST by performing compile-time evaluation."""
        # Create a copy to avoid modifying the original
        optimized: ast.AST = ast.copy_location(ast.parse(ast.unparse(node)), node)

        # First pass: collect constants
        self._collect_constants(optimized, report)

        # Second pass: optimize expressions
        optimized = self._optimize_node(optimized, report)

        return optimized

    def _collect_constants(self, node: ast.AST, report: CompileTimeReport) -> None:
        """Collect constant values from variable assignments."""
        for child in ast.walk(node):
            if isinstance(child, ast.AnnAssign) and child.value:
                if isinstance(child.target, ast.Name):
                    var_name = child.target.id
                    constant_value = self._evaluate_expression(child.value)
                    if constant_value is not None:
                        self._constants[var_name] = constant_value
                        report.constants_found[var_name] = constant_value

            elif isinstance(child, ast.Assign) and len(child.targets) == 1:
                target = child.targets[0]
                if isinstance(target, ast.Name):
                    var_name = target.id
                    constant_value = self._evaluate_expression(child.value)
                    if constant_value is not None:
                        self._constants[var_name] = constant_value
                        report.constants_found[var_name] = constant_value

    def _optimize_node(self, node: ast.AST, report: CompileTimeReport) -> ast.AST:
        """Recursively optimize AST nodes."""
        if isinstance(node, ast.BinOp):
            return self._optimize_binary_operation(node, report)
        elif isinstance(node, ast.UnaryOp):
            return self._optimize_unary_operation(node, report)
        elif isinstance(node, ast.Compare):
            return self._optimize_comparison(node, report)
        elif isinstance(node, ast.BoolOp):
            return self._optimize_boolean_operation(node, report)
        elif isinstance(node, ast.Call):
            return self._optimize_function_call(node, report)
        elif isinstance(node, ast.Name):
            return self._optimize_variable_reference(node, report)
        elif isinstance(node, ast.If):
            return self._optimize_conditional(node, report)
        elif hasattr(node, "_fields"):
            # Recursively optimize child nodes
            for field_name in node._fields:
                field_value = getattr(node, field_name)
                if isinstance(field_value, list):
                    new_list = []
                    for item in field_value:
                        if isinstance(item, ast.AST):
                            new_list.append(self._optimize_node(item, report))
                        else:
                            new_list.append(item)
                    setattr(node, field_name, new_list)
                elif isinstance(field_value, ast.AST):
                    setattr(node, field_name, self._optimize_node(field_value, report))

        return node

    def _optimize_binary_operation(self, node: ast.BinOp, report: CompileTimeReport) -> ast.AST:
        """Optimize binary operations through constant folding."""
        # First optimize operands
        left = self._optimize_node(node.left, report)
        right = self._optimize_node(node.right, report)

        # Try to evaluate the entire expression
        left_val = self._evaluate_expression(left)
        right_val = self._evaluate_expression(right)

        if left_val is not None and right_val is not None:
            result = self._apply_binary_operator(node.op, left_val.value, right_val.value)
            if result is not None:
                report.expressions_evaluated += 1
                report.expressions_optimized += 1

                # Create optimization candidate
                original_code = ast.unparse(node)
                optimized_code = repr(result.value)

                candidate = OptimizationCandidate(
                    node=node,
                    original_code=original_code,
                    optimized_code=optimized_code,
                    optimization_type="constant_folding",
                    confidence=0.95,
                    estimated_speedup=2.0,  # Constant access vs computation
                    memory_impact=-8,  # Reduced instruction complexity
                    safety_verified=True,
                )
                report.optimizations.append(candidate)

                # Return constant node
                return ast.Constant(value=result.value)

        # Partial optimization - check for algebraic simplifications
        simplified = self._apply_algebraic_simplifications(node.op, left, right, report)
        if simplified != node:
            return simplified

        # Return with optimized operands
        node.left = left if isinstance(left, ast.expr) else node.left  # type: ignore
        node.right = right if isinstance(right, ast.expr) else node.right  # type: ignore
        return node

    def _optimize_unary_operation(self, node: ast.UnaryOp, report: CompileTimeReport) -> ast.AST:
        """Optimize unary operations."""
        operand = self._optimize_node(node.operand, report)
        operand_val = self._evaluate_expression(operand)

        if operand_val is not None:
            result = self._apply_unary_operator(node.op, operand_val.value)
            if result is not None:
                report.expressions_evaluated += 1
                report.expressions_optimized += 1

                candidate = OptimizationCandidate(
                    node=node,
                    original_code=ast.unparse(node),
                    optimized_code=repr(result.value),
                    optimization_type="constant_folding",
                    confidence=0.95,
                    estimated_speedup=1.5,
                    safety_verified=True,
                )
                report.optimizations.append(candidate)

                return ast.Constant(value=result.value)

        node.operand = cast(ast.expr, operand)
        return node

    def _optimize_comparison(self, node: ast.Compare, report: CompileTimeReport) -> ast.AST:
        """Optimize comparison operations."""
        # Only handle simple comparisons for now
        if len(node.ops) == 1 and len(node.comparators) == 1:
            left = self._optimize_node(node.left, report)
            right = self._optimize_node(node.comparators[0], report)

            left_val = self._evaluate_expression(left)
            right_val = self._evaluate_expression(right)

            if left_val is not None and right_val is not None:
                op_func = self._comparison_operators.get(type(node.ops[0]))
                if op_func:
                    try:
                        result = op_func(left_val.value, right_val.value)
                        report.expressions_evaluated += 1
                        report.expressions_optimized += 1

                        candidate = OptimizationCandidate(
                            node=node,
                            original_code=ast.unparse(node),
                            optimized_code=str(result),
                            optimization_type="constant_folding",
                            confidence=0.95,
                            estimated_speedup=1.8,
                            safety_verified=True,
                        )
                        report.optimizations.append(candidate)

                        return ast.Constant(value=result)
                    except (TypeError, ValueError, ZeroDivisionError):
                        pass

            node.left = cast(ast.expr, left)
            node.comparators = [cast(ast.expr, right)]

        return node

    def _optimize_boolean_operation(self, node: ast.BoolOp, report: CompileTimeReport) -> ast.AST:
        """Optimize boolean operations (and, or)."""
        optimized_values = []
        all_constants = True

        for value in node.values:
            opt_value = self._optimize_node(value, report)
            optimized_values.append(opt_value)

            val = self._evaluate_expression(opt_value)
            if val is None:
                all_constants = False

        # If all values are constants, evaluate the entire expression
        if all_constants and len(optimized_values) > 1:
            if isinstance(node.op, ast.And):
                # Short-circuit evaluation for 'and'
                for opt_val in optimized_values:
                    const_val = self._evaluate_expression(opt_val)
                    if const_val and not const_val.value:
                        report.expressions_optimized += 1
                        return ast.Constant(value=False)
                # If we get here, all values are truthy
                last_val_node = optimized_values[-1]
                last_val = self._evaluate_expression(last_val_node)
                if last_val:
                    report.expressions_optimized += 1
                    return ast.Constant(value=last_val.value)

            elif isinstance(node.op, ast.Or):
                # Short-circuit evaluation for 'or'
                for opt_val in optimized_values:
                    const_val = self._evaluate_expression(opt_val)
                    if const_val and const_val.value:
                        report.expressions_optimized += 1
                        return ast.Constant(value=const_val.value)
                # If we get here, all values are falsy
                report.expressions_optimized += 1
                return ast.Constant(value=False)

        node.values = [cast(ast.expr, v) for v in optimized_values]
        return node

    def _optimize_function_call(self, node: ast.Call, report: CompileTimeReport) -> ast.AST:
        """Optimize function calls with constant arguments."""
        if isinstance(node.func, ast.Name) and node.func.id in self._safe_functions:
            # Optimize arguments first
            optimized_args = [self._optimize_node(arg, report) for arg in node.args]

            # Check if all arguments are constants
            arg_values = []
            all_constants = True
            for arg in optimized_args:
                val = self._evaluate_expression(arg)
                if val is not None:
                    arg_values.append(val.value)
                else:
                    all_constants = False
                    break

            if all_constants:
                try:
                    # Evaluate safe built-in functions
                    func_name = node.func.id
                    if func_name == "abs" and len(arg_values) == 1:
                        result = abs(arg_values[0])
                    elif func_name == "min" and len(arg_values) >= 1:
                        result = min(arg_values)
                    elif func_name == "max" and len(arg_values) >= 1:
                        result = max(arg_values)
                    elif func_name == "len" and len(arg_values) == 1:
                        result = len(arg_values[0])
                    elif func_name == "round" and len(arg_values) in (1, 2):
                        result = round(*arg_values)
                    elif func_name in ("int", "float", "bool", "str") and len(arg_values) == 1:
                        type_func = {"int": int, "float": float, "bool": bool, "str": str}[func_name]
                        result = type_func(arg_values[0])
                    else:
                        raise ValueError(f"Unsupported function: {func_name}")

                    report.expressions_evaluated += 1
                    report.expressions_optimized += 1

                    candidate = OptimizationCandidate(
                        node=node,
                        original_code=ast.unparse(node),
                        optimized_code=repr(result),
                        optimization_type="function_evaluation",
                        confidence=0.90,
                        estimated_speedup=3.0,  # Function call elimination
                        safety_verified=True,
                    )
                    report.optimizations.append(candidate)

                    return ast.Constant(value=result)

                except (TypeError, ValueError, OverflowError):
                    # Function call failed, keep original
                    pass

            node.args = [cast(ast.expr, arg) for arg in optimized_args]

        return node

    def _optimize_variable_reference(self, node: ast.Name, report: CompileTimeReport) -> ast.AST:
        """Optimize variable references to constants."""
        if isinstance(node.ctx, ast.Load) and node.id in self._constants:
            constant = self._constants[node.id]
            if constant.is_safe:
                report.expressions_optimized += 1

                candidate = OptimizationCandidate(
                    node=node,
                    original_code=node.id,
                    optimized_code=repr(constant.value),
                    optimization_type="constant_propagation",
                    confidence=constant.confidence,
                    estimated_speedup=1.2,
                    safety_verified=True,
                )
                report.optimizations.append(candidate)

                return ast.Constant(value=constant.value)

        return node

    def _optimize_conditional(self, node: ast.If, report: CompileTimeReport) -> ast.AST:
        """Optimize conditional statements with constant conditions."""
        optimized_test = self._optimize_node(node.test, report)
        test_val = self._evaluate_expression(optimized_test)

        if test_val is not None:
            # Constant condition - can eliminate branch
            if test_val.value:
                # Condition is always true - return body
                report.expressions_optimized += 1

                candidate = OptimizationCandidate(
                    node=node,
                    original_code="if " + ast.unparse(node.test),
                    optimized_code="# condition always true",
                    optimization_type="dead_branch_elimination",
                    confidence=0.95,
                    estimated_speedup=1.1,
                    safety_verified=True,
                )
                report.optimizations.append(candidate)

                # Return the body as a sequence
                if len(node.body) == 1:
                    return self._optimize_node(node.body[0], report)
                else:
                    # Create a new block for multiple statements
                    optimized_body = [cast(ast.stmt, self._optimize_node(stmt, report)) for stmt in node.body]
                    return ast.Module(body=optimized_body, type_ignores=[])
            else:
                # Condition is always false - return else clause or pass
                report.expressions_optimized += 1

                candidate = OptimizationCandidate(
                    node=node,
                    original_code="if " + ast.unparse(node.test),
                    optimized_code="# condition always false",
                    optimization_type="dead_branch_elimination",
                    confidence=0.95,
                    estimated_speedup=1.1,
                    safety_verified=True,
                )
                report.optimizations.append(candidate)

                if node.orelse:
                    if len(node.orelse) == 1:
                        return self._optimize_node(node.orelse[0], report)
                    else:
                        optimized_else = [cast(ast.stmt, self._optimize_node(stmt, report)) for stmt in node.orelse]
                        return ast.Module(body=optimized_else, type_ignores=[])
                else:
                    return ast.Pass()

        # Optimize body and else clause
        node.test = cast(ast.expr, optimized_test)
        node.body = [cast(ast.stmt, self._optimize_node(stmt, report)) for stmt in node.body]
        if node.orelse:
            node.orelse = [cast(ast.stmt, self._optimize_node(stmt, report)) for stmt in node.orelse]

        return node

    def _evaluate_expression(self, node: ast.AST) -> Optional[ConstantValue]:
        """Evaluate an expression to a constant value if possible."""
        try:
            if isinstance(node, ast.Constant):
                return ConstantValue(
                    value=node.value, type_name=type(node.value).__name__, is_safe=True, confidence=1.0
                )
            elif isinstance(node, ast.Name) and node.id in self._constants:
                return self._constants[node.id]
            else:
                return None
        except Exception:
            return None

    def _apply_binary_operator(self, op_node: ast.operator, left: Any, right: Any) -> Optional[ConstantValue]:
        """Apply a binary operator to two values."""
        op_func = self._binary_operators.get(type(op_node))
        if not op_func:
            return None

        try:
            result = op_func(left, right)
            return ConstantValue(value=result, type_name=type(result).__name__, is_safe=True, confidence=0.95)
        except (TypeError, ValueError, ZeroDivisionError, OverflowError):
            return None

    def _apply_unary_operator(self, op_node: ast.unaryop, operand: Any) -> Optional[ConstantValue]:
        """Apply a unary operator to a value."""
        op_func = self._unary_operators.get(type(op_node))
        if not op_func or not callable(op_func):
            return None

        try:
            result = op_func(operand)
            return ConstantValue(value=result, type_name=type(result).__name__, is_safe=True, confidence=0.95)
        except (TypeError, ValueError, OverflowError):
            return None

    def _apply_algebraic_simplifications(
        self, op_node: ast.operator, left: ast.AST, right: ast.AST, report: CompileTimeReport
    ) -> ast.AST:
        """Apply algebraic simplifications like x + 0, x * 1, etc."""
        left_val = self._evaluate_expression(left)
        right_val = self._evaluate_expression(right)

        # x + 0 = x, 0 + x = x
        if isinstance(op_node, ast.Add):
            if left_val and left_val.value == 0:
                report.expressions_optimized += 1
                return right
            elif right_val and right_val.value == 0:
                report.expressions_optimized += 1
                return left

        # x * 1 = x, 1 * x = x
        # x * 0 = 0, 0 * x = 0
        elif isinstance(op_node, ast.Mult):
            if left_val:
                if left_val.value == 1:
                    report.expressions_optimized += 1
                    return right
                elif left_val.value == 0:
                    report.expressions_optimized += 1
                    return ast.Constant(value=0)
            if right_val:
                if right_val.value == 1:
                    report.expressions_optimized += 1
                    return left
                elif right_val.value == 0:
                    report.expressions_optimized += 1
                    return ast.Constant(value=0)

        # x - 0 = x
        elif isinstance(op_node, ast.Sub):
            if right_val and right_val.value == 0:
                report.expressions_optimized += 1
                return left

        # x / 1 = x
        elif isinstance(op_node, ast.Div):
            if right_val and right_val.value == 1:
                report.expressions_optimized += 1
                return left

        # Return original binary operation
        return ast.BinOp(left=cast(ast.expr, left), op=op_node, right=cast(ast.expr, right))

    def _generate_transformations(self, report: CompileTimeReport) -> list[str]:
        """Generate list of transformations performed."""
        transformations = []

        transformations.append(f"Found {len(report.constants_found)} compile-time constants")
        transformations.append(f"Optimized {report.expressions_optimized} expressions")

        optimization_types: dict[str, int] = {}
        for opt in report.optimizations:
            optimization_types[opt.optimization_type] = optimization_types.get(opt.optimization_type, 0) + 1

        for opt_type, count in optimization_types.items():
            transformations.append(f"Applied {count} {opt_type} optimizations")

        return transformations

    def _estimate_performance_gain(self, report: CompileTimeReport) -> float:
        """Estimate overall performance gain from optimizations."""
        if not report.optimizations:
            return 1.0

        total_speedup = 1.0
        for opt in report.optimizations:
            # Use conservative estimate weighted by confidence
            weighted_speedup = 1.0 + (opt.estimated_speedup - 1.0) * opt.confidence
            total_speedup *= weighted_speedup

        # Cap the maximum speedup to be realistic
        return min(total_speedup, 10.0)

    def _analyze_safety(self, report: CompileTimeReport) -> dict[str, bool]:
        """Analyze safety of performed optimizations."""
        safety_analysis = {
            "constant_folding": True,
            "algebraic_simplification": True,
            "dead_branch_elimination": True,
            "function_evaluation": True,
            "all_optimizations_safe": True,
        }

        # Check for any unsafe optimizations
        for opt in report.optimizations:
            if not opt.safety_verified:
                safety_analysis["all_optimizations_safe"] = False
                if opt.optimization_type in safety_analysis:
                    safety_analysis[opt.optimization_type] = False

        return safety_analysis
