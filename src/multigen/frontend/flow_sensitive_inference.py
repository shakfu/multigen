"""Flow-Sensitive Type Inference for CGen.

This module provides advanced flow-sensitive type inference capabilities
based on the patterns from mini_py2c_module_fixed.py, integrated with
CGen's existing type system.
"""

import ast
from dataclasses import dataclass
from typing import Optional

from ..common import log
from .ast_analyzer import TypeInfo
from .type_inference import InferenceMethod, InferenceResult, TypeInferenceEngine


@dataclass(frozen=True)
class FlowType:
    """Flow-sensitive type representation."""

    name: str
    c_equivalent: Optional[str] = None
    is_unknown: bool = False
    is_union: bool = False
    union_options: Optional[set["FlowType"]] = None

    def __str__(self) -> str:
        """Return string representation of the flow type."""
        if self.is_union and self.union_options:
            return " | ".join(sorted(str(opt) for opt in self.union_options))
        return self.name

    def __hash__(self) -> int:
        """Return hash of the flow type for use in sets and dicts."""
        if self.is_union and self.union_options:
            return hash(frozenset(self.union_options))
        return hash((self.name, self.c_equivalent))


# Predefined common types
FLOW_INT = FlowType("int", "int")
FLOW_FLOAT = FlowType("float", "double")
FLOW_BOOL = FlowType("bool", "bool")
FLOW_STR = FlowType("str", "char*")
FLOW_UNKNOWN = FlowType("unknown", None, is_unknown=True)


class TypeUnifier:
    """Type unification system based on mini module's approach."""

    def __init__(self) -> None:
        self.log = log.config(self.__class__.__name__)

    def unify(self, type_a: FlowType, type_b: FlowType) -> FlowType:
        """Unify two types using constraint-based approach."""
        # Same types unify to themselves
        if type_a == type_b:
            return type_a

        # Unknown types unify to the known type
        if type_a.is_unknown:
            return type_b
        if type_b.is_unknown:
            return type_a

        # Numeric type coercion
        if (type_a == FLOW_INT and type_b == FLOW_FLOAT) or (type_a == FLOW_FLOAT and type_b == FLOW_INT):
            return FLOW_FLOAT

        # Bool-int coercion (bool is subtype of int in C)
        if (type_a == FLOW_BOOL and type_b == FLOW_INT) or (type_a == FLOW_INT and type_b == FLOW_BOOL):
            return FLOW_INT

        # Handle existing union types
        if type_a.is_union or type_b.is_union:
            return self._make_union(type_a, type_b)

        # Different types create a union
        return self._make_union(type_a, type_b)

    def _make_union(self, *types: FlowType) -> FlowType:
        """Create a union type from multiple types."""
        options = set()

        for t in types:
            if t.is_union and t.union_options:
                options |= t.union_options
            elif not t.is_unknown:
                options.add(t)

        if not options:
            return FLOW_UNKNOWN

        if len(options) == 1:
            return next(iter(options))

        return FlowType(name="union", is_union=True, union_options=set(options))


class FlowSensitiveInferencer:
    """Flow-sensitive type inference engine based on mini module patterns."""

    def __init__(self, fallback_engine: TypeInferenceEngine):
        self.log = log.config(self.__class__.__name__)
        self.fallback_engine = fallback_engine
        self.unifier = TypeUnifier()
        self.class_defs: dict[str, dict[str, FlowType]] = {}

        # Per-function state
        self.current_env: dict[str, FlowType] = {}
        self.return_types: list[FlowType] = []
        self.var_order: list[str] = []
        self.errors: list[str] = []

    def analyze_function_flow(self, func_node: ast.FunctionDef) -> dict[str, InferenceResult]:
        """Analyze function with flow-sensitive type inference."""
        self.log.debug(f"Starting flow-sensitive analysis for function: {func_node.name}")

        # Reset per-function state
        self.current_env = {}
        self.return_types = []
        self.var_order = []
        self.errors = []

        # Seed parameters with annotations or unknown types
        for arg in func_node.args.args:
            flow_type = self._annotation_to_flow_type(arg.annotation)
            self.current_env[arg.arg] = flow_type
            self._remember_var(arg.arg)

        # Flow-sensitive inference over function body
        end_env = self._infer_block(func_node.body, dict(self.current_env))

        # Parameter reconciliation (key improvement from mini module)
        for param_name in [arg.arg for arg in func_node.args.args]:
            start_type = self.current_env[param_name]
            end_type = end_env.get(param_name, FLOW_UNKNOWN)

            if start_type.is_unknown and not end_type.is_unknown:
                # Forward propagation: infer parameter type from usage
                self.current_env[param_name] = end_type
                self.log.debug(f"Inferred parameter '{param_name}' type from usage: {end_type}")
            elif not start_type.is_unknown and not end_type.is_unknown:
                # Reconcile annotated type with inferred usage
                unified = self.unifier.unify(start_type, end_type)
                self.current_env[param_name] = unified
                if unified != start_type:
                    self.log.debug(f"Reconciled parameter '{param_name}' type: {start_type} -> {unified}")

        # Merge inferred local variables
        for var_name, var_type in end_env.items():
            if var_name not in self.current_env:
                self.current_env[var_name] = var_type

        # Convert to CGen's InferenceResult format
        results = {}
        for var_name, flow_type in self.current_env.items():
            results[var_name] = self._flow_type_to_inference_result(flow_type, var_name)

        # Handle return type
        return_type = self._infer_return_type(func_node)
        if return_type:
            results["__return__"] = return_type

        self.log.debug(f"Completed flow-sensitive analysis for function: {func_node.name}")
        return results

    def _infer_block(self, stmts: list[ast.stmt], env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Infer types for a block of statements with flow sensitivity."""
        current_env = dict(env)

        for stmt in stmts:
            current_env = self._infer_stmt(stmt, current_env)

        return current_env

    def _infer_stmt(self, stmt: ast.stmt, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Infer types for a single statement."""
        if isinstance(stmt, ast.Assign):
            return self._handle_assignment(stmt, env)
        elif isinstance(stmt, ast.AnnAssign):
            return self._handle_annotated_assignment(stmt, env)
        elif isinstance(stmt, ast.Return):
            return self._handle_return(stmt, env)
        elif isinstance(stmt, ast.If):
            return self._handle_if(stmt, env)
        elif isinstance(stmt, ast.While):
            return self._handle_while(stmt, env)
        elif isinstance(stmt, ast.For):
            return self._handle_for(stmt, env)
        else:
            # Unknown statement type, return environment unchanged
            return env

    def _handle_assignment(self, stmt: ast.Assign, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle regular assignment statements."""
        if len(stmt.targets) != 1 or not isinstance(stmt.targets[0], ast.Name):
            return env

        target_name = stmt.targets[0].id
        rhs_type = self._infer_expr(stmt.value, env)

        # Unify with existing type if variable already exists
        existing_type = env.get(target_name, FLOW_UNKNOWN)
        new_type = self.unifier.unify(existing_type, rhs_type)

        new_env = dict(env)
        new_env[target_name] = new_type
        self._remember_var(target_name)

        return new_env

    def _handle_annotated_assignment(self, stmt: ast.AnnAssign, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle annotated assignment statements."""
        if not isinstance(stmt.target, ast.Name):
            return env

        target_name = stmt.target.id
        annotation_type = self._annotation_to_flow_type(stmt.annotation)
        rhs_type = self._infer_expr(stmt.value, env) if stmt.value else FLOW_UNKNOWN

        # Debug logging for nested list types
        if "list[list[" in annotation_type.name:
            self.log.debug(
                f"Annotated assignment: {target_name} : {annotation_type.name} (c_equiv={annotation_type.c_equivalent})"
            )
            self.log.debug(f"  RHS type: {rhs_type.name} (c_equiv={rhs_type.c_equivalent})")

        # Unify annotation with RHS type and existing type
        existing_type = env.get(target_name, FLOW_UNKNOWN)
        unified = self.unifier.unify(self.unifier.unify(existing_type, annotation_type), rhs_type)

        # Debug logging for unified result
        if "list[list[" in annotation_type.name:
            self.log.debug(f"  Unified type: {unified.name} (c_equiv={unified.c_equivalent})")

        new_env = dict(env)
        new_env[target_name] = unified
        self._remember_var(target_name)

        return new_env

    def _handle_return(self, stmt: ast.Return, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle return statements."""
        if stmt.value:
            return_type = self._infer_expr(stmt.value, env)
            self.return_types.append(return_type)

        return env

    def _handle_if(self, stmt: ast.If, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle if statements with flow-sensitive branching."""
        # Evaluate test condition (may refine types through comparisons)
        test_env = dict(env)
        self._infer_expr(stmt.test, test_env)

        # Flow-sensitive analysis of branches
        then_env = self._infer_block(stmt.body, test_env)
        else_env = self._infer_block(stmt.orelse, test_env) if stmt.orelse else dict(env)

        # Merge environments at join point
        merged_env = {}
        all_vars = set(then_env.keys()) | set(else_env.keys())

        for var_name in all_vars:
            then_type = then_env.get(var_name, FLOW_UNKNOWN)
            else_type = else_env.get(var_name, FLOW_UNKNOWN)
            merged_env[var_name] = self.unifier.unify(then_type, else_type)

        return merged_env

    def _handle_while(self, stmt: ast.While, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle while loops."""
        # Simple approach: analyze body once and join with entry
        self._infer_expr(stmt.test, env)
        body_env = self._infer_block(stmt.body, dict(env))

        # Join entry and body environments
        merged_env = {}
        all_vars = set(env.keys()) | set(body_env.keys())

        for var_name in all_vars:
            entry_type = env.get(var_name, FLOW_UNKNOWN)
            body_type = body_env.get(var_name, FLOW_UNKNOWN)
            merged_env[var_name] = self.unifier.unify(entry_type, body_type)

        return merged_env

    def _handle_for(self, stmt: ast.For, env: dict[str, FlowType]) -> dict[str, FlowType]:
        """Handle for loops (currently only range-based)."""
        if (
            isinstance(stmt.target, ast.Name)
            and isinstance(stmt.iter, ast.Call)
            and isinstance(stmt.iter.func, ast.Name)
            and stmt.iter.func.id == "range"
        ):
            # Range-based for loop - iteration variable is int
            iter_var = stmt.target.id
            new_env = dict(env)
            new_env[iter_var] = FLOW_INT
            self._remember_var(iter_var)

            # Analyze body and join with entry
            body_env = self._infer_block(stmt.body, new_env)

            merged_env = {}
            all_vars = set(new_env.keys()) | set(body_env.keys())

            for var_name in all_vars:
                entry_type = new_env.get(var_name, FLOW_UNKNOWN)
                body_type = body_env.get(var_name, FLOW_UNKNOWN)
                merged_env[var_name] = self.unifier.unify(entry_type, body_type)

            return merged_env

        # Unsupported for loop pattern
        return env

    def _infer_expr(self, expr: ast.expr, env: dict[str, FlowType]) -> FlowType:
        """Infer type of expression with comparison-driven type propagation."""
        if isinstance(expr, ast.Name):
            return env.get(expr.id, FLOW_UNKNOWN)

        elif isinstance(expr, ast.Constant):
            if isinstance(expr.value, bool):
                return FLOW_BOOL
            elif isinstance(expr.value, int):
                return FLOW_INT
            elif isinstance(expr.value, float):
                return FLOW_FLOAT
            elif isinstance(expr.value, str):
                return FLOW_STR
            else:
                return FLOW_UNKNOWN

        elif isinstance(expr, ast.BinOp):
            left_type = self._infer_expr(expr.left, env)
            right_type = self._infer_expr(expr.right, env)

            # Type-specific binary operation rules
            if isinstance(expr.op, (ast.Add, ast.Sub, ast.Mult)):
                # Arithmetic operations require numeric types
                result_type = self.unifier.unify(left_type, right_type)

                # If both operands are unknown, assume int for arithmetic
                if result_type.is_unknown:
                    result_type = FLOW_INT

                # Propagate inferred type back to operands
                if isinstance(expr.left, ast.Name) and left_type.is_unknown:
                    env[expr.left.id] = FLOW_INT
                    self._remember_var(expr.left.id)
                if isinstance(expr.right, ast.Name) and right_type.is_unknown:
                    env[expr.right.id] = FLOW_INT
                    self._remember_var(expr.right.id)

                return result_type
            elif isinstance(expr.op, ast.Div):
                return FLOW_FLOAT  # True division always returns float
            elif isinstance(expr.op, ast.FloorDiv):
                return FLOW_INT  # Floor division returns int
            else:
                return self.unifier.unify(left_type, right_type)

        elif isinstance(expr, ast.Compare):
            # Critical improvement: comparison-driven type propagation
            if len(expr.ops) == 1 and len(expr.comparators) == 1:
                left = expr.left
                right = expr.comparators[0]

                left_type = self._infer_expr(left, env)
                right_type = self._infer_expr(right, env)

                # Unify operand types - if both are unknown, assume numeric comparison
                unified_type = self.unifier.unify(left_type, right_type)
                if unified_type.is_unknown:
                    # Comparison implies comparable types - assume int for numeric comparisons
                    unified_type = FLOW_INT

                # Propagate unified type back to environment (key insight from mini module)
                if isinstance(left, ast.Name):
                    old_type = env.get(left.id, FLOW_UNKNOWN)
                    new_type = self.unifier.unify(old_type, unified_type)
                    env[left.id] = new_type
                    # Remember variables for ordering
                    self._remember_var(left.id)

                if isinstance(right, ast.Name):
                    old_type = env.get(right.id, FLOW_UNKNOWN)
                    new_type = self.unifier.unify(old_type, unified_type)
                    env[right.id] = new_type
                    # Remember variables for ordering
                    self._remember_var(right.id)

            return FLOW_BOOL

        elif isinstance(expr, ast.UnaryOp):
            operand_type = self._infer_expr(expr.operand, env)
            if isinstance(expr.op, (ast.UAdd, ast.USub)):
                return self.unifier.unify(operand_type, FLOW_INT)
            elif isinstance(expr.op, ast.Not):
                return FLOW_BOOL
            else:
                return operand_type

        elif isinstance(expr, ast.Call):
            # Basic function call inference
            if isinstance(expr.func, ast.Name):
                func_name = expr.func.id

                # Built-in functions
                builtin_returns = {
                    "len": FLOW_INT,
                    "str": FLOW_STR,
                    "int": FLOW_INT,
                    "float": FLOW_FLOAT,
                    "bool": FLOW_BOOL,
                }

                return builtin_returns.get(func_name, FLOW_UNKNOWN)

        # Default case
        return FLOW_UNKNOWN

    def _infer_return_type(self, func_node: ast.FunctionDef) -> Optional[InferenceResult]:
        """Infer function return type."""
        # Start with annotation if present
        annotation_type = self._annotation_to_flow_type(func_node.returns) if func_node.returns else FLOW_UNKNOWN

        # Unify with inferred return types
        if self.return_types:
            inferred_type = self.return_types[0]
            for ret_type in self.return_types[1:]:
                inferred_type = self.unifier.unify(inferred_type, ret_type)

            final_type = self.unifier.unify(annotation_type, inferred_type)
        else:
            final_type = annotation_type if not annotation_type.is_unknown else FlowType("void", "void")

        return self._flow_type_to_inference_result(final_type, "__return__")

    def _annotation_to_flow_type(self, annotation: Optional[ast.expr]) -> FlowType:
        """Convert AST annotation to FlowType."""
        if annotation is None:
            return FLOW_UNKNOWN

        if isinstance(annotation, ast.Name):
            type_map = {
                "int": FLOW_INT,
                "float": FLOW_FLOAT,
                "bool": FLOW_BOOL,
                "str": FLOW_STR,
            }
            return type_map.get(annotation.id, FLOW_UNKNOWN)

        # Handle subscripted types like list[int], list[list[int]], dict[str, int]
        if isinstance(annotation, ast.Subscript):
            if isinstance(annotation.value, ast.Name):
                base_type = annotation.value.id

                if base_type == "list":
                    # Recursively handle element type
                    element_type = self._annotation_to_flow_type(annotation.slice)

                    # Build type name and c_equivalent
                    if element_type == FLOW_INT:
                        return FlowType("list[int]", "vec_int")
                    elif element_type == FLOW_FLOAT:
                        return FlowType("list[float]", "vec_float")
                    elif element_type == FLOW_STR:
                        return FlowType("list[str]", "vec_cstr")
                    elif element_type.name.startswith("list["):
                        # Nested list like list[list[int]]
                        inner_c_type = element_type.c_equivalent or "unknown"
                        return FlowType(f"list[{element_type.name}]", f"vec_{inner_c_type}")
                    else:
                        # Generic list type
                        return FlowType("list", "vec_unknown")

                elif base_type == "dict":
                    # For now, just return a generic dict type
                    return FlowType("dict", "map_unknown")

        return FLOW_UNKNOWN

    def _flow_type_to_inference_result(self, flow_type: FlowType, var_name: str) -> InferenceResult:
        """Convert FlowType to InferenceResult."""
        # Convert FlowType to TypeInfo
        if flow_type.is_unknown:
            type_info = TypeInfo("unknown")
            confidence = 0.0
        else:
            type_info = TypeInfo(flow_type.name, c_equivalent=flow_type.c_equivalent)
            confidence = 0.9 if flow_type.c_equivalent else 0.7

        method = InferenceMethod.CONTEXT if flow_type.is_unknown else InferenceMethod.ASSIGNMENT

        return InferenceResult(
            type_info=type_info,
            confidence=confidence,
            method=method,
            evidence=[f"Flow-sensitive analysis for '{var_name}'"],
        )

    def _remember_var(self, var_name: str) -> None:
        """Track variable declaration order."""
        if var_name not in self.var_order:
            self.var_order.append(var_name)
