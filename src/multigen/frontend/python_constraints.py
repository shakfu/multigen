"""Python Code Constraint Checker - Universal validation for all backends.

Validates Python code for:
- Type safety issues
- Static analysis problems
- Code quality concerns

These checks apply to Python code regardless of target backend.
"""

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Optional

from .immutability_analyzer import MutabilityClass
from .type_inference import TypeInferenceEngine


class ConstraintCategory(Enum):
    """Categories of constraints."""

    TYPE_SAFETY = "type_safety"
    STATIC_ANALYSIS = "static_analysis"
    CODE_QUALITY = "code_quality"


@dataclass
class PythonConstraintViolation:
    """A Python code constraint violation."""

    category: ConstraintCategory
    rule_id: str
    message: str
    line: int
    severity: str  # "info" | "warning" | "error"
    suggestion: Optional[str] = None


class PythonConstraintChecker:
    """Universal Python code constraint validation.

    Checks Python code for safety and quality issues that apply
    regardless of target backend.
    """

    def __init__(self, immutability_results: Optional[dict] = None):
        """Initialize checker.

        Args:
            immutability_results: Optional immutability analysis results from pipeline
        """
        self.violations: list[PythonConstraintViolation] = []
        self.type_engine = TypeInferenceEngine()
        self.immutability_results = immutability_results or {}

    def check_code(self, source_code: str) -> list[PythonConstraintViolation]:
        """Validate Python code for safety and quality.

        Args:
            source_code: Python source code to check

        Returns:
            List of constraint violations
        """
        self.violations = []

        try:
            tree = ast.parse(source_code)

            # Type safety
            self._check_type_consistency(tree)
            self._check_implicit_conversions(tree)
            self._check_division_by_zero(tree)
            self._check_integer_overflow(tree)

            # Static analysis & code quality
            self._check_unreachable_code(tree)
            self._check_unused_variables(tree)
            self._check_uninitialized_variables(tree)
            self._check_parameter_mutability(tree)  # SA005 - uses immutability results
            self._check_function_complexity(tree)

        except SyntaxError:
            # Invalid Python - will be caught by validation phase
            pass

        return self.violations

    # Type Safety Rules (TS001-TS004)

    def _check_type_consistency(self, tree: ast.AST) -> None:
        """Check for type consistency violations (TS001)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.BinOp):
                # Check binary operation type compatibility
                left_type = self._infer_expression_type(node.left)
                right_type = self._infer_expression_type(node.right)

                if left_type and right_type:
                    if left_type != right_type:
                        # Flag type mismatches (e.g., int + str)
                        incompatible = self._are_types_incompatible(left_type, right_type, node.op)
                        if incompatible:
                            self.violations.append(
                                PythonConstraintViolation(
                                    category=ConstraintCategory.TYPE_SAFETY,
                                    rule_id="TS001",
                                    message=f"Type mismatch: {left_type} and {right_type} in binary operation",
                                    line=node.lineno,
                                    severity="error",
                                    suggestion="Ensure both operands have compatible types",
                                )
                            )

    def _check_implicit_conversions(self, tree: ast.AST) -> None:
        """Check for potentially dangerous implicit conversions (TS002)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.Assign):
                # Check assignments for type changes
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        # If target has annotation, check compatibility
                        if hasattr(target, "annotation") and target.annotation:
                            target_type = self._extract_type_from_annotation(target.annotation)
                            value_type = self._infer_expression_type(node.value)

                            if target_type and value_type and target_type != value_type:
                                # Check for lossy conversions
                                if target_type == "int" and value_type == "float":
                                    self.violations.append(
                                        PythonConstraintViolation(
                                            category=ConstraintCategory.TYPE_SAFETY,
                                            rule_id="TS002",
                                            message="Implicit float-to-int conversion loses precision",
                                            line=node.lineno,
                                            severity="warning",
                                            suggestion="Use explicit int() conversion",
                                        )
                                    )

    def _check_division_by_zero(self, tree: ast.AST) -> None:
        """Check for potential division by zero (TS003)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.BinOp):
                if isinstance(node.op, (ast.Div, ast.FloorDiv, ast.Mod)):
                    # Check for literal zero divisor
                    if isinstance(node.right, ast.Constant) and node.right.value == 0:
                        self.violations.append(
                            PythonConstraintViolation(
                                category=ConstraintCategory.TYPE_SAFETY,
                                rule_id="TS003",
                                message="Division by zero",
                                line=node.lineno,
                                severity="error",
                            )
                        )

                    # Check for (x - x) pattern
                    if isinstance(node.right, ast.BinOp) and isinstance(node.right.op, ast.Sub):
                        if isinstance(node.right.left, ast.Name) and isinstance(node.right.right, ast.Name):
                            if node.right.left.id == node.right.right.id:
                                self.violations.append(
                                    PythonConstraintViolation(
                                        category=ConstraintCategory.TYPE_SAFETY,
                                        rule_id="TS003",
                                        message="Potential division by zero: divisor is (x - x)",
                                        line=node.lineno,
                                        severity="warning",
                                        suggestion="Add zero check before division",
                                    )
                                )

    def _check_integer_overflow(self, tree: ast.AST) -> None:
        """Check for potential integer overflow (TS004)."""
        INT32_MAX = 2147483647
        INT32_MIN = -2147483648

        for node in ast.walk(tree):
            if isinstance(node, ast.Constant):
                if isinstance(node.value, int):
                    if node.value > INT32_MAX or node.value < INT32_MIN:
                        self.violations.append(
                            PythonConstraintViolation(
                                category=ConstraintCategory.TYPE_SAFETY,
                                rule_id="TS004",
                                message=f"Integer literal {node.value} exceeds 32-bit range (C/Go/Rust i32)",
                                line=node.lineno,
                                severity="warning",
                                suggestion="Use explicit 64-bit type or check target language limits",
                            )
                        )

    # Static Analysis Rules (SA001-SA003, SA005)

    def _check_unreachable_code(self, tree: ast.AST) -> None:
        """Detect unreachable code after returns (SA001)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                self._check_unreachable_in_block(node.body)

    def _check_unreachable_in_block(self, block: list) -> None:
        """Check for unreachable code in a statement block."""
        found_return = False
        for i, stmt in enumerate(block):
            if found_return:
                # Everything after return is unreachable
                self.violations.append(
                    PythonConstraintViolation(
                        category=ConstraintCategory.CODE_QUALITY,
                        rule_id="SA001",
                        message="Unreachable code after return statement",
                        line=stmt.lineno,
                        severity="warning",
                        suggestion="Remove unreachable code",
                    )
                )
                break

            if isinstance(stmt, ast.Return):
                found_return = True

            # Recursively check nested blocks
            if isinstance(stmt, (ast.If, ast.While, ast.For)):
                self._check_unreachable_in_block(stmt.body)
                if hasattr(stmt, "orelse"):
                    self._check_unreachable_in_block(stmt.orelse)

    def _check_unused_variables(self, tree: ast.AST) -> None:
        """Detect variables that are assigned but never used (SA002)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                # Track assigned variables
                assigned = set()
                used = set()

                for subnode in ast.walk(node):
                    # Track assignments
                    if isinstance(subnode, ast.Assign):
                        for target in subnode.targets:
                            if isinstance(target, ast.Name):
                                assigned.add(target.id)

                    # Track usage
                    if isinstance(subnode, ast.Name):
                        if isinstance(subnode.ctx, ast.Load):
                            used.add(subnode.id)

                # Report unused
                unused = assigned - used
                for var in unused:
                    # Find line number of assignment
                    for subnode in ast.walk(node):
                        if isinstance(subnode, ast.Assign):
                            for target in subnode.targets:
                                if isinstance(target, ast.Name) and target.id == var:
                                    self.violations.append(
                                        PythonConstraintViolation(
                                            category=ConstraintCategory.CODE_QUALITY,
                                            rule_id="SA002",
                                            message=f"Variable '{var}' is assigned but never used",
                                            line=subnode.lineno,
                                            severity="info",
                                            suggestion="Remove unused variable or use it",
                                        )
                                    )
                                    break

    def _check_uninitialized_variables(self, tree: ast.AST) -> None:
        """Detect potentially uninitialized variables (SA003)."""
        # Built-in functions and types that are always available
        builtins = {
            "int",
            "float",
            "str",
            "bool",
            "list",
            "dict",
            "set",
            "tuple",
            "len",
            "range",
            "min",
            "max",
            "sum",
            "abs",
            "print",
            "open",
            "enumerate",
            "zip",
            "map",
            "filter",
            "sorted",
            "reversed",
            "isinstance",
            "type",
            "hasattr",
            "getattr",
            "setattr",
            "True",
            "False",
            "None",
        }

        # Collect module-level function and class names
        module_names = set()
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                module_names.add(node.name)

        # This check is complex and prone to false positives
        # For now, disable it - it's better to have no check than wrong checks
        # TODO: Implement proper dataflow analysis for uninitialized variable detection
        return

    def _check_parameter_mutability(self, tree: ast.AST) -> None:
        """Suggest better type annotations for read-only parameters (SA005)."""
        if not self.immutability_results:
            return  # Skip if no immutability analysis available

        for func_name, params in self.immutability_results.items():
            for param, mutability in params.items():
                if mutability == MutabilityClass.READ_ONLY:
                    # Find function node to get line number
                    for node in ast.walk(tree):
                        if isinstance(node, ast.FunctionDef) and node.name == func_name:
                            self.violations.append(
                                PythonConstraintViolation(
                                    category=ConstraintCategory.CODE_QUALITY,
                                    rule_id="SA005",
                                    message=f"Parameter '{param}' in '{func_name}' is never modified. "
                                    f"Consider using immutable type (tuple, Sequence) for better type safety",
                                    line=node.lineno,
                                    severity="info",
                                    suggestion="Use tuple[T] or Sequence[T] instead of list[T]",
                                )
                            )
                            break

    def _check_function_complexity(self, tree: ast.AST) -> None:
        """Warn about overly complex functions (CC004)."""
        COMPLEXITY_THRESHOLD = 10

        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                complexity = self._calculate_cyclomatic_complexity(node)
                if complexity > COMPLEXITY_THRESHOLD:
                    self.violations.append(
                        PythonConstraintViolation(
                            category=ConstraintCategory.CODE_QUALITY,
                            rule_id="CC004",
                            message=f"Function '{node.name}' has high cyclomatic complexity ({complexity})",
                            line=node.lineno,
                            severity="warning",
                            suggestion=f"Consider refactoring (threshold: {COMPLEXITY_THRESHOLD})",
                        )
                    )

    def _calculate_cyclomatic_complexity(self, node: ast.FunctionDef) -> int:
        """Calculate cyclomatic complexity of a function."""
        complexity = 1  # Base complexity

        for subnode in ast.walk(node):
            # Each decision point adds 1
            if isinstance(subnode, (ast.If, ast.While, ast.For, ast.ExceptHandler)):
                complexity += 1
            elif isinstance(subnode, ast.BoolOp):
                # Each 'and'/'or' adds 1
                complexity += len(subnode.values) - 1

        return complexity

    # Helper methods

    def _infer_expression_type(self, node: ast.expr) -> Optional[str]:
        """Infer type of an expression (simple heuristic)."""
        if isinstance(node, ast.Constant):
            if isinstance(node.value, int):
                return "int"
            elif isinstance(node.value, float):
                return "float"
            elif isinstance(node.value, str):
                return "str"
            elif isinstance(node.value, bool):
                return "bool"
        elif isinstance(node, ast.List):
            return "list"
        elif isinstance(node, ast.Dict):
            return "dict"
        elif isinstance(node, ast.Set):
            return "set"
        return None

    def _extract_type_from_annotation(self, annotation: ast.expr) -> Optional[str]:
        """Extract type name from annotation."""
        if isinstance(annotation, ast.Name):
            return annotation.id
        elif isinstance(annotation, ast.Subscript):
            if isinstance(annotation.value, ast.Name):
                return annotation.value.id
        return None

    def _are_types_incompatible(self, type1: str, type2: str, op: ast.operator) -> bool:
        """Check if types are incompatible for given operation."""
        # int + str is incompatible
        if (type1 == "int" and type2 == "str") or (type1 == "str" and type2 == "int"):
            if isinstance(op, (ast.Add, ast.Sub, ast.Mult, ast.Div)):
                return True

        # list + int is incompatible
        if (type1 == "list" and type2 in ["int", "float"]) or (type2 == "list" and type1 in ["int", "float"]):
            return True

        return False
