"""Z3 Theorem Prover Integration for Formal Verification.

This module provides integration with the Z3 theorem prover for formal verification
of code properties, memory safety, and algorithm correctness.
"""

from __future__ import annotations

import ast
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any

# Z3 integration - graceful fallback if not installed
try:
    import z3  # type: ignore[import-untyped]

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    z3 = None  # type: ignore[assignment]


from ..base import AnalysisContext


class ProofStatus(Enum):
    """Status of a formal proof attempt."""

    PROVED = "proved"
    DISPROVED = "disproved"
    UNKNOWN = "unknown"
    TIMEOUT = "timeout"
    ERROR = "error"


class PropertyType(Enum):
    """Types of properties that can be verified."""

    MEMORY_SAFETY = "memory_safety"
    BOUNDS_CHECKING = "bounds_checking"
    NULL_POINTER_SAFETY = "null_pointer_safety"
    OVERFLOW_SAFETY = "overflow_safety"
    ALGORITHM_CORRECTNESS = "algorithm_correctness"
    PRECONDITION_SATISFACTION = "precondition_satisfaction"
    POSTCONDITION_SATISFACTION = "postcondition_satisfaction"
    LOOP_INVARIANT = "loop_invariant"
    TERMINATION = "termination"
    FUNCTIONAL_CORRECTNESS = "functional_correctness"
    PARTIAL_CORRECTNESS = "partial_correctness"
    TOTAL_CORRECTNESS = "total_correctness"


@dataclass
class ProofProperty:
    """A property to be verified."""

    name: str
    property_type: PropertyType
    description: str
    z3_formula: Any  # Z3 formula
    context: dict[str, Any] | None = None


@dataclass
class ProofResult:
    """Result of a formal verification attempt."""

    proof_property: ProofProperty  # Renamed from 'property' to avoid conflict with @property decorator
    status: ProofStatus
    proof_time: float
    counterexample: dict[str, Any] | None = None
    error_message: str | None = None
    z3_model: Any | None = None
    verification_steps: list[str] | None = None

    def __post_init__(self) -> None:
        """Initialize verification_steps list if not provided."""
        if self.verification_steps is None:
            self.verification_steps = []

    @property
    def is_verified(self) -> bool:
        """Returns True if the property was successfully proved."""
        return self.status == ProofStatus.PROVED

    @property
    def is_safe(self) -> bool:
        """Returns True if no counterexample was found (proved or unknown)."""
        return self.status in [ProofStatus.PROVED, ProofStatus.UNKNOWN]


class TheoremProver:
    """Z3-based theorem prover for formal verification."""

    def __init__(self, timeout: int = 30000):  # 30 seconds default
        """Initialize the theorem prover.

        Args:
            timeout: Timeout in milliseconds for Z3 solver
        """
        self.timeout = timeout
        self.z3_available = Z3_AVAILABLE
        self.solver = None

        if self.z3_available:
            self.solver = z3.Solver()
            self.solver.set("timeout", timeout)

        # Property templates for common verification patterns
        self.property_templates = {
            PropertyType.BOUNDS_CHECKING: self._create_bounds_check_template,
            PropertyType.NULL_POINTER_SAFETY: self._create_null_pointer_template,
            PropertyType.OVERFLOW_SAFETY: self._create_overflow_template,
            PropertyType.LOOP_INVARIANT: self._create_loop_invariant_template,
        }

    def verify_property(self, prop: ProofProperty) -> ProofResult:
        """Verify a single property using Z3.

        Args:
            prop: Property to verify

        Returns:
            ProofResult with verification outcome
        """
        if not self.z3_available:
            return ProofResult(
                proof_property=prop,
                status=ProofStatus.ERROR,
                proof_time=0.0,
                error_message="Z3 not available. Install with: pip install z3-solver",
            )

        start_time = time.time()

        try:
            # Create fresh solver for this property
            solver = z3.Solver()
            solver.set("timeout", self.timeout)

            # Add the negation of the property (looking for counterexample)
            negated_formula = z3.Not(prop.z3_formula)
            solver.add(negated_formula)

            # Check satisfiability
            result = solver.check()
            proof_time = time.time() - start_time

            if result == z3.sat:
                # Found counterexample - property is false
                model_result = solver.model()  # type: ignore[func-returns-value]
                counterexample = self._extract_counterexample(model_result) if model_result else {}
                return ProofResult(
                    proof_property=prop,
                    status=ProofStatus.DISPROVED,
                    proof_time=proof_time,
                    counterexample=counterexample,
                    z3_model=model_result,
                )
            elif result == z3.unsat:
                # No counterexample - property is true
                return ProofResult(
                    proof_property=prop,
                    status=ProofStatus.PROVED,
                    proof_time=proof_time,
                    verification_steps=[f"Z3 proved property in {proof_time:.3f}s"],
                )
            else:
                # Unknown result (timeout or undecidable)
                return ProofResult(
                    proof_property=prop,
                    status=ProofStatus.UNKNOWN,
                    proof_time=proof_time,
                    error_message="Z3 could not determine satisfiability",
                )

        except Exception as e:
            return ProofResult(
                proof_property=prop, status=ProofStatus.ERROR, proof_time=time.time() - start_time, error_message=str(e)
            )

    def verify_multiple_properties(self, properties: list[ProofProperty]) -> list[ProofResult]:
        """Verify multiple properties efficiently.

        Args:
            properties: List of properties to verify

        Returns:
            List of ProofResults
        """
        results = []

        for prop in properties:
            result = self.verify_property(prop)
            results.append(result)

            # Early termination if we find a safety violation
            if result.status == ProofStatus.DISPROVED:
                break

        return results

    def create_bounds_check_property(
        self, array_name: str, index_expr: str, array_size: int | str, context: dict[str, Any] | None = None
    ) -> ProofProperty:
        """Create a bounds checking property.

        Args:
            array_name: Name of the array
            index_expr: Index expression
            array_size: Size of the array
            context: Additional context for the property

        Returns:
            ProofProperty for bounds checking
        """
        if not self.z3_available:
            # Return mock property if Z3 not available
            return ProofProperty(
                name=f"bounds_check_{array_name}",
                property_type=PropertyType.BOUNDS_CHECKING,
                description=f"Index {index_expr} is within bounds of {array_name}[{array_size}]",
                z3_formula="mock_formula",
            )

        # Create Z3 variables
        index = z3.Int(f"index_{array_name}")
        size = z3.Int(f"size_{array_name}") if isinstance(array_size, str) else z3.IntVal(array_size)

        # Bounds check: 0 <= index < size
        bounds_check = z3.And(index >= 0, index < size)

        return ProofProperty(
            name=f"bounds_check_{array_name}",
            property_type=PropertyType.BOUNDS_CHECKING,
            description=f"Index {index_expr} is within bounds of {array_name}[{array_size}]",
            z3_formula=bounds_check,
            context=context,
        )

    def create_overflow_safety_property(
        self, operation: str, operands: list[str], result_type: str = "int", context: dict[str, Any] | None = None
    ) -> ProofProperty:
        """Create an overflow safety property.

        Args:
            operation: Type of operation (add, mul, sub, etc.)
            operands: List of operand names
            result_type: Result type for overflow bounds
            context: Additional context

        Returns:
            ProofProperty for overflow safety
        """
        if not self.z3_available:
            return ProofProperty(
                name=f"overflow_safety_{operation}",
                property_type=PropertyType.OVERFLOW_SAFETY,
                description=f"Operation {operation} on {operands} does not overflow",
                z3_formula="mock_formula",
            )

        # Create Z3 variables for operands
        z3_operands = [z3.Int(name) for name in operands]

        # Define bounds for different types
        type_bounds = {
            "int": (-(2**31), 2**31 - 1),
            "long": (-(2**63), 2**63 - 1),
            "short": (-(2**15), 2**15 - 1),
        }

        min_val, max_val = type_bounds.get(result_type, type_bounds["int"])

        # Create operation formula
        if operation == "add" and len(z3_operands) == 2:
            result = z3_operands[0] + z3_operands[1]
        elif operation == "mul" and len(z3_operands) == 2:
            result = z3_operands[0] * z3_operands[1]
        elif operation == "sub" and len(z3_operands) == 2:
            result = z3_operands[0] - z3_operands[1]
        else:
            raise ValueError(f"Unsupported operation: {operation}")

        # Overflow safety: result within type bounds
        overflow_safety = z3.And(result >= min_val, result <= max_val)

        return ProofProperty(
            name=f"overflow_safety_{operation}",
            property_type=PropertyType.OVERFLOW_SAFETY,
            description=f"Operation {operation} on {operands} does not overflow",
            z3_formula=overflow_safety,
            context=context,
        )

    def analyze_function_safety(self, context: AnalysisContext) -> list[ProofResult]:
        """Analyze a function for various safety properties.

        Args:
            context: Analysis context with AST and metadata

        Returns:
            List of verification results for safety properties
        """
        properties = []

        # Extract potential safety properties from AST
        safety_visitor = SafetyPropertyExtractor()
        safety_visitor.visit(context.ast_node)

        # Create properties for array accesses
        for array_access in safety_visitor.array_accesses:
            prop = self.create_bounds_check_property(
                array_access["array"], array_access["index"], array_access.get("size", "unknown")
            )
            properties.append(prop)

        # Create properties for arithmetic operations
        for operation in safety_visitor.arithmetic_ops:
            prop = self.create_overflow_safety_property(operation["op"], operation["operands"])
            properties.append(prop)

        # Verify all properties
        return self.verify_multiple_properties(properties)

    def _extract_counterexample(self, model: Any) -> dict[str, Any]:
        """Extract counterexample from Z3 model."""
        if not model:
            return {}

        counterexample = {}
        for decl in model.decls():
            counterexample[str(decl)] = str(model[decl])

        return counterexample

    def _create_bounds_check_template(self, **kwargs: Any) -> ProofProperty:
        """Template for bounds checking properties."""
        return self.create_bounds_check_property(**kwargs)

    def _create_null_pointer_template(self, **kwargs: Any) -> ProofProperty:
        """Template for null pointer safety properties."""
        # Implementation for null pointer checks
        return ProofProperty(
            name="null_pointer_safety",
            property_type=PropertyType.NULL_POINTER_SAFETY,
            description="Null pointer safety check",
            z3_formula="true",  # Placeholder formula
            context=kwargs,
        )

    def _create_overflow_template(self, **kwargs: Any) -> ProofProperty:
        """Template for overflow safety properties."""
        return self.create_overflow_safety_property(**kwargs)

    def _create_loop_invariant_template(self, **kwargs: Any) -> ProofProperty:
        """Template for loop invariant properties."""
        # Implementation for loop invariants
        return ProofProperty(
            name="loop_invariant",
            property_type=PropertyType.LOOP_INVARIANT,
            description="Loop invariant property",
            z3_formula="true",  # Placeholder formula
            context=kwargs,
        )


class SafetyPropertyExtractor(ast.NodeVisitor):
    """AST visitor to extract safety properties from Python code."""

    def __init__(self) -> None:
        self.array_accesses: list[dict[str, Any]] = []
        self.arithmetic_ops: list[dict[str, Any]] = []
        self.function_calls: list[dict[str, Any]] = []
        self.loop_bounds: list[dict[str, Any]] = []

    def visit_Subscript(self, node: ast.Subscript) -> None:
        """Extract array access patterns."""
        if isinstance(node.value, ast.Name):
            array_name = node.value.id

            # Try to extract index information
            if isinstance(node.slice, ast.Name):
                index_name = node.slice.id
            elif isinstance(node.slice, ast.Constant):
                index_name = str(node.slice.value)
            else:
                index_name = "complex_index"

            self.array_accesses.append({"array": array_name, "index": index_name, "line": node.lineno})

        self.generic_visit(node)

    def visit_BinOp(self, node: ast.BinOp) -> None:
        """Extract arithmetic operations for overflow checking."""
        if isinstance(node.op, (ast.Add, ast.Sub, ast.Mult)):
            op_name = {ast.Add: "add", ast.Sub: "sub", ast.Mult: "mul"}[type(node.op)]

            operands = []
            if isinstance(node.left, ast.Name):
                operands.append(node.left.id)
            if isinstance(node.right, ast.Name):
                operands.append(node.right.id)

            if operands:
                self.arithmetic_ops.append({"op": op_name, "operands": operands, "line": node.lineno})

        self.generic_visit(node)

    def visit_For(self, node: ast.For) -> None:
        """Extract loop information for termination analysis."""
        if isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name):
            if node.iter.func.id == "range":
                args = node.iter.args
                if args:
                    bound_info = {"type": "range", "args": len(args), "line": node.lineno}
                    self.loop_bounds.append(bound_info)

        self.generic_visit(node)
