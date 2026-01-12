"""Comprehensive tests for BoundsProver formal verification."""

import ast
import sys
from pathlib import Path

import pytest

# Add src directory to Python path
project_root = Path(__file__).parent.parent
src_path = project_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

from multigen.frontend import AnalysisContext, BoundsProver
from multigen.frontend.base import AnalysisLevel
from multigen.frontend.verifiers.z3_formula_generator import Z3FormulaGenerator

# Check if Z3 is available
try:
    import z3

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False


class TestBoundsProverComprehensive:
    """Comprehensive tests for BoundsProver."""

    def test_bounds_prover_with_simple_function(self):
        """Test bounds prover with a simple function."""
        code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert result is not None
        assert result.function_name == "add"
        assert hasattr(result, "is_safe")
        assert hasattr(result, "confidence")

    def test_bounds_prover_with_array_iteration(self):
        """Test bounds prover with array iteration."""
        code = """
def sum_array(arr: list[int], n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + arr[i]
    return total
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert result is not None
        assert result.function_name == "sum_array"

    def test_bounds_prover_with_nested_loops(self):
        """Test bounds prover with nested loops."""
        code = """
def matrix_sum(matrix: list[list[int]], rows: int, cols: int) -> int:
    total: int = 0
    for i in range(rows):
        for j in range(cols):
            total = total + matrix[i][j]
    return total
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert result is not None
        assert result.function_name == "matrix_sum"

    def test_bounds_prover_with_conditional_access(self):
        """Test bounds prover with conditional array access."""
        code = """
def conditional_access(arr: list[int], n: int, flag: bool) -> int:
    if flag:
        return arr[0]
    else:
        return arr[n - 1]
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert result is not None
        assert result.function_name == "conditional_access"

    def test_memory_safety_proof_attributes(self):
        """Test MemorySafetyProof has all required attributes."""
        code = """
def test_func(x: int) -> int:
    return x
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        # Check all required attributes
        assert hasattr(result, "function_name")
        assert hasattr(result, "safety_type")
        assert hasattr(result, "is_safe")
        assert hasattr(result, "proof_results")
        assert hasattr(result, "unsafe_accesses")
        assert hasattr(result, "recommendations")
        assert hasattr(result, "verification_time")
        assert hasattr(result, "confidence")
        assert hasattr(result, "summary")

    def test_verification_time_recorded(self):
        """Test that verification time is recorded."""
        code = """
def simple(x: int) -> int:
    return x + 1
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert result.verification_time >= 0
        assert isinstance(result.verification_time, float)

    def test_confidence_value_range(self):
        """Test that confidence is in valid range [0, 1]."""
        code = """
def test_func(x: int) -> int:
    return x
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert 0.0 <= result.confidence <= 1.0

    def test_summary_format(self):
        """Test that summary is properly formatted."""
        code = """
def test_func(x: int) -> int:
    return x
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = BoundsProver()
        result = prover.verify_memory_safety(context)

        assert isinstance(result.summary, str)
        assert "test_func" in result.summary
        assert "confidence" in result.summary.lower()


@pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
class TestBoundsProverWithZ3:
    """Tests that require Z3 to be available."""

    def test_z3_formula_generator_integration(self):
        """Test Z3FormulaGenerator integration."""
        gen = Z3FormulaGenerator()
        assert gen.z3_available is True

        # Test variable creation
        x = gen.get_or_create_var("x", "int")
        assert x is not None

    def test_array_bounds_verification_safe(self):
        """Test verification of safe array bounds."""
        from z3 import And, Solver, sat

        gen = Z3FormulaGenerator()

        # Test: for i in range(n), arr[i] is safe if arr_len >= n
        i = gen.get_or_create_var("i", "int")
        n = gen.get_or_create_var("n", "int")
        arr_len = gen.get_or_create_var("arr_len", "int")

        # Preconditions
        preconditions = And(arr_len >= n, n >= 0)

        # Bounds check for i
        bounds_check = And(i >= 0, i < arr_len)

        # Loop range
        loop_range = And(i >= 0, i < n)

        solver = Solver()
        solver.add(preconditions)
        solver.add(loop_range)
        solver.add(bounds_check)

        # Should be satisfiable (safe)
        assert solver.check() == sat

    def test_array_bounds_verification_unsafe(self):
        """Test detection of unsafe array bounds."""
        from z3 import And, Solver, unsat

        gen = Z3FormulaGenerator()

        i = gen.get_or_create_var("i", "int")
        n = gen.get_or_create_var("n", "int")
        arr_len = gen.get_or_create_var("arr_len", "int")

        # Preconditions: array has exactly n elements
        preconditions = And(arr_len == n, n >= 0)

        # Try to access arr[n] (out of bounds!)
        access_at_n = And(i == n, i >= 0, i < arr_len)

        solver = Solver()
        solver.add(preconditions)
        solver.add(access_at_n)

        # Should be unsatisfiable (unsafe)
        assert solver.check() == unsat

    def test_off_by_one_detection(self):
        """Test detection of off-by-one errors."""
        from z3 import And, Solver, unsat

        gen = Z3FormulaGenerator()

        i = gen.get_or_create_var("i", "int")
        n = gen.get_or_create_var("n", "int")
        arr_len = gen.get_or_create_var("arr_len", "int")

        # range(n+1) but array only has n elements
        preconditions = And(arr_len == n, n > 0)
        loop_range = And(i >= 0, i < n + 1)  # Bug: should be i < n
        bounds_check = And(i >= 0, i < arr_len)

        solver = Solver()
        solver.add(preconditions)
        solver.add(loop_range)
        solver.add(i == n)  # Check if i=n is in loop range
        solver.add(bounds_check)  # But not in array bounds

        # Should be unsatisfiable (proves the bug exists)
        assert solver.check() == unsat

    def test_complex_expression_verification(self):
        """Test verification with complex index expressions."""
        from z3 import And, Solver, sat

        gen = Z3FormulaGenerator()

        # Test: arr[i * 2 + 1]
        expr = ast.BinOp(
            left=ast.BinOp(left=ast.Name(id="i", ctx=ast.Load()), op=ast.Mult(), right=ast.Constant(value=2)),
            op=ast.Add(),
            right=ast.Constant(value=1),
        )

        z3_expr = gen._expr_to_z3(expr)
        assert z3_expr is not None

        # Verify with solver
        i = gen.get_or_create_var("i", "int")
        arr_len = gen.get_or_create_var("arr_len", "int")

        solver = Solver()
        solver.add(arr_len == 10)
        solver.add(i >= 0)
        solver.add(i < 4)  # i * 2 + 1 < 10 requires i < 4.5
        solver.add(z3_expr >= 0)
        solver.add(z3_expr < arr_len)

        assert solver.check() == sat


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
