"""Tests for Z3 formula generation from Python AST."""

import ast
import sys
from pathlib import Path

import pytest

# Add src directory to Python path
project_root = Path(__file__).parent.parent
src_path = project_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

from multigen.frontend.verifiers.z3_formula_generator import Z3FormulaGenerator

# Check if Z3 is available
try:
    import z3

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False


class TestZ3FormulaGenerator:
    """Test Z3 formula generation."""

    def test_initialization(self):
        """Test formula generator initializes correctly."""
        gen = Z3FormulaGenerator()
        assert gen is not None
        assert hasattr(gen, "z3_available")
        assert gen.z3_available == Z3_AVAILABLE

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_create_variable(self):
        """Test creating Z3 variables."""
        gen = Z3FormulaGenerator()
        x = gen.get_or_create_var("x", "int")
        assert x is not None

        # Getting same variable again should return same object
        x2 = gen.get_or_create_var("x", "int")
        if Z3_AVAILABLE:
            assert x.eq(x2)  # Z3 variables should be equal

    def test_constant_expr_to_z3(self):
        """Test converting constant expressions to Z3."""
        gen = Z3FormulaGenerator()

        # Integer constant
        expr = ast.Constant(value=42)
        z3_expr = gen._expr_to_z3(expr)
        if Z3_AVAILABLE:
            assert z3_expr == 42

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_variable_expr_to_z3(self):
        """Test converting variable expressions to Z3."""
        gen = Z3FormulaGenerator()

        # Variable reference
        expr = ast.Name(id="n", ctx=ast.Load())
        z3_expr = gen._expr_to_z3(expr)
        assert z3_expr is not None

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_binop_expr_to_z3(self):
        """Test converting binary operations to Z3."""
        gen = Z3FormulaGenerator()

        # n + 1
        expr = ast.BinOp(left=ast.Name(id="n", ctx=ast.Load()), op=ast.Add(), right=ast.Constant(value=1))

        z3_expr = gen._expr_to_z3(expr)
        assert z3_expr is not None

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_array_bounds_formula(self):
        """Test generating array bounds formula."""
        from z3 import And, Solver, sat

        gen = Z3FormulaGenerator()

        # arr[i] where i is a variable
        index_expr = ast.Name(id="i", ctx=ast.Load())

        formula = gen.generate_array_bounds_formula("arr", index_expr, "arr_len")
        assert formula is not None

        # Test the formula with Z3
        solver = Solver()
        arr_len = gen.get_or_create_var("arr_len", "int")
        i = gen.get_or_create_var("i", "int")

        # Preconditions: arr_len = 10, i = 5
        solver.add(arr_len == 10)
        solver.add(i == 5)
        solver.add(formula)

        result = solver.check()
        assert result == sat  # Should be satisfiable (i=5 is within [0, 10))

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_array_bounds_formula_violation(self):
        """Test detecting array bounds violation."""
        from z3 import And, Solver, unsat

        gen = Z3FormulaGenerator()

        index_expr = ast.Name(id="i", ctx=ast.Load())
        formula = gen.generate_array_bounds_formula("arr", index_expr, "arr_len")

        solver = Solver()
        arr_len = gen.get_or_create_var("arr_len", "int")
        i = gen.get_or_create_var("i", "int")

        # Preconditions: arr_len = 10, i = 10 (out of bounds!)
        solver.add(arr_len == 10)
        solver.add(i == 10)
        solver.add(formula)

        result = solver.check()
        assert result == unsat  # Should be unsatisfiable (i=10 is out of bounds)

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_loop_bounds_formula(self):
        """Test generating loop bounds formula."""
        from z3 import Solver, sat

        gen = Z3FormulaGenerator()

        # for i in range(n)
        end_expr = ast.Name(id="n", ctx=ast.Load())

        formula = gen.generate_loop_bounds_formula("i", 0, end_expr, "arr_len")
        assert formula is not None

        # Test with preconditions: arr_len >= n
        solver = Solver()
        arr_len = gen.get_or_create_var("arr_len", "int")
        n = gen.get_or_create_var("n", "int")

        solver.add(arr_len >= n)
        solver.add(n >= 0)
        solver.add(formula)

        result = solver.check()
        assert result == sat  # Should be satisfiable (safe access)

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_preconditions_formula(self):
        """Test generating preconditions formula."""
        from z3 import Solver, sat

        gen = Z3FormulaGenerator()

        # arr_len >= n
        min_len_expr = ast.Name(id="n", ctx=ast.Load())

        formula = gen.generate_preconditions("arr_len", min_len_expr)
        assert formula is not None

        # Test the preconditions
        solver = Solver()
        arr_len = gen.get_or_create_var("arr_len", "int")
        n = gen.get_or_create_var("n", "int")

        solver.add(formula)
        solver.add(n == 5)
        solver.add(arr_len == 10)

        result = solver.check()
        assert result == sat  # arr_len=10 >= n=5 is satisfiable

    def test_constraint_accumulation(self):
        """Test accumulating multiple constraints."""
        gen = Z3FormulaGenerator()

        gen.add_constraint("constraint1")
        gen.add_constraint("constraint2")

        constraints = gen.get_all_constraints()
        assert len(constraints) == 2

        gen.clear_constraints()
        assert len(gen.get_all_constraints()) == 0

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_variable_clearing(self):
        """Test clearing variables."""
        gen = Z3FormulaGenerator()

        gen.get_or_create_var("x", "int")
        gen.get_or_create_var("y", "int")
        assert len(gen.variables) == 2

        gen.clear_variables()
        assert len(gen.variables) == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
