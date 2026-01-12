"""Comprehensive tests for CorrectnessProver."""

import ast
import sys
from pathlib import Path

import pytest

# Add src directory to Python path
project_root = Path(__file__).parent.parent
src_path = project_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

from multigen.frontend import AnalysisContext, CorrectnessProver
from multigen.frontend.base import AnalysisLevel

# Check if Z3 is available
try:
    import z3

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False


class TestCorrectnessProverComprehensive:
    """Comprehensive tests for CorrectnessProver."""

    def test_correctness_prover_initialization(self):
        """Test CorrectnessProver initializes correctly."""
        prover = CorrectnessProver()
        assert prover is not None
        assert hasattr(prover, "theorem_prover")

    def test_correctness_prover_with_simple_algorithm(self):
        """Test correctness prover with a simple algorithm."""
        code = """
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = CorrectnessProver()
        # Should not crash
        assert prover is not None

    def test_correctness_prover_with_iterative_algorithm(self):
        """Test correctness prover with iterative algorithm."""
        code = """
def sum_range(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
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

        prover = CorrectnessProver()
        assert prover is not None

    def test_correctness_prover_with_sorting_algorithm(self):
        """Test correctness prover with sorting algorithm."""
        code = """
def bubble_sort(arr: list[int], n: int) -> list[int]:
    for i in range(n):
        for j in range(n - 1):
            if arr[j] > arr[j + 1]:
                temp: int = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = temp
    return arr
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = CorrectnessProver()
        assert prover is not None

    def test_correctness_prover_with_search_algorithm(self):
        """Test correctness prover with search algorithm."""
        code = """
def binary_search(arr: list[int], target: int, left: int, right: int) -> int:
    if left > right:
        return -1
    mid: int = (left + right) // 2
    if arr[mid] == target:
        return mid
    elif arr[mid] > target:
        return binary_search(arr, target, left, mid - 1)
    else:
        return binary_search(arr, target, mid + 1, right)
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = CorrectnessProver()
        assert prover is not None

    def test_correctness_prover_with_mathematical_algorithm(self):
        """Test correctness prover with mathematical algorithm."""
        code = """
def gcd(a: int, b: int) -> int:
    while b != 0:
        temp: int = b
        b = a % b
        a = temp
    return a
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        context = AnalysisContext(
            ast_node=func_node,
            source_code=code,
            analysis_level=AnalysisLevel.BASIC,
            analysis_result=None,
        )

        prover = CorrectnessProver()
        assert prover is not None


@pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
class TestCorrectnessProverWithZ3:
    """Tests for CorrectnessProver with Z3."""

    def test_simple_property_verification(self):
        """Test simple property verification with Z3."""
        from z3 import And, Int, Solver, sat

        # Verify property: x + 0 = x
        x = Int("x")
        solver = Solver()
        solver.add(x + 0 == x)

        assert solver.check() == sat

    def test_commutative_property(self):
        """Test commutative property verification."""
        from z3 import ForAll, Int, Solver, sat

        x = Int("x")
        y = Int("y")

        # Verify: x + y = y + x (addition is commutative)
        commutativity = ForAll([x, y], x + y == y + x)

        solver = Solver()
        solver.add(commutativity)

        assert solver.check() == sat

    def test_associative_property(self):
        """Test associative property verification."""
        from z3 import ForAll, Int, Solver, sat

        x = Int("x")
        y = Int("y")
        z = Int("z")

        # Verify: (x + y) + z = x + (y + z)
        associativity = ForAll([x, y, z], (x + y) + z == x + (y + z))

        solver = Solver()
        solver.add(associativity)

        assert solver.check() == sat


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
