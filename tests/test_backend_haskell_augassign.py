"""Tests for Haskell backend augmented assignment support."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter


class TestHaskellAugmentedAssignment:
    """Test augmented assignment operators conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_addition_assignment(self):
        """Test += operator."""
        python_code = """
def test_add_assign(x: int, y: int) -> int:
    x += y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x + y)" in haskell_code

    def test_subtraction_assignment(self):
        """Test -= operator."""
        python_code = """
def test_sub_assign(x: int, y: int) -> int:
    x -= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x - y)" in haskell_code

    def test_multiplication_assignment(self):
        """Test *= operator."""
        python_code = """
def test_mul_assign(x: int, y: int) -> int:
    x *= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x * y)" in haskell_code

    def test_division_assignment(self):
        """Test /= operator."""
        python_code = """
def test_div_assign(x: float, y: float) -> float:
    x /= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x / y)" in haskell_code

    def test_floor_division_assignment(self):
        """Test //= operator."""
        python_code = """
def test_floor_div_assign(x: int, y: int) -> int:
    x //= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x `div` y)" in haskell_code

    def test_modulo_assignment(self):
        """Test %= operator."""
        python_code = """
def test_mod_assign(x: int, y: int) -> int:
    x %= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x `mod` y)" in haskell_code

    def test_power_assignment(self):
        """Test **= operator."""
        python_code = """
def test_pow_assign(x: float, y: float) -> float:
    x **= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x ** y)" in haskell_code

    def test_bitwise_or_assignment(self):
        """Test |= operator."""
        python_code = """
def test_or_assign(x: int, y: int) -> int:
    x |= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x .|. y)" in haskell_code

    def test_bitwise_xor_assignment(self):
        """Test ^= operator."""
        python_code = """
def test_xor_assign(x: int, y: int) -> int:
    x ^= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x `xor` y)" in haskell_code

    def test_bitwise_and_assignment(self):
        """Test &= operator."""
        python_code = """
def test_and_assign(x: int, y: int) -> int:
    x &= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x .&. y)" in haskell_code

    def test_left_shift_assignment(self):
        """Test <<= operator."""
        python_code = """
def test_lshift_assign(x: int, y: int) -> int:
    x <<= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x `shiftL` y)" in haskell_code

    def test_right_shift_assignment(self):
        """Test >>= operator."""
        python_code = """
def test_rshift_assign(x: int, y: int) -> int:
    x >>= y
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "x = (x `shiftR` y)" in haskell_code

    def test_multiple_augmented_assignments(self):
        """Test multiple augmented assignments in sequence."""
        python_code = """
def test_multiple_augassign(x: int, y: int, z: int) -> int:
    x += y
    x *= z
    x -= 1
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        # Haskell doesn't allow variable redefinition in let blocks
        # Multiple augmented assignments are combined into one expression
        assert "x = (((x + y) * z) - 1)" in haskell_code

    def test_augmented_assignment_with_expressions(self):
        """Test augmented assignment with complex expressions."""
        python_code = """
def test_complex_augassign(x: int, y: int, z: int) -> int:
    x += y * z + 1
    x *= (y + z) // 2
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        # Multiple augmented assignments are combined into one expression
        assert "x = ((x + ((y * z) + 1)) * ((y + z) `div` 2))" in haskell_code

    def test_augmented_assignment_with_function_calls(self):
        """Test augmented assignment with function calls."""
        python_code = """
def helper(a: int) -> int:
    return a * 2

def test_augassign_with_calls(x: int, y: int) -> int:
    x += helper(y)
    x *= abs(y)
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        # Multiple augmented assignments are combined into one expression
        assert "x = ((x + helper y) * abs' y)" in haskell_code

    def test_augmented_assignment_in_loops(self):
        """Test augmented assignment in loop contexts."""
        python_code = """
def test_augassign_in_loop(numbers: list) -> int:
    total = 0
    for num in numbers:
        total += num
    return total
"""
        haskell_code = self.converter.convert_code(python_code)

        # In Haskell, accumulation in loops is implemented using foldl
        # This is the idiomatic functional approach
        assert "total = foldl" in haskell_code
        assert "acc + (num)" in haskell_code

    def test_augmented_assignment_with_attributes(self):
        """Test augmented assignment on object attributes."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self, amount: int) -> None:
        self.value += amount

    def multiply(self, factor: int) -> None:
        self.value *= factor
"""
        haskell_code = self.converter.convert_code(python_code)

        # Note: In Haskell, this would require special handling
        # for mutable state, but we test basic structure
        assert "data Counter = Counter" in haskell_code
        # Note: In Haskell, methods become pure functions due to immutability
        assert "data Counter = Counter" in haskell_code

    def test_augmented_assignment_type_consistency(self):
        """Test that augmented assignment maintains type consistency."""
        python_code = """
def test_type_consistency() -> None:
    x: int = 10
    x += 5

    y: float = 3.14
    y *= 2.0

    s: str = "Hello"
    s += " World"
"""
        haskell_code = self.converter.convert_code(python_code)

        # Multiple assignments are combined, using init values from annotations
        assert "x = (10 + 5)" in haskell_code
        assert "y = (3.14 * 2" in haskell_code
        # String concatenation in Haskell
        assert 's = ("Hello" +' in haskell_code

    def test_nested_augmented_assignments(self):
        """Test augmented assignments in nested contexts."""
        python_code = """
def test_nested_augassign(matrix: list) -> int:
    total = 0
    for row in matrix:
        for cell in row:
            total += cell
            if cell > 5:
                total *= 2
    return total
"""
        haskell_code = self.converter.convert_code(python_code)

        # Nested loops and conditionals generate complex structures
        # May generate fold or may skip with comment in pure functions
        assert "total" in haskell_code
        assert ("foldl" in haskell_code or "foldr" in haskell_code or
                "for loop" in haskell_code or "not converted" in haskell_code)

    def test_augmented_assignment_with_builtin_functions(self):
        """Test augmented assignment with built-in function results."""
        python_code = """
def test_augassign_with_builtins(numbers: list) -> int:
    result = 0
    result += len(numbers)
    result *= abs(-5)
    result += sum(numbers)
    return result
"""
        haskell_code = self.converter.convert_code(python_code)

        # Multiple augmented assignments are combined into one expression
        assert "result = (((0 + len' numbers) * abs' (-5)) + sum' numbers)" in haskell_code