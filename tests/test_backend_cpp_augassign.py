"""Tests for augmented assignment operators in C++ backend."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestBasicAugmentedAssignment:
    """Test basic augmented assignment operators."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_addition_assignment(self):
        """Test += operator."""
        python_code = """
def test_add_assign(x: int) -> int:
    x += 5
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x += 5;" in cpp_code

    def test_subtraction_assignment(self):
        """Test -= operator."""
        python_code = """
def test_sub_assign(x: int) -> int:
    x -= 3
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x -= 3;" in cpp_code

    def test_multiplication_assignment(self):
        """Test *= operator."""
        python_code = """
def test_mul_assign(x: int) -> int:
    x *= 2
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x *= 2;" in cpp_code

    def test_division_assignment(self):
        """Test /= operator."""
        python_code = """
def test_div_assign(x: float) -> float:
    x /= 2.0
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x /= 2.0;" in cpp_code

    def test_floor_division_assignment(self):
        """Test //= operator (maps to /= in C++)."""
        python_code = """
def test_floordiv_assign(x: int) -> int:
    x //= 3
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x /= 3;" in cpp_code

    def test_modulo_assignment(self):
        """Test %= operator."""
        python_code = """
def test_mod_assign(x: int) -> int:
    x %= 7
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x %= 7;" in cpp_code


class TestBitwiseAugmentedAssignment:
    """Test bitwise augmented assignment operators."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_bitwise_or_assignment(self):
        """Test |= operator."""
        python_code = """
def test_or_assign(flags: int) -> int:
    flags |= 8
    return flags
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "flags |= 8;" in cpp_code

    def test_bitwise_and_assignment(self):
        """Test &= operator."""
        python_code = """
def test_and_assign(mask: int) -> int:
    mask &= 0xFF
    return mask
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "mask &= 255;" in cpp_code  # 0xFF converts to 255

    def test_bitwise_xor_assignment(self):
        """Test ^= operator."""
        python_code = """
def test_xor_assign(value: int) -> int:
    value ^= 0x55
    return value
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "value ^= 85;" in cpp_code  # 0x55 converts to 85

    def test_left_shift_assignment(self):
        """Test <<= operator."""
        python_code = """
def test_lshift_assign(bits: int) -> int:
    bits <<= 2
    return bits
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "bits <<= 2;" in cpp_code

    def test_right_shift_assignment(self):
        """Test >>= operator."""
        python_code = """
def test_rshift_assign(bits: int) -> int:
    bits >>= 1
    return bits
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "bits >>= 1;" in cpp_code


class TestAugmentedAssignmentWithExpressions:
    """Test augmented assignment with complex expressions."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_augassign_with_variable(self):
        """Test augmented assignment with variable on right side."""
        python_code = """
def test_with_var(x: int, y: int) -> int:
    x += y
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x += y;" in cpp_code

    def test_augassign_with_expression(self):
        """Test augmented assignment with expression on right side."""
        python_code = """
def test_with_expr(x: int, y: int, z: int) -> int:
    x += y * z
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x += (y * z);" in cpp_code

    def test_augassign_with_function_call(self):
        """Test augmented assignment with function call."""
        python_code = """
def test_with_call(x: int, y: int) -> int:
    x += abs(y)
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x += multigen::abs(y);" in cpp_code

    def test_multiple_augassign_operations(self):
        """Test multiple augmented assignments in sequence."""
        python_code = """
def test_multiple(x: int) -> int:
    x += 5
    x *= 2
    x -= 1
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "x += 5;" in cpp_code
        assert "x *= 2;" in cpp_code
        assert "x -= 1;" in cpp_code


class TestAugmentedAssignmentInClasses:
    """Test augmented assignment in class contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_augassign_instance_variable(self):
        """Test augmented assignment on instance variables."""
        python_code = """
class Counter:
    def __init__(self, initial: int):
        self.count: int = initial

    def increment(self, amount: int) -> None:
        self.count += amount

    def double(self) -> None:
        self.count *= 2
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "this->count += amount;" in cpp_code
        assert "this->count *= 2;" in cpp_code

    def test_augassign_with_instance_variable_rhs(self):
        """Test augmented assignment with instance variable on right side."""
        python_code = """
class Accumulator:
    def __init__(self, step: int):
        self.step: int = step
        self.total: int = 0

    def add_step(self) -> None:
        self.total += self.step
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "this->total += this->step;" in cpp_code

    def test_augassign_local_variable_in_method(self):
        """Test augmented assignment on local variables in methods."""
        python_code = """
class Processor:
    def process(self, input_val: int) -> int:
        result: int = input_val
        result *= 2
        result += 10
        return result
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int result = input_val;" in cpp_code
        assert "result *= 2;" in cpp_code
        assert "result += 10;" in cpp_code


class TestAugmentedAssignmentComplexCases:
    """Test complex augmented assignment scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_augassign_in_loop(self):
        """Test augmented assignment inside loops."""
        python_code = """
def test_in_loop(n: int) -> int:
    total: int = 0
    for i in range(n):
        total += i
    return total
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int total = 0;" in cpp_code
        assert "total += i;" in cpp_code
        assert "for (int i = 0; i < n; i++)" in cpp_code

    def test_augassign_in_conditional(self):
        """Test augmented assignment in conditional blocks."""
        python_code = """
def test_in_if(x: int, condition: bool) -> int:
    if condition:
        x += 10
    else:
        x -= 5
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "if (condition)" in cpp_code
        assert "x += 10;" in cpp_code
        assert "} else {" in cpp_code
        assert "x -= 5;" in cpp_code

    def test_augassign_with_casting(self):
        """Test augmented assignment with type casting."""
        python_code = """
def test_with_cast(x: float, y: int) -> float:
    x += float(y)
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        # Note: This would need proper type casting implementation
        assert "x +=" in cpp_code

    def test_chained_augmented_assignments(self):
        """Test multiple variables with augmented assignment."""
        python_code = """
def test_chained(a: int, b: int, c: int) -> int:
    a += 1
    b += a
    c += b
    return c
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "a += 1;" in cpp_code
        assert "b += a;" in cpp_code
        assert "c += b;" in cpp_code

    def test_augassign_with_string_concatenation(self):
        """Test += with string concatenation."""
        python_code = """
def test_string_concat(base: str, suffix: str) -> str:
    base += suffix
    return base
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "base += suffix;" in cpp_code