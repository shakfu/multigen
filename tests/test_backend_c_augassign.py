"""Tests for augmented assignment operators support in C backend."""

import pytest

from multigen.backends.c.emitter import MultiGenPythonToCConverter
from multigen.backends.errors import UnsupportedFeatureError, TypeMappingError

class TestAugmentedAssignmentBasics:
    """Test basic augmented assignment functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_add_assignment(self):
        """Test += operator."""
        python_code = """
def test_add_assignment() -> int:
    x: int = 5
    x += 3
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "int x = 5;" in c_code
        assert "x += 3;" in c_code

    def test_simple_sub_assignment(self):
        """Test -= operator."""
        python_code = """
def test_sub_assignment() -> int:
    y: int = 10
    y -= 4
    return y
"""
        c_code = self.converter.convert_code(python_code)

        assert "int y = 10;" in c_code
        assert "y -= 4;" in c_code

    def test_simple_mult_assignment(self):
        """Test *= operator."""
        python_code = """
def test_mult_assignment() -> int:
    z: int = 3
    z *= 7
    return z
"""
        c_code = self.converter.convert_code(python_code)

        assert "int z = 3;" in c_code
        assert "z *= 7;" in c_code

    def test_simple_div_assignment(self):
        """Test /= operator."""
        python_code = """
def test_div_assignment() -> float:
    w: float = 20.0
    w /= 5.0
    return w
"""
        c_code = self.converter.convert_code(python_code)

        assert "double w = 20.0;" in c_code
        assert "w /= 5.0;" in c_code

    def test_floor_div_assignment(self):
        """Test //= operator (maps to /= in C)."""
        python_code = """
def test_floor_div_assignment() -> int:
    x: int = 15
    x //= 4
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "int x = 15;" in c_code
        assert "x /= 4;" in c_code  # Floor division maps to regular division

    def test_mod_assignment(self):
        """Test %= operator."""
        python_code = """
def test_mod_assignment() -> int:
    value: int = 17
    value %= 5
    return value
"""
        c_code = self.converter.convert_code(python_code)

        assert "int value = 17;" in c_code
        assert "value %= 5;" in c_code


class TestBitwiseAugmentedAssignment:
    """Test bitwise augmented assignment operators."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_bitwise_or_assignment(self):
        """Test |= operator."""
        python_code = """
def test_bitwise_or() -> int:
    flags: int = 0x01
    flags |= 0x04
    return flags
"""
        c_code = self.converter.convert_code(python_code)

        assert "int flags = 1;" in c_code  # 0x01 becomes 1
        assert "flags |= 4;" in c_code      # 0x04 becomes 4

    def test_bitwise_xor_assignment(self):
        """Test ^= operator."""
        python_code = """
def test_bitwise_xor() -> int:
    data: int = 0xFF
    data ^= 0x0F
    return data
"""
        c_code = self.converter.convert_code(python_code)

        assert "int data = 255;" in c_code  # 0xFF becomes 255
        assert "data ^= 15;" in c_code      # 0x0F becomes 15

    def test_bitwise_and_assignment(self):
        """Test &= operator."""
        python_code = """
def test_bitwise_and() -> int:
    mask: int = 0x3F
    mask &= 0x1F
    return mask
"""
        c_code = self.converter.convert_code(python_code)

        assert "int mask = 63;" in c_code   # 0x3F becomes 63
        assert "mask &= 31;" in c_code      # 0x1F becomes 31

    def test_left_shift_assignment(self):
        """Test <<= operator."""
        python_code = """
def test_left_shift() -> int:
    value: int = 3
    value <<= 2
    return value
"""
        c_code = self.converter.convert_code(python_code)

        assert "int value = 3;" in c_code
        assert "value <<= 2;" in c_code

    def test_right_shift_assignment(self):
        """Test >>= operator."""
        python_code = """
def test_right_shift() -> int:
    value: int = 16
    value >>= 3
    return value
"""
        c_code = self.converter.convert_code(python_code)

        assert "int value = 16;" in c_code
        assert "value >>= 3;" in c_code


class TestAugmentedAssignmentWithExpressions:
    """Test augmented assignment with complex expressions."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_augassign_with_variable_expression(self):
        """Test augmented assignment with variable expression."""
        python_code = """
def test_with_variable(a: int, b: int) -> int:
    total: int = 10
    total += a + b
    return total
"""
        c_code = self.converter.convert_code(python_code)

        assert "int total = 10;" in c_code
        assert "total += (a + b);" in c_code

    def test_augassign_with_function_call(self):
        """Test augmented assignment with function call."""
        python_code = """
def helper(x: int) -> int:
    return x * 2

def test_with_function_call() -> int:
    result: int = 5
    result += helper(3)
    return result
"""
        c_code = self.converter.convert_code(python_code)

        assert "int result = 5;" in c_code
        assert "result += helper(3);" in c_code

    def test_augassign_with_complex_expression(self):
        """Test augmented assignment with complex mathematical expression."""
        python_code = """
def test_complex_expression(x: int, y: int) -> int:
    value: int = 1
    value *= (x * 2 + y - 3)
    return value
"""
        c_code = self.converter.convert_code(python_code)

        assert "int value = 1;" in c_code
        assert "value *= (((x * 2) + y) - 3);" in c_code


class TestAugmentedAssignmentOOP:
    """Test augmented assignment with object-oriented features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_augassign_instance_variable(self):
        """Test augmented assignment on instance variables."""
        python_code = """
class Counter:
    def __init__(self, initial: int):
        self.count: int = initial

    def increment_by(self, amount: int):
        self.count += amount

    def multiply_by(self, factor: int):
        self.count *= factor
"""
        c_code = self.converter.convert_code(python_code)

        # Check struct definition
        assert "typedef struct Counter {" in c_code
        assert "int count;" in c_code

        # Check augmented assignment in methods
        assert "void Counter_increment_by(Counter* self, int amount)" in c_code
        assert "self->count += amount;" in c_code

        assert "void Counter_multiply_by(Counter* self, int factor)" in c_code
        assert "self->count *= factor;" in c_code

    def test_augassign_object_attribute(self):
        """Test augmented assignment on object attributes."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

def adjust_point() -> Point:
    p: Point = Point(5, 3)
    p.x += 2
    p.y *= 3
    return p
"""
        c_code = self.converter.convert_code(python_code)

        # Check object creation and attribute augmented assignment
        assert "Point p = Point_new(5, 3);" in c_code
        assert "p.x += 2;" in c_code
        assert "p.y *= 3;" in c_code


class TestAugmentedAssignmentErrorHandling:
    """Test error handling for augmented assignment."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_undeclared_variable_error(self):
        """Test error when using undeclared variable."""
        python_code = """
def test_undeclared() -> int:
    unknown_var += 5
    return unknown_var
"""
        with pytest.raises(TypeMappingError, match="Variable 'unknown_var' must be declared"):
            self.converter.convert_code(python_code)

    def test_unsupported_operator_error(self):
        """Test error for unsupported operators."""
        python_code = """
def test_unsupported() -> int:
    x: int = 5
    x **= 2  # Power assignment not supported
    return x
"""
        with pytest.raises(UnsupportedFeatureError, match="Unsupported augmented assignment operator"):
            self.converter.convert_code(python_code)

    def test_complex_target_error(self):
        """Test error for complex assignment targets."""
        python_code = """
def test_complex_target() -> list:
    my_list: list = [1, 2, 3]
    my_list[0] += 5  # Subscript assignment not supported
    return my_list
"""
        with pytest.raises(UnsupportedFeatureError, match="Only simple variable and attribute augmented assignments"):
            self.converter.convert_code(python_code)


class TestAugmentedAssignmentIntegration:
    """Test integration of augmented assignment with other features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_augassign_in_loops(self):
        """Test augmented assignment inside loops."""
        python_code = """
def sum_to_n(n: int) -> int:
    total: int = 0
    for i in range(n):
        total += i
    return total
"""
        c_code = self.converter.convert_code(python_code)

        assert "int total = 0;" in c_code
        assert "for (int i = 0; i < n; i += 1)" in c_code
        assert "total += i;" in c_code

    def test_augassign_with_conditionals(self):
        """Test augmented assignment with conditional statements."""
        python_code = """
def conditional_increment(x: int, should_double: bool) -> int:
    if should_double:
        x *= 2
    else:
        x += 1
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "if (should_double)" in c_code
        assert "x *= 2;" in c_code
        assert "} else {" in c_code
        assert "x += 1;" in c_code

    def test_multiple_augassign_operations(self):
        """Test multiple augmented assignment operations in sequence."""
        python_code = """
def multiple_operations() -> int:
    value: int = 10
    value += 5   # value = 15
    value *= 2   # value = 30
    value -= 8   # value = 22
    value /= 2   # value = 11
    value %= 7   # value = 4
    return value
"""
        c_code = self.converter.convert_code(python_code)

        assert "int value = 10;" in c_code
        assert "value += 5;" in c_code
        assert "value *= 2;" in c_code
        assert "value -= 8;" in c_code
        assert "value /= 2;" in c_code
        assert "value %= 7;" in c_code


@pytest.mark.parametrize("op_python,op_c", [
    ("+=", "+="),
    ("-=", "-="),
    ("*=", "*="),
    ("/=", "/="),
    ("//=", "/="),  # Floor division maps to regular division
    ("%=", "%="),
    ("|=", "|="),
    ("^=", "^="),
    ("&=", "&="),
    ("<<=", "<<="),
    (">>=", ">>="),
])
def test_augassign_operators_parametrized(op_python, op_c):
    """Test parametrized augmented assignment operators."""
    converter = MultiGenPythonToCConverter()

    python_code = f"""
def test_operator() -> int:
    x: int = 10
    x {op_python} 3
    return x
"""

    c_code = converter.convert_code(python_code)
    assert f"x {op_c} 3;" in c_code


@pytest.mark.integration
def test_augmented_assignment_comprehensive():
    """Comprehensive test of augmented assignment working with all features."""
    converter = MultiGenPythonToCConverter()

    python_code = """
class Calculator:
    def __init__(self, initial: int):
        self.value: int = initial

    def process_operations(self, a: int, b: int) -> int:
        # Test various augmented assignments
        self.value += a
        self.value *= 2

        temp: int = 5
        temp -= b
        temp |= 0x01

        if temp > 0:
            self.value += temp
        else:
            self.value -= 1

        return self.value

def test_calculator() -> int:
    calc: Calculator = Calculator(10)
    result: int = calc.process_operations(5, 3)
    return result
"""

    c_code = converter.convert_code(python_code)

    # Basic sanity checks
    assert c_code is not None
    assert len(c_code) > 200  # Should be substantial
    assert "typedef struct Calculator" in c_code
    assert "self->value += a;" in c_code
    assert "self->value *= 2;" in c_code
    assert "temp -= b;" in c_code
    assert "temp |= 1;" in c_code  # 0x01 becomes 1
    assert "#include" in c_code  # Should have includes