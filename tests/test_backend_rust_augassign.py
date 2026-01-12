"""Tests for Rust backend augmented assignment support."""

import pytest

from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestRustAugAssignBasic:
    """Test basic augmented assignment functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_add_assignment(self):
        """Test += operator."""
        python_code = """
def test_add_assign(x: int) -> int:
    x += 5
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x += 5;" in rust_code

    def test_subtract_assignment(self):
        """Test -= operator."""
        python_code = """
def test_sub_assign(x: int) -> int:
    x -= 3
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x -= 3;" in rust_code

    def test_multiply_assignment(self):
        """Test *= operator."""
        python_code = """
def test_mul_assign(x: int) -> int:
    x *= 2
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x *= 2;" in rust_code

    def test_divide_assignment(self):
        """Test /= operator."""
        python_code = """
def test_div_assign(x: float) -> float:
    x /= 2.0
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x /= 2;" in rust_code  # 2.0 -> 2

    def test_floor_divide_assignment(self):
        """Test //= operator (mapped to /= in Rust)."""
        python_code = """
def test_floor_div_assign(x: int) -> int:
    x //= 3
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x /= 3;" in rust_code

    def test_modulo_assignment(self):
        """Test %= operator."""
        python_code = """
def test_mod_assign(x: int) -> int:
    x %= 4
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x %= 4;" in rust_code

    def test_bitwise_or_assignment(self):
        """Test |= operator."""
        python_code = """
def test_or_assign(x: int) -> int:
    x |= 7
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x |= 7;" in rust_code

    def test_bitwise_xor_assignment(self):
        """Test ^= operator."""
        python_code = """
def test_xor_assign(x: int) -> int:
    x ^= 15
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x ^= 15;" in rust_code

    def test_bitwise_and_assignment(self):
        """Test &= operator."""
        python_code = """
def test_and_assign(x: int) -> int:
    x &= 31
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x &= 31;" in rust_code

    def test_left_shift_assignment(self):
        """Test <<= operator."""
        python_code = """
def test_lshift_assign(x: int) -> int:
    x <<= 2
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x <<= 2;" in rust_code

    def test_right_shift_assignment(self):
        """Test >>= operator."""
        python_code = """
def test_rshift_assign(x: int) -> int:
    x >>= 1
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x >>= 1;" in rust_code


class TestRustAugAssignVariables:
    """Test augmented assignment with different variable types."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_local_variable_assignment(self):
        """Test augmented assignment on local variables."""
        python_code = """
def test_local_vars() -> int:
    counter: int = 10
    multiplier: int = 3
    counter += multiplier
    counter *= 2
    return counter
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut counter: i32 = 10;" in rust_code
        assert "counter += multiplier;" in rust_code
        assert "counter *= 2;" in rust_code

    def test_multiple_assignments(self):
        """Test multiple augmented assignments in sequence."""
        python_code = """
def test_multiple() -> int:
    x: int = 5
    x += 2
    x *= 3
    x -= 1
    x /= 2
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "x += 2;" in rust_code
        assert "x *= 3;" in rust_code
        assert "x -= 1;" in rust_code
        assert "x /= 2;" in rust_code

    def test_assignment_with_expressions(self):
        """Test augmented assignment with complex expressions."""
        python_code = """
def test_expressions(a: int, b: int) -> int:
    result: int = 10
    result += a * b
    result -= a + b
    return result
"""
        rust_code = self.converter.convert_code(python_code)

        assert "result += (a * b);" in rust_code
        assert "result -= (a + b);" in rust_code


class TestRustAugAssignClasses:
    """Test augmented assignment in class contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_instance_variable_assignment(self):
        """Test augmented assignment on instance variables."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self, amount: int) -> None:
        self.value += amount

    def double(self) -> None:
        self.value *= 2
"""
        rust_code = self.converter.convert_code(python_code)

        assert "self.value += amount;" in rust_code
        assert "self.value *= 2;" in rust_code

    def test_complex_instance_operations(self):
        """Test complex augmented assignment operations."""
        python_code = """
class Calculator:
    def __init__(self, initial: float):
        self.total: float = initial
        self.count: int = 0

    def add_value(self, value: float) -> None:
        self.total += value
        self.count += 1

    def apply_tax(self, rate: float) -> None:
        self.total *= (1.0 + rate)

    def apply_discount(self, percent: float) -> None:
        self.total -= self.total * percent
"""
        rust_code = self.converter.convert_code(python_code)

        assert "self.total += value;" in rust_code
        assert "self.count += 1;" in rust_code
        assert "self.total *= (1 + rate);" in rust_code  # 1.0 -> 1
        assert "self.total -= (self.total * percent);" in rust_code

    def test_augassign_with_method_calls(self):
        """Test augmented assignment combined with method calls."""
        python_code = """
class Accumulator:
    def __init__(self, base: int):
        self.value: int = base

    def get_increment(self) -> int:
        return 5

    def update(self) -> None:
        self.value += self.get_increment()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "self.value += self.get_increment();" in rust_code

    def test_nested_class_operations(self):
        """Test augmented assignment in nested class scenarios."""
        python_code = """
class BankAccount:
    def __init__(self, balance: float):
        self.balance: float = balance

    def deposit(self, amount: float) -> None:
        self.balance += amount

    def withdraw(self, amount: float) -> bool:
        if amount <= self.balance:
            self.balance -= amount
            return True
        return False

    def apply_interest(self, rate: float) -> None:
        self.balance *= (1.0 + rate)

def test_account() -> float:
    account = BankAccount(1000.0)
    account.deposit(500.0)
    account.withdraw(200.0)
    account.apply_interest(0.05)
    return account.balance
"""
        rust_code = self.converter.convert_code(python_code)

        assert "self.balance += amount;" in rust_code
        assert "self.balance -= amount;" in rust_code
        assert "self.balance *= (1 + rate);" in rust_code  # 1.0 -> 1
        assert "let mut account = BankAccount::new(1000);" in rust_code  # 1000.0 -> 1000
        assert "account.deposit(500);" in rust_code  # 500.0 -> 500


class TestRustAugAssignAdvanced:
    """Test advanced augmented assignment scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_augassign_with_complex_expressions(self):
        """Test augmented assignment with complex right-hand expressions."""
        python_code = """
def test_complex_expressions(x: int, y: int, z: int) -> int:
    result: int = 0
    result += (x * y) + (z ** 2)
    result *= abs(x - y)
    return result
"""
        rust_code = self.converter.convert_code(python_code)

        assert "result += ((x * y) + z.pow(2 as u32));" in rust_code
        assert "result *= Builtins::abs_i32((x - y));" in rust_code

    def test_augassign_with_function_calls(self):
        """Test augmented assignment with function call results."""
        python_code = """
def helper_function(n: int) -> int:
    return n * 2

def test_function_calls() -> int:
    total: int = 10
    total += helper_function(5)
    total -= abs(-3)
    return total
"""
        rust_code = self.converter.convert_code(python_code)

        assert "total += helper_function(5);" in rust_code
        assert "total -= Builtins::abs_i32((-3));" in rust_code

    def test_augassign_in_loops(self):
        """Test augmented assignment within loops."""
        python_code = """
def test_in_loops(n: int) -> int:
    total: int = 0
    for i in range(n):
        total += i
        if i > 5:
            total *= 2
    return total
"""
        rust_code = self.converter.convert_code(python_code)

        assert "total += i;" in rust_code
        assert "total *= 2;" in rust_code

    def test_augassign_chaining(self):
        """Test chaining multiple augmented assignments."""
        python_code = """
def test_chaining() -> int:
    a: int = 10
    b: int = 5
    c: int = 2

    a += b
    b *= c
    c -= 1

    a += b + c
    return a
"""
        rust_code = self.converter.convert_code(python_code)

        assert "a += b;" in rust_code
        assert "b *= c;" in rust_code
        assert "c -= 1;" in rust_code
        assert "a += (b + c);" in rust_code