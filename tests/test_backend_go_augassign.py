"""Tests for Go backend augmented assignment support."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoAugAssignBasic:
    """Test basic augmented assignment functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_add_assignment(self):
        """Test += operator."""
        python_code = """
def test_add_assign(x: int) -> int:
    x += 5
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x += 5" in go_code

    def test_subtract_assignment(self):
        """Test -= operator."""
        python_code = """
def test_sub_assign(x: int) -> int:
    x -= 3
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x -= 3" in go_code

    def test_multiply_assignment(self):
        """Test *= operator."""
        python_code = """
def test_mul_assign(x: int) -> int:
    x *= 2
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x *= 2" in go_code

    def test_divide_assignment(self):
        """Test /= operator."""
        python_code = """
def test_div_assign(x: float) -> float:
    x /= 2.0
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x /= 2" in go_code

    def test_floor_divide_assignment(self):
        """Test //= operator (mapped to /= in Go)."""
        python_code = """
def test_floor_div_assign(x: int) -> int:
    x //= 3
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x /= 3" in go_code

    def test_modulo_assignment(self):
        """Test %= operator."""
        python_code = """
def test_mod_assign(x: int) -> int:
    x %= 4
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x %= 4" in go_code

    def test_bitwise_or_assignment(self):
        """Test |= operator."""
        python_code = """
def test_or_assign(x: int) -> int:
    x |= 7
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x |= 7" in go_code

    def test_bitwise_xor_assignment(self):
        """Test ^= operator."""
        python_code = """
def test_xor_assign(x: int) -> int:
    x ^= 15
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x ^= 15" in go_code

    def test_bitwise_and_assignment(self):
        """Test &= operator."""
        python_code = """
def test_and_assign(x: int) -> int:
    x &= 31
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x &= 31" in go_code

    def test_left_shift_assignment(self):
        """Test <<= operator."""
        python_code = """
def test_lshift_assign(x: int) -> int:
    x <<= 2
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x <<= 2" in go_code

    def test_right_shift_assignment(self):
        """Test >>= operator."""
        python_code = """
def test_rshift_assign(x: int) -> int:
    x >>= 1
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "x >>= 1" in go_code


class TestGoAugAssignVariables:
    """Test augmented assignment with different variable types."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

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
        go_code = self.converter.convert_code(python_code)

        assert "var counter int = 10" in go_code
        assert "counter += multiplier" in go_code
        assert "counter *= 2" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "x += 2" in go_code
        assert "x *= 3" in go_code
        assert "x -= 1" in go_code
        assert "x /= 2" in go_code

    def test_assignment_with_expressions(self):
        """Test augmented assignment with complex expressions."""
        python_code = """
def test_expressions(a: int, b: int) -> int:
    result: int = 10
    result += a * b
    result -= a + b
    return result
"""
        go_code = self.converter.convert_code(python_code)

        assert "result += (a * b)" in go_code
        assert "result -= (a + b)" in go_code


class TestGoAugAssignClasses:
    """Test augmented assignment in class contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

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
        go_code = self.converter.convert_code(python_code)

        assert "obj.Value += amount" in go_code
        assert "obj.Value *= 2" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "obj.Total += value" in go_code
        assert "obj.Count += 1" in go_code
        assert "obj.Total *= (1 + rate)" in go_code
        assert "obj.Total -= (obj.Total * percent)" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "obj.Value += obj.GetIncrement()" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "obj.Balance += amount" in go_code
        assert "obj.Balance -= amount" in go_code
        assert "obj.Balance *= (1 + rate)" in go_code
        assert "account := NewBankAccount(1000)" in go_code
        assert "account.Deposit(500)" in go_code