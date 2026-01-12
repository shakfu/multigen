"""Tests for Go backend object-oriented programming support."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoOOPBasics:
    """Test basic OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_class_conversion(self):
        """Test simple class with constructor."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        go_code = self.converter.convert_code(python_code)

        # Check struct definition
        assert "type Point struct {" in go_code
        assert "X int" in go_code
        assert "Y int" in go_code

        # Check constructor
        assert "func NewPoint(x int, y int) Point" in go_code
        assert "obj.X = x" in go_code
        assert "obj.Y = y" in go_code

    def test_class_with_methods(self):
        """Test class with instance methods."""
        python_code = """
class Calculator:
    def __init__(self, value: int):
        self.value: int = value

    def add(self, amount: int) -> None:
        self.value += amount

    def get_value(self) -> int:
        return self.value
"""
        go_code = self.converter.convert_code(python_code)

        # Check struct
        assert "type Calculator struct {" in go_code
        assert "Value int" in go_code

        # Check methods
        assert "func (obj *Calculator) Add(amount int)" in go_code
        assert "obj.Value += amount" in go_code
        assert "func (obj *Calculator) GetValue() int" in go_code
        assert "return obj.Value" in go_code

    def test_class_with_string_attributes(self):
        """Test class with string attributes."""
        python_code = """
class Person:
    def __init__(self, name: str, age: int):
        self.name: str = name
        self.age: int = age

    def greet(self) -> str:
        return "Hello, I'm " + self.name
"""
        go_code = self.converter.convert_code(python_code)

        assert "Name string" in go_code
        assert "Age int" in go_code
        assert 'return ("Hello, I\'m " + obj.Name)' in go_code

    def test_empty_class(self):
        """Test empty class definition."""
        python_code = """
class Empty:
    pass
"""
        go_code = self.converter.convert_code(python_code)

        assert "type Empty struct {" in go_code


class TestGoOOPAdvanced:
    """Test advanced OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_method_with_control_flow(self):
        """Test method with if statements and loops."""
        python_code = """
class ControlFlow:
    def __init__(self, limit: int):
        self.limit: int = limit

    def process(self, x: int) -> int:
        if x > self.limit:
            result = x * 2
        else:
            result = x
        return result
"""
        go_code = self.converter.convert_code(python_code)

        assert "func (obj *ControlFlow) Process(x int) int" in go_code
        assert "if (x > obj.Limit)" in go_code
        assert "result := (x * 2)" in go_code
        assert "} else {" in go_code
        assert "result := x" in go_code

    def test_method_with_local_variables(self):
        """Test method that declares local variables."""
        python_code = """
class LocalVars:
    def __init__(self, base: int):
        self.base: int = base

    def calculate(self, multiplier: int) -> int:
        temp: int = self.base * multiplier
        result: int = temp + 10
        return result
"""
        go_code = self.converter.convert_code(python_code)

        assert "var temp int = (obj.Base * multiplier)" in go_code
        assert "var result int = (temp + 10)" in go_code

    def test_multiple_classes(self):
        """Test multiple classes in the same module."""
        python_code = """
class Rectangle:
    def __init__(self, width: int, height: int):
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height

class Circle:
    def __init__(self, radius: float):
        self.radius: float = radius

    def area(self) -> float:
        return 3.14 * self.radius * self.radius
"""
        go_code = self.converter.convert_code(python_code)

        # Check both classes
        assert "type Rectangle struct {" in go_code
        assert "type Circle struct {" in go_code
        assert "Width int" in go_code
        assert "Height int" in go_code
        assert "Radius float64" in go_code

    def test_class_instantiation_in_function(self):
        """Test creating class instances in regular functions."""
        python_code = """
class BankAccount:
    def __init__(self, balance: float):
        self.balance: float = balance

    def withdraw(self, amount: float) -> bool:
        if amount <= self.balance:
            self.balance -= amount
            return True
        return False

def create_account() -> BankAccount:
    return BankAccount(1000.0)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return NewBankAccount(1000)" in go_code
        assert "func NewBankAccount(balance float64) BankAccount" in go_code

    def test_method_calls_on_objects(self):
        """Test calling methods on object instances."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self) -> None:
        self.value += 1

    def get_value(self) -> int:
        return self.value

def test_counter() -> int:
    counter = Counter(5)
    counter.increment()
    return counter.get_value()
"""
        go_code = self.converter.convert_code(python_code)

        assert "counter := NewCounter(5)" in go_code
        assert "counter.Increment()" in go_code
        assert "return counter.GetValue()" in go_code

    def test_augmented_assignment_in_methods(self):
        """Test augmented assignment operations in methods."""
        python_code = """
class Accumulator:
    def __init__(self, initial: int):
        self.total: int = initial

    def add(self, value: int) -> None:
        self.total += value

    def multiply(self, factor: int) -> None:
        self.total *= factor
"""
        go_code = self.converter.convert_code(python_code)

        assert "obj.Total += value" in go_code
        assert "obj.Total *= factor" in go_code