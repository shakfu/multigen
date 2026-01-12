"""Tests for Haskell backend object-oriented programming support."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestHaskellOOPBasic:
    """Test basic OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_empty_class(self):
        """Test empty class conversion."""
        python_code = """
class EmptyClass:
    pass
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "data EmptyClass = EmptyClass" in haskell_code
        assert "dummy :: ()" in haskell_code

    def test_simple_class_with_constructor(self):
        """Test class with constructor."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "data Point = Point" in haskell_code
        assert "x :: Int" in haskell_code
        assert "y :: Int" in haskell_code
        assert "newPoint :: Int -> Int -> Point" in haskell_code

    def test_class_with_methods(self):
        """Test class with instance methods."""
        python_code = """
class Rectangle:
    def __init__(self, width: int, height: int):
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height

    def perimeter(self) -> int:
        return 2 * (self.width + self.height)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "data Rectangle = Rectangle" in haskell_code
        assert "width :: Int" in haskell_code
        assert "height :: Int" in haskell_code
        assert "area :: Rectangle -> Int" in haskell_code
        assert "perimeter :: Rectangle -> Int" in haskell_code

    def test_class_with_void_method(self):
        """Test class with void method."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self) -> None:
        self.value += 1
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "increment :: Counter -> ()" in haskell_code

    def test_class_with_getters_setters(self):
        """Test class with getter and setter methods."""
        python_code = """
class Person:
    def __init__(self, name: str, age: int):
        self.name: str = name
        self.age: int = age

    def get_name(self) -> str:
        return self.name

    def set_age(self, new_age: int) -> None:
        self.age = new_age

    def get_age(self) -> int:
        return self.age
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "getName :: Person -> String" in haskell_code
        assert "setAge :: Person -> Int -> ()" in haskell_code
        assert "getAge :: Person -> Int" in haskell_code


class TestHaskellOOPAdvanced:
    """Test advanced OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

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
        haskell_code = self.converter.convert_code(python_code)

        assert "newCounter 5" in haskell_code
        assert "increment counter" in haskell_code
        assert "getValue counter" in haskell_code

    def test_multiple_objects(self):
        """Test creating multiple objects of same class."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

    def distance_from_origin(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5

def test_points() -> float:
    p1 = Point(3, 4)
    p2 = Point(5, 12)
    return p1.distance_from_origin() + p2.distance_from_origin()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "newPoint 3 4" in haskell_code
        assert "newPoint 5 12" in haskell_code
        assert "distanceFromOrigin p1" in haskell_code
        assert "distanceFromOrigin p2" in haskell_code

    def test_class_with_string_attributes(self):
        """Test class with string attributes."""
        python_code = """
class Book:
    def __init__(self, title: str, author: str):
        self.title: str = title
        self.author: str = author

    def get_info(self) -> str:
        return self.title + " by " + self.author
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "title :: String" in haskell_code
        assert "author :: String" in haskell_code
        assert "newBook :: String -> String -> Book" in haskell_code

    def test_class_with_mixed_types(self):
        """Test class with mixed attribute types."""
        python_code = """
class Student:
    def __init__(self, name: str, age: int, gpa: float, enrolled: bool):
        self.name: str = name
        self.age: int = age
        self.gpa: float = gpa
        self.enrolled: bool = enrolled

    def is_adult(self) -> bool:
        return self.age >= 18

    def get_summary(self) -> str:
        status = "enrolled" if self.enrolled else "not enrolled"
        return self.name + " is " + status
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "name :: String" in haskell_code
        assert "age :: Int" in haskell_code
        assert "gpa :: Double" in haskell_code
        assert "enrolled :: Bool" in haskell_code

    def test_class_method_with_logic(self):
        """Test class method with conditional logic."""
        python_code = """
class BankAccount:
    def __init__(self, balance: float):
        self.balance: float = balance

    def withdraw(self, amount: float) -> bool:
        if amount <= self.balance:
            self.balance -= amount
            return True
        else:
            return False

    def deposit(self, amount: float) -> None:
        self.balance += amount
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify method signatures (method bodies are simplified)
        assert "withdraw :: BankAccount -> Double -> Bool" in haskell_code
        assert "deposit :: BankAccount -> Double -> ()" in haskell_code


class TestHaskellOOPComplexScenarios:
    """Test complex OOP scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_class_with_method_chaining(self):
        """Test method calls within method implementations."""
        python_code = """
class Calculator:
    def __init__(self, value: int):
        self.value: int = value

    def add(self, x: int) -> None:
        self.value += x

    def multiply(self, x: int) -> None:
        self.value *= x

    def get_result(self) -> int:
        return self.value

    def calculate(self, a: int, b: int) -> int:
        self.add(a)
        self.multiply(b)
        return self.get_result()
"""
        haskell_code = self.converter.convert_code(python_code)

        # Haskell doesn't have mutable state in the same way,
        # so we just check for basic structure
        assert "data Calculator = Calculator" in haskell_code
        assert "calculate :: Calculator -> Int -> Int -> Int" in haskell_code

    def test_class_constructor_with_defaults(self):
        """Test class constructor with default-like behavior."""
        python_code = """
class Config:
    def __init__(self, debug: bool):
        self.debug: bool = debug
        self.max_retries: int = 3
        self.timeout: float = 30.0
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "debug :: Bool" in haskell_code
        assert "maxRetries :: Int" in haskell_code
        assert "timeout :: Double" in haskell_code
        assert "debug = debug" in haskell_code
        assert "maxRetries = 3" in haskell_code
        assert "timeout = 30" in haskell_code

    def test_multiple_classes_interaction(self):
        """Test multiple classes that interact."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

class Rectangle:
    def __init__(self, top_left: Point, width: int, height: int):
        self.top_left = top_left
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height
"""
        haskell_code = self.converter.convert_code(python_code)

        # Both classes should be defined
        assert "data Point = Point" in haskell_code
        assert "data Rectangle = Rectangle" in haskell_code

        # Point should have its constructor
        assert "newPoint :: Int -> Int -> Point" in haskell_code

        # Rectangle should have appropriate fields
        assert "width :: Int" in haskell_code
        assert "height :: Int" in haskell_code