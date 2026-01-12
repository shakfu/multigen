"""Tests for Rust backend object-oriented programming support."""

import pytest

from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestRustOOPBasic:
    """Test basic OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_empty_class(self):
        """Test empty class conversion."""
        python_code = """
class EmptyClass:
    pass
"""
        rust_code = self.converter.convert_code(python_code)

        assert "struct EmptyClass {" in rust_code
        assert "_dummy: ()," in rust_code
        assert "impl EmptyClass {" in rust_code

    def test_simple_class_with_constructor(self):
        """Test class with constructor."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        rust_code = self.converter.convert_code(python_code)

        assert "struct Point {" in rust_code
        assert "x: i32," in rust_code
        assert "y: i32," in rust_code
        assert "fn new(x: i32, y: i32) -> Self" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "struct Rectangle {" in rust_code
        assert "width: i32," in rust_code
        assert "height: i32," in rust_code
        assert "fn area(&mut self) -> i32" in rust_code
        assert "fn perimeter(&mut self) -> i32" in rust_code
        assert "(self.width * self.height)" in rust_code

    def test_class_with_void_method(self):
        """Test class with void method."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self) -> None:
        self.value += 1
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn increment(&mut self)" in rust_code  # No return type
        assert "self.value += 1;" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "fn get_name(&mut self) -> String" in rust_code
        assert "fn set_age(&mut self, new_age: i32)" in rust_code
        assert "fn get_age(&mut self) -> i32" in rust_code
        assert "self.name" in rust_code  # Return self.name
        assert "self.age = new_age;" in rust_code


class TestRustOOPAdvanced:
    """Test advanced OOP functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

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
        rust_code = self.converter.convert_code(python_code)

        assert "let mut counter = Counter::new(5);" in rust_code
        assert "counter.increment();" in rust_code
        assert "counter.get_value()" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "let mut p1 = Point::new(3, 4);" in rust_code
        assert "let mut p2 = Point::new(5, 12);" in rust_code
        assert "p1.distance_from_origin()" in rust_code
        assert "p2.distance_from_origin()" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "title: String," in rust_code
        assert "author: String," in rust_code
        assert "fn new(title: String, author: String)" in rust_code
        assert "((self.title + \" by \".to_string()) + self.author)" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "name: String," in rust_code
        assert "age: i32," in rust_code
        assert "gpa: f64," in rust_code
        assert "enrolled: bool," in rust_code
        assert "(self.age >= 18)" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "if (amount <= self.balance)" in rust_code
        assert "self.balance -= amount;" in rust_code
        assert "self.balance += amount;" in rust_code
        assert "true" in rust_code
        assert "false" in rust_code


class TestRustOOPComplexScenarios:
    """Test complex OOP scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

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
        rust_code = self.converter.convert_code(python_code)

        assert "self.add(a);" in rust_code
        assert "self.multiply(b);" in rust_code
        assert "self.get_result()" in rust_code

    def test_class_constructor_with_defaults(self):
        """Test class constructor with default-like behavior."""
        python_code = """
class Config:
    def __init__(self, debug: bool):
        self.debug: bool = debug
        self.max_retries: int = 3
        self.timeout: float = 30.0
"""
        rust_code = self.converter.convert_code(python_code)

        assert "debug: bool," in rust_code
        assert "max_retries: i32," in rust_code
        assert "timeout: f64," in rust_code
        assert "debug: debug," in rust_code
        assert "max_retries: 3," in rust_code
        assert "timeout: 30," in rust_code  # 30.0 -> 30

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
        rust_code = self.converter.convert_code(python_code)

        # Both classes should be defined
        assert "struct Point {" in rust_code
        assert "struct Rectangle {" in rust_code

        # Point should have its constructor
        assert "fn new(x: i32, y: i32) -> Self" in rust_code

        # Rectangle should reference Point type (though this is complex in Rust)
        assert "width: i32," in rust_code
        assert "height: i32," in rust_code