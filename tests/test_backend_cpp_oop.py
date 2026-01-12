"""Tests for Python object-oriented programming features in C++ backend."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestCppOOPBasics:
    """Test basic OOP conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_class_definition(self):
        """Test simple class with instance variables."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check class definition
        assert "class Point {" in cpp_code
        assert "public:" in cpp_code
        assert "int x;" in cpp_code
        assert "int y;" in cpp_code

        # Check constructor
        assert "Point(int x, int y)" in cpp_code
        assert "this->x = x;" in cpp_code
        assert "this->y = y;" in cpp_code

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
        cpp_code = self.converter.convert_code(python_code)

        # Check class structure
        assert "class Rectangle {" in cpp_code
        assert "int width;" in cpp_code
        assert "int height;" in cpp_code

        # Check constructor
        assert "Rectangle(int width, int height)" in cpp_code

        # Check methods
        assert "int area()" in cpp_code
        assert "return (this->width * this->height);" in cpp_code
        assert "int perimeter()" in cpp_code
        assert "return (2 * (this->width + this->height));" in cpp_code

    def test_class_with_mixed_types(self):
        """Test class with different instance variable types."""
        python_code = """
class Person:
    def __init__(self, name: str, age: int, height: float, active: bool):
        self.name: str = name
        self.age: int = age
        self.height: float = height
        self.active: bool = active
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string name;" in cpp_code
        assert "int age;" in cpp_code
        assert "double height;" in cpp_code
        assert "bool active;" in cpp_code

        assert "Person(std::string name, int age, double height, bool active)" in cpp_code

    def test_class_method_with_parameters(self):
        """Test class method that takes parameters."""
        python_code = """
class Calculator:
    def __init__(self, initial: int):
        self.value: int = initial

    def add(self, x: int) -> int:
        self.value = self.value + x
        return self.value
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int add(int x)" in cpp_code
        assert "this->value = (this->value + x);" in cpp_code
        assert "return this->value;" in cpp_code

    def test_class_method_calling_another_method(self):
        """Test class methods that call other methods (simplified)."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.count: int = start

    def increment(self) -> int:
        self.count = self.count + 1
        return self.count

    def double_increment(self) -> int:
        self.increment()
        return self.increment()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int increment()" in cpp_code
        assert "int double_increment()" in cpp_code
        assert "this->count = (this->count + 1);" in cpp_code


class TestCppOOPAdvanced:
    """Test advanced OOP features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_empty_class(self):
        """Test class with no instance variables."""
        python_code = """
class EmptyClass:
    def __init__(self):
        pass

    def do_nothing(self) -> None:
        pass
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "class EmptyClass {" in cpp_code
        assert "EmptyClass()" in cpp_code
        assert "void do_nothing()" in cpp_code

    def test_class_with_inferred_types(self):
        """Test class with type inference for instance variables."""
        python_code = """
class InferredTypes:
    def __init__(self):
        self.number = 42
        self.text = "hello"
        self.flag = True
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int number;" in cpp_code
        assert "std::string text;" in cpp_code
        assert "bool flag;" in cpp_code
        assert "this->number = 42;" in cpp_code
        assert 'this->text = "hello";' in cpp_code
        assert "this->flag = true;" in cpp_code

    def test_class_with_complex_constructor(self):
        """Test class with complex constructor logic."""
        python_code = """
class ComplexInit:
    def __init__(self, base: int):
        self.value: int = base * 2 + 1
        self.doubled: int = self.value * 2
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "this->value = ((base * 2) + 1);" in cpp_code
        assert "this->doubled = (this->value * 2);" in cpp_code

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
        cpp_code = self.converter.convert_code(python_code)

        assert "int process(int x)" in cpp_code
        assert "if ((x > this->limit))" in cpp_code
        assert ("int result = (x * 2);" in cpp_code or "auto result = (x * 2);" in cpp_code)
        assert "} else {" in cpp_code
        assert ("int result = x;" in cpp_code or "auto result = x;" in cpp_code)

    def test_method_with_local_variables(self):
        """Test method that declares local variables."""
        python_code = """
class LocalVars:
    def __init__(self, initial: int):
        self.value: int = initial

    def compute(self, factor: int) -> int:
        temp: int = self.value * factor
        result: int = temp + 10
        return result
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int temp = (this->value * factor);" in cpp_code
        assert "int result = (temp + 10);" in cpp_code
        assert "return result;" in cpp_code


class TestCppMultipleClasses:
    """Test multiple classes in the same module."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_multiple_classes(self):
        """Test module with multiple class definitions."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

class Circle:
    def __init__(self, center_x: int, center_y: int, radius: int):
        self.center_x: int = center_x
        self.center_y: int = center_y
        self.radius: int = radius

    def area(self) -> float:
        return 3.14 * self.radius * self.radius
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check both classes are generated
        assert "class Point {" in cpp_code
        assert "class Circle {" in cpp_code

        # Check Point class
        assert "Point(int x, int y)" in cpp_code
        assert "int x;" in cpp_code and "int y;" in cpp_code

        # Check Circle class
        assert "Circle(int center_x, int center_y, int radius)" in cpp_code
        assert "int center_x;" in cpp_code
        assert "int center_y;" in cpp_code
        assert "int radius;" in cpp_code
        assert "double area()" in cpp_code
        assert "return ((3.14 * this->radius) * this->radius);" in cpp_code

    def test_class_and_function(self):
        """Test module with both classes and functions."""
        python_code = """
class Data:
    def __init__(self, value: int):
        self.value: int = value

def process_data(d: Data) -> int:
    return d.value * 2
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check class
        assert "class Data {" in cpp_code
        assert "Data(int value)" in cpp_code

        # Check function (simplified - attribute access)
        assert "int process_data(Data d)" in cpp_code
        # Note: Complex object interactions would need more sophisticated handling