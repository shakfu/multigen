"""Tests for basic Go backend functionality."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoBasicsConversion:
    """Test basic conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_function_conversion(self):
        """Test simple function with type annotations."""
        python_code = """
def add_numbers(a: int, b: int) -> int:
    return a + b
"""
        go_code = self.converter.convert_code(python_code)

        assert "func add_numbers(a int, b int) int" in go_code
        assert "return (a + b)" in go_code
        assert 'import "multigenproject/multigen"' in go_code
        assert "package main" in go_code

    def test_function_with_string_parameters(self):
        """Test function with string parameters."""
        python_code = """
def greet(name: str) -> str:
    return "Hello " + name
"""
        go_code = self.converter.convert_code(python_code)

        assert "func greet(name string) string" in go_code
        assert 'return ("Hello " + name)' in go_code

    def test_function_with_multiple_types(self):
        """Test function with various parameter types."""
        python_code = """
def process(count: int, rate: float, active: bool, name: str) -> str:
    return name
"""
        go_code = self.converter.convert_code(python_code)

        assert "func process(count int, rate float64, active bool, name string) string" in go_code
        assert "return name" in go_code

    def test_void_return_function(self):
        """Test function with no return value."""
        python_code = """
def print_message(msg: str) -> None:
    print(msg)
"""
        go_code = self.converter.convert_code(python_code)

        assert "func print_message(msg string)" in go_code
        assert "multigen.Print(msg)" in go_code

    def test_auto_type_inference(self):
        """Test automatic type inference when annotations are missing."""
        python_code = """
def mystery_function(x, y):
    return x + y
"""
        go_code = self.converter.convert_code(python_code)

        assert "func mystery_function(x interface{}, y interface{}) interface{}" in go_code
        assert "return (x + y)" in go_code


class TestGoBasicStatements:
    """Test basic statement conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_variable_assignment(self):
        """Test variable assignment with type inference."""
        python_code = """
def test_assignment() -> int:
    x = 5
    y = "hello"
    z = 3.14
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "var x int = 5" in go_code
        assert 'var y string = "hello"' in go_code
        assert "var z float64 = 3.14" in go_code

    def test_annotated_assignment(self):
        """Test annotated variable assignment."""
        python_code = """
def test_annotated() -> int:
    count: int = 10
    name: str = "test"
    return count
"""
        go_code = self.converter.convert_code(python_code)

        assert "var count int = 10" in go_code
        assert 'var name string = "test"' in go_code

    def test_if_statement(self):
        """Test if statement conversion."""
        python_code = """
def test_if(x: int) -> int:
    if x > 5:
        return x * 2
    else:
        return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "if (x > 5)" in go_code
        assert "return (x * 2)" in go_code
        assert "} else {" in go_code
        assert "return x" in go_code

    def test_while_loop(self):
        """Test while loop conversion."""
        python_code = """
def test_while(n: int) -> int:
    i = 0
    while i < n:
        i = i + 1
    return i
"""
        go_code = self.converter.convert_code(python_code)

        assert "for (i < n)" in go_code
        assert "var i int = 0" in go_code
        assert "i = (i + 1)" in go_code

    def test_for_range_loop(self):
        """Test for loop with range conversion."""
        python_code = """
def test_for(n: int) -> int:
    total = 0
    for i in range(n):
        total = total + i
    return total
"""
        go_code = self.converter.convert_code(python_code)

        assert "for i := 0; i < n; i++" in go_code
        assert "var total int = 0" in go_code
        assert "total = (total + i)" in go_code

    def test_for_range_with_start_stop(self):
        """Test for loop with range(start, stop)."""
        python_code = """
def test_for_range(start: int, stop: int) -> int:
    total = 0
    for i in range(start, stop):
        total = total + i
    return total
"""
        go_code = self.converter.convert_code(python_code)

        assert "for i := start; i < stop; i++" in go_code


class TestGoExpressions:
    """Test expression conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_binary_operations(self):
        """Test binary operations."""
        python_code = """
def test_ops(a: int, b: int) -> int:
    return a + b - a * b / a
"""
        go_code = self.converter.convert_code(python_code)

        assert "return ((a + b) - ((a * b) / a))" in go_code

    def test_comparison_operations(self):
        """Test comparison operations."""
        python_code = """
def test_compare(a: int, b: int) -> bool:
    return a > b
"""
        go_code = self.converter.convert_code(python_code)

        assert "return (a > b)" in go_code

    def test_unary_operations(self):
        """Test unary operations."""
        python_code = """
def test_unary(x: int, flag: bool) -> int:
    return -x + +x
"""
        go_code = self.converter.convert_code(python_code)

        assert "return ((-x) + (+x))" in go_code

    def test_boolean_constants(self):
        """Test boolean constant conversion."""
        python_code = """
def test_bool() -> bool:
    x = True
    y = False
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert "var x bool = true" in go_code
        assert "var y bool = false" in go_code

    def test_string_constants(self):
        """Test string constant conversion."""
        python_code = """
def test_string() -> str:
    message = "Hello World"
    return message
"""
        go_code = self.converter.convert_code(python_code)

        assert 'var message string = "Hello World"' in go_code


class TestGoBuiltinFunctions:
    """Test built-in function conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_print_function(self):
        """Test print() function conversion."""
        python_code = """
def test_print(msg: str) -> None:
    print(msg)
    print("Hello", "World")
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.Print(msg)" in go_code
        assert 'multigen.Print("Hello", "World")' in go_code

    def test_len_function(self):
        """Test len() function conversion."""
        python_code = """
def test_len(items: list) -> int:
    return len(items)
"""
        go_code = self.converter.convert_code(python_code)

        # list annotation defaults to []int now
        assert "return multigen.Len[int](items)" in go_code

    def test_abs_function(self):
        """Test abs() function conversion."""
        python_code = """
def test_abs(x: int) -> int:
    return abs(x)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.AbsInt(x)" in go_code

    def test_range_function(self):
        """Test range() function conversion."""
        python_code = """
def test_range() -> None:
    r = range(10)
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.NewRange(10)" in go_code

    def test_min_max_functions(self):
        """Test min() and max() function conversion."""
        python_code = """
def test_min_max(items: list) -> int:
    minimum = min(items)
    maximum = max(items)
    return minimum
"""
        go_code = self.converter.convert_code(python_code)

        # list annotation defaults to []int now
        assert "multigen.Min[int](items)" in go_code
        assert "multigen.Max[int](items)" in go_code