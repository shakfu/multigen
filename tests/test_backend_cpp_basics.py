"""Tests for basic C++ backend functionality."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestCppBasicsConversion:
    """Test basic conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_function_conversion(self):
        """Test simple function with type annotations."""
        python_code = """
def add_numbers(a: int, b: int) -> int:
    return a + b
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int add_numbers(int a, int b)" in cpp_code
        assert "return (a + b);" in cpp_code
        assert "#include <iostream>" in cpp_code
        assert "using namespace std;" in cpp_code

    def test_function_with_string_parameters(self):
        """Test function with string parameters."""
        python_code = """
def greet(name: str) -> str:
    return "Hello " + name
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string greet(std::string name)" in cpp_code
        assert 'return ("Hello " + name);' in cpp_code

    def test_function_with_multiple_types(self):
        """Test function with various parameter types."""
        python_code = """
def process(count: int, rate: float, active: bool, name: str) -> str:
    return name
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string process(int count, double rate, bool active, std::string name)" in cpp_code
        assert "return name;" in cpp_code

    def test_void_return_function(self):
        """Test function with no return value."""
        python_code = """
def print_message(msg: str) -> None:
    print(msg)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "void print_message(std::string msg)" in cpp_code
        assert "cout << msg << endl;" in cpp_code

    def test_auto_type_inference(self):
        """Test automatic type inference when annotations are missing."""
        python_code = """
def mystery_function(x, y):
    return x + y
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "auto mystery_function(auto x, auto y)" in cpp_code
        assert "return (x + y);" in cpp_code


class TestCppBasicStatements:
    """Test basic statement conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_variable_assignment(self):
        """Test variable assignment with type inference."""
        python_code = """
def test_assignment() -> int:
    x = 5
    y = "hello"
    z = 3.14
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int x = 5;" in cpp_code
        assert 'std::string y = "hello";' in cpp_code
        assert "double z = 3.14;" in cpp_code

    def test_annotated_assignment(self):
        """Test annotated variable assignment."""
        python_code = """
def test_annotated() -> int:
    count: int = 10
    name: str = "test"
    return count
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int count = 10;" in cpp_code
        assert 'std::string name = "test";' in cpp_code

    def test_if_statement(self):
        """Test if statement conversion."""
        python_code = """
def test_if(x: int) -> int:
    if x > 5:
        return x * 2
    else:
        return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "if ((x > 5))" in cpp_code
        assert "return (x * 2);" in cpp_code
        assert "} else {" in cpp_code
        assert "return x;" in cpp_code

    def test_while_loop(self):
        """Test while loop conversion."""
        python_code = """
def test_while(n: int) -> int:
    i = 0
    while i < n:
        i = i + 1
    return i
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "while ((i < n))" in cpp_code
        assert "int i = 0;" in cpp_code
        assert "i = (i + 1);" in cpp_code

    def test_for_range_loop(self):
        """Test for loop with range conversion."""
        python_code = """
def test_for(n: int) -> int:
    total = 0
    for i in range(n):
        total = total + i
    return total
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "for (int i = 0; i < n; i++)" in cpp_code
        assert "int total = 0;" in cpp_code
        assert "total = (total + i);" in cpp_code

    def test_for_range_with_start_stop(self):
        """Test for loop with range(start, stop)."""
        python_code = """
def test_for_range(start: int, stop: int) -> int:
    total = 0
    for i in range(start, stop):
        total = total + i
    return total
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "for (int i = start; i < stop; i++)" in cpp_code


class TestCppExpressions:
    """Test expression conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_binary_operations(self):
        """Test binary operations."""
        python_code = """
def test_ops(a: int, b: int) -> int:
    return a + b - a * b / a
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "return ((a + b) - ((a * b) / a));" in cpp_code

    def test_comparison_operations(self):
        """Test comparison operations."""
        python_code = """
def test_compare(a: int, b: int) -> bool:
    return a > b and a >= b or a < b
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "((a > b) && (a >= b)) || (a < b)" in cpp_code

    def test_unary_operations(self):
        """Test unary operations."""
        python_code = """
def test_unary(x: int, flag: bool) -> int:
    return -x + +x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "return ((-x) + (+x));" in cpp_code

    def test_boolean_constants(self):
        """Test boolean constant conversion."""
        python_code = """
def test_bool() -> bool:
    x = True
    y = False
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "bool x = true;" in cpp_code
        assert "bool y = false;" in cpp_code

    def test_string_constants(self):
        """Test string constant conversion."""
        python_code = """
def test_string() -> str:
    message = "Hello World"
    return message
"""
        cpp_code = self.converter.convert_code(python_code)

        assert 'std::string message = "Hello World";' in cpp_code


class TestCppBuiltinFunctions:
    """Test built-in function conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_print_function(self):
        """Test print() function conversion."""
        python_code = """
def test_print(msg: str) -> None:
    print(msg)
    print("Hello", "World")
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "cout << msg << endl;" in cpp_code
        assert 'cout << "Hello" << " " << "World" << endl;' in cpp_code

    def test_len_function(self):
        """Test len() function conversion."""
        python_code = """
def test_len(items: list) -> int:
    return len(items)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "return multigen::len(items);" in cpp_code

    def test_abs_function(self):
        """Test abs() function conversion."""
        python_code = """
def test_abs(x: int) -> int:
    return abs(x)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "return multigen::abs(x);" in cpp_code

    def test_range_function(self):
        """Test range() function conversion."""
        python_code = """
def test_range() -> None:
    r = range(10)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "Range(10)" in cpp_code