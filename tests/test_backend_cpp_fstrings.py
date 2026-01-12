"""Tests for C++ backend f-string support."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter


class TestCppFStringConversion:
    """Test f-string conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_f_string(self):
        """Test simple f-string with single variable."""
        python_code = """
def greet(name: str) -> str:
    return f"Hello {name}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string greet(std::string name)" in cpp_code
        # F-string should be converted to string concatenation
        assert '("Hello " + name)' in cpp_code or '"Hello " + name' in cpp_code

    def test_f_string_with_int(self):
        """Test f-string with integer variable."""
        python_code = """
def format_number(x: int) -> str:
    return f"Result: {x}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_number(int x)" in cpp_code
        # Should use std::to_string for int
        assert "std::to_string(x)" in cpp_code
        assert '"Result: "' in cpp_code

    def test_f_string_with_multiple_parts(self):
        """Test f-string with multiple expressions."""
        python_code = """
def format_info(count: int, name: str) -> str:
    return f"Count: {count} items for {name}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_info(int count, std::string name)" in cpp_code
        assert "std::to_string(count)" in cpp_code
        assert '"Count: "' in cpp_code
        assert '" items for "' in cpp_code

    def test_f_string_with_expression(self):
        """Test f-string with an expression."""
        python_code = """
def calculate_message(x: int, y: int) -> str:
    return f"Sum: {x + y}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string calculate_message(int x, int y)" in cpp_code
        # Expression should be wrapped in std::to_string
        assert "std::to_string((x + y))" in cpp_code
        assert '"Sum: "' in cpp_code

    def test_f_string_with_bool(self):
        """Test f-string with boolean variable."""
        python_code = """
def format_bool(flag: bool) -> str:
    return f"Status: {flag}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_bool(bool flag)" in cpp_code
        # Boolean should be converted to "true" or "false"
        assert '(flag ? "true" : "false")' in cpp_code
        assert '"Status: "' in cpp_code

    def test_f_string_with_float(self):
        """Test f-string with float variable."""
        python_code = """
def format_float(value: float) -> str:
    return f"Value: {value}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_float(double value)" in cpp_code
        assert "std::to_string(value)" in cpp_code
        assert '"Value: "' in cpp_code

    def test_f_string_only_literal(self):
        """Test f-string with only literal parts (no expressions)."""
        python_code = """
def get_message() -> str:
    return f"Hello World"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string get_message()" in cpp_code
        # Should just be a string literal
        assert '"Hello World"' in cpp_code

    def test_f_string_with_function_call(self):
        """Test f-string with function call expression."""
        python_code = """
def format_length(items: list[int]) -> str:
    return f"Length: {len(items)}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_length(std::vector<int> items)" in cpp_code
        assert "std::to_string(multigen::len(items))" in cpp_code
        assert '"Length: "' in cpp_code

    def test_f_string_in_variable_assignment(self):
        """Test f-string used in variable assignment."""
        python_code = """
def create_message(x: int) -> str:
    msg: str = f"Number: {x}"
    return msg
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string create_message(int x)" in cpp_code
        assert "std::string msg" in cpp_code
        assert "std::to_string(x)" in cpp_code
        assert '"Number: "' in cpp_code

    def test_nested_f_string_expressions(self):
        """Test f-string with nested expressions."""
        python_code = """
def complex_format(a: int, b: int, c: int) -> str:
    return f"Result: {a + b * c}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string complex_format(int a, int b, int c)" in cpp_code
        # Expression should be properly parenthesized
        assert "std::to_string((a + (b * c)))" in cpp_code
        assert '"Result: "' in cpp_code

    def test_f_string_empty_expression(self):
        """Test that empty f-string parts work correctly."""
        python_code = """
def format_edge(start: str, end: str) -> str:
    return f"{start} to {end}"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "std::string format_edge(std::string start, std::string end)" in cpp_code
        # Should handle strings without std::to_string
        assert "start" in cpp_code
        assert "end" in cpp_code
        assert '" to "' in cpp_code
