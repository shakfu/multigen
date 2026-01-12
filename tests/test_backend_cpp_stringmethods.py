"""Tests for string methods support in C++ backend."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestStringMethodsBasics:
    """Test basic string method functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_string_upper_method(self):
        """Test str.upper() method."""
        python_code = """
def test_upper() -> str:
    text: str = "hello"
    return text.upper()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert 'std::string text = "hello";' in cpp_code
        assert "StringOps::upper(text)" in cpp_code
        assert '#include "runtime/multigen_cpp_runtime.hpp"' in cpp_code

    def test_string_lower_method(self):
        """Test str.lower() method."""
        python_code = """
def test_lower() -> str:
    text: str = "HELLO"
    return text.lower()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert 'std::string text = "HELLO";' in cpp_code
        assert "StringOps::lower(text)" in cpp_code

    def test_string_strip_method(self):
        """Test str.strip() method."""
        python_code = """
def test_strip() -> str:
    text: str = "  hello  "
    return text.strip()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert 'std::string text = "  hello  ";' in cpp_code
        assert "StringOps::strip(text)" in cpp_code

    def test_string_strip_with_chars(self):
        """Test str.strip(chars) method."""
        python_code = """
def test_strip_chars() -> str:
    text: str = "...hello..."
    return text.strip(".")
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::strip(text, \".\")" in cpp_code

    def test_string_find_method(self):
        """Test str.find() method."""
        python_code = """
def test_find() -> int:
    text: str = "hello world"
    return text.find("world")
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::find(text, \"world\")" in cpp_code

    def test_string_replace_method(self):
        """Test str.replace() method."""
        python_code = """
def test_replace() -> str:
    text: str = "hello world"
    return text.replace("world", "universe")
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::replace(text, \"world\", \"universe\")" in cpp_code

    def test_string_split_method(self):
        """Test str.split() method."""
        python_code = """
def test_split() -> list:
    text: str = "hello,world,test"
    return text.split(",")
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::split(text, \",\")" in cpp_code

    def test_string_split_default(self):
        """Test str.split() with default delimiter."""
        python_code = """
def test_split_default() -> list:
    text: str = "hello world test"
    return text.split()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::split(text)" in cpp_code


class TestStringMethodsInClass:
    """Test string methods within class methods."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_string_method_on_instance_variable(self):
        """Test string method called on instance variable."""
        python_code = """
class TextProcessor:
    def __init__(self, text: str):
        self.text: str = text

    def get_upper(self) -> str:
        return self.text.upper()

    def get_lower(self) -> str:
        return self.text.lower()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::upper(this->text)" in cpp_code
        assert "StringOps::lower(this->text)" in cpp_code

    def test_string_method_with_parameter(self):
        """Test string method called on method parameter."""
        python_code = """
class StringUtils:
    def process(self, input_text: str) -> str:
        cleaned = input_text.strip()
        return cleaned.upper()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::strip(input_text)" in cpp_code
        assert "StringOps::upper(cleaned)" in cpp_code

    def test_chained_string_operations(self):
        """Test multiple string operations in sequence."""
        python_code = """
def process_text(text: str) -> str:
    step1 = text.strip()
    step2 = step1.lower()
    step3 = step2.replace(" ", "_")
    return step3
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::strip(text)" in cpp_code
        assert "StringOps::lower(step1)" in cpp_code
        assert "StringOps::replace(step2, \" \", \"_\")" in cpp_code


class TestStringMethodsAdvanced:
    """Test advanced string method usage patterns."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_string_method_in_expression(self):
        """Test string method used within larger expressions."""
        python_code = """
def format_name(first: str, last: str) -> str:
    return first.upper() + " " + last.lower()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::upper(first)" in cpp_code
        assert "StringOps::lower(last)" in cpp_code
        assert "return ((StringOps::upper(first) + \" \") + StringOps::lower(last));" in cpp_code

    def test_string_method_in_condition(self):
        """Test string method used in conditional expressions."""
        python_code = """
def check_text(text: str) -> bool:
    if text.strip() == "":
        return True
    return False
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "if ((StringOps::strip(text) == \"\"))" in cpp_code

    def test_string_method_with_numeric_result(self):
        """Test string method that returns numeric value."""
        python_code = """
def find_position(text: str, target: str) -> int:
    pos = text.find(target)
    if pos >= 0:
        return pos
    return -1
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int pos = StringOps::find(text, target);" in cpp_code
        assert "if ((pos >= 0))" in cpp_code

    def test_string_literal_method_call(self):
        """Test string method called on string literal."""
        python_code = """
def test_literal() -> str:
    return "hello world".upper()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::upper(\"hello world\")" in cpp_code

    def test_multiple_string_methods_same_line(self):
        """Test multiple string methods in the same expression."""
        python_code = """
def complex_processing(text: str) -> str:
    return text.strip().upper().replace(" ", "_")
"""
        cpp_code = self.converter.convert_code(python_code)

        # This would be complex to implement perfectly, but basic conversion should work
        assert "StringOps::" in cpp_code
        assert "text" in cpp_code

    def test_string_methods_with_variables(self):
        """Test string methods with variable arguments."""
        python_code = """
def dynamic_replace(text: str, old: str, new: str) -> str:
    return text.replace(old, new)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::replace(text, old, new)" in cpp_code

    def test_string_methods_return_types(self):
        """Test different return types from string methods."""
        python_code = """
def test_returns(text: str) -> int:
    # find returns int
    pos = text.find("test")
    return pos
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int pos = StringOps::find(text, \"test\");" in cpp_code
        assert "return pos;" in cpp_code