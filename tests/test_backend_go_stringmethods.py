"""Tests for Go backend string methods support."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoStringMethods:
    """Test string method conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_string_upper_method(self):
        """Test string upper() method conversion."""
        python_code = """
def test_upper(text: str) -> str:
    return text.upper()
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Upper(text)" in go_code

    def test_string_lower_method(self):
        """Test string lower() method conversion."""
        python_code = """
def test_lower(text: str) -> str:
    return text.lower()
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Lower(text)" in go_code

    def test_string_strip_method(self):
        """Test string strip() method conversion."""
        python_code = """
def test_strip(text: str) -> str:
    return text.strip()
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Strip(text)" in go_code

    def test_string_strip_with_chars(self):
        """Test string strip() method with characters."""
        python_code = """
def test_strip_chars(text: str, chars: str) -> str:
    return text.strip(chars)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.StripChars(text, chars)" in go_code

    def test_string_find_method(self):
        """Test string find() method conversion."""
        python_code = """
def test_find(text: str, substr: str) -> int:
    return text.find(substr)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Find(text, substr)" in go_code

    def test_string_replace_method(self):
        """Test string replace() method conversion."""
        python_code = """
def test_replace(text: str, old: str, new: str) -> str:
    return text.replace(old, new)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Replace(text, old, new)" in go_code

    def test_string_split_method(self):
        """Test string split() method conversion."""
        python_code = """
def test_split(text: str) -> list:
    return text.split()
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Split(text)" in go_code

    def test_string_split_with_separator(self):
        """Test string split() method with separator."""
        python_code = """
def test_split_sep(text: str, sep: str) -> list:
    return text.split(sep)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.SplitSep(text, sep)" in go_code

    def test_string_methods_on_literals(self):
        """Test string methods on string literals."""
        python_code = """
def test_literal_methods() -> str:
    return "Hello World".upper()
"""
        go_code = self.converter.convert_code(python_code)

        assert 'multigen.StrOps.Upper("Hello World")' in go_code

    def test_chained_string_methods(self):
        """Test chained string method calls."""
        python_code = """
def test_chained(text: str) -> str:
    cleaned = text.strip()
    upper_text = cleaned.upper()
    return upper_text
"""
        go_code = self.converter.convert_code(python_code)

        assert "cleaned := multigen.StrOps.Strip(text)" in go_code
        assert "upper_text := multigen.StrOps.Upper(cleaned)" in go_code


class TestGoStringMethodsInClasses:
    """Test string methods within class contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_string_methods_on_instance_variables(self):
        """Test string methods on instance variables."""
        python_code = """
class TextProcessor:
    def __init__(self, text: str):
        self.text: str = text

    def get_upper(self) -> str:
        return self.text.upper()

    def get_length(self) -> int:
        return len(self.text)
"""
        go_code = self.converter.convert_code(python_code)

        assert "return multigen.StrOps.Upper(obj.Text)" in go_code
        assert "return multigen.Len" in go_code  # Generic len function

    def test_string_methods_with_assignment(self):
        """Test string methods with assignment to instance variables."""
        python_code = """
class StringManipulator:
    def __init__(self, initial: str):
        self.value: str = initial

    def to_upper(self) -> None:
        self.value = self.value.upper()

    def trim_and_lower(self) -> None:
        self.value = self.value.strip().lower()
"""
        go_code = self.converter.convert_code(python_code)

        assert "obj.Value = multigen.StrOps.Upper(obj.Value)" in go_code
        assert "obj.Value = multigen.StrOps.Lower(multigen.StrOps.Strip(obj.Value))" in go_code

    def test_complex_string_processing(self):
        """Test complex string processing in class methods."""
        python_code = """
class DataCleaner:
    def __init__(self, prefix: str):
        self.prefix: str = prefix

    def clean_data(self, raw_data: str) -> str:
        trimmed = raw_data.strip()
        no_spaces = trimmed.replace(" ", "_")
        upper_case = no_spaces.upper()
        return self.prefix + upper_case
"""
        go_code = self.converter.convert_code(python_code)

        assert "trimmed := multigen.StrOps.Strip(raw_data)" in go_code
        assert 'no_spaces := multigen.StrOps.Replace(trimmed, " ", "_")' in go_code
        assert "upper_case := multigen.StrOps.Upper(no_spaces)" in go_code
        assert "return (obj.Prefix + upper_case)" in go_code

    def test_string_methods_in_conditions(self):
        """Test string methods used in conditional expressions."""
        python_code = """
class Validator:
    def __init__(self, required: str):
        self.required: str = required

    def is_valid(self, input_text: str) -> bool:
        cleaned = input_text.strip().lower()
        return cleaned == self.required.lower()
"""
        go_code = self.converter.convert_code(python_code)

        assert "cleaned := multigen.StrOps.Lower(multigen.StrOps.Strip(input_text))" in go_code
        assert "return (cleaned == multigen.StrOps.Lower(obj.Required))" in go_code