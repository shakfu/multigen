"""Tests for Rust backend string methods support."""

import pytest

from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestRustStringMethods:
    """Test string method conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_string_upper_method(self):
        """Test string upper() method conversion."""
        python_code = """
def test_upper(text: str) -> str:
    return text.upper()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::upper(&text)" in rust_code

    def test_string_lower_method(self):
        """Test string lower() method conversion."""
        python_code = """
def test_lower(text: str) -> str:
    return text.lower()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::lower(&text)" in rust_code

    def test_string_strip_method(self):
        """Test string strip() method conversion."""
        python_code = """
def test_strip(text: str) -> str:
    return text.strip()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::strip(&text)" in rust_code

    def test_string_strip_with_chars(self):
        """Test string strip() method with characters."""
        python_code = """
def test_strip_chars(text: str, chars: str) -> str:
    return text.strip(chars)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::strip_chars(&text, &chars)" in rust_code

    def test_string_find_method(self):
        """Test string find() method conversion."""
        python_code = """
def test_find(text: str, substr: str) -> int:
    return text.find(substr)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::find(&text, &substr)" in rust_code

    def test_string_replace_method(self):
        """Test string replace() method conversion."""
        python_code = """
def test_replace(text: str, old: str, new: str) -> str:
    return text.replace(old, new)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::replace(&text, &old, &new)" in rust_code

    def test_string_split_method(self):
        """Test string split() method conversion."""
        python_code = """
def test_split(text: str) -> list:
    return text.split()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::split(&text)" in rust_code

    def test_string_split_with_separator(self):
        """Test string split() method with separator."""
        python_code = """
def test_split_sep(text: str, sep: str) -> list:
    return text.split(sep)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::split_sep(&text, &sep)" in rust_code

    def test_string_methods_on_literals(self):
        """Test string methods on string literals."""
        python_code = """
def test_literal_methods() -> str:
    return "Hello World".upper()
"""
        rust_code = self.converter.convert_code(python_code)

        assert 'StrOps::upper(&"Hello World".to_string())' in rust_code

    def test_chained_string_methods(self):
        """Test chained string method calls."""
        python_code = """
def test_chained(text: str) -> str:
    cleaned = text.strip()
    upper_text = cleaned.upper()
    return upper_text
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut cleaned = StrOps::strip(&text);" in rust_code
        assert "let mut upper_text = StrOps::upper(&cleaned);" in rust_code


class TestRustStringMethodsInClasses:
    """Test string methods within class contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

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
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::upper(&self.text)" in rust_code
        assert "Builtins::len_string(&self.text)" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "self.value = StrOps::upper(&self.value);" in rust_code
        assert "self.value = StrOps::lower(&StrOps::strip(&self.value));" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "let mut trimmed = StrOps::strip(&raw_data);" in rust_code
        assert 'let mut no_spaces = StrOps::replace(&trimmed, &" ".to_string(), &"_".to_string());' in rust_code
        assert "let mut upper_case = StrOps::upper(&no_spaces);" in rust_code
        assert "(self.prefix + upper_case)" in rust_code

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
        rust_code = self.converter.convert_code(python_code)

        assert "let mut cleaned = StrOps::lower(&StrOps::strip(&input_text));" in rust_code
        assert "(cleaned == StrOps::lower(&self.required))" in rust_code

    def test_string_methods_with_function_calls(self):
        """Test string methods combined with function calls."""
        python_code = """
class Formatter:
    def __init__(self, template: str):
        self.template: str = template

    def format_message(self, message: str) -> str:
        processed = message.upper().strip()
        return self.template.replace("{msg}", processed)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut processed = StrOps::strip(&StrOps::upper(&message));" in rust_code
        assert 'StrOps::replace(&self.template, &"{msg}".to_string(), &processed)' in rust_code

    def test_string_concatenation_with_methods(self):
        """Test string concatenation with method results."""
        python_code = """
class MessageBuilder:
    def __init__(self, prefix: str, suffix: str):
        self.prefix: str = prefix
        self.suffix: str = suffix

    def build_message(self, content: str) -> str:
        clean_content = content.strip().upper()
        return self.prefix + clean_content + self.suffix
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut clean_content = StrOps::upper(&StrOps::strip(&content));" in rust_code
        assert "((self.prefix + clean_content) + self.suffix)" in rust_code


class TestRustStringMethodsAdvanced:
    """Test advanced string method scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_nested_string_method_calls(self):
        """Test deeply nested string method calls."""
        python_code = """
def process_text(text: str) -> str:
    return text.strip().lower().replace(" ", "_").upper()
"""
        rust_code = self.converter.convert_code(python_code)

        # This should generate nested method calls
        assert "StrOps::" in rust_code
        assert "strip" in rust_code
        assert "lower" in rust_code
        assert "replace" in rust_code
        assert "upper" in rust_code

    def test_string_methods_with_variables(self):
        """Test string methods with variable arguments."""
        python_code = """
def flexible_replace(text: str, old_char: str, new_char: str) -> str:
    cleaned = text.strip()
    replaced = cleaned.replace(old_char, new_char)
    return replaced.upper()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut cleaned = StrOps::strip(&text);" in rust_code
        assert "let mut replaced = StrOps::replace(&cleaned, &old_char, &new_char);" in rust_code
        assert "StrOps::upper(&replaced)" in rust_code

    def test_string_methods_return_types(self):
        """Test that string methods generate appropriate return types."""
        python_code = """
def string_analysis(text: str) -> None:
    upper_text = text.upper()         # Should be String
    position = text.find("hello")     # Should be i32
    words = text.split()              # Should be Vec<String>
"""
        rust_code = self.converter.convert_code(python_code)

        assert "StrOps::upper(&text)" in rust_code
        assert "StrOps::find(&text" in rust_code
        assert "StrOps::split(&text)" in rust_code