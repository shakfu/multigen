"""Tests for Haskell backend string method support."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter


class TestHaskellStringMethods:
    """Test string methods and operations."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_string_upper(self):
        """Test string upper method."""
        python_code = """
def test_upper(text: str) -> str:
    return text.upper()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "upper text" in haskell_code

    def test_string_lower(self):
        """Test string lower method."""
        python_code = """
def test_lower(text: str) -> str:
    return text.lower()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "lower text" in haskell_code

    def test_string_strip(self):
        """Test string strip method."""
        python_code = """
def test_strip(text: str) -> str:
    return text.strip()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "strip text" in haskell_code

    def test_string_find(self):
        """Test string find method."""
        python_code = """
def test_find(text: str, substring: str) -> int:
    return text.find(substring)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "find text substring" in haskell_code

    def test_string_replace(self):
        """Test string replace method."""
        python_code = """
def test_replace(text: str, old: str, new: str) -> str:
    return text.replace(old, new)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "replace text old new" in haskell_code

    def test_string_split(self):
        """Test string split method."""
        python_code = """
def test_split(text: str, delimiter: str) -> list:
    return text.split(delimiter)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "split text delimiter" in haskell_code

    def test_chained_string_methods(self):
        """Test chained string method calls."""
        python_code = """
def process_text(text: str) -> str:
    cleaned = text.strip().lower()
    return cleaned.upper()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "MultiGenRuntime.lower (MultiGenRuntime.strip text)" in haskell_code
        assert "MultiGenRuntime.upper cleaned" in haskell_code

    def test_string_methods_with_literals(self):
        """Test string methods called on string literals."""
        python_code = """
def test_literal_methods() -> str:
    upper_hello = "hello".upper()
    lower_world = "WORLD".lower()
    return upper_hello + lower_world
"""
        haskell_code = self.converter.convert_code(python_code)

        assert 'upper "hello"' in haskell_code
        assert 'lower "WORLD"' in haskell_code

    def test_string_methods_in_expressions(self):
        """Test string methods in complex expressions."""
        python_code = """
def complex_string_ops(name: str, surname: str) -> str:
    full_name = name.upper() + " " + surname.lower()
    if full_name.find("JOHN") != -1:
        return full_name.replace("JOHN", "Jane").strip()
    return full_name
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "upper name" in haskell_code
        assert "lower surname" in haskell_code
        assert "find fullName" in haskell_code
        assert "replace" in haskell_code
        assert "strip" in haskell_code

    def test_string_methods_with_class_attributes(self):
        """Test string methods on class attributes."""
        python_code = """
class TextProcessor:
    def __init__(self, text: str):
        self.text: str = text

    def process(self) -> str:
        return self.text.upper().strip()

    def find_pattern(self, pattern: str) -> int:
        return self.text.lower().find(pattern)
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify method signatures (methods are simplified in current implementation)
        assert "process :: TextProcessor -> String" in haskell_code
        assert "findPattern :: TextProcessor -> String -> Int" in haskell_code

    def test_string_concatenation(self):
        """Test string concatenation operations."""
        python_code = """
def concatenate_strings(first: str, second: str) -> str:
    result = first + " " + second
    return result.upper()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(first + " in haskell_code
        assert '"' in haskell_code
        assert "upper result" in haskell_code

    def test_string_comparison_with_methods(self):
        """Test string comparison combined with methods."""
        python_code = """
def compare_strings(text1: str, text2: str) -> bool:
    return text1.lower() == text2.lower()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "lower text1" in haskell_code
        assert "lower text2" in haskell_code
        assert "==" in haskell_code

    def test_string_methods_in_conditions(self):
        """Test string methods in conditional expressions."""
        python_code = """
def check_string(text: str, pattern: str) -> str:
    if text.upper().find(pattern.upper()) != -1:
        return "Found"
    else:
        return "Not found"
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "upper text" in haskell_code
        assert "upper pattern" in haskell_code
        assert "find" in haskell_code
        assert "if" in haskell_code

    def test_string_methods_return_types(self):
        """Test proper handling of string method return types."""
        python_code = """
def string_analysis(text: str) -> dict:
    return {
        "length": len(text),
        "upper": text.upper(),
        "first_char": text[0] if text else "",
        "contains_hello": text.find("hello") != -1
    }
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "len' text" in haskell_code
        assert "upper text" in haskell_code
        assert "find text" in haskell_code
        assert "Map.fromList" in haskell_code

    def test_nested_string_operations(self):
        """Test nested string operations and method calls."""
        python_code = """
def nested_operations(text: str) -> str:
    # Strip, then upper, then find, then replace if found
    cleaned = text.strip()
    upper_cleaned = cleaned.upper()
    position = upper_cleaned.find("TEST")
    if position != -1:
        return upper_cleaned.replace("TEST", "PASS")
    return upper_cleaned
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "strip text" in haskell_code
        assert "upper cleaned" in haskell_code
        assert "find upperCleaned" in haskell_code
        assert "replace upperCleaned" in haskell_code