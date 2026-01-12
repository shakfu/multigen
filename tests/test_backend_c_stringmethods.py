"""Tests for string methods support in C backend."""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestStringMethodsBasics:
    """Test basic string method functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_string_upper_method(self):
        """Test str.upper() method."""
        python_code = """
def test_upper() -> str:
    text: str = "hello"
    return text.upper()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "hello";' in c_code
        assert "multigen_str_upper(text)" in c_code
        assert '#include "multigen_string_ops.h"' in c_code

    def test_string_lower_method(self):
        """Test str.lower() method."""
        python_code = """
def test_lower() -> str:
    text: str = "WORLD"
    return text.lower()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "WORLD";' in c_code
        assert "multigen_str_lower(text)" in c_code
        assert '#include "multigen_string_ops.h"' in c_code

    def test_string_strip_method(self):
        """Test str.strip() method."""
        python_code = """
def test_strip() -> str:
    text: str = "  hello  "
    return text.strip()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "  hello  ";' in c_code
        assert "multigen_str_strip(text)" in c_code
        assert '#include "multigen_string_ops.h"' in c_code

    def test_string_strip_with_chars(self):
        """Test str.strip(chars) method."""
        python_code = """
def test_strip_chars() -> str:
    text: str = "!!!hello!!!"
    return text.strip("!")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "!!!hello!!!";' in c_code
        assert 'multigen_str_strip_chars(text, "!")' in c_code

    def test_string_find_method(self):
        """Test str.find() method."""
        python_code = """
def test_find() -> int:
    text: str = "hello world"
    return text.find("world")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "hello world";' in c_code
        assert 'multigen_str_find(text, "world")' in c_code

    def test_string_replace_method(self):
        """Test str.replace() method."""
        python_code = """
def test_replace() -> str:
    text: str = "hello world"
    return text.replace("world", "python")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "hello world";' in c_code
        assert 'multigen_str_replace(text, "world", "python")' in c_code

    def test_string_split_method(self):
        """Test str.split() method."""
        python_code = """
def test_split() -> str:
    text: str = "hello world"
    return text.split()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "hello world";' in c_code
        assert "multigen_str_split(text, NULL)" in c_code

    def test_string_split_with_delimiter(self):
        """Test str.split(delimiter) method."""
        python_code = """
def test_split_delimiter() -> str:
    text: str = "a,b,c"
    return text.split(",")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* text = "a,b,c";' in c_code
        assert 'multigen_str_split(text, ",")' in c_code


class TestStringMethodsWithVariables:
    """Test string methods with variables and expressions."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_string_method_on_variable(self):
        """Test string method on a variable."""
        python_code = """
def test_variable_method(input_text: str) -> str:
    return input_text.upper()
"""
        c_code = self.converter.convert_code(python_code)

        assert "char* test_variable_method(char* input_text)" in c_code
        assert "multigen_str_upper(input_text)" in c_code

    def test_string_method_chaining_concept(self):
        """Test concept of string method results being used."""
        python_code = """
def test_method_result() -> str:
    text: str = "  Hello World  "
    stripped: str = text.strip()
    return stripped.lower()
"""
        c_code = self.converter.convert_code(python_code)

        assert "multigen_str_strip(text)" in c_code
        assert "multigen_str_lower(stripped)" in c_code

    def test_string_method_with_parameters(self):
        """Test string methods with parameterized arguments."""
        python_code = """
def test_with_params(text: str, old_val: str, new_val: str) -> str:
    return text.replace(old_val, new_val)
"""
        c_code = self.converter.convert_code(python_code)

        assert "multigen_str_replace(text, old_val, new_val)" in c_code

    def test_find_with_variable_search(self):
        """Test find method with variable search term."""
        python_code = """
def test_find_variable(text: str, search_term: str) -> int:
    return text.find(search_term)
"""
        c_code = self.converter.convert_code(python_code)

        assert "multigen_str_find(text, search_term)" in c_code


class TestStringMethodsLiterals:
    """Test string methods on string literals."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_literal_string_upper(self):
        """Test upper() on string literal."""
        python_code = """
def test_literal_upper() -> str:
    return "hello".upper()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'multigen_str_upper("hello")' in c_code

    def test_literal_string_lower(self):
        """Test lower() on string literal."""
        python_code = """
def test_literal_lower() -> str:
    return "HELLO".lower()
"""
        c_code = self.converter.convert_code(python_code)

        assert 'multigen_str_lower("HELLO")' in c_code

    def test_literal_string_find(self):
        """Test find() on string literal."""
        python_code = """
def test_literal_find() -> int:
    return "hello world".find("world")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'multigen_str_find("hello world", "world")' in c_code

    def test_literal_string_replace(self):
        """Test replace() on string literal."""
        python_code = """
def test_literal_replace() -> str:
    return "hello world".replace("world", "python")
"""
        c_code = self.converter.convert_code(python_code)

        assert 'multigen_str_replace("hello world", "world", "python")' in c_code


class TestStringMethodsErrorHandling:
    """Test error handling for string methods."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_upper_with_arguments_error(self):
        """Test error when upper() is called with arguments."""
        python_code = """
def test_upper_error() -> str:
    text: str = "hello"
    return text.upper("invalid")
"""
        with pytest.raises(UnsupportedFeatureError, match="str.upper\\(\\) takes no arguments"):
            self.converter.convert_code(python_code)

    def test_lower_with_arguments_error(self):
        """Test error when lower() is called with arguments."""
        python_code = """
def test_lower_error() -> str:
    text: str = "HELLO"
    return text.lower("invalid")
"""
        with pytest.raises(UnsupportedFeatureError, match="str.lower\\(\\) takes no arguments"):
            self.converter.convert_code(python_code)

    def test_find_wrong_arguments_error(self):
        """Test error when find() is called with wrong number of arguments."""
        python_code = """
def test_find_error() -> int:
    text: str = "hello"
    return text.find()
"""
        with pytest.raises(UnsupportedFeatureError, match="str.find\\(\\) requires exactly one argument"):
            self.converter.convert_code(python_code)

    def test_replace_wrong_arguments_error(self):
        """Test error when replace() is called with wrong number of arguments."""
        python_code = """
def test_replace_error() -> str:
    text: str = "hello"
    return text.replace("l")
"""
        with pytest.raises(UnsupportedFeatureError, match="str.replace\\(\\) requires exactly two arguments"):
            self.converter.convert_code(python_code)

    def test_strip_too_many_arguments_error(self):
        """Test error when strip() is called with too many arguments."""
        python_code = """
def test_strip_error() -> str:
    text: str = "hello"
    return text.strip("h", "o")
"""
        with pytest.raises(UnsupportedFeatureError, match="str.strip\\(\\) takes at most one argument"):
            self.converter.convert_code(python_code)

    def test_split_too_many_arguments_error(self):
        """Test error when split() is called with too many arguments."""
        python_code = """
def test_split_error() -> str:
    text: str = "hello"
    return text.split(",", "extra")
"""
        with pytest.raises(UnsupportedFeatureError, match="str.split\\(\\) takes at most one argument"):
            self.converter.convert_code(python_code)

    def test_unsupported_string_method_error(self):
        """Test error for unsupported string methods."""
        python_code = """
def test_unsupported() -> str:
    text: str = "hello"
    return text.capitalize()
"""
        with pytest.raises(UnsupportedFeatureError, match="Unsupported string method: capitalize"):
            self.converter.convert_code(python_code)


class TestStringMethodsIntegration:
    """Test string methods integration with other features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_string_methods_in_conditionals(self):
        """Test string methods used in conditional statements."""
        python_code = """
def test_conditional(text: str) -> str:
    if text.find("hello") >= 0:
        return text.upper()
    else:
        return text.lower()
"""
        c_code = self.converter.convert_code(python_code)

        assert "if ((multigen_str_find(text, \"hello\") >= 0))" in c_code
        assert "return multigen_str_upper(text);" in c_code
        assert "return multigen_str_lower(text);" in c_code

    def test_string_methods_in_loops(self):
        """Test string methods used in loops."""
        python_code = """
def test_loop() -> str:
    result: str = "hello"
    for i in range(3):
        result = result.upper()
    return result
"""
        c_code = self.converter.convert_code(python_code)

        assert "for (int i = 0; i < 3; i += 1)" in c_code
        assert "result = multigen_str_upper(result);" in c_code

    def test_string_methods_with_oop(self):
        """Test string methods used with object-oriented features."""
        python_code = """
class TextProcessor:
    def __init__(self, text: str):
        self.text: str = text

    def process(self) -> str:
        return self.text.strip()

def test_oop() -> str:
    processor: TextProcessor = TextProcessor("  hello  ")
    return processor.process()
"""
        c_code = self.converter.convert_code(python_code)

        # Check that string methods work within class methods
        assert "typedef struct TextProcessor" in c_code
        assert "TextProcessor_process" in c_code
        assert "multigen_str_strip(self->text)" in c_code


@pytest.mark.parametrize("method,expected_func", [
    ("upper", "multigen_str_upper"),
    ("lower", "multigen_str_lower"),
    ("strip", "multigen_str_strip"),
])
def test_string_methods_parametrized(method, expected_func):
    """Test parametrized string methods."""
    converter = MultiGenPythonToCConverter()

    python_code = f"""
def test_method() -> str:
    text: str = "hello"
    return text.{method}()
"""

    c_code = converter.convert_code(python_code)
    assert f"{expected_func}(text)" in c_code


@pytest.mark.integration
def test_string_methods_comprehensive():
    """Comprehensive test of string methods working together."""
    converter = MultiGenPythonToCConverter()

    python_code = """
def text_processing(input_text: str) -> str:
    # Strip whitespace
    cleaned: str = input_text.strip()

    # Convert to lowercase
    lowered: str = cleaned.lower()

    # Find and replace
    if lowered.find("world") >= 0:
        result: str = lowered.replace("world", "python")
    else:
        result: str = lowered.upper()

    return result

def test_processing() -> str:
    return text_processing("  Hello WORLD  ")
"""

    c_code = converter.convert_code(python_code)

    # Basic sanity checks
    assert c_code is not None
    assert len(c_code) > 200  # Should be substantial
    assert "multigen_str_strip(" in c_code
    assert "multigen_str_lower(" in c_code
    assert "multigen_str_find(" in c_code
    assert "multigen_str_replace(" in c_code
    assert "multigen_str_upper(" in c_code
    assert '#include "multigen_string_ops.h"' in c_code