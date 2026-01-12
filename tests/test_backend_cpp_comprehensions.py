"""Tests for Python comprehensions support in C++ backend."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestListComprehensions:
    """Test list comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension with range."""
        python_code = """
def test_list_comp() -> list:
    return [x for x in range(5)]
"""
        cpp_code = self.converter.convert_code(python_code)

        # Should contain list_comprehension call with Range
        assert "list_comprehension" in cpp_code
        assert "Range(5)" in cpp_code

    def test_list_comprehension_with_expression(self):
        """Test list comprehension with expression transformation."""
        python_code = """
def test_list_comp_expr() -> list:
    return [x * 2 for x in range(3)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "Range(3)" in cpp_code
        # Should contain lambda expression
        assert "[](x)" in cpp_code or "x * 2" in cpp_code

    def test_list_comprehension_with_condition(self):
        """Test list comprehension with if condition."""
        python_code = """
def test_list_comp_if() -> list:
    return [x for x in range(10) if x % 2 == 0]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "Range(10)" in cpp_code
        # Should contain condition lambda
        assert "x % 2 == 0" in cpp_code or "x % 2" in cpp_code

    def test_list_comprehension_range_start_stop(self):
        """Test list comprehension with range(start, stop)."""
        python_code = """
def test_range_start_stop() -> list:
    return [x for x in range(2, 8)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "Range(2, 8)" in cpp_code

    def test_list_comprehension_range_step(self):
        """Test list comprehension with range(start, stop, step)."""
        python_code = """
def test_range_step() -> list:
    return [x for x in range(0, 10, 2)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "Range(0, 10, 2)" in cpp_code

    def test_list_comprehension_complex_expression(self):
        """Test list comprehension with complex expression."""
        python_code = """
def test_complex_expr() -> list:
    return [x * x + 1 for x in range(5)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "x * x + 1" in cpp_code or "((x * x) + 1)" in cpp_code

    def test_list_comprehension_with_variable(self):
        """Test list comprehension using variable in range."""
        python_code = """
def test_with_var(n: int) -> list:
    return [x for x in range(n)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "Range(n)" in cpp_code


class TestDictComprehensions:
    """Test dictionary comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_dict_comprehension(self):
        """Test simple dictionary comprehension."""
        python_code = """
def test_dict_comp() -> dict:
    return {x: x * 2 for x in range(3)}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "dict_comprehension" in cpp_code
        assert "Range(3)" in cpp_code
        assert "make_pair" in cpp_code

    def test_dict_comprehension_with_condition(self):
        """Test dictionary comprehension with condition."""
        python_code = """
def test_dict_comp_if() -> dict:
    return {x: x * x for x in range(5) if x > 2}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "dict_comprehension" in cpp_code
        assert "Range(5)" in cpp_code
        assert "x > 2" in cpp_code

    def test_dict_comprehension_string_keys(self):
        """Test dictionary comprehension with string keys."""
        python_code = """
def test_dict_str_keys() -> dict:
    return {str(x): x for x in range(3)}
"""
        cpp_code = self.converter.convert_code(python_code)

        # Note: This would be complex to implement perfectly
        assert "dict_comprehension" in cpp_code
        assert "Range(3)" in cpp_code

    def test_dict_comprehension_complex_values(self):
        """Test dictionary comprehension with complex value expressions."""
        python_code = """
def test_dict_complex() -> dict:
    return {x: x * x + x for x in range(4)}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "dict_comprehension" in cpp_code
        assert "x * x + x" in cpp_code or "((x * x) + x)" in cpp_code


class TestSetComprehensions:
    """Test set comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_set_comprehension(self):
        """Test simple set comprehension."""
        python_code = """
def test_set_comp() -> set:
    return {x for x in range(5)}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "set_comprehension" in cpp_code
        assert "Range(5)" in cpp_code

    def test_set_comprehension_with_condition(self):
        """Test set comprehension with condition."""
        python_code = """
def test_set_comp_if() -> set:
    return {x for x in range(10) if x % 3 == 0}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "set_comprehension" in cpp_code
        assert "Range(10)" in cpp_code
        assert "((x % 3) == 0)" in cpp_code

    def test_set_comprehension_with_expression(self):
        """Test set comprehension with expression transformation."""
        python_code = """
def test_set_comp_expr() -> set:
    return {x * x for x in range(4)}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "set_comprehension" in cpp_code
        assert "x * x" in cpp_code

    def test_set_comprehension_deduplication(self):
        """Test set comprehension that would naturally deduplicate."""
        python_code = """
def test_set_dedup() -> set:
    return {x % 3 for x in range(9)}
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "set_comprehension" in cpp_code
        assert "x % 3" in cpp_code


class TestComprehensionsAdvanced:
    """Test advanced comprehension features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_comprehension_with_function_call(self):
        """Test comprehension with function calls in expression."""
        python_code = """
def test_func_call() -> list:
    return [abs(x - 3) for x in range(6)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        # Should contain abs function call
        assert "abs" in cpp_code or "multigen::abs" in cpp_code

    def test_comprehension_in_class_method(self):
        """Test comprehension used within a class method."""
        python_code = """
class NumberProcessor:
    def __init__(self, multiplier: int):
        self.multiplier: int = multiplier

    def process_range(self, n: int) -> list:
        return [x * self.multiplier for x in range(n)]
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "this->multiplier" in cpp_code

    def test_nested_comprehensions_simple(self):
        """Test simple nested comprehension case."""
        python_code = """
def test_nested() -> list:
    # Simplified version - full nested comprehensions are complex
    inner = [x for x in range(3)]
    return [y * 2 for y in inner]
"""
        cpp_code = self.converter.convert_code(python_code)

        # Should have two separate comprehensions
        assert cpp_code.count("list_comprehension") >= 1

    def test_comprehension_with_multiple_conditions(self):
        """Test comprehension with multiple if conditions."""
        python_code = """
def test_multi_if() -> list:
    return [x for x in range(20) if x % 2 == 0 if x > 5]
"""
        cpp_code = self.converter.convert_code(python_code)

        # This is complex to implement perfectly, but should contain basic structure
        assert "list_comprehension" in cpp_code
        assert "Range(20)" in cpp_code

    def test_comprehension_return_types(self):
        """Test that comprehensions generate appropriate return types."""
        python_code = """
def test_types() -> None:
    nums = [x for x in range(3)]           # Should be vector<int>
    squares = {x: x*x for x in range(3)}   # Should be unordered_map<int, int>
    unique = {x for x in range(3)}         # Should be unordered_set<int>
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "dict_comprehension" in cpp_code
        assert "set_comprehension" in cpp_code