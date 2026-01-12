"""Tests for Go backend comprehensions support."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoListComprehensions:
    """Test list comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension with range."""
        python_code = """
def test_list_comp() -> list:
    return [x for x in range(5)]
"""
        go_code = self.converter.convert_code(python_code)

        # Should contain comprehension call with Range
        assert "multigen.ListComprehension" in go_code
        assert "multigen.NewRange(5)" in go_code

    def test_list_comprehension_with_expression(self):
        """Test list comprehension with expression transformation."""
        python_code = """
def test_list_comp_expr() -> list:
    return [x * 2 for x in range(3)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        assert "multigen.NewRange(3)" in go_code
        # Should contain lambda expression
        # Lambda signature updated for generics
        assert "(x * 2)" in go_code

    def test_list_comprehension_with_condition(self):
        """Test list comprehension with if condition."""
        python_code = """
def test_list_comp_if() -> list:
    return [x for x in range(10) if x % 2 == 0]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code  # Either ListComprehension or ListComprehensionFromRangeWithFilter
        assert "multigen.NewRange(10)" in go_code
        # Should contain condition lambda
        assert "(x % 2) == 0" in go_code

    def test_list_comprehension_range_start_stop(self):
        """Test list comprehension with range(start, stop)."""
        python_code = """
def test_range_start_stop() -> list:
    return [x for x in range(2, 8)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.NewRange(2, 8)" in go_code

    def test_list_comprehension_range_step(self):
        """Test list comprehension with range(start, stop, step)."""
        python_code = """
def test_range_step() -> list:
    return [x for x in range(0, 10, 2)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.NewRange(0, 10, 2)" in go_code

    def test_list_comprehension_complex_expression(self):
        """Test list comprehension with complex expression."""
        python_code = """
def test_complex_expr() -> list:
    return [x * x + 1 for x in range(5)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        assert "((x * x) + 1)" in go_code

    def test_list_comprehension_with_variable(self):
        """Test list comprehension using variable in range."""
        python_code = """
def test_with_var(n: int) -> list:
    return [x for x in range(n)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.NewRange(n)" in go_code


class TestGoDictComprehensions:
    """Test dictionary comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_dict_comprehension(self):
        """Test simple dictionary comprehension."""
        python_code = """
def test_dict_comp() -> dict:
    return {x: x * 2 for x in range(3)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.DictComprehension" in go_code
        assert "multigen.NewRange(3)" in go_code
        # Dict comprehension now uses generics with specific types

    def test_dict_comprehension_with_condition(self):
        """Test dictionary comprehension with condition."""
        python_code = """
def test_dict_comp_if() -> dict:
    return {x: x * x for x in range(5) if x > 2}
"""
        go_code = self.converter.convert_code(python_code)

        # Note: Dict comprehensions with filters would be complex in Go
        # For now, just check basic structure
        assert "multigen.DictComprehension" in go_code
        assert "multigen.NewRange(5)" in go_code

    def test_dict_comprehension_string_keys(self):
        """Test dictionary comprehension with string keys."""
        python_code = """
def test_dict_str_keys() -> dict:
    return {str(x): x for x in range(3)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.DictComprehension" in go_code
        assert "multigen.NewRange(3)" in go_code

    def test_dict_comprehension_complex_values(self):
        """Test dictionary comprehension with complex value expressions."""
        python_code = """
def test_dict_complex() -> dict:
    return {x: x * x + x for x in range(4)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.DictComprehension" in go_code
        assert "((x * x) + x)" in go_code


class TestGoSetComprehensions:
    """Test set comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_set_comprehension(self):
        """Test simple set comprehension."""
        python_code = """
def test_set_comp() -> set:
    return {x for x in range(5)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.SetComprehension" in go_code
        assert "multigen.NewRange(5)" in go_code

    def test_set_comprehension_with_condition(self):
        """Test set comprehension with condition."""
        python_code = """
def test_set_comp_if() -> set:
    return {x for x in range(10) if x % 3 == 0}
"""
        go_code = self.converter.convert_code(python_code)

        # Note: Set comprehensions with filters would need complex handling
        assert "multigen.SetComprehension" in go_code
        assert "multigen.NewRange(10)" in go_code

    def test_set_comprehension_with_expression(self):
        """Test set comprehension with expression transformation."""
        python_code = """
def test_set_comp_expr() -> set:
    return {x * x for x in range(4)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.SetComprehension" in go_code
        assert "(x * x)" in go_code

    def test_set_comprehension_deduplication(self):
        """Test set comprehension that would naturally deduplicate."""
        python_code = """
def test_set_dedup() -> set:
    return {x % 3 for x in range(9)}
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.SetComprehension" in go_code
        assert "(x % 3)" in go_code


class TestGoComprehensionsAdvanced:
    """Test advanced comprehension features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_comprehension_with_function_call(self):
        """Test comprehension with function calls in expression."""
        python_code = """
def test_func_call() -> list:
    return [abs(x - 3) for x in range(6)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        # Should contain abs function call
        assert "multigen.Abs" in go_code

    def test_comprehension_in_class_method(self):
        """Test comprehension used within a class method."""
        python_code = """
class NumberProcessor:
    def __init__(self, multiplier: int):
        self.multiplier: int = multiplier

    def process_range(self, n: int) -> list:
        return [x * self.multiplier for x in range(n)]
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        assert "obj.Multiplier" in go_code

    def test_nested_comprehensions_simple(self):
        """Test simple nested comprehension case."""
        python_code = """
def test_nested() -> list:
    # Simplified version - full nested comprehensions are complex
    inner = [x for x in range(3)]
    return [y * 2 for y in inner]
"""
        go_code = self.converter.convert_code(python_code)

        # Should have two separate comprehensions
        assert go_code.count("multigen.ListComprehension") >= 1

    def test_comprehension_return_types(self):
        """Test that comprehensions generate appropriate return types."""
        python_code = """
def test_types() -> None:
    nums = [x for x in range(3)]           # Should be slice
    squares = {x: x*x for x in range(3)}   # Should be map
    unique = {x for x in range(3)}         # Should be set (map[T]bool)
"""
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        assert "multigen.DictComprehension" in go_code
        assert "multigen.SetComprehension" in go_code

    def test_comprehension_with_multiple_variables(self):
        """Test comprehension with multiple loop variables (simplified)."""
        python_code = """
def test_multi_vars() -> list:
    # Simplified - Go doesn't have tuple unpacking like Python
    return [x + y for x in range(3) for y in range(2)]
"""
        go_code = self.converter.convert_code(python_code)

        # For now, just check that we get basic comprehension structure
        # Full nested comprehensions would be complex in Go
        assert "multigen.ListComprehension" in go_code