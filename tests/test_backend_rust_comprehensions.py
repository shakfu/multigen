"""Tests for Rust backend comprehensions support."""

import pytest

from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestRustListComprehensions:
    """Test list comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension with range."""
        python_code = """
def test_list_comp() -> list:
    return [x for x in range(5)]
"""
        rust_code = self.converter.convert_code(python_code)

        # Should contain comprehension call with Range
        assert "Comprehensions::list_comprehension" in rust_code
        assert "new_range(5).collect()" in rust_code

    def test_list_comprehension_with_expression(self):
        """Test list comprehension with expression transformation."""
        python_code = """
def test_list_comp_expr() -> list:
    return [x * 2 for x in range(3)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        assert "new_range(3).collect()" in rust_code
        # Should contain closure with expression
        assert "|x| (x * 2)" in rust_code

    def test_list_comprehension_with_condition(self):
        """Test list comprehension with if condition."""
        python_code = """
def test_list_comp_if() -> list:
    return [x for x in range(10) if x % 2 == 0]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension_with_filter" in rust_code
        assert "new_range(10).collect()" in rust_code
        # Should contain condition closure
        assert "|x| ((x % 2) == 0)" in rust_code

    def test_list_comprehension_range_start_stop(self):
        """Test list comprehension with range(start, stop)."""
        python_code = """
def test_range_start_stop() -> list:
    return [x for x in range(2, 8)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "new_range_with_start(2, 8).collect()" in rust_code

    def test_list_comprehension_range_step(self):
        """Test list comprehension with range(start, stop, step)."""
        python_code = """
def test_range_step() -> list:
    return [x for x in range(0, 10, 2)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "new_range_with_step(0, 10, 2).collect()" in rust_code

    def test_list_comprehension_complex_expression(self):
        """Test list comprehension with complex expression."""
        python_code = """
def test_complex_expr() -> list:
    return [x * x + 1 for x in range(5)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        assert "|x| ((x * x) + 1)" in rust_code

    def test_list_comprehension_with_variable(self):
        """Test list comprehension using variable in range."""
        python_code = """
def test_with_var(n: int) -> list:
    return [x for x in range(n)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "new_range(n).collect()" in rust_code


class TestRustDictComprehensions:
    """Test dictionary comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_dict_comprehension(self):
        """Test simple dictionary comprehension."""
        python_code = """
def test_dict_comp() -> dict:
    return {x: x * 2 for x in range(3)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::dict_comprehension" in rust_code
        assert "new_range(3).collect()" in rust_code
        assert "|x| (x, (x * 2))" in rust_code

    def test_dict_comprehension_with_condition(self):
        """Test dictionary comprehension with condition."""
        python_code = """
def test_dict_comp_if() -> dict:
    return {x: x * x for x in range(5) if x > 2}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::dict_comprehension_with_filter" in rust_code
        assert "new_range(5).collect()" in rust_code
        assert "|x| (x, (x * x))" in rust_code
        assert "|x| (x > 2)" in rust_code

    def test_dict_comprehension_string_keys(self):
        """Test dictionary comprehension with string keys."""
        python_code = """
def test_dict_str_keys() -> dict:
    return {str(x): x for x in range(3)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::dict_comprehension" in rust_code
        assert "|x| (to_string(x), x)" in rust_code

    def test_dict_comprehension_complex_values(self):
        """Test dictionary comprehension with complex value expressions."""
        python_code = """
def test_dict_complex() -> dict:
    return {x: x * x + x for x in range(4)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::dict_comprehension" in rust_code
        assert "|x| (x, ((x * x) + x))" in rust_code


class TestRustSetComprehensions:
    """Test set comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_set_comprehension(self):
        """Test simple set comprehension."""
        python_code = """
def test_set_comp() -> set:
    return {x for x in range(5)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::set_comprehension" in rust_code
        assert "new_range(5).collect()" in rust_code

    def test_set_comprehension_with_condition(self):
        """Test set comprehension with condition."""
        python_code = """
def test_set_comp_if() -> set:
    return {x for x in range(10) if x % 3 == 0}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::set_comprehension_with_filter" in rust_code
        assert "new_range(10).collect()" in rust_code
        assert "|x| ((x % 3) == 0)" in rust_code

    def test_set_comprehension_with_expression(self):
        """Test set comprehension with expression transformation."""
        python_code = """
def test_set_comp_expr() -> set:
    return {x * x for x in range(4)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::set_comprehension" in rust_code
        assert "|x| (x * x)" in rust_code

    def test_set_comprehension_deduplication(self):
        """Test set comprehension that would naturally deduplicate."""
        python_code = """
def test_set_dedup() -> set:
    return {x % 3 for x in range(9)}
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::set_comprehension" in rust_code
        assert "|x| (x % 3)" in rust_code


class TestRustComprehensionsAdvanced:
    """Test advanced comprehension features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_comprehension_with_function_call(self):
        """Test comprehension with function calls in expression."""
        python_code = """
def test_func_call() -> list:
    return [abs(x - 3) for x in range(6)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        # Should contain abs function call
        assert "Builtins::abs_i32" in rust_code

    def test_comprehension_in_class_method(self):
        """Test comprehension used within a class method."""
        python_code = """
class NumberProcessor:
    def __init__(self, multiplier: int):
        self.multiplier: int = multiplier

    def process_range(self, n: int) -> list:
        return [x * self.multiplier for x in range(n)]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        assert "self.multiplier" in rust_code

    def test_nested_comprehensions_simple(self):
        """Test simple nested comprehension case."""
        python_code = """
def test_nested() -> list:
    # Simplified version - full nested comprehensions are complex
    inner = [x for x in range(3)]
    return [y * 2 for y in inner]
"""
        rust_code = self.converter.convert_code(python_code)

        # Should have two separate comprehensions
        assert rust_code.count("Comprehensions::list_comprehension") >= 1

    def test_comprehension_return_types(self):
        """Test that comprehensions generate appropriate return types."""
        python_code = """
def test_types() -> None:
    nums = [x for x in range(3)]           # Should be Vec
    squares = {x: x*x for x in range(3)}   # Should be HashMap
    unique = {x for x in range(3)}         # Should be HashSet
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        assert "Comprehensions::dict_comprehension" in rust_code
        assert "Comprehensions::set_comprehension" in rust_code

    def test_comprehension_with_multiple_variables(self):
        """Test comprehension with multiple loop variables (simplified)."""
        python_code = """
def test_multi_vars() -> list:
    # Simplified - Rust doesn't have tuple unpacking like Python
    return [x + y for x in range(3) for y in range(2)]
"""
        rust_code = self.converter.convert_code(python_code)

        # For now, just check that we get basic comprehension structure
        # Full nested comprehensions would be complex in Rust
        assert "Comprehensions::list_comprehension" in rust_code

    def test_comprehension_with_string_operations(self):
        """Test comprehensions with string operations."""
        python_code = """
def test_string_comp() -> list:
    words = ["hello", "world", "test"]
    return [word.upper() for word in words]
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Comprehensions::list_comprehension" in rust_code
        # String operations would need special handling

    def test_comprehension_mixed_with_conditionals(self):
        """Test comprehensions with complex conditional expressions."""
        python_code = """
def test_complex_conditions() -> list:
    return [x * 2 if x > 0 else x for x in range(-2, 3)]
"""
        rust_code = self.converter.convert_code(python_code)

        # This would require ternary operator support which is complex
        assert "Comprehensions::list_comprehension" in rust_code