"""Tests for Haskell backend comprehension support."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter


class TestHaskellListComprehensions:
    """Test list comprehension conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension."""
        python_code = """
def test_comprehension(numbers: list) -> list:
    return [x * 2 for x in numbers]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehension numbers" in haskell_code
        assert "(x * 2)" in haskell_code

    def test_list_comprehension_with_filter(self):
        """Test list comprehension with filter condition."""
        python_code = """
def test_filtered_comprehension(numbers: list) -> list:
    return [x * x for x in numbers if x > 0]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehensionWithFilter numbers" in haskell_code
        assert "(x > 0)" in haskell_code
        assert "(x * x)" in haskell_code

    def test_list_comprehension_with_range(self):
        """Test list comprehension with range."""
        python_code = """
def test_range_comprehension(n: int) -> list:
    return [i * 2 for i in range(n)]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehension" in haskell_code
        assert "rangeList (range n)" in haskell_code
        assert "(i * 2)" in haskell_code

    def test_list_comprehension_with_range_and_filter(self):
        """Test list comprehension with range and filter."""
        python_code = """
def test_range_filtered_comprehension(n: int) -> list:
    return [i for i in range(n) if i % 2 == 0]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehensionWithFilter" in haskell_code
        assert "rangeList (range n)" in haskell_code
        assert "(`mod` 2)" in haskell_code or "(i `mod` 2)" in haskell_code

    def test_complex_list_comprehension(self):
        """Test complex list comprehension with mathematical operations."""
        python_code = """
def test_complex_comprehension(numbers: list) -> list:
    return [x * x + 1 for x in numbers if x > 5]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehensionWithFilter numbers" in haskell_code
        assert "(x > 5)" in haskell_code
        assert "((x * x) + 1)" in haskell_code

    def test_nested_list_comprehension_expression(self):
        """Test list comprehension with nested expressions."""
        python_code = """
def test_nested_expression(matrix: list) -> list:
    return [len(row) for row in matrix]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehension matrix" in haskell_code
        assert "len' row" in haskell_code


class TestHaskellDictComprehensions:
    """Test dictionary comprehension conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_dict_comprehension(self):
        """Test simple dictionary comprehension."""
        python_code = """
def test_dict_comprehension(keys: list) -> dict:
    return {k: k * 2 for k in keys}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "dictComprehension keys" in haskell_code
        assert "(k * 2)" in haskell_code

    def test_dict_comprehension_with_filter(self):
        """Test dictionary comprehension with filter."""
        python_code = """
def test_filtered_dict_comprehension(numbers: list) -> dict:
    return {n: n * n for n in numbers if n > 0}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "dictComprehensionWithFilter numbers" in haskell_code
        assert "(n > 0)" in haskell_code
        assert "(n * n)" in haskell_code

    def test_dict_comprehension_with_range(self):
        """Test dictionary comprehension with range."""
        python_code = """
def test_range_dict_comprehension(n: int) -> dict:
    return {i: i * i for i in range(n)}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "dictComprehension" in haskell_code
        assert "rangeList (range n)" in haskell_code
        assert "(i * i)" in haskell_code

    def test_dict_comprehension_string_keys(self):
        """Test dictionary comprehension with string transformations."""
        python_code = """
def test_string_dict_comprehension(words: list) -> dict:
    return {word.upper(): len(word) for word in words}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "dictComprehension words" in haskell_code
        assert "upper word" in haskell_code
        assert "len' word" in haskell_code

    def test_complex_dict_comprehension(self):
        """Test complex dictionary comprehension with conditions."""
        python_code = """
def test_complex_dict_comprehension(data: list) -> dict:
    return {str(x): x * 2 + 1 for x in data if x % 3 == 0}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "dictComprehensionWithFilter data" in haskell_code
        assert "(`mod` 3)" in haskell_code or "(x `mod` 3)" in haskell_code
        assert "toString x" in haskell_code
        assert "((x * 2) + 1)" in haskell_code


class TestHaskellSetComprehensions:
    """Test set comprehension conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_set_comprehension(self):
        """Test simple set comprehension."""
        python_code = """
def test_set_comprehension(numbers: list) -> set:
    return {x * 2 for x in numbers}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "setComprehension numbers" in haskell_code
        assert "(x * 2)" in haskell_code

    def test_set_comprehension_with_filter(self):
        """Test set comprehension with filter."""
        python_code = """
def test_filtered_set_comprehension(numbers: list) -> set:
    return {x for x in numbers if x > 10}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "setComprehensionWithFilter numbers" in haskell_code
        assert "(x > 10)" in haskell_code

    def test_set_comprehension_with_range(self):
        """Test set comprehension with range."""
        python_code = """
def test_range_set_comprehension(n: int) -> set:
    return {i % 3 for i in range(n)}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "setComprehension" in haskell_code
        assert "rangeList (range n)" in haskell_code
        assert "(`mod` 3)" in haskell_code or "(i `mod` 3)" in haskell_code

    def test_set_comprehension_deduplication(self):
        """Test set comprehension with operations that create duplicates."""
        python_code = """
def test_dedup_set_comprehension(numbers: list) -> set:
    return {x // 2 for x in numbers if x > 0}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "setComprehensionWithFilter numbers" in haskell_code
        assert "(x > 0)" in haskell_code
        assert "`div`" in haskell_code

    def test_set_comprehension_with_string_ops(self):
        """Test set comprehension with string operations."""
        python_code = """
def test_string_set_comprehension(words: list) -> set:
    return {word.upper() for word in words if len(word) > 3}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "setComprehensionWithFilter words" in haskell_code
        assert "len' word" in haskell_code
        assert "upper word" in haskell_code


class TestHaskellComprehensionIntegration:
    """Test comprehensions in various contexts."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_comprehensions_in_function_calls(self):
        """Test comprehensions used as function arguments."""
        python_code = """
def test_comprehension_args(numbers: list) -> int:
    squares = [x * x for x in numbers]
    return sum(squares)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehension numbers" in haskell_code
        assert "sum' squares" in haskell_code

    def test_comprehensions_with_multiple_operations(self):
        """Test comprehensions with multiple mathematical operations."""
        python_code = """
def test_multi_op_comprehension(data: list) -> list:
    return [x * 2 + 1 for x in data if x % 2 == 0 and x > 5]
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehensionWithFilter data" in haskell_code
        assert "&&" in haskell_code
        assert "((x * 2) + 1)" in haskell_code
        assert "(x > 5)" in haskell_code

    def test_nested_comprehensions_context(self):
        """Test comprehensions used in complex expressions."""
        python_code = """
def test_nested_context(matrix: list) -> dict:
    return {
        "row_sums": [sum(row) for row in matrix],
        "simple_elements": [x for x in [1, 2, 3] if x % 2 == 0]
    }
"""
        haskell_code = self.converter.convert_code(python_code)

        # First comprehension should be present
        assert "listComprehension matrix" in haskell_code
        assert "sum' row" in haskell_code

        # Second comprehension should be present
        assert "listComprehensionWithFilter" in haskell_code

    def test_comprehension_return_direct(self):
        """Test direct return of comprehensions."""
        python_code = """
def get_squares(numbers: list) -> list:
    return [n * n for n in numbers if n > 0]

def get_word_lengths(words: list) -> dict:
    return {word: len(word) for word in words}

def get_unique_lengths(words: list) -> set:
    return {len(word) for word in words}
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "listComprehensionWithFilter numbers" in haskell_code
        assert "dictComprehension words" in haskell_code
        assert "setComprehension words" in haskell_code