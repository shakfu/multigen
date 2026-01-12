"""Tests for Haskell backend basic functionality."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestHaskellBasics:
    """Test basic Haskell code generation functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_function(self):
        """Test simple function conversion."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "add :: Int -> Int -> Int" in haskell_code
        assert "add x y = (x + y)" in haskell_code

    def test_function_with_no_params(self):
        """Test function with no parameters."""
        python_code = """
def hello() -> str:
    return "Hello, World!"
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "hello :: String" in haskell_code
        assert 'hello = "Hello, World!"' in haskell_code

    def test_function_with_multiple_statements(self):
        """Test function with multiple statements."""
        python_code = """
def calculate(x: int, y: int) -> int:
    sum_val = x + y
    product = x * y
    return sum_val + product
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "calculate :: Int -> Int -> Int" in haskell_code
        assert "sumVal = (x + y)" in haskell_code
        assert "product = (x * y)" in haskell_code
        assert "(sumVal + product)" in haskell_code

    def test_type_inference(self):
        """Test type inference for various constants."""
        python_code = """
def test_types() -> None:
    int_val = 42
    float_val = 3.14
    str_val = "hello"
    bool_val = True
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "intVal = 42" in haskell_code
        assert "floatVal = 3.14" in haskell_code
        assert 'strVal = "hello"' in haskell_code
        assert "boolVal = True" in haskell_code

    def test_main_function(self):
        """Test main function generation."""
        python_code = """
def main() -> None:
    print("Hello from Haskell!")
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "main :: IO ()" in haskell_code
        assert 'printValue "Hello from Haskell!"' in haskell_code

    def test_binary_operations(self):
        """Test binary operations conversion."""
        python_code = """
def math_ops(a: int, b: int) -> int:
    addition = a + b
    subtraction = a - b
    multiplication = a * b
    division = a / b
    return addition
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a + b)" in haskell_code
        assert "(a - b)" in haskell_code
        assert "(a * b)" in haskell_code
        assert "(a / b)" in haskell_code

    def test_comparison_operations(self):
        """Test comparison operations."""
        python_code = """
def compare(a: int, b: int) -> bool:
    equal = a == b
    not_equal = a != b
    less_than = a < b
    greater_than = a > b
    return equal
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a == b)" in haskell_code
        assert "(a /= b)" in haskell_code
        assert "(a < b)" in haskell_code
        assert "(a > b)" in haskell_code

    def test_boolean_operations(self):
        """Test boolean operations."""
        python_code = """
def bool_ops(a: bool, b: bool) -> bool:
    and_result = a and b
    or_result = a or b
    return and_result
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a && b)" in haskell_code
        assert "(a || b)" in haskell_code

    def test_unary_operations(self):
        """Test unary operations."""
        python_code = """
def unary_ops(x: int, flag: bool) -> int:
    positive = +x
    negative = -x
    not_flag = not flag
    return negative
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(+x)" in haskell_code
        assert "(-x)" in haskell_code
        assert "(not flag)" in haskell_code

    def test_builtin_functions(self):
        """Test built-in function calls."""
        python_code = """
def test_builtins(numbers: list) -> int:
    length = len(numbers)
    absolute = abs(-5)
    minimum = min(numbers)
    maximum = max(numbers)
    total = sum(numbers)
    return length
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "len' numbers" in haskell_code
        assert "abs' (-5)" in haskell_code
        assert "min' numbers" in haskell_code
        assert "max' numbers" in haskell_code
        assert "sum' numbers" in haskell_code

    def test_print_function(self):
        """Test print function conversion."""
        python_code = """
def test_print(message: str) -> None:
    print("Hello")
    print(message)
    print(42)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert 'printValue "Hello"' in haskell_code
        assert "printValue message" in haskell_code
        assert "printValue 42" in haskell_code

    def test_ternary_expression(self):
        """Test ternary expression conversion."""
        python_code = """
def test_ternary(x: int, y: int) -> int:
    result = x if x > y else y
    return result
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(if (x > y) then x else y)" in haskell_code

    def test_list_literal(self):
        """Test list literal conversion."""
        python_code = """
def test_list() -> list:
    numbers = [1, 2, 3, 4, 5]
    return numbers
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "[1, 2, 3, 4, 5]" in haskell_code

    def test_dict_literal(self):
        """Test dictionary literal conversion."""
        python_code = """
def test_dict() -> dict:
    data = {"key1": "value1", "key2": "value2"}
    return data
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "Map.fromList" in haskell_code
        assert '("key1", "value1")' in haskell_code
        assert '("key2", "value2")' in haskell_code

    def test_range_function(self):
        """Test range function conversion."""
        python_code = """
def test_range() -> list:
    range1 = range(5)
    range2 = range(1, 6)
    range3 = range(0, 10, 2)
    return range1
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "rangeList (range 5)" in haskell_code
        assert "rangeList (range2 1 6)" in haskell_code
        assert "rangeList (range3 0 10 2)" in haskell_code

    def test_unsupported_features(self):
        """Test that unsupported features raise errors."""
        # Generator expression
        python_code_generator = """
def test_generator():
    return (x for x in range(5))
"""
        with pytest.raises(UnsupportedFeatureError):
            self.converter.convert_code(python_code_generator)

        # Try/except
        python_code_exception = """
def test_exception():
    try:
        x = 1 / 0
    except ZeroDivisionError:
        x = 0
    return x
"""
        with pytest.raises(UnsupportedFeatureError):
            self.converter.convert_code(python_code_exception)

    def test_module_structure(self):
        """Test complete module structure generation."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y

def main() -> None:
    result = add(5, 3)
    print(result)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "module Main where" in haskell_code
        assert "import MultiGenRuntime" in haskell_code
        assert "add :: Int -> Int -> Int" in haskell_code
        assert "main :: IO ()" in haskell_code