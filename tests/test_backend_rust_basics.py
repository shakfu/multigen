"""Tests for Rust backend basic functionality."""

import pytest

from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestRustBasicConversion:
    """Test basic Python-to-Rust conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_function(self):
        """Test simple function conversion."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn add(x: i32, y: i32) -> i32" in rust_code
        assert "(x + y)" in rust_code

    def test_function_with_no_return_type(self):
        """Test function without explicit return type."""
        python_code = """
def greet(name: str):
    print("Hello, " + name)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn greet(name: String)" in rust_code
        assert 'print_value(("Hello, ".to_string() + name))' in rust_code

    def test_function_with_multiple_parameters(self):
        """Test function with multiple parameters."""
        python_code = """
def calculate(a: int, b: int, c: float) -> float:
    return float(a + b) * c
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn calculate(a: i32, b: i32, c: f64) -> f64" in rust_code
        assert "(to_f64_from_i32((a + b)) * c)" in rust_code

    def test_function_with_boolean_return(self):
        """Test function with boolean return type."""
        python_code = """
def is_positive(x: int) -> bool:
    return x > 0
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn is_positive(x: i32) -> bool" in rust_code
        assert "(x > 0)" in rust_code

    def test_void_function(self):
        """Test function with void return (None)."""
        python_code = """
def print_message(msg: str) -> None:
    print(msg)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn print_message(msg: String)" in rust_code  # No return type specified
        assert "print_value(msg)" in rust_code


class TestRustExpressions:
    """Test expression conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_arithmetic_expressions(self):
        """Test arithmetic expression conversion."""
        python_code = """
def test_arithmetic(a: int, b: int) -> int:
    return a + b * 2 - 1
"""
        rust_code = self.converter.convert_code(python_code)

        assert "((a + (b * 2)) - 1)" in rust_code

    def test_boolean_expressions(self):
        """Test boolean expression conversion."""
        python_code = """
def test_bool_logic(a: bool, b: bool) -> bool:
    return a and b or not a
"""
        rust_code = self.converter.convert_code(python_code)
        # Python's 'and'/'or'/'not' map to Rust's &&/||/!
        assert "&&" in rust_code
        assert "||" in rust_code
        assert "!a" in rust_code

    def test_comparison_expressions(self):
        """Test comparison expression conversion."""
        python_code = """
def test_compare(x: int, y: int) -> bool:
    return x <= y
"""
        rust_code = self.converter.convert_code(python_code)

        assert "(x <= y)" in rust_code

    def test_string_concatenation(self):
        """Test string concatenation."""
        python_code = """
def test_concat(first: str, second: str) -> str:
    return first + second
"""
        rust_code = self.converter.convert_code(python_code)

        assert "(first + second)" in rust_code

    def test_constant_values(self):
        """Test constant value conversion."""
        python_code = """
def test_constants() -> str:
    x = 42
    y = 3.14
    z = "hello"
    b = True
    return z
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut x: i32 = 42;" in rust_code
        assert "let mut y: f64 = 3.14;" in rust_code
        assert 'let mut z: String = "hello".to_string();' in rust_code
        assert "let mut b: bool = true;" in rust_code

    def test_boolean_constants(self):
        """Test boolean constant conversion."""
        python_code = """
def test_bool() -> bool:
    x = True
    y = False
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut x: bool = true;" in rust_code
        assert "let mut y: bool = false;" in rust_code


class TestRustBasicStatements:
    """Test statement conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_variable_assignment(self):
        """Test variable assignment."""
        python_code = """
def test_assignment() -> int:
    x = 10
    y = x + 5
    return y
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut x: i32 = 10;" in rust_code
        assert "let mut y = (x + 5);" in rust_code

    def test_annotated_assignment(self):
        """Test annotated assignment."""
        python_code = """
def test_annotated() -> int:
    count: int = 0
    name: str = "test"
    return count
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut count: i32 = 0;" in rust_code
        assert 'let mut name: String = "test".to_string();' in rust_code

    def test_if_statement(self):
        """Test if statement conversion."""
        python_code = """
def test_if(x: int) -> str:
    if x > 0:
        return "positive"
    else:
        return "not positive"
"""
        rust_code = self.converter.convert_code(python_code)

        assert "if (x > 0)" in rust_code
        assert '"positive".to_string()' in rust_code
        assert '"not positive".to_string()' in rust_code

    def test_while_loop(self):
        """Test while loop conversion."""
        python_code = """
def test_while(n: int) -> int:
    i = 0
    while i < n:
        i = i + 1
    return i
"""
        rust_code = self.converter.convert_code(python_code)

        assert "while (i < n)" in rust_code
        assert "i = (i + 1);" in rust_code

    def test_for_range_loop(self):
        """Test for loop with range conversion."""
        python_code = """
def test_for(n: int) -> int:
    total = 0
    for i in range(n):
        total = total + i
    return total
"""
        rust_code = self.converter.convert_code(python_code)

        assert "for i in 0.." in rust_code
        assert "total = (total + i);" in rust_code

    def test_for_range_with_start_stop(self):
        """Test for loop with range(start, stop)."""
        python_code = """
def test_for_range(start: int, stop: int) -> int:
    total = 0
    for i in range(start, stop):
        total += i
    return total
"""
        rust_code = self.converter.convert_code(python_code)

        assert f"for i in start..stop" in rust_code
        assert "total += i;" in rust_code


class TestRustBuiltinFunctions:
    """Test built-in function conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_print_function(self):
        """Test print function conversion."""
        python_code = """
def test_print() -> None:
    print("Hello, world!")
"""
        rust_code = self.converter.convert_code(python_code)

        assert 'print_value("Hello, world!".to_string())' in rust_code

    def test_len_function(self):
        """Test len function conversion."""
        python_code = """
def test_len(text: str) -> int:
    return len(text)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Builtins::len_string(&text)" in rust_code

    def test_abs_function(self):
        """Test abs function conversion."""
        python_code = """
def test_abs(x: int) -> int:
    return abs(x)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Builtins::abs_i32(x)" in rust_code

    def test_min_max_functions(self):
        """Test min and max function conversion."""
        python_code = """
def test_min_max(a: int, b: int) -> int:
    smaller = min(a, b)
    larger = max(a, b)
    return larger - smaller
"""
        rust_code = self.converter.convert_code(python_code)

        assert "Builtins::min_i32(a, b)" in rust_code
        assert "Builtins::max_i32(a, b)" in rust_code

    def test_type_conversion_functions(self):
        """Test type conversion functions."""
        python_code = """
def test_conversions(x: int, y: float, z: bool) -> str:
    a = str(x)
    b = float(y)
    c = int(y)
    return a
"""
        rust_code = self.converter.convert_code(python_code)

        assert "to_string(x)" in rust_code
        assert "to_f64_from_i32(y)" in rust_code
        assert "to_i32_from_f64(y)" in rust_code


class TestRustTypeInference:
    """Test type inference functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_integer_inference(self):
        """Test integer type inference."""
        python_code = """
def test_int() -> int:
    x = 42
    return x * 2
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut x: i32 = 42;" in rust_code

    def test_string_inference(self):
        """Test string type inference."""
        python_code = """
def test_string() -> str:
    message = "Hello"
    return message
"""
        rust_code = self.converter.convert_code(python_code)

        assert 'let mut message: String = "Hello".to_string();' in rust_code

    def test_boolean_inference(self):
        """Test boolean type inference."""
        python_code = """
def test_bool() -> bool:
    flag = True
    return not flag
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut flag: bool = true;" in rust_code
        assert "!flag" in rust_code

    def test_auto_type_inference(self):
        """Test automatic type inference when no annotation."""
        python_code = """
def mystery_function(x, y):
    result = x + y
    return result
"""
        rust_code = self.converter.convert_code(python_code)

        assert "fn mystery_function(x: i32, y: i32) -> i32" in rust_code
        assert "let mut result = (x + y);" in rust_code

    def test_subscripted_list_type(self):
        """Test subscripted list type annotation (Python 3.9+)."""
        python_code = """
def process_numbers(numbers: list[int]) -> int:
    total: int = 0
    for n in numbers:
        total += n
    return total
"""
        rust_code = self.converter.convert_code(python_code)

        # Immutability analysis detects read-only parameter, generates immutable reference
        assert "fn process_numbers(numbers: &Vec<i32>) -> i32" in rust_code
        assert "let mut total: i32 = 0;" in rust_code

    def test_subscripted_dict_type(self):
        """Test subscripted dict type annotation (Python 3.9+)."""
        python_code = """
def lookup_score(scores: dict[str, int], name: str) -> int:
    return scores.get(name, 0)
"""
        rust_code = self.converter.convert_code(python_code)

        # Immutability analysis detects read-only parameter, generates immutable reference
        assert "fn lookup_score(scores: &std::collections::HashMap<String, i32>, name: String) -> i32" in rust_code

    def test_subscripted_set_type(self):
        """Test subscripted set type annotation (Python 3.9+)."""
        python_code = """
def has_duplicates(unique_values: set[int]) -> bool:
    return len(unique_values) > 0
"""
        rust_code = self.converter.convert_code(python_code)

        # Immutability analysis detects read-only parameter, generates immutable reference
        assert "fn has_duplicates(unique_values: &std::collections::HashSet<i32>) -> bool" in rust_code

    def test_list_literal_with_annotation(self):
        """Test list literal type inference with annotation."""
        python_code = """
def create_list() -> list[int]:
    values: list[int] = [10, 20, 30]
    return values
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut values: Vec<i32> = vec![10, 20, 30];" in rust_code

    def test_dict_literal_with_annotation(self):
        """Test dict literal type inference with annotation."""
        python_code = """
def create_dict() -> dict[str, int]:
    mapping: dict[str, int] = {"key1": 1, "key2": 2}
    return mapping
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut mapping: std::collections::HashMap<String, i32>" in rust_code

    def test_set_literal_with_annotation(self):
        """Test set literal type inference with annotation."""
        python_code = """
def create_set() -> set[int]:
    unique: set[int] = {1, 2, 3}
    return unique
"""
        rust_code = self.converter.convert_code(python_code)

        assert "let mut unique: std::collections::HashSet<i32>" in rust_code


class TestRustAdvancedExpressions:
    """Test advanced expression handling."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToRustConverter()

    def test_nested_expressions(self):
        """Test nested expression conversion."""
        python_code = """
def test_nested(a: int, b: int, c: int) -> int:
    return (a + b) * (c - a)
"""
        rust_code = self.converter.convert_code(python_code)

        assert "((a + b) * (c - a))" in rust_code

    def test_power_operator(self):
        """Test power operator conversion."""
        python_code = """
def test_power(base: int, exp: int) -> int:
    return base ** exp
"""
        rust_code = self.converter.convert_code(python_code)

        assert "base.pow(exp as u32)" in rust_code

    def test_complex_conditions(self):
        """Test complex conditional expressions."""
        python_code = """
def test_complex(x: int, y: int) -> bool:
    return x > y and x < 100
"""
        rust_code = self.converter.convert_code(python_code)
        # Complex conditions with comparison and boolean operators
        assert "(x > y)" in rust_code
        assert "(x < 100)" in rust_code
        assert "&&" in rust_code