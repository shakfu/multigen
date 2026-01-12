"""Tests specifically for Python-to-C converter functionality."""


import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestPy2CBasicConversion:
    """Test basic Python-to-C conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_arithmetic(self):
        """Test simple arithmetic operations."""
        python_code = """
def calc(a: int, b: int) -> int:
    return a + b * 2 - 1
"""
        c_code = self.converter.convert_code(python_code)

        assert "int calc(int a, int b)" in c_code
        assert "return ((a + (b * 2)) - 1);" in c_code

    def test_variable_declarations(self):
        """Test variable declarations and assignments."""
        python_code = """
def test_vars(x: int) -> int:
    y: int = x * 2
    z: float = 3.14
    return y
"""
        c_code = self.converter.convert_code(python_code)

        assert "int y = (x * 2);" in c_code
        assert "double z = 3.14;" in c_code

    def test_comparison_operations(self):
        """Test comparison operations."""
        python_code = """
def compare(a: int, b: int) -> bool:
    return a > b and a != 0
"""
        c_code = self.converter.convert_code(python_code)

        assert "bool compare(int a, int b)" in c_code
        # Note: 'and' operation will need special handling

    def test_string_handling(self):
        """Test string constant handling."""
        python_code = """
def get_message() -> str:
    msg: str = "Hello, World!"
    return msg
"""
        c_code = self.converter.convert_code(python_code)

        assert "char* get_message(void)" in c_code
        assert '"Hello, World!"' in c_code

    def test_boolean_values(self):
        """Test boolean constant handling."""
        python_code = """
def get_bool() -> bool:
    return True
"""
        c_code = self.converter.convert_code(python_code)

        assert "bool get_bool(void)" in c_code
        assert "return true;" in c_code

    def test_unary_operations(self):
        """Test unary operations."""
        python_code = """
def negate(x: int) -> int:
    return -x
"""
        c_code = self.converter.convert_code(python_code)

        assert "return (-x);" in c_code

    def test_multiple_parameters(self):
        """Test functions with multiple parameters."""
        python_code = """
def multi_param(a: int, b: float, c: bool, d: str) -> float:
    return b
"""
        c_code = self.converter.convert_code(python_code)

        assert "double multi_param(int a, double b, bool c, char* d)" in c_code

    def test_no_parameters(self):
        """Test functions with no parameters."""
        python_code = """
def no_params() -> int:
    return 42
"""
        c_code = self.converter.convert_code(python_code)

        assert "int no_params(void)" in c_code

    def test_void_return(self):
        """Test functions with no return value."""
        python_code = """
def void_func(x: int):
    pass
"""
        c_code = self.converter.convert_code(python_code)

        assert "void void_func(int x)" in c_code




class TestPy2CBuiltinFunctions:
    """Test built-in function conversion with runtime support."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_abs_function(self):
        """Test abs() function conversion."""
        python_code = """
def test_abs(x: int) -> int:
    return abs(x)
"""
        c_code = self.converter.convert_code(python_code)

        assert "multigen_abs_int(x)" in c_code

    def test_bool_function(self):
        """Test bool() function conversion."""
        python_code = """
def test_bool(x: int) -> bool:
    return bool(x)
"""
        c_code = self.converter.convert_code(python_code)

        assert "multigen_bool_int(x)" in c_code

    def test_len_function(self):
        """Test len() function conversion."""
        python_code = """
def test_len(arr):
    return len(arr)
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int_size" in c_code


class TestPy2CTypeInference:
    """Test type inference capabilities."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_integer_inference(self):
        """Test integer type inference."""
        python_code = """
def infer_int():
    x = 42
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "int x = 42;" in c_code

    def test_float_inference(self):
        """Test float type inference."""
        python_code = """
def infer_float():
    x = 3.14
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "double x = 3.14;" in c_code

    def test_bool_inference(self):
        """Test boolean type inference."""
        python_code = """
def infer_bool():
    x = True
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "bool x = true;" in c_code

    def test_string_inference(self):
        """Test string type inference."""
        python_code = """
def infer_string():
    x = "hello"
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert 'char* x = "hello";' in c_code


class TestPy2CExpressions:
    """Test expression conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_arithmetic_precedence(self):
        """Test arithmetic operator precedence."""
        python_code = """
def precedence_test(a: int, b: int, c: int) -> int:
    return a + b * c
"""
        c_code = self.converter.convert_code(python_code)

        assert "return (a + (b * c));" in c_code

    def test_parentheses_handling(self):
        """Test parentheses in expressions."""
        python_code = """
def paren_test(a: int, b: int, c: int) -> int:
    return (a + b) * c
"""
        c_code = self.converter.convert_code(python_code)

        assert "return ((a + b) * c);" in c_code

    def test_division_operations(self):
        """Test division operations."""
        python_code = """
def div_test(a: int, b: int) -> int:
    return a / b
"""
        c_code = self.converter.convert_code(python_code)

        assert "return (a / b);" in c_code

    def test_modulo_operation(self):
        """Test modulo operation."""
        python_code = """
def mod_test(a: int, b: int) -> int:
    return a % b
"""
        c_code = self.converter.convert_code(python_code)

        assert "return (a % b);" in c_code


class TestPy2CErrorHandling:
    """Test error handling and edge cases."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_invalid_syntax(self):
        """Test handling of invalid Python syntax."""
        invalid_code = """
def invalid_syntax(
    # Missing closing parenthesis
"""
        with pytest.raises((SyntaxError, UnsupportedFeatureError)):
            self.converter.convert_code(invalid_code)

    def test_empty_function(self):
        """Test handling of empty functions."""
        python_code = """
def empty_func():
    pass
"""
        c_code = self.converter.convert_code(python_code)

        assert "void empty_func(void)" in c_code

    def test_complex_unsupported_feature(self):
        """Test handling of unsupported features."""
        python_code = """
def use_list_comprehension():
    return [x for x in range(10) if x % 2 == 0]
"""
        # Should handle gracefully, either by conversion or error
        try:
            c_code = self.converter.convert_code(python_code)
            assert c_code is not None
        except UnsupportedFeatureError:
            # This is acceptable for unsupported features
            pass

    def test_multiple_functions(self):
        """Test handling of multiple functions in one module."""
        python_code = """
def func1(x: int) -> int:
    return x + 1

def func2(y: int) -> int:
    return y * 2
"""
        c_code = self.converter.convert_code(python_code)

        assert "int func1(int x)" in c_code
        assert "int func2(int y)" in c_code
        assert "return (x + 1);" in c_code
        assert "return (y * 2);" in c_code


@pytest.mark.parametrize("python_op,c_op", [
    ("+", "+"),
    ("-", "-"),
    ("*", "*"),
    ("/", "/"),
    ("%", "%"),
    ("==", "=="),
    ("!=", "!="),
    ("<", "<"),
    ("<=", "<="),
    (">", ">"),
    (">=", ">="),
])
def test_operator_conversion(python_op, c_op):
    """Test that Python operators are correctly converted to C operators."""
    converter = MultiGenPythonToCConverter()

    python_code = f"""
def test_op(a: int, b: int) -> int:
    return a {python_op} b
"""

    c_code = converter.convert_code(python_code)
    assert f"a {c_op} b" in c_code


@pytest.mark.parametrize("python_type,c_type", [
    ("int", "int"),
    ("float", "double"),
    ("bool", "bool"),
    ("str", "char*"),
    ("None", "void"),
])
def test_type_conversion_parametrized(python_type, c_type):
    """Test parametrized type conversion."""
    converter = MultiGenPythonToCConverter()

    if python_type == "None":
        python_code = """
def test_type():
    pass
"""
        c_code = converter.convert_code(python_code)
        assert f"{c_type} test_type(void)" in c_code
    else:
        python_code = f"""
def test_type(x: {python_type}) -> {python_type}:
    return x
"""
        c_code = converter.convert_code(python_code)
        assert f"{c_type} test_type({c_type} x)" in c_code
