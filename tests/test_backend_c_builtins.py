"""Tests for Python builtins support in C backend."""


from multigen.backends.c.converter import MultiGenPythonToCConverter


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
