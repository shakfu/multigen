"""Tests specifically for Python-to-C inference."""


from multigen.backends.c.emitter import MultiGenPythonToCConverter


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
