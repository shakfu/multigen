"""Tests for Python control flow support in C backend."""



from multigen.backends.c.emitter import MultiGenPythonToCConverter


class TestPy2CControlFlow:
    """Test control flow conversion."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_if_else(self):
        """Test if-else statements."""
        python_code = """
def check_sign(x: int) -> int:
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0
"""
        c_code = self.converter.convert_code(python_code)

        assert "if ((x > 0))" in c_code
        assert "} else {" in c_code
        assert "return 1;" in c_code
        assert "return (-1);" in c_code
        assert "return 0;" in c_code

    def test_while_loop(self):
        """Test while loops."""
        python_code = """
def count_down(n: int) -> int:
    while n > 0:
        n = n - 1
    return n
"""
        c_code = self.converter.convert_code(python_code)

        assert "while ((n > 0))" in c_code
        assert "n = (n - 1);" in c_code

    def test_for_range_simple(self):
        """Test simple for range loop."""
        python_code = """
def sum_n(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
    return total
"""
        c_code = self.converter.convert_code(python_code)

        assert "for (int i = 0; i < n; i += 1)" in c_code
        assert "total = (total + i);" in c_code

    def test_for_range_with_start_stop(self):
        """Test for range with start and stop."""
        python_code = """
def sum_range(start: int, stop: int) -> int:
    total: int = 0
    for i in range(start, stop):
        total = total + i
    return total
"""
        c_code = self.converter.convert_code(python_code)

        assert "for (int i = start; i < stop; i += 1)" in c_code

    def test_for_range_with_step(self):
        """Test for range with step."""
        python_code = """
def sum_step(n: int) -> int:
    total: int = 0
    for i in range(0, n, 2):
        total = total + i
    return total
"""
        c_code = self.converter.convert_code(python_code)

        assert "for (int i = 0; i < n; i += 2)" in c_code

    def test_nested_control_flow(self):
        """Test nested control structures."""
        python_code = """
def nested_test(n: int) -> int:
    result: int = 0
    for i in range(n):
        if i % 2 == 0:
            result = result + i
    return result
"""
        c_code = self.converter.convert_code(python_code)

        assert "for (int i = 0; i < n; i += 1)" in c_code
        assert "if (((i % 2) == 0))" in c_code

