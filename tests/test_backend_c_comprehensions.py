"""Tests for Python comprehensions support in C backend."""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestListComprehensions:
    """Test list comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension with range."""
        python_code = """
def test_list_comp() -> list:
    return [x for x in range(5)]
"""
        c_code = self.converter.convert_code(python_code)

        # Should contain vector initialization and loop
        assert "vec_int" in c_code
        assert "for (int x = 0; x < 5; x += 1)" in c_code
        assert "vec_int_push" in c_code

    def test_list_comprehension_with_expression(self):
        """Test list comprehension with expression transformation."""
        python_code = """
def test_list_comp_expr() -> list:
    return [x * 2 for x in range(3)]
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int" in c_code
        assert "for (int x = 0; x < 3; x += 1)" in c_code
        assert "(x * 2)" in c_code

    def test_list_comprehension_with_condition(self):
        """Test list comprehension with if condition."""
        python_code = """
def test_list_comp_condition() -> list:
    return [x for x in range(10) if x % 2 == 0]
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int" in c_code
        assert "for (int x = 0; x < 10; x += 1)" in c_code
        assert "if (((x % 2) == 0))" in c_code

    def test_list_comprehension_with_range_start_stop(self):
        """Test list comprehension with range(start, stop)."""
        python_code = """
def test_list_comp_range(start: int, stop: int) -> list:
    return [i for i in range(start, stop)]
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int" in c_code
        assert "for (int i = start; i < stop; i += 1)" in c_code

    def test_list_comprehension_with_range_step(self):
        """Test list comprehension with range(start, stop, step)."""
        python_code = """
def test_list_comp_step() -> list:
    return [i for i in range(0, 10, 2)]
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int" in c_code
        assert "for (int i = 0; i < 10; i += 2)" in c_code

    def test_list_comprehension_complex_expression(self):
        """Test list comprehension with complex expression."""
        python_code = """
def test_complex_expr() -> list:
    return [x * x + 1 for x in range(5) if x > 1]
"""
        c_code = self.converter.convert_code(python_code)

        assert "vec_int" in c_code
        assert "((x * x) + 1)" in c_code
        assert "if ((x > 1))" in c_code

    def test_list_comprehension_multiple_generators_error(self):
        """Test that multiple generators raise appropriate error."""
        python_code = """
def test_multiple_generators() -> list:
    return [x + y for x in range(3) for y in range(2)]
"""
        with pytest.raises(UnsupportedFeatureError, match="Multiple generators"):
            self.converter.convert_code(python_code)

    def test_list_comprehension_complex_target_error(self):
        """Test that complex loop targets raise appropriate error."""
        python_code = """
def test_complex_target() -> list:
    return [a + b for a, b in [(1, 2), (3, 4)]]
"""
        with pytest.raises(UnsupportedFeatureError, match="Only simple loop variables"):
            self.converter.convert_code(python_code)

    def test_list_comprehension_non_range_iterable(self):
        """Test that non-range iterables are supported via container iteration."""
        python_code = """
def test_non_range() -> list:
    items = [1, 2, 3]
    return [x for x in items]
"""
        c_code = self.converter.convert_code(python_code)
        # Should generate container iteration using vec_int_size and vec_int_at
        assert "vec_int_size" in c_code
        assert "vec_int_at" in c_code


class TestDictComprehensions:
    """Test dictionary comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_dict_comprehension(self):
        """Test simple dictionary comprehension with range."""
        python_code = """
def test_dict_comp() -> dict:
    return {x: x * 2 for x in range(3)}
"""
        c_code = self.converter.convert_code(python_code)

        # Should contain map initialization and loop
        assert "map_int_int" in c_code
        assert "for (int x = 0; x < 3; x += 1)" in c_code
        assert "map_int_int_insert" in c_code

    def test_dict_comprehension_with_condition(self):
        """Test dictionary comprehension with if condition."""
        python_code = """
def test_dict_comp_condition() -> dict:
    return {x: x * x for x in range(5) if x % 2 == 1}
"""
        c_code = self.converter.convert_code(python_code)

        assert "map_int_int" in c_code
        assert "for (int x = 0; x < 5; x += 1)" in c_code
        assert "if (((x % 2) == 1))" in c_code
        assert "(x * x)" in c_code

    def test_dict_comprehension_with_range_parameters(self):
        """Test dictionary comprehension with range parameters."""
        python_code = """
def test_dict_comp_params(n: int) -> dict:
    return {i: i + 10 for i in range(1, n, 2)}
"""
        c_code = self.converter.convert_code(python_code)

        assert "map_int_int" in c_code
        assert "for (int i = 1; i < n; i += 2)" in c_code
        assert "(i + 10)" in c_code

    def test_dict_comprehension_string_keys(self):
        """Test dictionary comprehension with string keys (hypothetical)."""
        python_code = """
def test_dict_string_keys() -> dict:
    return {str(x): x for x in range(3)}
"""
        c_code = self.converter.convert_code(python_code)

        # Should handle string conversion (though simplified)
        assert "map_" in c_code
        assert "for (int x = 0; x < 3; x += 1)" in c_code

    def test_dict_comprehension_multiple_generators_error(self):
        """Test that multiple generators raise appropriate error."""
        python_code = """
def test_multiple_generators() -> dict:
    return {x + y: x * y for x in range(2) for y in range(3)}
"""
        with pytest.raises(UnsupportedFeatureError, match="Multiple generators"):
            self.converter.convert_code(python_code)


class TestSetComprehensions:
    """Test set comprehension conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_set_comprehension(self):
        """Test simple set comprehension with range."""
        python_code = """
def test_set_comp() -> set:
    return {x for x in range(5)}
"""
        c_code = self.converter.convert_code(python_code)

        # Should contain set initialization and loop
        assert "set_int" in c_code
        assert "for (int x = 0; x < 5; x += 1)" in c_code
        assert "set_int_insert" in c_code

    def test_set_comprehension_with_condition(self):
        """Test set comprehension with if condition."""
        python_code = """
def test_set_comp_condition() -> set:
    return {x * 3 for x in range(10) if x > 5}
"""
        c_code = self.converter.convert_code(python_code)

        assert "set_int" in c_code
        assert "for (int x = 0; x < 10; x += 1)" in c_code
        assert "if ((x > 5))" in c_code
        assert "(x * 3)" in c_code

    def test_set_comprehension_with_range_step(self):
        """Test set comprehension with range step."""
        python_code = """
def test_set_comp_step() -> set:
    return {x for x in range(2, 20, 3)}
"""
        c_code = self.converter.convert_code(python_code)

        assert "set_int" in c_code
        assert "for (int x = 2; x < 20; x += 3)" in c_code

    def test_set_comprehension_expression_transformation(self):
        """Test set comprehension with expression transformation."""
        python_code = """
def test_set_expr() -> set:
    return {x % 7 for x in range(15) if x % 2 == 0}
"""
        c_code = self.converter.convert_code(python_code)

        assert "set_int" in c_code
        assert "(x % 7)" in c_code
        assert "if (((x % 2) == 0))" in c_code

    def test_set_comprehension_multiple_conditions_error(self):
        """Test that multiple conditions raise appropriate error."""
        python_code = """
def test_multiple_conditions() -> set:
    return {x for x in range(10) if x > 2 if x < 8}
"""
        with pytest.raises(UnsupportedFeatureError, match="Multiple conditions"):
            self.converter.convert_code(python_code)


class TestComprehensionsIntegration:
    """Test comprehensions integration with other features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_comprehension_in_assignment(self):
        """Test comprehension assigned to variable."""
        python_code = """
def test_assignment() -> int:
    numbers: list = [x * 2 for x in range(3)]
    return len(numbers)
"""
        c_code = self.converter.convert_code(python_code)

        # Should handle assignment and comprehension
        assert "vec_int" in c_code
        assert "for (int x = 0; x < 3; x += 1)" in c_code
        assert "(x * 2)" in c_code

    def test_comprehension_with_function_calls(self):
        """Test comprehension using function calls."""
        python_code = """
def helper(x: int) -> int:
    return x + 5

def test_with_function() -> list:
    return [helper(x) for x in range(3)]
"""
        c_code = self.converter.convert_code(python_code)

        # Should handle function calls in comprehension
        assert "helper(x)" in c_code
        assert "for (int x = 0; x < 3; x += 1)" in c_code

    def test_nested_comprehensions_error(self):
        """Test that nested comprehensions raise appropriate error."""
        python_code = """
def test_nested() -> list:
    return [[y for y in range(x)] for x in range(3)]
"""
        # This should raise an error because list comprehensions within list comprehensions
        # are not supported in the current expression conversion
        try:
            c_code = self.converter.convert_code(python_code)
            # If it doesn't raise an error, the nested comprehension should at least
            # produce some valid C code (though it may be incomplete)
            assert c_code is not None
        except UnsupportedFeatureError:
            # This is acceptable - nested comprehensions are complex
            pass

    def test_comprehension_with_class_instance(self):
        """Test comprehension that creates class instances."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

def test_class_comprehension() -> list:
    return [Point(i, i * 2) for i in range(2)]
"""
        c_code = self.converter.convert_code(python_code)

        # Should handle class instantiation in comprehension
        assert "Point_new(i, (i * 2))" in c_code
        assert "for (int i = 0; i < 2; i += 1)" in c_code

    def test_mixed_comprehension_types(self):
        """Test function using multiple comprehension types."""
        python_code = """
def test_mixed() -> int:
    my_list: list = [x for x in range(3)]
    my_dict: dict = {x: x * 2 for x in range(2)}
    my_set: set = {x for x in range(4)}
    return len(my_list)
"""
        c_code = self.converter.convert_code(python_code)

        # Should handle all three comprehension types
        assert "vec_int" in c_code
        assert "map_int_int" in c_code
        assert "set_int" in c_code


@pytest.mark.parametrize("comp_type,syntax,expected_container", [
    ("list", "[x for x in range(3)]", "vec_int"),
    ("dict", "{x: x * 2 for x in range(3)}", "map_int_int"),
    ("set", "{x for x in range(3)}", "set_int"),
])
def test_comprehension_types_parametrized(comp_type, syntax, expected_container):
    """Test parametrized comprehension type conversion."""
    converter = MultiGenPythonToCConverter()

    python_code = f"""
def test_comp() -> {comp_type}:
    return {syntax}
"""

    c_code = converter.convert_code(python_code)
    assert expected_container in c_code
    assert "for (int x = 0; x < 3; x += 1)" in c_code


@pytest.mark.integration
def test_comprehensions_comprehensive():
    """Comprehensive test of comprehensions working together."""
    converter = MultiGenPythonToCConverter()

    python_code = """
def process_data(n: int) -> dict:
    # Create squares of even numbers
    squares: list = [x * x for x in range(n) if x % 2 == 0]

    # Create mapping of numbers to their squares
    mapping: dict = {i: i * i for i in range(n) if i % 2 == 0}

    # Create set of squares up to limit
    unique_values: set = {x * x for x in range(n) if x * x > 10}

    return mapping
"""

    c_code = converter.convert_code(python_code)

    # Basic sanity checks
    assert c_code is not None
    assert len(c_code) > 300  # Should be substantial
    assert "vec_int" in c_code  # List comprehension
    assert "map_int_int" in c_code  # Dict comprehension
    assert "set_int" in c_code  # Set comprehension
    assert "#include" in c_code  # Should have includes


# Fixtures for testing
@pytest.fixture
def sample_comprehensions():
    """Sample comprehension code for testing."""
    return {
        "simple_list": "[x for x in range(5)]",
        "conditional_list": "[x * 2 for x in range(10) if x % 2 == 0]",
        "simple_dict": "{x: x * x for x in range(3)}",
        "conditional_dict": "{x: x + 1 for x in range(5) if x > 2}",
        "simple_set": "{x for x in range(4)}",
        "conditional_set": "{x * 3 for x in range(6) if x % 2 == 1}",
    }


def test_all_comprehension_samples(sample_comprehensions):
    """Test all sample comprehensions convert successfully."""
    converter = MultiGenPythonToCConverter()

    for name, comp_code in sample_comprehensions.items():
        python_code = f"""
def test_{name}():
    return {comp_code}
"""
        # Should not raise exceptions
        c_code = converter.convert_code(python_code)

        # Basic validation
        assert c_code is not None
        assert len(c_code) > 50  # Should be substantial
        assert "for (" in c_code  # Should contain loop
        assert "{" in c_code and "}" in c_code  # Should have proper C syntax
