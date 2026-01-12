"""Tests for Python object-oriented programming features in C backend."""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestPy2COOPBasics:
    """Test basic OOP conversion functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_simple_class_definition(self):
        """Test simple class with instance variables."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        c_code = self.converter.convert_code(python_code)

        # Check struct definition
        assert "typedef struct Point {" in c_code
        assert "int x;" in c_code
        assert "int y;" in c_code
        assert "} Point;" in c_code

        # Check constructor
        assert "Point Point_new(int x, int y)" in c_code
        assert "obj.x = x;" in c_code
        assert "obj.y = y;" in c_code
        assert "return obj;" in c_code

    def test_class_with_methods(self):
        """Test class with methods."""
        python_code = """
class Calculator:
    def __init__(self, initial: int):
        self.value: int = initial

    def add(self, x: int) -> int:
        self.value = self.value + x
        return self.value

    def get_value(self) -> int:
        return self.value
"""
        c_code = self.converter.convert_code(python_code)

        # Check struct definition
        assert "typedef struct Calculator {" in c_code
        assert "int value;" in c_code

        # Check constructor
        assert "Calculator Calculator_new(int initial)" in c_code
        assert "obj.value = initial;" in c_code

        # Check methods
        assert "int Calculator_add(Calculator* self, int x)" in c_code
        assert "self->value = (self->value + x);" in c_code
        assert "return self->value;" in c_code

        assert "int Calculator_get_value(Calculator* self)" in c_code
        assert "return self->value;" in c_code

    def test_empty_class(self):
        """Test class with no instance variables."""
        python_code = """
class Empty:
    def __init__(self):
        pass

    def do_nothing(self) -> int:
        return 42
"""
        c_code = self.converter.convert_code(python_code)

        # Check struct definition with dummy member
        assert "typedef struct Empty {" in c_code
        assert "char _dummy;" in c_code

        # Check constructor
        assert "Empty Empty_new(void)" in c_code

        # Check method
        assert "int Empty_do_nothing(Empty* self)" in c_code
        assert "return 42;" in c_code

    def test_class_instantiation(self):
        """Test class instantiation in function."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

def create_point() -> Point:
    return Point(10, 20)
"""
        c_code = self.converter.convert_code(python_code)

        # Check that Point() is converted to Point_new()
        assert "return Point_new(10, 20);" in c_code

    def test_method_calls(self):
        """Test method calls on class instances."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.count: int = start

    def increment(self) -> int:
        self.count = self.count + 1
        return self.count

def test_counter() -> int:
    c: Counter = Counter(5)
    return c.increment()
"""
        c_code = self.converter.convert_code(python_code)

        # Check variable declaration and initialization
        assert "Counter c = Counter_new(5);" in c_code

        # Check method call conversion
        # Should be Counter_increment(&c)
        assert "Counter_increment(&c)" in c_code


class TestPy2COOPTypeInference:
    """Test type inference for OOP features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_instance_variable_type_inference(self):
        """Test type inference for instance variables."""
        python_code = """
class DataHolder:
    def __init__(self):
        self.number = 42
        self.text = "hello"
        self.flag = True
        self.decimal = 3.14
"""
        c_code = self.converter.convert_code(python_code)

        # Check inferred types
        assert "int number;" in c_code
        assert "char* text;" in c_code
        assert "bool flag;" in c_code
        assert "double decimal;" in c_code

    def test_mixed_typed_and_inferred_variables(self):
        """Test mix of type annotations and inference."""
        python_code = """
class Mixed:
    def __init__(self, param: int):
        self.typed_var: float = 1.5
        self.inferred_var = param * 2
"""
        c_code = self.converter.convert_code(python_code)

        assert "double typed_var;" in c_code
        assert "int inferred_var;" in c_code
        assert "obj.typed_var = 1.5;" in c_code
        assert "obj.inferred_var = (param * 2);" in c_code


class TestPy2COOPAdvanced:
    """Test advanced OOP features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_method_with_parameters(self):
        """Test methods with multiple parameters."""
        python_code = """
class Rectangle:
    def __init__(self, width: int, height: int):
        self.width: int = width
        self.height: int = height

    def resize(self, new_width: int, new_height: int):
        self.width = new_width
        self.height = new_height

    def area(self) -> int:
        return self.width * self.height
"""
        c_code = self.converter.convert_code(python_code)

        # Check method with parameters
        assert "void Rectangle_resize(Rectangle* self, int new_width, int new_height)" in c_code
        assert "self->width = new_width;" in c_code
        assert "self->height = new_height;" in c_code

        # Check method returning calculation
        assert "int Rectangle_area(Rectangle* self)" in c_code
        assert "return (self->width * self->height);" in c_code

    def test_method_with_control_flow(self):
        """Test methods containing control flow."""
        python_code = """
class Validator:
    def __init__(self, min_val: int, max_val: int):
        self.min_val: int = min_val
        self.max_val: int = max_val

    def is_valid(self, value: int) -> bool:
        if value < self.min_val:
            return False
        elif value > self.max_val:
            return False
        else:
            return True
"""
        c_code = self.converter.convert_code(python_code)

        # Check method with control flow
        assert "bool Validator_is_valid(Validator* self, int value)" in c_code
        assert "if ((value < self->min_val))" in c_code
        assert "return false;" in c_code
        assert "} else {" in c_code
        assert "return true;" in c_code

    def test_complex_class_usage(self):
        """Test complex usage with multiple classes and interactions."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

    def distance_from_origin(self) -> int:
        # Simplified distance calculation
        return self.x + self.y

class Circle:
    def __init__(self, center: Point, radius: int):
        self.center: Point = center
        self.radius: int = radius

def create_circle() -> Circle:
    p: Point = Point(3, 4)
    return Circle(p, 5)
"""
        c_code = self.converter.convert_code(python_code)

        # Check Point class
        assert "typedef struct Point {" in c_code
        assert "Point Point_new(int x, int y)" in c_code

        # Check Circle class with Point member
        assert "typedef struct Circle {" in c_code
        assert "Point center;" in c_code
        assert "int radius;" in c_code

        # Check function usage
        assert "Point p = Point_new(3, 4);" in c_code
        assert "return Circle_new(p, 5);" in c_code


class TestPy2COOPErrorHandling:
    """Test error handling for OOP features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_class_without_init(self):
        """Test class without __init__ method."""
        python_code = """
class NoInit:
    def method(self) -> int:
        return 42
"""
        c_code = self.converter.convert_code(python_code)

        # Should still generate struct and method
        assert "typedef struct NoInit {" in c_code
        assert "char _dummy;" in c_code  # Empty struct placeholder
        assert "int NoInit_method(NoInit* self)" in c_code

    def test_complex_inheritance_unsupported(self):
        """Test that complex inheritance patterns raise appropriate errors."""
        python_code = """
class Base:
    def __init__(self):
        self.value: int = 1

class Derived(Base):
    def __init__(self):
        super().__init__()
        self.extra: int = 2
"""
        # This should either convert gracefully or raise UnsupportedFeatureError
        try:
            c_code = self.converter.convert_code(python_code)
            # If it converts, check that it at least generates some valid C code
            assert "typedef struct" in c_code
        except UnsupportedFeatureError:
            # This is acceptable for complex features
            pass


class TestPy2COOPIntegration:
    """Test integration of OOP features with other converter features."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCConverter()

    def test_class_with_loops(self):
        """Test class methods containing loops."""
        python_code = """
class Accumulator:
    def __init__(self):
        self.total: int = 0

    def sum_to_n(self, n: int) -> int:
        for i in range(n):
            self.total = self.total + i
        return self.total
"""
        c_code = self.converter.convert_code(python_code)

        # Check method with for loop
        assert "int Accumulator_sum_to_n(Accumulator* self, int n)" in c_code
        assert "for (int i = 0; i < n; i += 1)" in c_code
        assert "self->total = (self->total + i);" in c_code

    def test_class_with_function_calls(self):
        """Test class methods calling other functions."""
        python_code = """
def helper_function(x: int) -> int:
    return x * 2

class Calculator:
    def __init__(self, base: int):
        self.base: int = base

    def calculate(self, input: int) -> int:
        result: int = helper_function(input)
        return self.base + result
"""
        c_code = self.converter.convert_code(python_code)

        # Check that both function and class method are generated
        assert "int helper_function(int x)" in c_code
        assert "int Calculator_calculate(Calculator* self, int input)" in c_code
        assert "result = helper_function(input);" in c_code
        assert "return (self->base + result);" in c_code


@pytest.mark.parametrize("python_type,c_type", [
    ("int", "int"),
    ("float", "double"),
    ("bool", "bool"),
    ("str", "char*"),
])
def test_instance_variable_type_mapping(python_type, c_type):
    """Test parametrized type mapping for instance variables."""
    converter = MultiGenPythonToCConverter()

    python_code = f"""
class TypeTest:
    def __init__(self, value: {python_type}):
        self.value: {python_type} = value
"""

    c_code = converter.convert_code(python_code)
    assert f"{c_type} value;" in c_code
    assert f"TypeTest TypeTest_new({c_type} value)" in c_code


# Integration test
@pytest.mark.integration
def test_oop_comprehensive():
    """Comprehensive test of OOP features working together."""
    converter = MultiGenPythonToCConverter()

    python_code = """
class BankAccount:
    def __init__(self, initial_balance: int):
        self.balance: int = initial_balance

    def deposit(self, amount: int):
        self.balance = self.balance + amount

    def withdraw(self, amount: int) -> bool:
        if amount > self.balance:
            return False
        else:
            self.balance = self.balance - amount
            return True

    def get_balance(self) -> int:
        return self.balance

def test_banking() -> int:
    account: BankAccount = BankAccount(100)
    account.deposit(50)
    success: bool = account.withdraw(30)
    if success:
        return account.get_balance()
    else:
        return -1
"""

    c_code = converter.convert_code(python_code)

    # Basic sanity checks
    assert c_code is not None
    assert len(c_code) > 200  # Should be substantial
    assert "typedef struct BankAccount" in c_code
    assert "BankAccount_deposit" in c_code
    assert "BankAccount_withdraw" in c_code
    assert "BankAccount_get_balance" in c_code
    assert "#include" in c_code  # Should have includes
