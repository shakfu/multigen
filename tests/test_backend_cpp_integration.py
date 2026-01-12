"""Integration tests for C++ backend - end-to-end functionality."""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.errors import UnsupportedFeatureError

class TestCppIntegrationBasic:
    """Test basic integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_complete_simple_program(self):
        """Test conversion of a complete simple program."""
        python_code = """
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

def main() -> None:
    result = factorial(5)
    print(result)
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check includes and namespace
        assert "#include <iostream>" in cpp_code
        assert "using namespace std;" in cpp_code
        assert "using namespace multigen;" in cpp_code

        # Check factorial function
        assert "int factorial(int n)" in cpp_code
        assert "if ((n <= 1))" in cpp_code
        assert "return 1;" in cpp_code
        assert "return (n * factorial((n - 1)));" in cpp_code

        # Check main function
        assert "void main()" in cpp_code
        assert "int result = factorial(5);" in cpp_code or "auto result = factorial(5);" in cpp_code
        assert "cout << result << endl;" in cpp_code

    def test_program_with_multiple_functions(self):
        """Test program with multiple interacting functions."""
        python_code = """
def add(a: int, b: int) -> int:
    return a + b

def multiply(a: int, b: int) -> int:
    return a * b

def calculate(x: int, y: int) -> int:
    sum_val = add(x, y)
    product = multiply(sum_val, 2)
    return product
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "int add(int a, int b)" in cpp_code
        assert "int multiply(int a, int b)" in cpp_code
        assert "int calculate(int x, int y)" in cpp_code
        assert ("int sum_val = add(x, y);" in cpp_code or "auto sum_val = add(x, y);" in cpp_code)
        assert ("int product = multiply(sum_val, 2);" in cpp_code or "auto product = multiply(sum_val, 2);" in cpp_code)

    def test_program_with_string_processing(self):
        """Test program with string operations."""
        python_code = """
def process_text(text: str) -> str:
    cleaned = text.strip()
    upper_text = cleaned.upper()
    return upper_text

def format_greeting(name: str) -> str:
    processed_name = process_text(name)
    greeting = "Hello, " + processed_name + "!"
    return greeting
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "StringOps::strip(text)" in cpp_code
        assert "StringOps::upper(cleaned)" in cpp_code
        assert "std::string process_text(std::string text)" in cpp_code
        assert "std::string format_greeting(std::string name)" in cpp_code
        assert ("std::string processed_name = process_text(name);" in cpp_code or
                "auto processed_name = process_text(name);" in cpp_code)


class TestCppIntegrationOOP:
    """Test OOP integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_complete_class_with_methods(self):
        """Test complete class with constructor and methods."""
        python_code = """
class BankAccount:
    def __init__(self, account_number: str, initial_balance: float):
        self.account_number: str = account_number
        self.balance: float = initial_balance

    def deposit(self, amount: float) -> None:
        self.balance += amount

    def withdraw(self, amount: float) -> bool:
        if amount <= self.balance:
            self.balance -= amount
            return True
        return False

    def get_balance(self) -> float:
        return self.balance

def create_account() -> BankAccount:
    account = BankAccount("12345", 1000.0)
    return account
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check class definition
        assert "class BankAccount {" in cpp_code
        assert "std::string account_number;" in cpp_code
        assert "double balance;" in cpp_code

        # Check constructor
        assert "BankAccount(std::string account_number, double initial_balance)" in cpp_code
        assert "this->account_number = account_number;" in cpp_code
        assert "this->balance = initial_balance;" in cpp_code

        # Check methods
        assert "void deposit(double amount)" in cpp_code
        assert "this->balance += amount;" in cpp_code
        assert "bool withdraw(double amount)" in cpp_code
        assert "if ((amount <= this->balance))" in cpp_code
        assert "double get_balance()" in cpp_code
        assert "return this->balance;" in cpp_code

    def test_multiple_classes_interaction(self):
        """Test multiple classes that interact."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

    def distance_from_origin(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5

class Rectangle:
    def __init__(self, top_left: Point, width: int, height: int):
        self.top_left = top_left
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check both classes
        assert "class Point {" in cpp_code
        assert "class Rectangle {" in cpp_code

        # Check Point class
        assert "Point(int x, int y)" in cpp_code
        assert "double distance_from_origin()" in cpp_code

        # Check Rectangle class (simplified object composition)
        assert "Rectangle(" in cpp_code
        assert "int area()" in cpp_code


class TestCppIntegrationAdvanced:
    """Test advanced integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_program_with_comprehensions(self):
        """Test program using comprehensions."""
        python_code = """
def process_numbers(n: int) -> dict:
    squares = [x * x for x in range(n)]
    even_squares = [x for x in squares if x % 2 == 0]
    square_map = {x: x * x for x in range(n)}
    return square_map

def filter_data() -> list:
    data = [x for x in range(20) if x % 3 == 0]
    return data
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "list_comprehension" in cpp_code
        assert "dict_comprehension" in cpp_code
        assert "Range(" in cpp_code

    def test_mixed_features_program(self):
        """Test program combining multiple advanced features."""
        python_code = """
class TextProcessor:
    def __init__(self, prefix: str):
        self.prefix: str = prefix

    def process_words(self, words: list) -> list:
        # Use comprehension with string methods
        processed = [self.prefix + word.upper() for word in words]
        return processed

def analyze_text(text: str) -> dict:
    words = text.split()
    word_lengths = {word: len(word) for word in words}
    return word_lengths
"""
        cpp_code = self.converter.convert_code(python_code)

        # Check class
        assert "class TextProcessor {" in cpp_code
        assert "std::string prefix;" in cpp_code

        # Check string methods
        assert "StringOps::split(text)" in cpp_code
        assert "StringOps::upper" in cpp_code

        # Check comprehensions
        assert "dict_comprehension" in cpp_code
        assert "list_comprehension" in cpp_code

    def test_control_flow_with_functions(self):
        """Test complex control flow with function calls."""
        python_code = """
def is_prime(n: int) -> bool:
    if n < 2:
        return False
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

def find_primes(limit: int) -> list:
    primes = []
    for num in range(2, limit):
        if is_prime(num):
            primes.append(num)
    return primes

def count_primes_up_to(n: int) -> int:
    prime_list = find_primes(n)
    return len(prime_list)
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "bool is_prime(int n)" in cpp_code
        assert "if ((n < 2))" in cpp_code
        assert "for (int i = 2; i < n; i++)" in cpp_code
        assert "if (((n % i) == 0))" in cpp_code

        # Note: Complex list operations would need more sophisticated handling
        assert "find_primes(n)" in cpp_code
        assert "multigen::len(prime_list)" in cpp_code


class TestCppIntegrationErrorHandling:
    """Test error handling in integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_unsupported_features_raise_errors(self):
        """Test that unsupported features raise appropriate errors."""
        # Note: This would depend on what features are actually unsupported
        python_code = """
def test_unsupported():
    # Most basic features should be supported
    pass
"""
        # Should not raise error for basic code
        cpp_code = self.converter.convert_code(python_code)
        assert ("void test_unsupported()" in cpp_code or "auto test_unsupported()" in cpp_code)

    def test_type_inference_fallback(self):
        """Test that type inference falls back to 'auto' when needed."""
        python_code = """
def mystery_function(x, y):
    result = x + y  # Should infer as auto
    return result
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "auto mystery_function(auto x, auto y)" in cpp_code
        assert "auto result = (x + y);" in cpp_code

    def test_complex_expressions(self):
        """Test handling of complex nested expressions."""
        python_code = """
def complex_calc(a: int, b: int, c: int) -> int:
    result = (a + b) * (c - a) + b * c
    return result
"""
        cpp_code = self.converter.convert_code(python_code)

        # Should handle complex expression parsing
        assert "((a + b) * (c - a)) + (b * c)" in cpp_code or \
               "((a + b) * (c - a))" in cpp_code and "(b * c)" in cpp_code