"""Integration tests for Go backend - end-to-end functionality."""

import pytest

from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestGoIntegrationBasic:
    """Test basic integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

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
        go_code = self.converter.convert_code(python_code)

        # Check imports and package
        assert "package main" in go_code
        assert 'import "multigenproject/multigen"' in go_code

        # Check factorial function
        assert "func factorial(n int) int" in go_code
        assert "if (n <= 1)" in go_code
        assert "return 1" in go_code
        assert "return (n * factorial((n - 1)))" in go_code

        # Check main function
        assert "func main()" in go_code
        assert "result := factorial(5)" in go_code
        assert "multigen.Print(result)" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "func add(a int, b int) int" in go_code
        assert "func multiply(a int, b int) int" in go_code
        assert "func calculate(x int, y int) int" in go_code
        assert "sum_val := add(x, y)" in go_code
        assert "product := multiply(sum_val, 2)" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "multigen.StrOps.Strip(text)" in go_code
        assert "multigen.StrOps.Upper(cleaned)" in go_code
        assert "func process_text(text string) string" in go_code
        assert "func format_greeting(name string) string" in go_code
        assert "processed_name := process_text(name)" in go_code


class TestGoIntegrationOOP:
    """Test OOP integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

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
        go_code = self.converter.convert_code(python_code)

        # Check struct definition
        assert "type BankAccount struct {" in go_code
        assert "AccountNumber string" in go_code
        assert "Balance float64" in go_code

        # Check constructor
        assert "func NewBankAccount(account_number string, initial_balance float64) BankAccount" in go_code
        assert "obj.AccountNumber = account_number" in go_code
        assert "obj.Balance = initial_balance" in go_code

        # Check methods
        assert "func (obj *BankAccount) Deposit(amount float64)" in go_code
        assert "obj.Balance += amount" in go_code
        assert "func (obj *BankAccount) Withdraw(amount float64) bool" in go_code
        assert "if (amount <= obj.Balance)" in go_code
        assert "func (obj *BankAccount) GetBalance() float64" in go_code
        assert "return obj.Balance" in go_code

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
        go_code = self.converter.convert_code(python_code)

        # Check both classes
        assert "type Point struct {" in go_code
        assert "type Rectangle struct {" in go_code

        # Check Point class
        assert "func NewPoint(x int, y int) Point" in go_code
        assert "func (obj *Point) DistanceFromOrigin() float64" in go_code

        # Check Rectangle class
        assert "func NewRectangle(" in go_code
        assert "func (obj *Rectangle) Area() int" in go_code


class TestGoIntegrationAdvanced:
    """Test advanced integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

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
        go_code = self.converter.convert_code(python_code)

        assert "multigen.ListComprehension" in go_code
        assert "multigen.DictComprehension" in go_code
        assert "multigen.NewRange(" in go_code

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
        go_code = self.converter.convert_code(python_code)

        # Check class
        assert "type TextProcessor struct {" in go_code
        assert "Prefix string" in go_code

        # Check string methods
        assert "multigen.StrOps.Split(text)" in go_code
        assert "multigen.StrOps.Upper" in go_code

        # Check comprehensions
        assert "multigen.DictComprehension" in go_code
        assert "multigen.ListComprehension" in go_code

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
        go_code = self.converter.convert_code(python_code)

        assert "func is_prime(n int) bool" in go_code
        assert "if (n < 2)" in go_code
        assert "for i := 2; i < n; i++" in go_code
        assert "if ((n % i) == 0)" in go_code

        # Note: Complex list operations would need more sophisticated handling
        assert "find_primes(n)" in go_code
        assert "multigen.Len" in go_code  # Generic len function


class TestGoIntegrationErrorHandling:
    """Test error handling in integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToGoConverter()

    def test_basic_code_conversion(self):
        """Test that basic features work correctly."""
        python_code = """
def test_basic():
    # Most basic features should be supported
    x = 42
    return x
"""
        # Should not raise error for basic code
        go_code = self.converter.convert_code(python_code)
        assert "func test_basic()" in go_code

    def test_type_inference_fallback(self):
        """Test that type inference falls back to 'interface{}' when needed."""
        python_code = """
def mystery_function(x, y):
    result = x + y  # Should infer as interface{}
    return result
"""
        go_code = self.converter.convert_code(python_code)

        assert "func mystery_function(x interface{}, y interface{}) interface{}" in go_code
        assert "result := (x + y)" in go_code

    def test_complex_expressions(self):
        """Test handling of complex nested expressions."""
        python_code = """
def complex_calc(a: int, b: int, c: int) -> int:
    result = (a + b) * (c - a) + b * c
    return result
"""
        go_code = self.converter.convert_code(python_code)

        # Should handle complex expression parsing
        assert "((a + b) * (c - a)) + (b * c)" in go_code or \
               "((a + b) * (c - a))" in go_code and "(b * c)" in go_code

    def test_comprehensive_example(self):
        """Test a comprehensive example combining multiple features."""
        python_code = """
class DataProcessor:
    def __init__(self, name: str):
        self.name: str = name
        self.count: int = 0

    def process_data(self, data: list) -> dict:
        self.count += len(data)
        processed = [item.upper() for item in data]
        result = {item: len(item) for item in processed}
        return result

def main() -> None:
    processor = DataProcessor("Main")
    test_data = ["hello", "world", "test"]
    result = processor.process_data(test_data)
    print("Processed:", len(result))
"""
        go_code = self.converter.convert_code(python_code)

        # Verify class structure
        assert "type DataProcessor struct {" in go_code
        assert "Name string" in go_code
        assert "Count int" in go_code

        # Verify method functionality
        assert "obj.Count +=" in go_code
        assert "multigen.Len" in go_code  # Generic len function
        assert "multigen.ListComprehension" in go_code
        assert "multigen.DictComprehension" in go_code

        # Verify main function
        assert "processor := NewDataProcessor" in go_code
        assert "processor.ProcessData" in go_code