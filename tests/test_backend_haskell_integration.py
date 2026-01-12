"""Integration tests for Haskell backend end-to-end scenarios."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter 
from multigen.backends.errors import UnsupportedFeatureError


class TestHaskellIntegrationComplete:
    """Test complete end-to-end integration scenarios."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_complete_class_with_all_features(self):
        """Test class with all advanced features integrated."""
        python_code = """
class DataProcessor:
    def __init__(self, name: str, threshold: int):
        self.name: str = name
        self.threshold: int = threshold
        self.count: int = 0

    def process_numbers(self, numbers: list) -> list:
        # Use comprehensions for filtering
        filtered = [x for x in numbers if x > self.threshold]

        # Use string methods
        processed_name = self.name.upper().strip()

        # Use augmented assignment
        self.count += len(filtered)

        # Use range-based comprehension
        squared = [x * x for x in range(len(filtered))]

        return squared

    def get_stats(self) -> dict:
        return {
            "name": self.name.lower(),
            "count": self.count,
            "threshold": self.threshold
        }
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify data type definition
        assert "data DataProcessor = DataProcessor" in haskell_code
        assert "name :: String" in haskell_code
        assert "threshold :: Int" in haskell_code
        assert "count :: Int" in haskell_code

        # Verify constructor
        assert "newDataProcessor :: String -> Int -> DataProcessor" in haskell_code

        # Verify basic structure (methods are simplified in current implementation)
        assert "processNumbers :: DataProcessor -> [a] -> [a]" in haskell_code
        assert "getStats :: DataProcessor -> Dict String a" in haskell_code or "getStats :: DataProcessor" in haskell_code

    def test_multiple_classes_interaction(self):
        """Test multiple classes working together."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y

    def distance_squared(self) -> int:
        return self.x * self.x + self.y * self.y

class Circle:
    def __init__(self, center: Point, radius: int):
        self.center = center
        self.radius: int = radius

    def area_approximation(self) -> int:
        # Use comprehensions and class methods
        coords = [self.center.x, self.center.y]
        radius_squared = self.radius * self.radius

        # Simple integer approximation of pi * r^2
        return 3 * radius_squared

def create_circles() -> list:
    points = [Point(i, i + 1) for i in range(3)]
    circles = [Circle(p, i + 5) for i, p in enumerate(points)]
    return circles
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify both data types
        assert "data Point = Point" in haskell_code
        assert "data Circle = Circle" in haskell_code

        # Verify Point methods
        assert "distanceSquared :: Point -> Int" in haskell_code

        # Verify Circle methods
        assert "areaApproximation :: Circle -> Int" in haskell_code

        # Verify constructors
        assert "newPoint :: Int -> Int -> Point" in haskell_code
        assert "newCircle :: " in haskell_code

        # Note: The enumerate function and complex multi-class interactions
        # would require more sophisticated handling in Haskell

    def test_advanced_comprehensions_with_methods(self):
        """Test comprehensions with method calls and complex expressions."""
        python_code = """
class StringProcessor:
    def __init__(self, prefix: str):
        self.prefix: str = prefix

    def clean_word(self, word: str) -> str:
        return word.strip().lower()

    def process_words(self, words: list) -> dict:
        # Complex comprehension with method calls
        cleaned = [self.clean_word(word) for word in words]

        # Dictionary comprehension with string operations
        result = {word.upper(): len(word) for word in cleaned if len(word) > 2}

        return result

def test_processing() -> dict:
    processor = StringProcessor("test")
    words = ["Hello", "World", "Test", "A", "B"]
    return processor.process_words(words)
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify basic method structure
        assert "cleanWord :: StringProcessor -> String -> String" in haskell_code
        assert "processWords :: StringProcessor -> [a] -> Dict String a" in haskell_code or "processWords :: StringProcessor" in haskell_code

    def test_nested_control_flow_with_features(self):
        """Test nested control structures with advanced features."""
        python_code = """
class Calculator:
    def __init__(self, precision: int):
        self.precision: int = precision
        self.results: list = []

    def calculate_batch(self, numbers: list) -> list:
        for num in numbers:
            if num > 0:
                # Use comprehensions in loop
                squares = [x * x for x in range(num) if x % 2 == 0]
                total = 0

                for square in squares:
                    total += square

                # Use string methods and augmented assignment
                result_str = str(total).strip()
                if len(result_str) > self.precision:
                    result_str = result_str.upper()

                self.results.append(total)
            else:
                # Use dictionary comprehension
                negative_map = {abs(num): num for num in [num]}
                self.results.append(list(negative_map.values())[0])

        return self.results
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify basic structure
        assert "calculateBatch :: Calculator -> [a] -> [a]" in haskell_code or "calculateBatch :: Calculator" in haskell_code

    def test_complex_expressions_integration(self):
        """Test complex expressions with all features combined."""
        python_code = """
def complex_processing(data: list, threshold: float) -> dict:
    # Complex comprehension with multiple operations
    processed = [
        x * 2 + 1 for x in data
        if x > threshold and len(str(x)) > 1
    ]

    # Nested comprehensions with string operations
    string_data = [str(x).upper().strip() for x in processed]

    # Dictionary with complex key-value expressions
    result = {
        item.lower(): len(item) * 2
        for item in string_data
        if item.find("0") == -1
    }

    # Set comprehension with mathematical operations
    unique_lengths = {len(key) for key in result.keys() if len(key) % 2 == 0}

    return {
        "processed_count": len(processed),
        "result_keys": list(result.keys()),
        "unique_lengths": list(unique_lengths)
    }
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify complex comprehensions
        assert "listComprehensionWithFilter" in haskell_code
        assert "dictComprehensionWithFilter" in haskell_code
        assert "setComprehensionWithFilter" in haskell_code

        # Verify string methods
        assert "upper" in haskell_code
        assert "strip" in haskell_code
        assert "lower" in haskell_code
        assert "find" in haskell_code

        # Verify built-in functions
        assert "len'" in haskell_code
        assert "toString" in haskell_code

        # Verify complex expressions
        assert "((x * 2) + 1)" in haskell_code
        assert "* 2)" in haskell_code  # Check for multiplication by 2 in expressions

    def test_error_handling_for_unsupported_features(self):
        """Test that unsupported features raise appropriate errors."""

        # Test generator expression (should be unsupported)
        python_code_generator = """
def test_generator():
    return (x for x in range(5))
"""
        with pytest.raises(UnsupportedFeatureError):
            self.converter.convert_code(python_code_generator)

        # Test try/except (should be unsupported)
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

        # Test with statement (should be unsupported)
        python_code_with = """
def test_with():
    with open("file.txt") as f:
        content = f.read()
    return content
"""
        with pytest.raises(UnsupportedFeatureError):
            self.converter.convert_code(python_code_with)

    def test_complete_program_generation(self):
        """Test complete program that would compile and run."""
        python_code = """
class MathUtils:
    def __init__(self, base: int):
        self.base: int = base
        self.operations_count: int = 0

    def process_numbers(self, numbers: list) -> dict:
        # Filter numbers above base
        filtered = [x for x in numbers if x > self.base]

        # Square each filtered number
        squared = [x * x for x in filtered]

        # Count operations
        self.operations_count += len(squared)

        # Create result dictionary
        result = {
            "count": len(squared),
            "sum": sum(squared) if squared else 0,
            "max": max(squared) if squared else 0
        }

        return result

def main() -> int:
    utils = MathUtils(5)
    test_data = [1, 3, 5, 7, 9, 11, 13]
    result = utils.process_numbers(test_data)
    return result["sum"]
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify complete structure
        assert "data MathUtils = MathUtils" in haskell_code
        assert "newMathUtils :: Int -> MathUtils" in haskell_code
        assert "processNumbers :: MathUtils -> [a] -> Dict String a" in haskell_code or "processNumbers :: MathUtils" in haskell_code
        assert "main :: IO ()" in haskell_code

        # Verify all features work together
        assert "processNumbers :: MathUtils -> [a] -> Dict String a" in haskell_code or "processNumbers :: MathUtils" in haskell_code
        assert "newMathUtils 5" in haskell_code
        assert "processNumbers" in haskell_code

    def test_functional_programming_patterns(self):
        """Test Haskell-specific functional programming patterns."""
        python_code = """
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def map_example(numbers: list) -> list:
    return [x * 2 for x in numbers]

def filter_example(numbers: list) -> list:
    return [x for x in numbers if x > 0]

def reduce_example(numbers: list) -> int:
    total = 0
    for num in numbers:
        total += num
    return total
"""
        haskell_code = self.converter.convert_code(python_code)

        # Verify recursive function
        assert "fibonacci :: Int -> Int" in haskell_code
        assert "fibonacci (n - 1)" in haskell_code
        assert "fibonacci (n - 2)" in haskell_code

        # Verify list comprehensions (map-like)
        assert "listComprehension numbers" in haskell_code

        # Verify filtered comprehensions (filter-like)
        assert "listComprehensionWithFilter numbers" in haskell_code

        # Verify basic structure - early return pattern optimization
        assert "if (n <= 1) then n else" in haskell_code or "fibonacci n = if" in haskell_code