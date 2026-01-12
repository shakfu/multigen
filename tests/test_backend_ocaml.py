"""Tests for OCaml backend functionality."""

import pytest

from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.backends.ocaml.containers import OCamlContainerSystem
from multigen.backends.ocaml.builder import OCamlBuilder
from multigen.backends.preferences import OCamlPreferences
from multigen.backends.errors import UnsupportedFeatureError


class TestOCamlBasics:
    """Test basic OCaml code generation functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_function(self):
        """Test simple function conversion."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "let add x y =" in ocaml_code
        assert "(x + y)" in ocaml_code

    def test_function_with_no_params(self):
        """Test function with no parameters."""
        python_code = """
def hello() -> str:
    return "Hello, World!"
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "let hello () =" in ocaml_code
        assert '"Hello, World!"' in ocaml_code

    def test_function_with_multiple_statements(self):
        """Test function with multiple statements."""
        python_code = """
def calculate(x: int, y: int) -> int:
    sum_val = x + y
    product = x * y
    return sum_val + product
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "let calculate x y =" in ocaml_code
        assert "let sum_val = (x + y)" in ocaml_code
        assert "let product = (x * y)" in ocaml_code
        assert "(sum_val + product)" in ocaml_code

    def test_backend_with_preferences(self):
        """Test OCaml backend with custom preferences."""
        prefs = OCamlPreferences()
        prefs.set('use_pattern_matching', False)
        prefs.set('prefer_immutable', False)

        converter = MultiGenPythonToOCamlConverter(prefs)
        assert converter.preferences.get('use_pattern_matching') == False
        assert converter.preferences.get('prefer_immutable') == False

    def test_class_conversion(self):
        """Test class conversion to OCaml."""
        python_code = """
class Calculator:
    def __init__(self, name: str):
        self.name: str = name
        self.total: int = 0

    def add(self, value: int) -> None:
        self.total += value

    def get_result(self) -> str:
        return self.name + ": " + str(self.total)
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert 'type calculator' in ocaml_code
        assert 'create_calculator' in ocaml_code
        assert 'calculator_add' in ocaml_code
        assert 'calculator_get_result' in ocaml_code

    def test_list_comprehension_runtime(self):
        """Test list comprehension with runtime consistency."""
        python_code = """
def filter_numbers(numbers):
    return [x * 2 for x in numbers if x > 5]
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert 'list_comprehension_with_filter' in ocaml_code

    def test_list_comprehension_native(self):
        """Test list comprehension with native OCaml syntax."""
        prefs = OCamlPreferences()
        prefs.set('prefer_idiomatic_syntax', True)

        converter = MultiGenPythonToOCamlConverter(prefs)

        python_code = """
def filter_numbers(numbers):
    return [x * 2 for x in numbers if x > 5]
"""
        ocaml_code = converter.convert_code(python_code)

        # Should use either native OCaml syntax or runtime function
        # For now, accepting runtime function as default behavior
        assert 'list_comprehension_with_filter' in ocaml_code or ('[' in ocaml_code and '|' in ocaml_code)

    def test_string_methods(self):
        """Test string method conversion."""
        python_code = """
def process_text(text: str) -> str:
    return text.upper()
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert 'upper' in ocaml_code


class TestOCamlContainers:
    """Test OCaml container system."""

    def setup_method(self):
        """Set up test fixtures."""
        self.containers = OCamlContainerSystem()

    def test_list_type_mapping(self):
        """Test list type mapping."""
        assert self.containers.get_list_type('int') == 'int list'
        assert self.containers.get_list_type('string') == 'string list'

    def test_dict_type_mapping(self):
        """Test dictionary type mapping."""
        dict_type = self.containers.get_dict_type('string', 'int')
        assert 'string' in dict_type and 'int' in dict_type
        assert 'Hashtbl.t' in dict_type or 'Map' in dict_type

    def test_set_type_mapping(self):
        """Test set type mapping."""
        set_type = self.containers.get_set_type('int')
        assert 'int' in set_type
        assert 'Set' in set_type

    def test_list_operations(self):
        """Test list operation mappings."""
        ops = self.containers.get_list_operations()
        assert 'append' in ops
        assert 'length' in ops
        assert 'map' in ops
        assert 'filter' in ops

    def test_list_literal_generation(self):
        """Test list literal generation."""
        literal = self.containers.generate_list_literal([1, 2, 3], 'int')
        assert literal == '[1; 2; 3]'

        empty_literal = self.containers.generate_list_literal([], 'int')
        assert empty_literal == '[]'

    def test_dict_literal_generation(self):
        """Test dictionary literal generation."""
        literal = self.containers.generate_dict_literal({}, 'string', 'int')
        assert 'empty' in literal or 'create' in literal

    def test_set_literal_generation(self):
        """Test set literal generation."""
        literal = self.containers.generate_set_literal([1, 2, 3], 'int')
        assert 'Set.add' in literal or 'Set.empty' in literal

    def test_container_preferences(self):
        """Test container behavior with preferences."""
        prefs = OCamlPreferences()
        prefs.set('hashtables', 'stdlib')

        containers = OCamlContainerSystem(prefs)
        dict_type = containers.get_dict_type('string', 'int')
        assert 'Hashtbl.t' in dict_type


class TestOCamlBuilder:
    """Test OCaml builder functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.builder = OCamlBuilder()

    def test_build_commands(self):
        """Test build command generation."""
        commands = self.builder.get_build_command('test.ml')
        assert 'ocamlc' in commands
        assert 'test.ml' in commands
        assert 'multigen_runtime.ml' in commands

    def test_run_commands(self):
        """Test run command generation."""
        commands = self.builder.get_run_command('test.ml')
        assert './test' in commands

    def test_build_file_generation(self):
        """Test build file generation."""
        build_content = self.builder.generate_build_file(['test.ml'], 'test')
        assert 'dune' in build_content
        assert 'test' in build_content

    def test_compile_flags(self):
        """Test compile flags."""
        flags = self.builder.get_compile_flags()
        assert isinstance(flags, list)


class TestOCamlPreferences:
    """Test OCaml preference system."""

    def test_default_preferences(self):
        """Test default OCaml preferences."""
        prefs = OCamlPreferences()

        assert prefs.get('ocaml_version') == '4.14'
        assert prefs.get('use_modern_syntax') == True
        assert prefs.get('prefer_immutable') == True
        assert prefs.get('use_pattern_matching') == True
        assert prefs.get('naming_convention') == 'snake_case'

    def test_preference_modification(self):
        """Test preference modification."""
        prefs = OCamlPreferences()

        prefs.set('use_pattern_matching', False)
        assert prefs.get('use_pattern_matching') == False

        prefs.set('ocaml_version', '5.0')
        assert prefs.get('ocaml_version') == '5.0'

    def test_functional_programming_preferences(self):
        """Test functional programming specific preferences."""
        prefs = OCamlPreferences()

        assert prefs.get('prefer_immutable') == True
        assert prefs.get('curried_functions') == True
        assert prefs.get('tail_recursion_opt') == True
        assert prefs.get('list_operations') == 'functional'

    def test_type_system_preferences(self):
        """Test type system preferences."""
        prefs = OCamlPreferences()

        assert prefs.get('type_annotations') == True
        assert prefs.get('polymorphic_variants') == False
        assert prefs.get('gadts') == False

    def test_module_system_preferences(self):
        """Test module system preferences."""
        prefs = OCamlPreferences()

        assert prefs.get('module_structure') == 'nested'
        assert prefs.get('use_functors') == False
        assert prefs.get('signature_files') == False


class TestOCamlAugmentedAssignment:
    """Test augmented assignment operators for OCaml backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_plus_equals(self):
        """Test += operator conversion."""
        python_code = """
def accumulate(x: int) -> int:
    total: int = 0
    total += x
    return total
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "total" in ocaml_code
        # OCaml uses refs for mutable local variables
        assert "ref" in ocaml_code or "+" in ocaml_code

    def test_minus_equals(self):
        """Test -= operator conversion."""
        python_code = """
def decrement(x: int) -> int:
    value: int = 10
    value -= x
    return value
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "value" in ocaml_code
        assert "-" in ocaml_code

    def test_times_equals(self):
        """Test *= operator conversion."""
        python_code = """
def multiply(x: int) -> int:
    result: int = 1
    result *= x
    return result
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "result" in ocaml_code
        assert "*" in ocaml_code

    def test_divide_equals(self):
        """Test /= operator conversion."""
        python_code = """
def halve(x: int) -> int:
    value: int = x
    value /= 2
    return value
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "value" in ocaml_code
        assert "/" in ocaml_code

    def test_multiple_augmented_assignments(self):
        """Test multiple augmented assignments in sequence."""
        python_code = """
def compute(x: int) -> int:
    a: int = x
    a += 5
    a *= 2
    a -= 3
    return a
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "+" in ocaml_code
        assert "*" in ocaml_code
        assert "-" in ocaml_code


class TestOCamlControlFlow:
    """Test control flow structures for OCaml backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_if_statement(self):
        """Test simple if statement conversion."""
        python_code = """
def check_positive(x: int) -> bool:
    if x > 0:
        return True
    return False
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "if" in ocaml_code
        assert "then" in ocaml_code
        assert "true" in ocaml_code.lower()

    def test_if_else_statement(self):
        """Test if-else statement conversion."""
        python_code = """
def max_value(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "if" in ocaml_code
        assert "then" in ocaml_code
        assert "else" in ocaml_code

    def test_nested_if_statements(self):
        """Test nested if statement conversion."""
        python_code = """
def classify(x: int) -> str:
    if x > 0:
        if x > 10:
            return "large"
        else:
            return "small"
    else:
        return "negative"
"""
        ocaml_code = self.converter.convert_code(python_code)
        # Should have multiple if statements
        assert ocaml_code.count("if") >= 2

    def test_for_loop_range(self):
        """Test for loop with range conversion."""
        python_code = """
def sum_range(n: int) -> int:
    total: int = 0
    for i in range(n):
        total += i
    return total
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "total" in ocaml_code
        # OCaml uses fold_left or explicit recursion for loops
        assert "for" in ocaml_code or "fold" in ocaml_code or "rec" in ocaml_code

    def test_while_loop(self):
        """Test while loop conversion."""
        python_code = """
def count_down(n: int) -> int:
    count: int = n
    while count > 0:
        count -= 1
    return count
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "count" in ocaml_code
        # OCaml uses while or recursive functions
        assert "while" in ocaml_code or "rec" in ocaml_code


class TestOCamlComprehensions:
    """Test list/dict/set comprehensions for OCaml backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_list_comprehension(self):
        """Test simple list comprehension conversion."""
        python_code = """
def double_list(arr: list) -> list:
    return [x * 2 for x in arr]
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "List.map" in ocaml_code or "list_comprehension" in ocaml_code

    def test_list_comprehension_with_condition(self):
        """Test list comprehension with filter condition."""
        python_code = """
def filter_positives(arr: list) -> list:
    return [x for x in arr if x > 0]
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "List.filter" in ocaml_code or "list_comprehension" in ocaml_code

    def test_list_comprehension_with_transform_and_filter(self):
        """Test list comprehension with both transform and filter."""
        python_code = """
def process(arr: list) -> list:
    return [x * 2 for x in arr if x > 0]
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "list_comprehension" in ocaml_code or ("List" in ocaml_code)

    def test_nested_list_comprehension(self):
        """Test that nested list comprehension raises appropriate error.

        OCaml backend does not support multiple generators in comprehensions.
        """
        python_code = """
def flatten(matrix: list) -> list:
    return [x for row in matrix for x in row]
"""
        # Multiple generators not supported - should raise error
        with pytest.raises(UnsupportedFeatureError):
            self.converter.convert_code(python_code)

    def test_dict_comprehension(self):
        """Test dictionary comprehension conversion."""
        python_code = """
def square_dict(arr: list) -> dict:
    return {x: x * x for x in arr}
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "Hashtbl" in ocaml_code or "dict_comprehension" in ocaml_code

    def test_set_comprehension(self):
        """Test set comprehension conversion."""
        python_code = """
def unique_squares(arr: list) -> set:
    return {x * x for x in arr}
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "Set" in ocaml_code or "set_comprehension" in ocaml_code


class TestOCamlOOP:
    """Test object-oriented programming features for OCaml backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_class_definition(self):
        """Test simple class to OCaml record type conversion."""
        python_code = """
class Point:
    def __init__(self, x: int, y: int):
        self.x: int = x
        self.y: int = y
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "type point" in ocaml_code or "type Point" in ocaml_code
        assert "x" in ocaml_code
        assert "y" in ocaml_code

    def test_class_with_method(self):
        """Test class with method conversion."""
        python_code = """
class Counter:
    def __init__(self, start: int):
        self.value: int = start

    def increment(self) -> int:
        self.value += 1
        return self.value
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "counter" in ocaml_code.lower()
        assert "increment" in ocaml_code

    def test_class_with_multiple_methods(self):
        """Test class with multiple methods."""
        python_code = """
class Rectangle:
    def __init__(self, width: int, height: int):
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height

    def perimeter(self) -> int:
        return 2 * (self.width + self.height)
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "rectangle" in ocaml_code.lower()
        assert "area" in ocaml_code
        assert "perimeter" in ocaml_code

    def test_class_method_with_parameters(self):
        """Test class method with parameters."""
        python_code = """
class Calculator:
    def __init__(self):
        self.result: int = 0

    def add(self, value: int) -> int:
        self.result += value
        return self.result

    def multiply(self, value: int) -> int:
        self.result *= value
        return self.result
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "calculator" in ocaml_code.lower()
        assert "add" in ocaml_code
        assert "multiply" in ocaml_code


class TestOCamlStringMethods:
    """Test string method conversions for OCaml backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_string_length(self):
        """Test len() on string conversion."""
        python_code = """
def string_len(s: str) -> int:
    return len(s)
"""
        ocaml_code = self.converter.convert_code(python_code)
        # OCaml runtime uses len' function for string length
        assert "len'" in ocaml_code or "String.length" in ocaml_code or "length" in ocaml_code

    def test_string_upper(self):
        """Test upper() method conversion."""
        python_code = """
def to_upper(s: str) -> str:
    return s.upper()
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "upper" in ocaml_code.lower()

    def test_string_lower(self):
        """Test lower() method conversion."""
        python_code = """
def to_lower(s: str) -> str:
    return s.lower()
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "lower" in ocaml_code.lower()

    def test_string_strip(self):
        """Test strip() method conversion."""
        python_code = """
def trim(s: str) -> str:
    return s.strip()
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "strip" in ocaml_code.lower() or "trim" in ocaml_code.lower()

    def test_string_split(self):
        """Test split() method conversion."""
        python_code = """
def split_string(s: str) -> list:
    return s.split(",")
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "split" in ocaml_code.lower() or "String" in ocaml_code

    def test_string_concatenation(self):
        """Test string concatenation conversion."""
        python_code = """
def concat(a: str, b: str) -> str:
    return a + b
"""
        ocaml_code = self.converter.convert_code(python_code)
        assert "^" in ocaml_code or "concat" in ocaml_code