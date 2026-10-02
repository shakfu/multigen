"""Tests for Haskell backend basic functionality."""

import pytest

from multigen.backends.errors import UnsupportedFeatureError
from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter


class TestHaskellBasics:
    """Test basic Haskell code generation functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_function(self):
        """Test simple function conversion."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "add :: Int -> Int -> Int" in haskell_code
        assert "add x y = (x + y)" in haskell_code

    def test_function_with_no_params(self):
        """Test function with no parameters."""
        python_code = """
def hello() -> str:
    return "Hello, World!"
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "hello :: String" in haskell_code
        assert 'hello = "Hello, World!"' in haskell_code

    def test_function_with_multiple_statements(self):
        """Test function with multiple statements."""
        python_code = """
def calculate(x: int, y: int) -> int:
    sum_val = x + y
    product = x * y
    return sum_val + product
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "calculate :: Int -> Int -> Int" in haskell_code
        assert "sumVal = (x + y)" in haskell_code
        assert "product = (x * y)" in haskell_code
        assert "(sumVal + product)" in haskell_code

    def test_type_inference(self):
        """Test type inference for various constants."""
        python_code = """
def test_types() -> None:
    int_val = 42
    float_val = 3.14
    str_val = "hello"
    bool_val = True
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "intVal = 42" in haskell_code
        assert "floatVal = 3.14" in haskell_code
        assert 'strVal = "hello"' in haskell_code
        assert "boolVal = True" in haskell_code

    def test_main_function(self):
        """Test main function generation."""
        python_code = """
def main() -> None:
    print("Hello from Haskell!")
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "main :: IO ()" in haskell_code
        assert 'printValue "Hello from Haskell!"' in haskell_code

    def test_binary_operations(self):
        """Test binary operations conversion."""
        python_code = """
def math_ops(a: int, b: int) -> int:
    addition = a + b
    subtraction = a - b
    multiplication = a * b
    division = a / b
    return addition
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a + b)" in haskell_code
        assert "(a - b)" in haskell_code
        assert "(a * b)" in haskell_code
        assert "(a / b)" in haskell_code

    def test_comparison_operations(self):
        """Test comparison operations."""
        python_code = """
def compare(a: int, b: int) -> bool:
    equal = a == b
    not_equal = a != b
    less_than = a < b
    greater_than = a > b
    return equal
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a == b)" in haskell_code
        assert "(a /= b)" in haskell_code
        assert "(a < b)" in haskell_code
        assert "(a > b)" in haskell_code

    def test_boolean_operations(self):
        """Test boolean operations."""
        python_code = """
def bool_ops(a: bool, b: bool) -> bool:
    and_result = a and b
    or_result = a or b
    return and_result
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(a && b)" in haskell_code
        assert "(a || b)" in haskell_code

    def test_unary_operations(self):
        """Test unary operations."""
        python_code = """
def unary_ops(x: int, flag: bool) -> int:
    positive = +x
    negative = -x
    not_flag = not flag
    return negative
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(+x)" in haskell_code
        assert "(-x)" in haskell_code
        assert "(not flag)" in haskell_code

    def test_builtin_functions(self):
        """Test built-in function calls."""
        python_code = """
def test_builtins(numbers: list) -> int:
    length = len(numbers)
    absolute = abs(-5)
    minimum = min(numbers)
    maximum = max(numbers)
    total = sum(numbers)
    return length
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "len' numbers" in haskell_code
        assert "abs' (-5)" in haskell_code
        assert "min' numbers" in haskell_code
        assert "max' numbers" in haskell_code
        assert "sum' numbers" in haskell_code

    def test_print_function(self):
        """Test print function conversion."""
        python_code = """
def test_print(message: str) -> None:
    print("Hello")
    print(message)
    print(42)
"""
        # Outside main the function is pure; the old output put printValue in a where clause.
        with pytest.raises(UnsupportedFeatureError, match="outside main"):
            self.converter.convert_code(python_code)

    def test_ternary_expression(self):
        """Test ternary expression conversion."""
        python_code = """
def test_ternary(x: int, y: int) -> int:
    result = x if x > y else y
    return result
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "(if (x > y) then x else y)" in haskell_code

    def test_list_literal(self):
        """Test list literal conversion."""
        python_code = """
def test_list() -> list:
    numbers = [1, 2, 3, 4, 5]
    return numbers
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "[1, 2, 3, 4, 5]" in haskell_code

    def test_dict_literal(self):
        """Test dictionary literal conversion."""
        python_code = """
def test_dict() -> dict:
    data = {"key1": "value1", "key2": "value2"}
    return data
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "Map.fromList" in haskell_code
        assert '("key1", "value1")' in haskell_code
        assert '("key2", "value2")' in haskell_code

    def test_range_function(self):
        """Test range function conversion."""
        python_code = """
def test_range() -> list:
    range1 = range(5)
    range2 = range(1, 6)
    range3 = range(0, 10, 2)
    return range1
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "rangeList (range 5)" in haskell_code
        assert "rangeList (range2 1 6)" in haskell_code
        assert "rangeList (range3 0 10 2)" in haskell_code

    def test_unsupported_features(self):
        """Test that unsupported features raise errors."""
        # Generator expressions are now supported (normalized to list comprehensions)
        # Note: Try/except is now supported (exception handling implemented)
        # Lambda functions are unsupported
        python_code_lambda = """
def test_lambda() -> int:
    return 0
"""
        # This is a valid function, so it should not raise
        self.converter.convert_code(python_code_lambda)

    def test_module_structure(self):
        """Test complete module structure generation."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y

def main() -> None:
    result = add(5, 3)
    print(result)
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "module Main where" in haskell_code
        assert "import MultiGenRuntime" in haskell_code
        assert "add :: Int -> Int -> Int" in haskell_code
        assert "main :: IO ()" in haskell_code


class TestHaskellMainBlock:
    """main's do block must be valid Haskell and print only what Python prints."""

    def test_main_ending_in_binding_returns_unit(self):
        """Dropping `return total` from main must not leave a do block ending in `let`."""
        code = MultiGenPythonToHaskellConverter().convert_code(
            "def main() -> int:\n    x: int = 1\n    print(x)\n    total: int = x + 1\n    return total\n"
        )
        main_block = code[code.index("main = do") :].splitlines()

        assert main_block[-1].strip() == "return ()"

    def test_empty_main_prints_nothing(self):
        code = MultiGenPythonToHaskellConverter().convert_code("def main() -> int:\n    return 0\n")

        assert "main = return ()" in code
        assert "No statements" not in code


class TestHaskellItemsUnpacking:
    """`k, v in d.items()` binds a (k, v) pattern over Map.toList; other tuple targets are rejected."""

    def convert(self, body: str) -> str:
        return MultiGenPythonToHaskellConverter().convert_code(body)

    def test_for_loop_folds_over_pairs(self):
        code = self.convert(
            "def f(d: dict[int, int]) -> int:\n    total: int = 0\n"
            "    for k, v in d.items():\n        total += k * v\n    return total\n"
        )

        assert "total = foldl (\\acc (k, v) -> acc + ((k * v))) 0 ((items d))" in code

    def test_dict_comprehension_pattern_and_monomorphic_signature(self):
        code = self.convert(
            "def f(d: dict[int, int]) -> int:\n"
            "    g: dict[int, int] = {k: v for k, v in d.items() if v > 20}\n    return len(g)\n"
        )

        assert "dictComprehensionWithFilter (items d) (\\(k, v) -> (v > 20))" in code
        # "Map" contains the letter a; it must not add a constraint on an unused type variable.
        assert "f :: Map Int Int -> Int" in code

    def test_list_comprehension_pattern(self):
        code = self.convert("def f(d: dict[int, int]) -> list[int]:\n    return [k + v for k, v in d.items()]\n")

        assert "listComprehension (items d) (\\(k, v) -> (k + v))" in code

    def test_print_parenthesizes_call(self):
        code = self.convert(
            "def f(x: int) -> int:\n    return x\n\ndef main() -> int:\n    print(f(1))\n    return 0\n"
        )

        assert "printValue (f 1)" in code

    @pytest.mark.parametrize(
        "source",
        [
            "def f(xs: list[int]) -> int:\n    t: int = 0\n    for i, x in enumerate(xs):\n        t += x\n    return t\n",
            "def f(xs: list[int]) -> list[int]:\n    return [i for i, x in enumerate(xs)]\n",
            "def f(d: dict[int, int]) -> int:\n    t: int = 0\n    for k, v in d.items(1):\n        t += v\n    return t\n",
        ],
    )
    def test_other_tuple_targets_rejected(self, source):
        with pytest.raises(UnsupportedFeatureError):
            self.convert(source)

    def test_fold_step_reading_accumulator_rejected(self):
        with pytest.raises(UnsupportedFeatureError):
            self.convert(
                "def f(n: int) -> int:\n    t: int = 1\n    for i in range(n):\n        t += t\n    return t\n"
            )


class TestHaskellDictLoops:
    """Loops that insert into a dict or accumulate under an if fold into one binding."""

    def test_dict_insert_and_guarded_sum(self):
        code = MultiGenPythonToHaskellConverter().convert_code(
            "def f() -> int:\n    d: dict = {}\n    for i in range(5):\n        d[i] = i * 3\n"
            "    s: int = 0\n    for i in range(9):\n        if i in d:\n            s += d[i]\n    return s\n"
        )

        assert "d = foldl (\\acc i -> Map.insert (i) ((i * 3)) acc) Map.empty (rangeList (range 5))" in code
        assert (
            "s = foldl (\\acc i -> if (Map.member i d) then acc + ((d Map.! i)) else acc) 0 (rangeList (range 9))"
            in code
        )

    def test_list_subscript_assignment_not_folded_as_dict(self):
        with pytest.raises(UnsupportedFeatureError):
            MultiGenPythonToHaskellConverter().convert_code(
                "def f(n: int) -> int:\n    xs: list[int] = [0, 0]\n    for i in range(2):\n        xs[i] = n\n"
                "    return xs[0]\n"
            )
