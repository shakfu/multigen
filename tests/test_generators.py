"""Tests for generator/yield and yield from support (eager collection strategy).

Tests generator function conversion across all backends:
- Generators are converted to functions that return a collected list
- yield x becomes an append to an accumulator
- yield from expr extends the accumulator with all elements
- The function returns the accumulated list

Scope: yield and yield from. No .send(), .throw(), generator expressions.
"""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.frontend.subset_validator import StaticPythonSubsetValidator

# --- Test Python code snippets ---

SIMPLE_WHILE_GENERATOR = """
def count_up(n: int) -> int:
    i: int = 0
    while i < n:
        yield i
        i += 1
"""

FOR_LOOP_GENERATOR = """
def squares(n: int) -> int:
    for i in range(n):
        yield i * i
"""

CONDITIONAL_YIELD = """
def evens(n: int) -> int:
    for i in range(n):
        if i % 2 == 0:
            yield i
"""

MULTIPLE_YIELDS = """
def multi(n: int) -> int:
    yield n
    yield n + 1
    yield n + 2
"""


class TestGeneratorValidation:
    """Test validation of generator constraints."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_simple_yield_valid(self) -> None:
        """Simple yield should be valid (partially supported)."""
        result = self.validator.validate_code(SIMPLE_WHILE_GENERATOR)
        assert result.is_valid

    def test_yield_from_valid(self) -> None:
        """yield from with function call/range/variable should be valid."""
        code = """
def gen(n: int) -> int:
    yield from range(n)
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_yield_from_with_generator_call_valid(self) -> None:
        """yield from calling another generator should be valid."""
        code = """
def inner(n: int) -> int:
    for i in range(n):
        yield i

def outer(n: int) -> int:
    yield from inner(n)
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_generator_expression_rejected(self) -> None:
        """Generator expressions should be rejected."""
        code = """
def use_gen() -> int:
    total: int = sum(x for x in range(10))
    return total
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("Generator Expressions" in v for v in result.violations)

    def test_generator_with_type_annotation_valid(self) -> None:
        """Generator with type annotation should be valid."""
        result = self.validator.validate_code(FOR_LOOP_GENERATOR)
        assert result.is_valid

    def test_generator_without_annotation_invalid(self) -> None:
        """Generator without return annotation is invalid (missing type annotation rule)."""
        code = """
def gen(n: int):
    i: int = 0
    while i < n:
        yield i
        i += 1
"""
        result = self.validator.validate_code(code)
        # Invalid because of missing return type annotation (pre-existing rule)
        assert not result.is_valid


class TestCppGenerators:
    """Test C++ generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces vector with push_back."""
        cpp = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "std::vector<int>" in cpp
        assert "__mgen_result" in cpp
        assert "push_back" in cpp
        assert "return __mgen_result" in cpp

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator."""
        cpp = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "std::vector<int>" in cpp
        assert "push_back" in cpp

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        cpp = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "std::vector<int>" in cpp
        assert "push_back" in cpp

    def test_generator_return_type(self) -> None:
        """Test that return type is std::vector."""
        cpp = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "std::vector<int> count_up" in cpp

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points."""
        cpp = self.converter.convert_code(MULTIPLE_YIELDS)
        assert cpp.count("push_back") == 3


class TestCGenerators:
    """Test C generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces vec_int with push."""
        c = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "vec_int" in c
        assert "__mgen_result" in c
        assert "vec_int_push" in c
        assert "return __mgen_result" in c

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator."""
        c = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "vec_int" in c
        assert "vec_int_push" in c

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        c = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "vec_int" in c
        assert "vec_int_push" in c

    def test_generator_return_type(self) -> None:
        """Test that return type is vec_int."""
        c = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "vec_int count_up" in c

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points."""
        c = self.converter.convert_code(MULTIPLE_YIELDS)
        assert c.count("vec_int_push") == 3


class TestRustGenerators:
    """Test Rust generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces Vec with push."""
        rust = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "Vec<i32>" in rust
        assert "__mgen_result" in rust
        assert ".push(" in rust

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator."""
        rust = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "Vec<i32>" in rust
        assert ".push(" in rust

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        rust = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "Vec<i32>" in rust
        assert ".push(" in rust

    def test_generator_return_type(self) -> None:
        """Test that return type is Vec<i32>."""
        rust = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "-> Vec<i32>" in rust

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points."""
        rust = self.converter.convert_code(MULTIPLE_YIELDS)
        assert rust.count(".push(") == 3


class TestGoGenerators:
    """Test Go generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces slice with append."""
        go = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "[]int" in go
        assert "__mgen_result" in go
        assert "append(" in go
        assert "return __mgen_result" in go

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator."""
        go = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "[]int" in go
        assert "append(" in go

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        go = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "[]int" in go
        assert "append(" in go

    def test_generator_return_type(self) -> None:
        """Test that return type is []int."""
        go = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "[]int" in go

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points."""
        go = self.converter.convert_code(MULTIPLE_YIELDS)
        assert go.count("append(") >= 3


class TestHaskellGenerators:
    """Test Haskell generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces list with accumulation."""
        hs = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "[Int]" in hs
        assert "helper" in hs or "map" in hs or "++" in hs

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator produces list."""
        hs = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "[Int]" in hs
        assert "map" in hs or "[" in hs

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        hs = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "[Int]" in hs

    def test_generator_return_type(self) -> None:
        """Test that return type is [Int]."""
        hs = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert ":: Int -> [Int]" in hs

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points produce list."""
        hs = self.converter.convert_code(MULTIPLE_YIELDS)
        assert "[Int]" in hs


class TestOCamlGenerators:
    """Test OCaml generator conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_generator(self) -> None:
        """Test while-loop generator produces list with ref accumulation."""
        ocaml = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "__mgen_result" in ocaml
        assert "ref" in ocaml
        assert "List.rev" in ocaml

    def test_generator_with_for_loop(self) -> None:
        """Test for-loop based generator."""
        ocaml = self.converter.convert_code(FOR_LOOP_GENERATOR)
        assert "__mgen_result" in ocaml
        assert "ref" in ocaml

    def test_generator_with_conditional_yield(self) -> None:
        """Test yield inside if/else."""
        ocaml = self.converter.convert_code(CONDITIONAL_YIELD)
        assert "__mgen_result" in ocaml

    def test_generator_return_type(self) -> None:
        """Test that accumulator uses ref list pattern."""
        ocaml = self.converter.convert_code(SIMPLE_WHILE_GENERATOR)
        assert "ref []" in ocaml
        assert "List.rev" in ocaml

    def test_generator_multiple_yields(self) -> None:
        """Test multiple yield points."""
        ocaml = self.converter.convert_code(MULTIPLE_YIELDS)
        assert ocaml.count("__mgen_result :=") >= 3


class TestIRGenerators:
    """Test IR layer generator support."""

    def test_ir_yield_node(self) -> None:
        """Test IRYield node creation and is_generator flag."""
        from multigen.frontend.static_ir import build_ir_from_code

        code = """
def gen(n: int) -> int:
    i: int = 0
    while i < n:
        yield i
        i += 1
"""
        ir_module = build_ir_from_code(code)
        assert len(ir_module.functions) == 1
        func = ir_module.functions[0]
        assert func.is_generator is True

    def test_ir_non_generator(self) -> None:
        """Test that non-generator functions have is_generator=False."""
        from multigen.frontend.static_ir import build_ir_from_code

        code = """
def add(a: int, b: int) -> int:
    return a + b
"""
        ir_module = build_ir_from_code(code)
        assert len(ir_module.functions) == 1
        func = ir_module.functions[0]
        assert func.is_generator is False

    def test_ir_yield_in_body(self) -> None:
        """Test that IRYield appears in function body."""
        from multigen.frontend.static_ir import IRYield, build_ir_from_code

        code = """
def gen(n: int) -> int:
    yield n
"""
        ir_module = build_ir_from_code(code)
        func = ir_module.functions[0]
        yield_stmts = [s for s in func.body if isinstance(s, IRYield)]
        assert len(yield_stmts) == 1

    def test_ir_yield_from_node(self) -> None:
        """Test IRYieldFrom node creation and is_generator flag."""
        from multigen.frontend.static_ir import IRYieldFrom, build_ir_from_code

        code = """
def gen(n: int) -> int:
    yield from range(n)
"""
        ir_module = build_ir_from_code(code)
        func = ir_module.functions[0]
        assert func.is_generator is True
        yield_from_stmts = [s for s in func.body if isinstance(s, IRYieldFrom)]
        assert len(yield_from_stmts) == 1


# --- yield from test snippets ---

YIELD_FROM_RANGE = """
def gen(n: int) -> int:
    yield from range(n)
"""

YIELD_FROM_GENERATOR = """
def inner(n: int) -> int:
    for i in range(n):
        yield i

def outer(n: int) -> int:
    yield from inner(n)
"""

YIELD_FROM_MIXED = """
def gen(n: int) -> int:
    yield 0
    yield from range(n)
    yield 99
"""


class TestCppYieldFrom:
    """Test C++ yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces a counting loop."""
        cpp = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "std::vector<int>" in cpp
        assert "__mgen_result" in cpp
        assert "__mgen_yf" in cpp
        assert "push_back" in cpp

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call uses for-each loop."""
        cpp = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert "push_back" in cpp
        assert "__mgen_yf" in cpp

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from."""
        cpp = self.converter.convert_code(YIELD_FROM_MIXED)
        assert "push_back" in cpp
        assert "__mgen_yf" in cpp
        assert "__mgen_result" in cpp


class TestCYieldFrom:
    """Test C yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces a counting loop."""
        c = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "vec_int" in c
        assert "__mgen_result" in c
        assert "__mgen_yf" in c
        assert "vec_int_push" in c

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call loops over result."""
        c = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert "vec_int_push" in c

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from."""
        c = self.converter.convert_code(YIELD_FROM_MIXED)
        assert "vec_int_push" in c
        assert "__mgen_result" in c


class TestRustYieldFrom:
    """Test Rust yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces a range loop."""
        rust = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "Vec<i32>" in rust
        assert "__mgen_result" in rust
        assert "__mgen_yf" in rust
        assert ".push(" in rust

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call uses extend."""
        rust = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert ".extend(" in rust or ".push(" in rust

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from."""
        rust = self.converter.convert_code(YIELD_FROM_MIXED)
        assert ".push(" in rust
        assert "__mgen_result" in rust


class TestGoYieldFrom:
    """Test Go yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces a counting loop."""
        go = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "[]int" in go
        assert "__mgen_result" in go
        assert "__mgen_yf" in go
        assert "append(" in go

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call uses variadic append."""
        go = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert "append(" in go

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from."""
        go = self.converter.convert_code(YIELD_FROM_MIXED)
        assert "append(" in go
        assert "__mgen_result" in go


class TestHaskellYieldFrom:
    """Test Haskell yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces list."""
        hs = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "[Int]" in hs

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call."""
        hs = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert "[Int]" in hs

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from produces concatenation."""
        hs = self.converter.convert_code(YIELD_FROM_MIXED)
        assert "[Int]" in hs
        assert "++" in hs


class TestOCamlYieldFrom:
    """Test OCaml yield from conversion."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_yield_from_range(self) -> None:
        """Test yield from range produces loop."""
        ocaml = self.converter.convert_code(YIELD_FROM_RANGE)
        assert "__mgen_result" in ocaml
        assert "__mgen_yf" in ocaml

    def test_yield_from_function_call(self) -> None:
        """Test yield from function call uses List.iter."""
        ocaml = self.converter.convert_code(YIELD_FROM_GENERATOR)
        assert "__mgen_result" in ocaml

    def test_yield_from_mixed(self) -> None:
        """Test mixed yield and yield from."""
        ocaml = self.converter.convert_code(YIELD_FROM_MIXED)
        assert "__mgen_result" in ocaml
        assert "List.rev" in ocaml
