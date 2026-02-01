"""Tests for exception handling (try/except) support.

Tests cover:
- C++ backend: try/catch with std::exception subclasses
- OCaml backend: try/with pattern matching
- Validator: rejection of unsupported features (else, finally, exception chaining)
"""

import pytest

from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.frontend.subset_validator import StaticPythonSubsetValidator


class TestCppExceptionHandling:
    """Test C++ exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "try {" in cpp_code
        assert "catch (const multigen::ZeroDivisionError&)" in cpp_code
        assert "return 0;" in cpp_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "try {" in cpp_code
        assert "catch (const multigen::ValueError& e)" in cpp_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "catch (const multigen::ZeroDivisionError&)" in cpp_code
        assert "catch (const multigen::ValueError&)" in cpp_code

    def test_bare_except(self) -> None:
        """Test bare except clause (catch all)."""
        python_code = """
def catch_all() -> int:
    try:
        return 1 // 0
    except:
        return 0
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "catch (...)" in cpp_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        cpp_code = self.converter.convert_code(python_code)

        assert 'throw multigen::ValueError("negative value");' in cpp_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "throw multigen::RuntimeError();" in cpp_code

    def test_reraise(self) -> None:
        """Test re-raise (bare raise)."""
        python_code = """
def reraise_error() -> int:
    try:
        return 1 // 0
    except ZeroDivisionError:
        raise
"""
        cpp_code = self.converter.convert_code(python_code)

        # Should have a bare throw statement
        assert "throw;" in cpp_code

    def test_exception_type_mapping(self) -> None:
        """Test all exception type mappings."""
        python_code = """
def test_types() -> None:
    try:
        pass
    except ValueError:
        pass
    except TypeError:
        pass
    except KeyError:
        pass
    except IndexError:
        pass
    except RuntimeError:
        pass
"""
        cpp_code = self.converter.convert_code(python_code)

        assert "multigen::ValueError" in cpp_code
        assert "multigen::TypeError" in cpp_code
        assert "multigen::KeyError" in cpp_code
        assert "multigen::IndexError" in cpp_code
        assert "multigen::RuntimeError" in cpp_code


class TestOCamlExceptionHandling:
    """Test OCaml exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "try" in ocaml_code
        assert "with" in ocaml_code
        assert "Zero_division_error" in ocaml_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "try" in ocaml_code
        assert "with" in ocaml_code
        assert "Value_error e" in ocaml_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "| Zero_division_error" in ocaml_code
        assert "| Value_error" in ocaml_code

    def test_bare_except(self) -> None:
        """Test bare except clause (catch all)."""
        python_code = """
def catch_all() -> int:
    try:
        return 1 // 0
    except:
        return 0
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "| _ ->" in ocaml_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert 'raise (Value_error "negative value")' in ocaml_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "raise (Runtime_error" in ocaml_code

    def test_exception_type_mapping(self) -> None:
        """Test all exception type mappings."""
        python_code = """
def test_types() -> None:
    try:
        pass
    except ValueError:
        pass
    except TypeError:
        pass
    except KeyError:
        pass
    except IndexError:
        pass
    except RuntimeError:
        pass
"""
        ocaml_code = self.converter.convert_code(python_code)

        assert "Value_error" in ocaml_code
        assert "Type_error" in ocaml_code
        assert "Key_error" in ocaml_code
        assert "Index_error" in ocaml_code
        assert "Runtime_error" in ocaml_code


class TestExceptionValidation:
    """Test exception handling validation in subset validator."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.validator = StaticPythonSubsetValidator()

    def test_basic_try_except_valid(self) -> None:
        """Test that basic try/except is valid."""
        code = """
def safe_divide(a: int, b: int) -> int:
    try:
        return a // b
    except ZeroDivisionError:
        return 0
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_try_except_with_variable_valid(self) -> None:
        """Test that try/except with 'as e' is valid."""
        code = """
def handle_error() -> int:
    try:
        return 1 // 0
    except ValueError as e:
        return -1
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_multiple_handlers_valid(self) -> None:
        """Test that multiple exception handlers are valid."""
        code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_else_clause_rejected(self) -> None:
        """Test that try/except/else is rejected."""
        code = """
def with_else() -> int:
    try:
        x: int = 1
    except ValueError:
        return -1
    else:
        return 0
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("else" in v.lower() for v in result.violations)

    def test_finally_clause_rejected(self) -> None:
        """Test that try/except/finally is rejected."""
        code = """
def with_finally() -> int:
    try:
        x: int = 1
    except ValueError:
        return -1
    finally:
        print(0)
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("finally" in v.lower() for v in result.violations)

    def test_exception_chaining_rejected(self) -> None:
        """Test that exception chaining (raise ... from ...) is rejected."""
        code = """
def chained_exception() -> int:
    try:
        return 1 // 0
    except ZeroDivisionError as e:
        raise ValueError("wrapped") from e
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("chaining" in v.lower() for v in result.violations)

    def test_raise_statement_valid(self) -> None:
        """Test that basic raise statements are valid."""
        code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative")
    return x
"""
        result = self.validator.validate_code(code)
        assert result.is_valid


class TestNestedTryBlocks:
    """Test nested try/except blocks."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        self.cpp_converter = MultiGenPythonToCppConverter()
        self.ocaml_converter = MultiGenPythonToOCamlConverter()

    def test_nested_try_cpp(self) -> None:
        """Test nested try blocks in C++."""
        python_code = """
def nested_try(x: int) -> int:
    try:
        try:
            return x // 0
        except ZeroDivisionError:
            raise ValueError("wrapped")
    except ValueError:
        return -1
"""
        cpp_code = self.cpp_converter.convert_code(python_code)

        # Should have two try blocks
        assert cpp_code.count("try {") == 2
        assert "multigen::ZeroDivisionError" in cpp_code
        assert "multigen::ValueError" in cpp_code

    def test_nested_try_ocaml(self) -> None:
        """Test nested try blocks in OCaml."""
        python_code = """
def nested_try(x: int) -> int:
    try:
        try:
            return x // 0
        except ZeroDivisionError:
            raise ValueError("wrapped")
    except ValueError:
        return -1
"""
        ocaml_code = self.ocaml_converter.convert_code(python_code)

        # Should have try/with pattern matching
        assert "try" in ocaml_code
        assert "with" in ocaml_code
        assert "Zero_division_error" in ocaml_code
        assert "Value_error" in ocaml_code


class TestRustExceptionHandling:
    """Test Rust exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        from multigen.backends.rust.converter import MultiGenPythonToRustConverter

        self.converter = MultiGenPythonToRustConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        rust_code = self.converter.convert_code(python_code)

        assert "std::panic::catch_unwind" in rust_code
        assert "ZeroDivisionError" in rust_code
        assert "return 0;" in rust_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        rust_code = self.converter.convert_code(python_code)

        assert "std::panic::catch_unwind" in rust_code
        assert "ValueError" in rust_code
        # Should extract the exception variable
        assert "let e = e.downcast_ref" in rust_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        rust_code = self.converter.convert_code(python_code)

        assert "ZeroDivisionError" in rust_code
        assert "ValueError" in rust_code

    def test_bare_except(self) -> None:
        """Test bare except clause (catch all)."""
        python_code = """
def catch_all() -> int:
    try:
        return 1 // 0
    except:
        return 0
"""
        rust_code = self.converter.convert_code(python_code)

        # Should catch all exceptions
        assert "Catch all exceptions" in rust_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        rust_code = self.converter.convert_code(python_code)

        assert 'panic!(ValueError::new("negative value".to_string()));' in rust_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        rust_code = self.converter.convert_code(python_code)

        assert "panic!(RuntimeError::new" in rust_code

    def test_reraise(self) -> None:
        """Test re-raise (bare raise)."""
        python_code = """
def reraise_error() -> int:
    try:
        return 1 // 0
    except ZeroDivisionError:
        raise
"""
        rust_code = self.converter.convert_code(python_code)

        # Should have a bare panic! for re-raise
        assert "panic!();" in rust_code


class TestGoExceptionHandling:
    """Test Go exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        from multigen.backends.go.converter import MultiGenPythonToGoConverter

        self.converter = MultiGenPythonToGoConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        go_code = self.converter.convert_code(python_code)

        assert "defer func()" in go_code
        assert "recover()" in go_code
        assert "ZeroDivisionError" in go_code
        assert "return 0" in go_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        go_code = self.converter.convert_code(python_code)

        assert "defer func()" in go_code
        assert "recover()" in go_code
        assert "ValueError" in go_code
        # Should extract the exception variable
        assert "e := r.(multigen.ValueError)" in go_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        go_code = self.converter.convert_code(python_code)

        assert "ZeroDivisionError" in go_code
        assert "ValueError" in go_code

    def test_bare_except(self) -> None:
        """Test bare except clause (catch all)."""
        python_code = """
def catch_all() -> int:
    try:
        return 1 // 0
    except:
        return 0
"""
        go_code = self.converter.convert_code(python_code)

        # Should catch all exceptions
        assert "Catch all exceptions" in go_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        go_code = self.converter.convert_code(python_code)

        assert 'panic(multigen.NewValueError("negative value"))' in go_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        go_code = self.converter.convert_code(python_code)

        assert "panic(multigen.NewRuntimeError" in go_code


class TestHaskellExceptionHandling:
    """Test Haskell exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter

        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "catch" in haskell_code
        assert "ZeroDivisionError" in haskell_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "catch" in haskell_code
        assert "ValueError" in haskell_code
        # Should capture the exception variable
        assert "ValueError e" in haskell_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        haskell_code = self.converter.convert_code(python_code)

        assert "ZeroDivisionError" in haskell_code
        assert "ValueError" in haskell_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        haskell_code = self.converter.convert_code(python_code)

        assert 'throw (ValueError "negative value")' in haskell_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        haskell_code = self.converter.convert_code(python_code)

        assert 'throw (RuntimeError ""' in haskell_code


class TestCExceptionHandling:
    """Test C exception handling conversion."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        from multigen.backends.c.converter import MultiGenPythonToCConverter

        self.converter = MultiGenPythonToCConverter()

    def test_simple_try_except(self) -> None:
        """Test basic try/except conversion."""
        python_code = """
def safe_divide(a: int, b: int) -> int:
    try:
        result: int = a // b
        return result
    except ZeroDivisionError:
        return 0
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_TRY" in c_code
        assert "MGEN_CATCH" in c_code
        assert "MGEN_END_TRY" in c_code

    def test_try_except_with_variable(self) -> None:
        """Test try/except with exception variable capture."""
        python_code = """
def handle_error() -> str:
    try:
        x: int = 1 // 0
        return "ok"
    except ValueError as e:
        return "error"
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_TRY" in c_code
        assert "MGEN_CATCH" in c_code
        # Should capture exception message
        assert "mgen_current_exception_message()" in c_code

    def test_multiple_except_handlers(self) -> None:
        """Test try with multiple exception handlers."""
        python_code = """
def multi_handler(x: int) -> int:
    try:
        return x // 0
    except ZeroDivisionError:
        return -1
    except ValueError:
        return -2
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_CATCH" in c_code
        # Should have multiple catch blocks
        assert c_code.count("MGEN_CATCH") >= 2

    def test_bare_except(self) -> None:
        """Test bare except clause (catch all)."""
        python_code = """
def catch_all() -> int:
    try:
        return 1 // 0
    except:
        return 0
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_CATCH_ALL" in c_code

    def test_raise_exception(self) -> None:
        """Test raise statement conversion."""
        python_code = """
def validate(x: int) -> int:
    if x < 0:
        raise ValueError("negative value")
    return x
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_RAISE" in c_code
        assert "MGEN_ERROR_VALUE" in c_code

    def test_raise_without_message(self) -> None:
        """Test raise statement without message."""
        python_code = """
def always_fail() -> int:
    raise RuntimeError()
"""
        c_code = self.converter.convert_code(python_code)

        assert "MGEN_RAISE(MGEN_ERROR_RUNTIME" in c_code
