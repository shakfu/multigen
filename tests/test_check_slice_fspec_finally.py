"""Tests for new features: mgen check, slicing, f-string format specs, finally clause.

Covers:
1. mgen check CLI command
2. List slicing across backends (C++, Go, Rust, OCaml + existing C, Haskell)
3. F-string format specifications across backends
4. try/except/finally clause across backends
"""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.cli.main import MultiGenCLI
from multigen.frontend.subset_validator import StaticPythonSubsetValidator

# =============================================================================
# Test Python code snippets
# =============================================================================

SLICE_BASIC = """
def slice_list(nums: list[int]) -> list[int]:
    subset: list[int] = nums[1:3]
    return subset
"""

SLICE_FROM_START = """
def slice_from_start(nums: list[int]) -> list[int]:
    first_two: list[int] = nums[:2]
    return first_two
"""

SLICE_TO_END = """
def slice_to_end(nums: list[int]) -> list[int]:
    rest: list[int] = nums[1:]
    return rest
"""

FSTRING_FORMAT_FLOAT = """
def format_float(x: float) -> str:
    result: str = f"{x:.2f}"
    return result
"""

FSTRING_FORMAT_HEX = """
def format_hex(n: int) -> str:
    result: str = f"{n:x}"
    return result
"""

FSTRING_FORMAT_INT = """
def format_int(n: int) -> str:
    result: str = f"value: {n:d}"
    return result
"""

FSTRING_FORMAT_MIXED = """
def format_mixed(name: str, score: float) -> str:
    result: str = f"Player {name} scored {score:.1f}"
    return result
"""

FINALLY_BASIC = """
def with_finally() -> int:
    x: int = 0
    try:
        x = 10
    except ValueError:
        x = -1
    finally:
        x = x + 1
    return x
"""

FINALLY_WITH_CLEANUP = """
def cleanup_example() -> int:
    result: int = 0
    try:
        result = 42
    except RuntimeError:
        result = -1
    finally:
        print(result)
    return result
"""

STRING_SLICE_BASIC = """
def substr(s: str) -> str:
    result: str = s[1:3]
    return result
"""

STRING_SLICE_FROM_START = """
def substr_start(s: str) -> str:
    result: str = s[:3]
    return result
"""

STRING_SLICE_TO_END = """
def substr_end(s: str) -> str:
    result: str = s[2:]
    return result
"""

ELSE_BASIC = """
def with_else() -> int:
    x: int = 0
    try:
        x = 10
    except ValueError:
        x = -1
    else:
        x = x + 100
    return x
"""

ELSE_WITH_FINALLY = """
def else_and_finally() -> int:
    x: int = 0
    try:
        x = 5
    except ValueError:
        x = -1
    else:
        x = x + 10
    finally:
        x = x + 1
    return x
"""


# =============================================================================
# 1. mgen check CLI tests
# =============================================================================


class TestCheckCommand:
    """Test the mgen check CLI command."""

    def setup_method(self) -> None:
        self.cli = MultiGenCLI()
        self.cli.verbose = False

    def test_check_command_in_parser(self) -> None:
        """Check command should be registered in the argument parser."""
        parser = self.cli.create_parser()
        # Parse a check command to verify it's registered
        args = parser.parse_args(["check", "test.py"])
        assert args.command == "check"
        assert args.input_files == ["test.py"]

    def test_check_command_report_flag(self) -> None:
        """Check command should accept --report flag."""
        parser = self.cli.create_parser()
        args = parser.parse_args(["check", "--report", "test.py"])
        assert args.report is True

    def test_check_valid_file(self, tmp_path: object) -> None:
        """Check command should return 0 for valid Python file."""
        import pathlib

        assert isinstance(tmp_path, pathlib.Path)
        test_file = tmp_path / "valid.py"
        test_file.write_text("def add(x: int, y: int) -> int:\n    return x + y\n")
        result = self.cli.run(["check", str(test_file)])
        assert result == 0

    def test_check_invalid_file(self, tmp_path: object) -> None:
        """Check command should return 1 for invalid Python file."""
        import pathlib

        assert isinstance(tmp_path, pathlib.Path)
        test_file = tmp_path / "invalid.py"
        test_file.write_text("f = lambda x: x * 2\n")
        result = self.cli.run(["check", str(test_file)])
        assert result == 1

    def test_check_missing_file(self) -> None:
        """Check command should return 1 for missing file."""
        result = self.cli.run(["check", "nonexistent_file_xyz.py"])
        assert result == 1

    def test_check_multiple_files(self, tmp_path: object) -> None:
        """Check command should handle multiple files."""
        import pathlib

        assert isinstance(tmp_path, pathlib.Path)
        f1 = tmp_path / "a.py"
        f1.write_text("def f(x: int) -> int:\n    return x\n")
        f2 = tmp_path / "b.py"
        f2.write_text("def g(y: int) -> int:\n    return y + 1\n")
        result = self.cli.run(["check", str(f1), str(f2)])
        assert result == 0


# =============================================================================
# 2. Slicing tests
# =============================================================================


class TestSliceValidation:
    """Test that slicing passes validation."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_basic_slice_valid(self) -> None:
        """Basic slice should pass validation."""
        result = self.validator.validate_code(SLICE_BASIC)
        assert result.is_valid

    def test_slice_from_start_valid(self) -> None:
        """Slice from start should pass validation."""
        result = self.validator.validate_code(SLICE_FROM_START)
        assert result.is_valid

    def test_slice_to_end_valid(self) -> None:
        """Slice to end should pass validation."""
        result = self.validator.validate_code(SLICE_TO_END)
        assert result.is_valid


class TestSliceCpp:
    """Test C++ backend slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_basic_slice(self) -> None:
        """C++ should generate vector iterator constructor for slice."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert ".begin()" in result
        assert ".end()" not in result or ".begin() + " in result

    def test_slice_to_end(self) -> None:
        """C++ should use .end() for open-ended slice."""
        result = self.converter.convert_code(SLICE_TO_END)
        assert ".end()" in result

    def test_slice_from_start(self) -> None:
        """C++ should use .begin() for start-omitted slice."""
        result = self.converter.convert_code(SLICE_FROM_START)
        assert ".begin()" in result


class TestSliceGo:
    """Test Go backend slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_basic_slice(self) -> None:
        """Go should use native slice syntax."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert "[1:3]" in result

    def test_slice_to_end(self) -> None:
        """Go should use [1:] syntax."""
        result = self.converter.convert_code(SLICE_TO_END)
        assert "[1:]" in result

    def test_slice_from_start(self) -> None:
        """Go should use [:2] syntax."""
        result = self.converter.convert_code(SLICE_FROM_START)
        assert "[:2]" in result


class TestSliceRust:
    """Test Rust backend slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_basic_slice(self) -> None:
        """Rust should use range syntax with .to_vec()."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert "[1..3]" in result
        assert ".to_vec()" in result

    def test_slice_to_end(self) -> None:
        """Rust should use [1..] syntax."""
        result = self.converter.convert_code(SLICE_TO_END)
        assert "[1..]" in result
        assert ".to_vec()" in result

    def test_slice_from_start(self) -> None:
        """Rust should use [..2] syntax."""
        result = self.converter.convert_code(SLICE_FROM_START)
        assert "[..2]" in result
        assert ".to_vec()" in result


class TestSliceOCaml:
    """Test OCaml backend slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_basic_slice(self) -> None:
        """OCaml should use Array.sub."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert "Array.sub" in result

    def test_slice_to_end(self) -> None:
        """OCaml should use Array.length for open-ended slice."""
        result = self.converter.convert_code(SLICE_TO_END)
        assert "Array.sub" in result
        assert "Array.length" in result


class TestSliceC:
    """Test C backend slicing (already implemented, regression test)."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_basic_slice(self) -> None:
        """C should use loop-based slicing."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert "slice_result" in result or "_i" in result


class TestSliceHaskell:
    """Test Haskell backend slicing (already implemented, regression test)."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_basic_slice(self) -> None:
        """Haskell should use drop/take."""
        result = self.converter.convert_code(SLICE_BASIC)
        assert "take" in result or "drop" in result

    def test_slice_to_end(self) -> None:
        """Haskell should use drop for [1:]."""
        result = self.converter.convert_code(SLICE_TO_END)
        assert "drop" in result

    def test_slice_from_start(self) -> None:
        """Haskell should use take for [:2]."""
        result = self.converter.convert_code(SLICE_FROM_START)
        assert "take" in result


# =============================================================================
# 3. F-string format spec tests
# =============================================================================


class TestFStringFormatSpecValidation:
    """Test validation of f-string format specifications."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_float_precision_valid(self) -> None:
        """f-string with .2f format spec should be valid."""
        result = self.validator.validate_code(FSTRING_FORMAT_FLOAT)
        assert result.is_valid

    def test_hex_format_valid(self) -> None:
        """f-string with hex format spec should be valid."""
        result = self.validator.validate_code(FSTRING_FORMAT_HEX)
        assert result.is_valid

    def test_int_format_valid(self) -> None:
        """f-string with int format spec should be valid."""
        result = self.validator.validate_code(FSTRING_FORMAT_INT)
        assert result.is_valid

    def test_conversion_flag_still_rejected(self) -> None:
        """f-string conversion flags (!r, !s) should still be rejected."""
        code = """
def fmt(x: int) -> str:
    return f"{x!r}"
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid

    def test_mixed_format_valid(self) -> None:
        """f-string mixing plain and formatted values should be valid."""
        result = self.validator.validate_code(FSTRING_FORMAT_MIXED)
        assert result.is_valid


class TestFStringFormatSpecCpp:
    """Test C++ f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_float_precision(self) -> None:
        """C++ should use snprintf for float precision."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "snprintf" in result or "%.2f" in result

    def test_hex_format(self) -> None:
        """C++ should format hex values."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert "%x" in result


class TestFStringFormatSpecGo:
    """Test Go f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_float_precision(self) -> None:
        """Go should use fmt.Sprintf with %.2f."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "%.2f" in result
        assert "Sprintf" in result

    def test_hex_format(self) -> None:
        """Go should use %x format."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert "%x" in result


class TestFStringFormatSpecRust:
    """Test Rust f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_float_precision(self) -> None:
        """Rust should use format! with :.2 spec."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "format!" in result
        assert ":.2" in result

    def test_hex_format(self) -> None:
        """Rust should use :x format spec."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert ":x" in result


class TestFStringFormatSpecHaskell:
    """Test Haskell f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_float_precision(self) -> None:
        """Haskell should use Text.Printf for format specs."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "Printf" in result or "printf" in result

    def test_hex_format(self) -> None:
        """Haskell should handle hex format."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert "Printf" in result or "printf" in result


class TestFStringFormatSpecOCaml:
    """Test OCaml f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_float_precision(self) -> None:
        """OCaml should use Printf.sprintf for format specs."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "Printf.sprintf" in result or "%.2f" in result

    def test_hex_format(self) -> None:
        """OCaml should handle hex format."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert "Printf.sprintf" in result or "%x" in result


class TestFStringFormatSpecC:
    """Test C f-string format spec code generation."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_float_precision(self) -> None:
        """C should use printf-style format directly."""
        result = self.converter.convert_code(FSTRING_FORMAT_FLOAT)
        assert "%.2f" in result

    def test_hex_format(self) -> None:
        """C should use %x format."""
        result = self.converter.convert_code(FSTRING_FORMAT_HEX)
        assert "%x" in result


# =============================================================================
# 4. Finally clause tests
# =============================================================================


class TestFinallyValidation:
    """Test validation of finally clause."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_finally_valid(self) -> None:
        """try/except/finally should be valid."""
        result = self.validator.validate_code(FINALLY_BASIC)
        assert result.is_valid

    def test_else_now_accepted(self) -> None:
        """try/except/else should now be accepted."""
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
        assert result.is_valid

    def test_finally_with_cleanup(self) -> None:
        """try/except/finally with cleanup code should be valid."""
        result = self.validator.validate_code(FINALLY_WITH_CLEANUP)
        assert result.is_valid


class TestFinallyCpp:
    """Test C++ backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_finally_generates_code(self) -> None:
        """C++ should generate finally code after try/catch."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "try" in result
        assert "catch" in result
        assert "finally" in result.lower()


class TestFinallyC:
    """Test C backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_finally_generates_code(self) -> None:
        """C should generate finally code after MGEN_END_TRY."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "MGEN_TRY" in result
        assert "MGEN_END_TRY" in result
        assert "finally" in result.lower()


class TestFinallyGo:
    """Test Go backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_finally_generates_defer(self) -> None:
        """Go should use defer for finally cleanup."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "defer" in result


class TestFinallyRust:
    """Test Rust backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_finally_generates_code(self) -> None:
        """Rust should generate finally code after catch_unwind match."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "catch_unwind" in result
        assert "finally" in result.lower()


class TestFinallyHaskell:
    """Test Haskell backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_finally_generates_code(self) -> None:
        """Haskell should generate finally code after catch."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "catch" in result
        assert "finally" in result.lower()


class TestFinallyOCaml:
    """Test OCaml backend finally clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_finally_generates_code(self) -> None:
        """OCaml should generate finally code after try/with."""
        result = self.converter.convert_code(FINALLY_BASIC)
        assert "try" in result
        assert "with" in result
        assert "finally" in result.lower()


# =============================================================================
# 5. String slicing tests
# =============================================================================


class TestStringSliceCpp:
    """Test C++ string slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_string_slice_basic(self) -> None:
        """C++ should use .substr() for string slicing."""
        result = self.converter.convert_code(STRING_SLICE_BASIC)
        assert "substr" in result

    def test_string_slice_to_end(self) -> None:
        """C++ should use .substr(start) for open-ended string slice."""
        result = self.converter.convert_code(STRING_SLICE_TO_END)
        assert "substr" in result


class TestStringSliceGo:
    """Test Go string slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_string_slice_basic(self) -> None:
        """Go should use native slice syntax for strings."""
        result = self.converter.convert_code(STRING_SLICE_BASIC)
        assert "[1:3]" in result

    def test_string_slice_to_end(self) -> None:
        """Go should use [2:] for open-ended string slice."""
        result = self.converter.convert_code(STRING_SLICE_TO_END)
        assert "[2:]" in result


class TestStringSliceRust:
    """Test Rust string slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_string_slice_basic(self) -> None:
        """Rust should use range indexing with .to_string()."""
        result = self.converter.convert_code(STRING_SLICE_BASIC)
        assert ".to_string()" in result
        assert "[1..3]" in result

    def test_string_slice_to_end(self) -> None:
        """Rust should use [2..] for open-ended string slice."""
        result = self.converter.convert_code(STRING_SLICE_TO_END)
        assert ".to_string()" in result


class TestStringSliceOCaml:
    """Test OCaml string slicing."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_string_slice_basic(self) -> None:
        """OCaml should use String.sub for string slicing."""
        result = self.converter.convert_code(STRING_SLICE_BASIC)
        assert "String.sub" in result

    def test_string_slice_to_end(self) -> None:
        """OCaml should use String.length for open-ended string slice."""
        result = self.converter.convert_code(STRING_SLICE_TO_END)
        assert "String.sub" in result
        assert "String.length" in result


class TestStringSliceHaskell:
    """Test Haskell string slicing (strings are lists of Char)."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_string_slice_basic(self) -> None:
        """Haskell should use drop/take for string slicing."""
        result = self.converter.convert_code(STRING_SLICE_BASIC)
        assert "take" in result or "drop" in result


# =============================================================================
# 6. Else clause tests
# =============================================================================


class TestElseValidation:
    """Test validation of else clause."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_else_valid(self) -> None:
        """try/except/else should be valid."""
        result = self.validator.validate_code(ELSE_BASIC)
        assert result.is_valid

    def test_else_with_finally_valid(self) -> None:
        """try/except/else/finally should be valid."""
        result = self.validator.validate_code(ELSE_WITH_FINALLY)
        assert result.is_valid

    def test_exception_chaining_still_rejected(self) -> None:
        """raise ... from ... should still be rejected."""
        code = """
def chained() -> int:
    try:
        return 1
    except ValueError as e:
        raise RuntimeError("wrapped") from e
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid


class TestElseCpp:
    """Test C++ else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_else_generates_code(self) -> None:
        """C++ should emit else body inside try block."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "try" in result
        assert "else" in result.lower()


class TestElseC:
    """Test C else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_else_generates_code(self) -> None:
        """C should emit else body inside MGEN_TRY block."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "MGEN_TRY" in result
        assert "else" in result.lower()


class TestElseGo:
    """Test Go else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_else_generates_code(self) -> None:
        """Go should emit else body in try function."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "else" in result.lower()


class TestElseRust:
    """Test Rust else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_else_generates_code(self) -> None:
        """Rust should emit else body in catch_unwind closure."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "catch_unwind" in result
        assert "else" in result.lower()


class TestElseHaskell:
    """Test Haskell else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_else_generates_code(self) -> None:
        """Haskell should emit else body in catch do-block."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "catch" in result
        assert "else" in result.lower()


class TestElseOCaml:
    """Test OCaml else clause."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_else_generates_code(self) -> None:
        """OCaml should emit else body in try block."""
        result = self.converter.convert_code(ELSE_BASIC)
        assert "try" in result
        assert "else" in result.lower()
