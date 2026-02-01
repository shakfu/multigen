"""Tests for context manager (with statement) support.

Tests context manager conversion across all backends:
- C++: RAII with ifstream/ofstream
- C: fopen/fclose pattern
- Rust: Scope-based Drop
- Go: defer pattern
- Haskell: bracket pattern
- OCaml: Fun.protect pattern
"""

import pytest

from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.frontend.subset_validator import StaticPythonSubsetValidator


class TestContextManagerValidation:
    """Test validation of context manager constraints."""

    def setup_method(self) -> None:
        self.validator = StaticPythonSubsetValidator()

    def test_single_context_manager_valid(self) -> None:
        """Single context manager with 'as' binding should be valid."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        result = self.validator.validate_code(code)
        assert result.is_valid

    def test_multiple_context_managers_rejected(self) -> None:
        """Multiple context managers in single 'with' should be rejected."""
        code = """
def copy_file(src: str, dst: str) -> None:
    with open(src) as f1, open(dst, "w") as f2:
        f2.write(f1.read())
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("multiple" in v.lower() for v in result.violations)

    def test_context_manager_without_as_rejected(self) -> None:
        """Context manager without 'as' binding should be rejected."""
        code = """
def process() -> None:
    with open("file.txt"):
        x: int = 1
"""
        result = self.validator.validate_code(code)
        assert not result.is_valid
        assert any("as" in v.lower() for v in result.violations)


class TestCppContextManagers:
    """Test C++ context manager conversion using RAII."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCppConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with ifstream."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        cpp = self.converter.convert_code(code)
        assert "ifstream" in cpp
        assert "{" in cpp  # Scope block for RAII

    def test_file_write(self) -> None:
        """Test file writing with ofstream."""
        code = """
def write_file(filename: str, data: str) -> None:
    with open(filename, "w") as f:
        f.write(data)
"""
        cpp = self.converter.convert_code(code)
        assert "ofstream" in cpp

    def test_raii_scope(self) -> None:
        """Test that RAII scope block is generated."""
        code = """
def process_file(path: str) -> int:
    with open(path, "r") as f:
        x: int = 42
    return x
"""
        cpp = self.converter.convert_code(code)
        # Should have nested braces for scope
        assert cpp.count("{") >= 2


class TestCContextManagers:
    """Test C context manager conversion using fopen/fclose."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToCConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with fopen."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        c = self.converter.convert_code(code)
        assert "fopen" in c
        assert "fclose" in c
        assert "FILE*" in c

    def test_null_check(self) -> None:
        """Test that NULL check is generated."""
        code = """
def read_file(path: str) -> int:
    with open(path, "r") as f:
        x: int = 1
    return x
"""
        c = self.converter.convert_code(code)
        assert "NULL" in c

    def test_write_mode(self) -> None:
        """Test write mode handling."""
        code = """
def write_file(path: str) -> None:
    with open(path, "w") as f:
        f.write("hello")
"""
        c = self.converter.convert_code(code)
        assert '"w"' in c


class TestRustContextManagers:
    """Test Rust context manager conversion using scope-based Drop."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToRustConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with File::open."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        rust = self.converter.convert_code(code)
        assert "File::open" in rust
        assert "unwrap()" in rust

    def test_file_write(self) -> None:
        """Test file writing with File::create."""
        code = """
def write_file(filename: str, data: str) -> None:
    with open(filename, "w") as f:
        f.write(data)
"""
        rust = self.converter.convert_code(code)
        assert "File::create" in rust

    def test_scope_block(self) -> None:
        """Test that scope block is generated for Drop."""
        code = """
def process_file(path: str) -> i32:
    with open(path, "r") as f:
        x: int = 42
    return x
"""
        rust = self.converter.convert_code(code)
        # Should have nested braces for scope
        assert rust.count("{") >= 2


class TestGoContextManagers:
    """Test Go context manager conversion using defer."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToGoConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with os.Open."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        go = self.converter.convert_code(code)
        assert "os.Open" in go
        assert "defer" in go
        assert "Close()" in go

    def test_file_write(self) -> None:
        """Test file writing with os.Create."""
        code = """
def write_file(filename: str, data: str) -> None:
    with open(filename, "w") as f:
        f.write(data)
"""
        go = self.converter.convert_code(code)
        assert "os.Create" in go
        assert "defer" in go


class TestHaskellContextManagers:
    """Test Haskell context manager conversion using bracket."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToHaskellConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with bracket."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        haskell = self.converter.convert_code(code)
        assert "bracket" in haskell
        assert "openFile" in haskell
        assert "hClose" in haskell

    def test_read_mode(self) -> None:
        """Test ReadMode for read operations."""
        code = """
def read_file(path: str) -> str:
    with open(path, "r") as f:
        data: str = f.read()
    return data
"""
        haskell = self.converter.convert_code(code)
        assert "ReadMode" in haskell

    def test_write_mode(self) -> None:
        """Test WriteMode for write operations."""
        code = """
def write_file(path: str) -> None:
    with open(path, "w") as f:
        f.write("hello")
"""
        haskell = self.converter.convert_code(code)
        assert "WriteMode" in haskell


class TestOCamlContextManagers:
    """Test OCaml context manager conversion using Fun.protect."""

    def setup_method(self) -> None:
        self.converter = MultiGenPythonToOCamlConverter()

    def test_simple_file_read(self) -> None:
        """Test basic file reading with Fun.protect."""
        code = """
def read_file(filename: str) -> str:
    with open(filename, "r") as f:
        content: str = f.read()
    return content
"""
        ocaml = self.converter.convert_code(code)
        assert "Fun.protect" in ocaml
        assert "open_in" in ocaml
        assert "close_in" in ocaml

    def test_file_write(self) -> None:
        """Test file writing with open_out/close_out."""
        code = """
def write_file(filename: str, data: str) -> None:
    with open(filename, "w") as f:
        f.write(data)
"""
        ocaml = self.converter.convert_code(code)
        assert "open_out" in ocaml
        assert "close_out" in ocaml

    def test_finally_cleanup(self) -> None:
        """Test that ~finally cleanup is generated."""
        code = """
def process_file(path: str) -> int:
    with open(path, "r") as f:
        x: int = 42
    return x
"""
        ocaml = self.converter.convert_code(code)
        assert "~finally" in ocaml


class TestContextManagerEdgeCases:
    """Test edge cases and corner cases for context managers."""

    def test_nested_with_statements(self) -> None:
        """Test nested with statements (each individual with is valid)."""
        validator = StaticPythonSubsetValidator()
        code = """
def process(src: str, dst: str) -> None:
    with open(src, "r") as f1:
        with open(dst, "w") as f2:
            content: str = f1.read()
            f2.write(content)
"""
        result = validator.validate_code(code)
        # Nested with statements (each with single context manager) should be valid
        assert result.is_valid

    def test_variable_naming(self) -> None:
        """Test various variable names work correctly."""
        converter = MultiGenPythonToCppConverter()
        code = """
def read_data(path: str) -> str:
    with open(path, "r") as input_file:
        data: str = input_file.read()
    return data
"""
        cpp = converter.convert_code(code)
        assert "input_file" in cpp

    def test_mode_without_quotes_in_args(self) -> None:
        """Test handling of mode string in different positions."""
        converter = MultiGenPythonToCConverter()
        code = """
def append_file(path: str) -> None:
    with open(path, "a") as f:
        f.write("data")
"""
        c = converter.convert_code(code)
        # 'a' mode should still work (treated as read-like for now)
        assert "fopen" in c
