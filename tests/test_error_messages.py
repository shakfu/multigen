"""Tests for enhanced error message system."""

import ast

import pytest

from multigen.error_formatter import ErrorFormatter, format_error
from multigen.errors import (
    ErrorCode,
    MultiGenError,
    SourceLocation,
    TypeMappingError,
    UnsupportedFeatureError,
    create_unsupported_feature_error,
    suggest_fix,
)


class TestSourceLocation:
    """Test SourceLocation class."""

    def test_basic_location(self):
        """Test basic source location."""
        loc = SourceLocation(filename="test.py", line=42, column=10)
        assert loc.filename == "test.py"
        assert loc.line == 42
        assert loc.column == 10

    def test_location_string(self):
        """Test source location string formatting."""
        loc = SourceLocation(filename="test.py", line=42, column=10)
        assert str(loc) == "test.py:42:10"

        loc_no_col = SourceLocation(filename="test.py", line=42)
        assert str(loc_no_col) == "test.py:42"

    def test_from_ast_node(self):
        """Test creating location from AST node."""
        code = "result = x + y"
        tree = ast.parse(code)
        assign = tree.body[0]

        loc = SourceLocation.from_ast_node(assign, filename="example.py")
        assert loc.filename == "example.py"
        assert loc.line == 1
        assert loc.column >= 0


class TestErrorClasses:
    """Test enhanced error classes."""

    def test_basic_unsupported_feature_error(self):
        """Test basic unsupported feature error."""
        error = UnsupportedFeatureError("Generator expressions not supported")
        assert "Generator expressions" in str(error)
        assert error.context is not None

    def test_error_with_location(self):
        """Test error with source location."""
        loc = SourceLocation(filename="test.py", line=10, column=5)
        error = UnsupportedFeatureError("Async not supported", location=loc)

        assert error.context.location == loc
        assert error.context.location.line == 10

    def test_error_with_suggestion(self):
        """Test error with helpful suggestion."""
        error = UnsupportedFeatureError(
            "Generators not supported", suggestion="Use list comprehensions instead"
        )

        assert error.context.suggestion == "Use list comprehensions instead"

    def test_error_with_code(self):
        """Test error with error code."""
        error = UnsupportedFeatureError("Feature not supported", error_code=ErrorCode.E1001)

        assert error.context.error_code == ErrorCode.E1001

    def test_type_mapping_error(self):
        """Test type mapping error."""
        error = TypeMappingError("Cannot map type Complex[T]")
        assert "Complex[T]" in str(error)
        assert error.context.error_code == ErrorCode.E2001

    def test_error_from_ast_node(self):
        """Test creating error from AST node."""
        code = "x = (i for i in range(10))"
        tree = ast.parse(code, filename="test.py")

        # Find generator expression
        genexp = None
        for node in ast.walk(tree):
            if isinstance(node, ast.GeneratorExp):
                genexp = node
                break

        assert genexp is not None

        error = UnsupportedFeatureError.from_ast_node(
            message="Generators not supported",
            node=genexp,
            filename="test.py",
            suggestion="Use list comprehension",
        )

        assert error.context.location is not None
        assert error.context.location.filename == "test.py"
        assert error.context.suggestion == "Use list comprehension"


class TestSuggestions:
    """Test suggestion system."""

    def test_generator_suggestion(self):
        """Test generator expression suggestion."""
        suggestion = suggest_fix("generator expression")
        assert suggestion is not None
        assert "list comprehension" in suggestion.lower()

    def test_async_suggestion(self):
        """Test async/await suggestion."""
        suggestion = suggest_fix("async function")
        assert suggestion is not None
        assert "synchronous" in suggestion.lower()

    def test_yield_suggestion(self):
        """Test yield suggestion."""
        suggestion = suggest_fix("yield statement")
        assert suggestion is not None
        assert "yield" in suggestion.lower()

    def test_unknown_feature(self):
        """Test suggestion for unknown feature."""
        suggestion = suggest_fix("some_unknown_feature_xyz")
        assert suggestion is None

    def test_create_unsupported_feature_helper(self):
        """Test helper function for creating unsupported feature errors."""
        code = "yield x"
        tree = ast.parse(code)
        yield_node = tree.body[0].value

        error = create_unsupported_feature_error(
            feature_name="yield", node=yield_node, filename="test.py", backend="C++"
        )

        assert "yield" in str(error)
        assert "C++" in str(error)
        assert error.context.suggestion is not None
        assert error.context.help_text is not None


class TestErrorFormatter:
    """Test error formatting."""

    def test_formatter_creation(self):
        """Test creating error formatter."""
        formatter = ErrorFormatter(use_color=False)
        assert formatter is not None
        assert not formatter.use_color

    def test_format_basic_error(self):
        """Test formatting basic error."""
        formatter = ErrorFormatter(use_color=False)
        error = UnsupportedFeatureError("Test error")

        formatted = formatter.format_error(error)
        assert "error" in formatted
        assert "Test error" in formatted

    def test_format_error_with_location(self):
        """Test formatting error with location."""
        formatter = ErrorFormatter(use_color=False)
        loc = SourceLocation(filename="test.py", line=42, column=10)
        error = UnsupportedFeatureError("Test error", location=loc, source_line="    x = 123")

        formatted = formatter.format_error(error)
        assert "test.py:42" in formatted
        assert "x = 123" in formatted

    def test_format_error_with_suggestion(self):
        """Test formatting error with suggestion."""
        formatter = ErrorFormatter(use_color=False)
        error = UnsupportedFeatureError("Test error", suggestion="Try this instead")

        formatted = formatter.format_error(error)
        assert "help" in formatted
        assert "Try this instead" in formatted

    def test_format_error_with_code(self):
        """Test formatting error with error code."""
        formatter = ErrorFormatter(use_color=False)
        error = UnsupportedFeatureError("Test error", error_code=ErrorCode.E1001)

        formatted = formatter.format_error(error)
        assert "E1001" in formatted

    def test_format_regular_exception(self):
        """Test formatting regular Python exception."""
        formatter = ErrorFormatter(use_color=False)
        error = ValueError("Invalid value")

        formatted = formatter.format_simple(error)
        assert "error" in formatted
        assert "ValueError" in formatted
        assert "Invalid value" in formatted

    def test_format_error_function(self):
        """Test format_error function."""
        error = UnsupportedFeatureError("Test")
        formatted = format_error(error)
        assert "Test" in formatted

    def test_colorize_when_disabled(self):
        """Test that colorize returns plain text when colors disabled."""
        formatter = ErrorFormatter(use_color=False)
        result = formatter.colorize("test", "\033[31m")
        assert result == "test"
        assert "\033" not in result


class TestBackwardCompatibility:
    """Test backward compatibility with existing code."""

    def test_simple_raise_unsupported(self):
        """Test that simple raise UnsupportedFeatureError still works."""
        with pytest.raises(UnsupportedFeatureError):
            raise UnsupportedFeatureError("Test")

    def test_simple_raise_type_mapping(self):
        """Test that simple raise TypeMappingError still works."""
        with pytest.raises(TypeMappingError):
            raise TypeMappingError("Test")

    def test_catch_as_exception(self):
        """Test catching as base Exception."""
        try:
            raise UnsupportedFeatureError("Test")
        except Exception as e:
            assert isinstance(e, Exception)
            assert "Test" in str(e)

    def test_error_message_accessible(self):
        """Test that error message is accessible via str()."""
        error = UnsupportedFeatureError("Test message")
        assert str(error) == "Test message"

    def test_import_from_backends_errors(self):
        """Test importing from backends.errors module for compatibility."""
        from multigen.backends.errors import TypeMappingError, UnsupportedFeatureError

        error1 = UnsupportedFeatureError("Test 1")
        error2 = TypeMappingError("Test 2")

        assert isinstance(error1, Exception)
        assert isinstance(error2, Exception)
