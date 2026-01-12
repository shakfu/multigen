"""Enhanced error handling system for MultiGen.

This module provides rich error messages with source location, colored output,
and helpful suggestions to improve the developer experience.
"""

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Optional, TypeVar

# TypeVar for class methods that return the same type as the class
T_MultiGenError = TypeVar("T_MultiGenError", bound="MultiGenError")


class ErrorCode(Enum):
    """Error codes for categorizing different types of errors."""

    # Feature support errors (E1xxx)
    E1001 = "unsupported_feature"
    E1002 = "unsupported_statement"
    E1003 = "unsupported_expression"
    E1004 = "unsupported_operator"
    E1005 = "unsupported_constant_type"

    # Type system errors (E2xxx)
    E2001 = "type_mapping_failed"
    E2002 = "type_inference_failed"
    E2003 = "incompatible_types"
    E2004 = "missing_type_annotation"

    # Syntax errors (E3xxx)
    E3001 = "invalid_syntax"
    E3002 = "parse_error"

    # Import/module errors (E4xxx)
    E4001 = "import_not_found"
    E4002 = "circular_import"

    # Runtime errors (E5xxx)
    E5001 = "code_generation_failed"
    E5002 = "compilation_failed"


@dataclass
class SourceLocation:
    """Represents a location in source code."""

    filename: str
    line: int
    column: int = 0
    end_line: Optional[int] = None
    end_column: Optional[int] = None

    @classmethod
    def from_ast_node(cls, node: ast.AST, filename: str = "<unknown>") -> "SourceLocation":
        """Create SourceLocation from an AST node."""
        return cls(
            filename=filename,
            line=getattr(node, "lineno", 0),
            column=getattr(node, "col_offset", 0),
            end_line=getattr(node, "end_lineno", None),
            end_column=getattr(node, "end_col_offset", None),
        )

    def __str__(self) -> str:
        """Format as filename:line:column."""
        if self.column > 0:
            return f"{self.filename}:{self.line}:{self.column}"
        return f"{self.filename}:{self.line}"


@dataclass
class ErrorContext:
    """Additional context for an error."""

    location: Optional[SourceLocation] = None
    source_line: Optional[str] = None
    suggestion: Optional[str] = None
    help_text: Optional[str] = None
    related_info: Optional[list[str]] = None
    error_code: Optional[ErrorCode] = None

    def get_source_snippet(self, num_lines: int = 3) -> Optional[list[str]]:
        """Get source code snippet around the error location.

        Args:
            num_lines: Number of lines to show before and after error line

        Returns:
            List of source code lines with context, or None if source not available
        """
        if not self.location or not self.source_line:
            return None

        # For now, just return the single line
        # In future, could read from file to get surrounding context
        return [self.source_line]


class MultiGenError(Exception):
    """Base class for all MultiGen errors with rich context."""

    def __init__(
        self,
        message: str,
        location: Optional[SourceLocation] = None,
        suggestion: Optional[str] = None,
        help_text: Optional[str] = None,
        error_code: Optional[ErrorCode] = None,
        source_line: Optional[str] = None,
    ):
        """Initialize error with context.

        Args:
            message: Primary error message
            location: Source location where error occurred
            suggestion: Helpful suggestion to fix the error
            help_text: Additional help text or documentation link
            error_code: Error code for categorization
            source_line: The source line where error occurred
        """
        super().__init__(message)
        self.context = ErrorContext(
            location=location,
            source_line=source_line,
            suggestion=suggestion,
            help_text=help_text,
            error_code=error_code,
        )

    @classmethod
    def from_ast_node(
        cls: type[T_MultiGenError],
        message: str,
        node: ast.AST,
        filename: str = "<unknown>",
        suggestion: Optional[str] = None,
        help_text: Optional[str] = None,
        error_code: Optional[ErrorCode] = None,
    ) -> T_MultiGenError:
        """Create error from AST node with automatic location extraction.

        Args:
            message: Error message
            node: AST node where error occurred
            filename: Source filename
            suggestion: Helpful suggestion
            help_text: Additional help
            error_code: Error code

        Returns:
            MultiGenError instance with location context
        """
        location = SourceLocation.from_ast_node(node, filename)
        return cls(
            message=message,
            location=location,
            suggestion=suggestion,
            help_text=help_text,
            error_code=error_code,
        )


class UnsupportedFeatureError(MultiGenError):
    """Raised when encountering unsupported Python features."""

    def __init__(
        self,
        message: str,
        location: Optional[SourceLocation] = None,
        suggestion: Optional[str] = None,
        help_text: Optional[str] = None,
        error_code: Optional[ErrorCode] = None,
        source_line: Optional[str] = None,
    ):
        """Initialize with default error code."""
        super().__init__(
            message=message,
            location=location,
            suggestion=suggestion,
            help_text=help_text,
            error_code=error_code or ErrorCode.E1001,
            source_line=source_line,
        )


class TypeMappingError(MultiGenError):
    """Raised when type annotation cannot be mapped to target language."""

    def __init__(
        self,
        message: str,
        location: Optional[SourceLocation] = None,
        suggestion: Optional[str] = None,
        help_text: Optional[str] = None,
        error_code: Optional[ErrorCode] = None,
        source_line: Optional[str] = None,
    ):
        """Initialize with default error code."""
        super().__init__(
            message=message,
            location=location,
            suggestion=suggestion,
            help_text=help_text,
            error_code=error_code or ErrorCode.E2001,
            source_line=source_line,
        )


# Suggestions database for common errors
ERROR_SUGGESTIONS = {
    "generator": "MultiGen does not support generator expressions yet. Try using a list comprehension instead.",
    "async": "MultiGen does not support async/await. Use synchronous functions instead.",
    "with": "MultiGen does not support context managers (with statement) yet. Use try/finally for cleanup.",
    "lambda": "MultiGen has limited lambda support. Consider using a named function instead.",
    "yield": "MultiGen does not support generators (yield). Use regular functions that return lists.",
    "*args": "MultiGen does not support *args. Use explicit parameters or a list parameter.",
    "**kwargs": "MultiGen does not support **kwargs. Use explicit parameters or a dict parameter.",
    "decorator": "MultiGen has limited decorator support. Avoid decorators or use simple function calls.",
    "global": "Avoid global variables when possible. Use function parameters or class attributes.",
    "nonlocal": "MultiGen has limited nonlocal support. Avoid nested function closures when possible.",
}


def suggest_fix(feature_name: str) -> Optional[str]:
    """Get suggestion for unsupported feature.

    Args:
        feature_name: Name of the unsupported feature

    Returns:
        Suggestion string or None
    """
    feature_lower = feature_name.lower()
    for key, suggestion in ERROR_SUGGESTIONS.items():
        if key in feature_lower:
            return suggestion
    return None


def create_unsupported_feature_error(
    feature_name: str,
    node: Optional[ast.AST] = None,
    filename: str = "<unknown>",
    backend: Optional[str] = None,
) -> UnsupportedFeatureError:
    """Create a helpful unsupported feature error.

    Args:
        feature_name: Name of the unsupported feature
        node: AST node (if available)
        filename: Source filename
        backend: Target backend name

    Returns:
        UnsupportedFeatureError with helpful context
    """
    backend_str = f" in {backend} backend" if backend else ""
    message = f"Unsupported feature: {feature_name}{backend_str}"

    suggestion = suggest_fix(feature_name)

    help_text = (
        "See https://github.com/yourusername/multigen/docs/supported-features.md "
        "for a complete list of supported Python features."
    )

    if node:
        return UnsupportedFeatureError.from_ast_node(
            message=message,
            node=node,
            filename=filename,
            suggestion=suggestion,
            help_text=help_text,
            error_code=ErrorCode.E1001,
        )
    else:
        return UnsupportedFeatureError(
            message=message,
            suggestion=suggestion,
            help_text=help_text,
            error_code=ErrorCode.E1001,
        )
