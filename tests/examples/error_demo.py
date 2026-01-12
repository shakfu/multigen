"""Demonstration of enhanced error messages in MultiGen.

This script shows various error scenarios and how they are presented
with the new colored error formatting system.
"""

import ast

# Add parent directory to path to import multigen
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from multigen.error_formatter import format_error, print_error, set_color_mode
from multigen.errors import (
    ErrorCode,
    SourceLocation,
    TypeMappingError,
    UnsupportedFeatureError,
    create_unsupported_feature_error,
)


def demo_basic_error():
    """Show basic unsupported feature error."""
    print("\n" + "=" * 80)
    print("Demo 1: Basic Unsupported Feature Error")
    print("=" * 80)

    error = UnsupportedFeatureError(
        "Generator expressions are not supported",
        error_code=ErrorCode.E1001,
        suggestion="Try using a list comprehension instead: [x for x in items]",
    )

    print_error(error)


def demo_error_with_location():
    """Show error with source location."""
    print("\n" + "=" * 80)
    print("Demo 2: Error with Source Location")
    print("=" * 80)

    location = SourceLocation(filename="example.py", line=42, column=10, end_column=25)

    error = UnsupportedFeatureError(
        "Async functions are not supported",
        location=location,
        source_line="    async def process_data(items):",
        suggestion="Use synchronous functions instead. Remove 'async' keyword.",
        help_text="See https://github.com/yourusername/multigen/docs/async-support.md for more information.",
        error_code=ErrorCode.E1001,
    )

    print_error(error)


def demo_error_from_ast():
    """Show error created from AST node."""
    print("\n" + "=" * 80)
    print("Demo 3: Error from AST Node")
    print("=" * 80)

    # Parse some Python code with an unsupported feature
    code = """
def example():
    result = (x for x in range(10))  # Generator expression
    return result
""".strip()

    tree = ast.parse(code, filename="test.py")

    # Find the generator expression
    for node in ast.walk(tree):
        if isinstance(node, ast.GeneratorExp):
            error = UnsupportedFeatureError.from_ast_node(
                message="Generator expressions are not supported in the current backend",
                node=node,
                filename="test.py",
                suggestion="Use a list comprehension instead: result = [x for x in range(10)]",
                help_text="MultiGen does not support generators. See supported features documentation.",
                error_code=ErrorCode.E1001,
            )

            # Try to get the source line
            lines = code.split("\n")
            if hasattr(node, "lineno") and 1 <= node.lineno <= len(lines):
                error.context.source_line = lines[node.lineno - 1]

            print_error(error)
            break


def demo_type_error():
    """Show type mapping error."""
    print("\n" + "=" * 80)
    print("Demo 4: Type Mapping Error")
    print("=" * 80)

    location = SourceLocation(filename="types_example.py", line=15, column=20, end_column=35)

    error = TypeMappingError(
        "Cannot map type 'CustomGeneric[T, K]' to target language",
        location=location,
        source_line="def process(data: CustomGeneric[T, K]):",
        suggestion="Try using a simpler type like dict[str, Any] or a concrete type",
        help_text="Complex generic types may not have direct equivalents in the target language",
        error_code=ErrorCode.E2001,
    )

    print_error(error)


def demo_helper_function():
    """Show helper function for creating errors."""
    print("\n" + "=" * 80)
    print("Demo 5: Using Helper Function")
    print("=" * 80)

    # Parse code
    code = "yield from items"
    tree = ast.parse(code, filename="generator.py")

    for node in ast.walk(tree):
        if isinstance(node, ast.YieldFrom):
            error = create_unsupported_feature_error(
                feature_name="yield from", node=node, filename="generator.py", backend="Rust"
            )

            # Add source line
            error.context.source_line = code

            print_error(error)
            break


def demo_no_color():
    """Show error without colors."""
    print("\n" + "=" * 80)
    print("Demo 6: Same Error Without Colors (for CI/logs)")
    print("=" * 80)

    # Temporarily disable colors
    set_color_mode(False)

    location = SourceLocation(filename="example.py", line=23, column=8)

    error = UnsupportedFeatureError(
        "Lambda expressions are not fully supported",
        location=location,
        source_line="    callback = lambda x: x * 2",
        suggestion="Use a named function instead",
        error_code=ErrorCode.E1001,
    )

    print_error(error)

    # Re-enable colors
    set_color_mode(True)


if __name__ == "__main__":
    print("\n")
    print("╔" + "=" * 78 + "╗")
    print("║" + " " * 20 + "MultiGen Enhanced Error Messages Demo" + " " * 24 + "║")
    print("╚" + "=" * 78 + "╝")

    demo_basic_error()
    demo_error_with_location()
    demo_error_from_ast()
    demo_type_error()
    demo_helper_function()
    demo_no_color()

    print("\n" + "=" * 80)
    print("Demo Complete!")
    print("=" * 80 + "\n")
