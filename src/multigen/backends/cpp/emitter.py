"""C++ code emitter for MultiGen with integrated runtime libraries and sophisticated py2cpp conversion.

This module provides sophisticated Python-to-C++ conversion capabilities using STL containers
while maintaining MultiGen's architecture and API compatibility.

Supported Features:
- Type-annotated function definitions
- Basic data types (int, float, bool, std::string)
- Variable declarations with type annotations
- Advanced arithmetic and comparison operations
- Control structures (if/else, while, for with range)
- Function calls and method calls
- Return statements
- STL container operations (vector, map, set)
- Object-oriented programming (classes, methods, constructors)
- Memory management with RAII
- Comprehensions (list, dict, set) with range iteration and conditional filtering
- String methods with STL string operations
- Augmented assignment operators
"""

import ast
from typing import Any, Optional, Union

from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToCppConverter
from .factory import CppFactory


class CppEmitter(AbstractEmitter):
    """Enhanced C++ emitter using MultiGen Python-to-C++ converter."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize the C++ emitter."""
        super().__init__(preferences)
        self.factory = CppFactory()
        self.converter = MultiGenPythonToCppConverter()
        self.indent_level = 0
        # Use preferences for indent size if available
        self.indent_size = preferences.get("indent_size", 4) if preferences else 4

    def emit_module(self, source_code: str, analysis_result: Optional[Any] = None) -> str:
        """Emit a complete C++ module from Python source."""
        return self.converter.convert_code(source_code)

    def emit_statement(self, node: ast.stmt) -> str:
        """Emit a C++ statement from a Python AST node."""
        # Delegate to converter for comprehensive statement handling
        return self.converter._convert_statement(node)

    def emit_expression(self, node: ast.expr) -> str:
        """Emit a C++ expression from a Python AST node."""
        # Delegate to converter for comprehensive expression handling
        return self.converter._convert_expression(node)

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to C++ type."""
        return self.converter.type_mapping.get(python_type, "auto")

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Check if simple emission can be used for this code."""
        # Always use full sophisticated conversion
        return False

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Emit a C++ function from a Python function definition."""
        # Delegate to converter for comprehensive function handling
        return self.converter._convert_function(func_node)

    def emit_class(self, node: ast.ClassDef) -> str:
        """Emit a C++ class from a Python class definition."""
        # Delegate to converter for comprehensive class handling
        return self.converter._convert_class(node)

    def emit_comprehension(self, node: Union[ast.ListComp, ast.DictComp, ast.SetComp]) -> str:
        """Emit C++ code for comprehensions."""
        if isinstance(node, ast.ListComp):
            return self.converter._convert_list_comprehension(node)
        elif isinstance(node, ast.DictComp):
            return self.converter._convert_dict_comprehension(node)
        elif isinstance(node, ast.SetComp):
            return self.converter._convert_set_comprehension(node)
        else:
            return "/* Unknown comprehension type */"
