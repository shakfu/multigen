"""Enhanced Go code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import Any, Optional, Union

from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToGoConverter


class GoEmitter(AbstractEmitter):
    """Enhanced Go code emitter implementation with comprehensive Python support."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize Go emitter."""
        super().__init__(preferences)
        self.converter = MultiGenPythonToGoConverter()

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to Go type."""
        return self.converter.type_map.get(python_type, "interface{}")

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate Go function code using the advanced converter."""
        return self.converter._convert_function(func_node)

    def emit_module(self, source_code: str, analysis_result: Any) -> str:
        """Generate complete Go module using the advanced converter."""
        return self.converter.convert_code(source_code)

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Check if function can use simple emission strategy."""
        # Advanced converter handles all cases
        return True

    def emit_class(self, node: ast.ClassDef) -> str:
        """Emit Go struct and methods for Python class."""
        return self.converter._convert_class(node)

    def emit_comprehension(self, node: Union[ast.ListComp, ast.DictComp, ast.SetComp]) -> str:
        """Emit Go code for comprehensions."""
        if isinstance(node, ast.ListComp):
            return self.converter._convert_list_comprehension(node)
        elif isinstance(node, ast.DictComp):
            return self.converter._convert_dict_comprehension(node)
        elif isinstance(node, ast.SetComp):
            return self.converter._convert_set_comprehension(node)
        else:
            return "/* Unsupported comprehension */"
