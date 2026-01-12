"""Enhanced OCaml code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import Any, Optional

from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToOCamlConverter


class OCamlEmitter(AbstractEmitter):
    """OCaml code emitter using the Python-to-OCaml converter."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize OCaml emitter with optional preferences."""
        super().__init__(preferences)
        self.converter = MultiGenPythonToOCamlConverter(preferences)

    def emit_code(self, ast_node: ast.AST) -> str:
        """Generate OCaml code from Python AST."""
        if isinstance(ast_node, ast.Module):
            return self.converter._convert_module(ast_node)
        else:
            raise ValueError("Expected ast.Module node")

    def emit_from_source(self, source_code: str) -> str:
        """Generate OCaml code from Python source."""
        return self.converter.convert_code(source_code)

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate complete function in OCaml."""
        return "\n".join(self.converter._convert_function_def(func_node))

    def emit_module(self, source_code: str, analysis_result: Optional[Any] = None) -> str:
        """Generate complete module/file in OCaml."""
        return self.converter.convert_code(source_code)

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to OCaml type."""
        return self.converter.type_map.get(python_type, "'a")

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Determine if function can use simple emission strategy."""
        # For now, always use the full converter
        return False
