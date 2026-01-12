"""Enhanced Haskell code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import Any, Optional

from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToHaskellConverter


class HaskellEmitter(AbstractEmitter):
    """Haskell code emitter using the Python-to-Haskell converter."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize Haskell emitter with optional preferences."""
        super().__init__(preferences)
        self.converter = MultiGenPythonToHaskellConverter(preferences)

    def emit_code(self, ast_node: ast.AST) -> str:
        """Generate Haskell code from Python AST."""
        if isinstance(ast_node, ast.Module):
            return self.converter._convert_module(ast_node)
        else:
            raise ValueError("Expected ast.Module node")

    def emit_from_source(self, source_code: str) -> str:
        """Generate Haskell code from Python source."""
        return self.converter.convert_code(source_code)

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate complete function in Haskell."""
        return self.converter._convert_function(func_node)

    def emit_module(self, source_code: str, analysis_result: Any) -> str:
        """Generate complete module/file in Haskell."""
        return self.converter.convert_code(source_code)

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to Haskell type."""
        return self.converter.type_map.get(python_type, python_type)

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Determine if function can use simple emission strategy."""
        # For now, use complex emission for all functions to ensure full feature support
        return False
