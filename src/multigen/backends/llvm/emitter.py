"""LLVM IR emitter for MultiGen."""

import ast
from typing import Any, Optional

from ...frontend.static_ir import build_ir_from_code
from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .ir_to_llvm import IRToLLVMConverter


class LLVMEmitter(AbstractEmitter):
    """Emitter that generates LLVM IR from Python code via Static IR."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize the LLVM emitter."""
        super().__init__(preferences)
        self.converter = IRToLLVMConverter()

    def emit_module(self, source_code: str, analysis_result: Any = None) -> str:
        """Generate LLVM IR from Python source code.

        Args:
            source_code: Python source code to convert
            analysis_result: Optional analysis result (unused for now)

        Returns:
            LLVM IR as text string
        """
        # Build Static IR from Python source
        ir_module = build_ir_from_code(source_code)

        # Convert Static IR to LLVM IR
        llvm_module = self.converter.visit_module(ir_module)

        # Return LLVM IR as text
        return str(llvm_module)

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate LLVM IR for a single function.

        Args:
            func_node: AST function definition node
            type_context: Type information for variables

        Returns:
            LLVM IR for the function
        """
        # Convert AST to source code and emit
        import astor  # type: ignore[import-not-found]

        function_code = astor.to_source(func_node)
        return self.emit_module(function_code)

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to LLVM IR type.

        Args:
            python_type: Python type name

        Returns:
            LLVM IR type string
        """
        type_mapping = {
            "int": "i64",
            "float": "double",
            "bool": "i1",
            "str": "i8*",
            "void": "void",
        }
        return type_mapping.get(python_type, "i64")

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Determine if function can use simple emission strategy.

        Args:
            func_node: AST function definition node
            type_context: Type information for variables

        Returns:
            True if simple emission can be used
        """
        # LLVM backend always uses IR-based emission
        return True
