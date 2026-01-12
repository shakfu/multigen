"""Enhanced Rust code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import Any, Optional

from ..base import AbstractEmitter
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToRustConverter


class RustEmitter(AbstractEmitter):
    """Enhanced Rust code emitter using the MultiGenPythonToRustConverter."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize Rust emitter."""
        super().__init__(preferences)
        self.converter = MultiGenPythonToRustConverter()

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to Rust type."""
        return self.converter.type_map.get(python_type, "i32")

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate Rust function code using converter."""
        # Create a simple module with just this function
        module = ast.Module(body=[func_node], type_ignores=[])
        full_code = self.converter._convert_module(module)

        # Extract just the function part (remove imports and main)
        lines = full_code.split("\n")
        function_lines = []
        in_function = False
        brace_count = 0

        for line in lines:
            if line.strip().startswith(f"fn {func_node.name}("):
                in_function = True
                function_lines.append(line)
                if "{" in line:
                    brace_count += line.count("{") - line.count("}")
            elif in_function:
                function_lines.append(line)
                if "{" in line or "}" in line:
                    brace_count += line.count("{") - line.count("}")
                    if brace_count == 0:
                        break

        return "\n".join(function_lines)

    def emit_module(self, source_code: str, analysis_result: Any) -> str:
        """Generate complete Rust module using converter."""
        return self.converter.convert_code(source_code)

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Check if function can use simple emission strategy."""
        # Use the advanced converter for all functions
        return False
