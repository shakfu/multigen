"""C code emitter for MultiGen with integrated runtime libraries and sophisticated py2c conversion.

This module provides sophisticated Python-to-C conversion capabilities adapted from CGen
while maintaining MultiGen's architecture and API compatibility.

Supported Features:
- Type-annotated function definitions
- Basic data types (int, float, bool, str)
- Variable declarations with type annotations
- Basic arithmetic and comparison operations
- Control structures (if/else, while, for with range)
- Function calls and method calls
- Return statements
- Container operations (list, dict, set)
- Object-oriented programming (classes, methods, constructors)
- Memory management with runtime support
- Comprehensions (list, dict, set) with range iteration and conditional filtering
"""

import ast
from pathlib import Path
from typing import Any, Optional

from ..base import AbstractEmitter
from ..errors import UnsupportedFeatureError
from ..preferences import BackendPreferences
from .converter import MultiGenPythonToCConverter


class CEmitter(AbstractEmitter):
    """C code emitter with integrated runtime libraries and sophisticated py2c conversion."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize C emitter with sophisticated py2c conversion and runtime support."""
        super().__init__(preferences)
        self.runtime_dir = Path(__file__).parent / "runtime"
        self.use_runtime = self.runtime_dir.exists()

        # Initialize sophisticated py2c converter with preferences
        self.py2c_converter = MultiGenPythonToCConverter(preferences)

        # Enhanced type mapping for C code generation
        self.type_map = {
            "int": "int",
            "float": "double",
            "bool": "bool",
            "str": "char*",
            "void": "void",
            "list": "vec_int",  # Default to int vector, will be specialized
            "dict": "map_str_int",  # Default to string->int map
            "set": "set_int",  # Default to int set
        }

    def map_python_type(self, python_type: str) -> str:
        """Map Python type to C type."""
        return self.type_map.get(python_type, "int")

    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate C function code."""
        return self._emit_function_basic(func_node, type_context)

    def emit_module(self, source_code: str, analysis_result: Optional[Any] = None) -> str:
        """Generate complete C module using sophisticated py2c conversion."""
        try:
            # Try sophisticated py2c conversion first
            return self.py2c_converter.convert_code(source_code)
        except UnsupportedFeatureError as e:
            # Log detailed error and fail gracefully
            error_msg = f"C backend does not support: {e}"
            raise UnsupportedFeatureError(error_msg) from e
        except Exception as e:
            # Unexpected error - provide diagnostics
            error_msg = f"C code generation failed: {e}\nPlease report this issue with the input file."
            raise UnsupportedFeatureError(error_msg) from e

    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Check if simple emission can be used for this code."""
        # With runtime support, we can handle more complex cases
        return not self.use_runtime

    def _emit_module_with_runtime(self, source_code: str) -> str:
        """Generate C module with MultiGen runtime support."""
        if self.use_runtime:
            return self._emit_module_enhanced(source_code)
        else:
            return self._emit_module_basic(source_code)

    def _emit_module_enhanced(self, source_code: str) -> str:
        """Generate enhanced C module with full runtime support."""
        # Parse AST
        tree = ast.parse(source_code)

        # Generate includes with runtime support
        includes = [
            "#include <stdio.h>",
            "#include <stdlib.h>",
            "#include <stdbool.h>",
            '#include "multigen_error_handling.h"',
            '#include "multigen_python_ops.h"',
            '#include "multigen_memory_ops.h"',
        ]

        if self._uses_containers(tree):
            includes.append('#include "multigen_stc_bridge.h"')
            includes.append("// STC container declarations will be added here")

        # Generate functions with runtime support
        functions = []
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                type_context = self._infer_simple_types(node)
                func_code = self._emit_function_enhanced(node, type_context)
                functions.append(func_code)

        # Add main function if not present
        if not any("main" in func for func in functions):
            main_func = """int main() {
    printf("Hello from MultiGen-generated C code with runtime support!\\n");
    return 0;
}"""
            functions.append(main_func)

        # Combine all parts
        parts = includes + [""] + functions
        return "\n".join(parts)

    def _emit_function_enhanced(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Enhanced C function generation with runtime support."""
        func_name = func_node.name

        # Build parameter list with better type inference
        params = []
        for arg in func_node.args.args:
            param_type = self.type_map.get(type_context.get(arg.arg, "int"), "int")
            params.append(f"{param_type} {arg.arg}")

        # Get return type
        return_type = self.type_map.get(type_context.get("__return__", "void"), "void")

        # Build function signature
        params_str = ", ".join(params) if params else "void"
        signature = f"{return_type} {func_name}({params_str})"

        # Generate enhanced function body
        body = self._emit_function_body_enhanced(func_node, type_context)

        return f"{signature} {{\n{body}\n}}"

    def _emit_function_body_enhanced(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Enhanced function body generation with runtime support."""
        body_lines = []

        for stmt in func_node.body:
            if isinstance(stmt, ast.Return):
                if isinstance(stmt.value, ast.BinOp):
                    if isinstance(stmt.value.op, ast.Add):
                        left = self._emit_expression_enhanced(stmt.value.left)
                        right = self._emit_expression_enhanced(stmt.value.right)
                        body_lines.append(f"    return {left} + {right};")
                    elif isinstance(stmt.value.op, ast.Mult):
                        left = self._emit_expression_enhanced(stmt.value.left)
                        right = self._emit_expression_enhanced(stmt.value.right)
                        body_lines.append(f"    return {left} * {right};")
                    elif isinstance(stmt.value.op, ast.Sub):
                        left = self._emit_expression_enhanced(stmt.value.left)
                        right = self._emit_expression_enhanced(stmt.value.right)
                        body_lines.append(f"    return {left} - {right};")
                    elif isinstance(stmt.value.op, ast.Div):
                        left = self._emit_expression_enhanced(stmt.value.left)
                        right = self._emit_expression_enhanced(stmt.value.right)
                        body_lines.append(f"    return {left} / {right};")
                elif isinstance(stmt.value, ast.Name):
                    body_lines.append(f"    return {stmt.value.id};")
                elif isinstance(stmt.value, ast.Constant):
                    val = stmt.value.value
                    if isinstance(val, bytes):
                        body_lines.append(f"    return {val!r};")
                    else:
                        body_lines.append(f"    return {val};")
                else:
                    expr = self._emit_expression_enhanced(stmt.value) if stmt.value is not None else "NULL"
                    body_lines.append(f"    return {expr};")
            else:
                # Handle other statement types
                body_lines.append("    /* TODO: Enhanced statement generation */")

        if not body_lines:
            body_lines.append("    /* Enhanced function body with runtime support */")

        return "\n".join(body_lines)

    def _emit_expression_enhanced(self, expr: ast.expr) -> str:
        """Enhanced expression generation with runtime support."""
        if isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.Constant):
            return str(expr.value)
        elif isinstance(expr, ast.BinOp):
            left = self._emit_expression_enhanced(expr.left)
            right = self._emit_expression_enhanced(expr.right)
            if isinstance(expr.op, ast.Add):
                return f"({left} + {right})"
            elif isinstance(expr.op, ast.Mult):
                return f"({left} * {right})"
            elif isinstance(expr.op, ast.Sub):
                return f"({left} - {right})"
            elif isinstance(expr.op, ast.Div):
                return f"({left} / {right})"
        return "0"

    def _uses_containers(self, tree: ast.AST) -> bool:
        """Check if the AST uses container types."""
        for node in ast.walk(tree):
            if isinstance(node, ast.Name) and node.id in ["list", "dict", "set"]:
                return True
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
                if node.func.id in ["list", "dict", "set", "append", "extend"]:
                    return True
        return False

    # Basic fallback methods

    def _emit_module_basic(self, source_code: str) -> str:
        """Basic C module generation fallback."""
        # Parse AST
        tree = ast.parse(source_code)

        # Generate includes
        includes = [
            "#include <stdio.h>",
            "#include <stdlib.h>",
            "#include <stdbool.h>",
        ]

        # Generate functions
        functions = []
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                type_context = self._infer_simple_types(node)
                func_code = self._emit_function_basic(node, type_context)
                functions.append(func_code)

        # Add main function if not present
        if not any("main" in func for func in functions):
            main_func = """int main() {
    printf("Hello from MultiGen-generated C code!\\n");
    return 0;
}"""
            functions.append(main_func)

        # Combine all parts
        parts = includes + [""] + functions
        return "\n".join(parts)

    def _emit_function_basic(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Basic C function generation fallback."""
        func_name = func_node.name

        # Build parameter list
        params = []
        for arg in func_node.args.args:
            param_type = self.type_map.get(type_context.get(arg.arg, "int"), "int")
            params.append(f"{param_type} {arg.arg}")

        # Get return type
        return_type = self.type_map.get(type_context.get("__return__", "void"), "void")

        # Build function signature
        params_str = ", ".join(params) if params else "void"
        signature = f"{return_type} {func_name}({params_str})"

        # Generate function body
        body = self._emit_function_body_basic(func_node, type_context)

        return f"{signature} {{\n{body}\n}}"

    def _emit_function_body_basic(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Basic function body generation fallback."""
        if len(func_node.body) == 1 and isinstance(func_node.body[0], ast.Return):
            return_node = func_node.body[0]
            if isinstance(return_node.value, ast.BinOp):
                if isinstance(return_node.value.op, ast.Add):
                    left = self._emit_expression_basic(return_node.value.left)
                    right = self._emit_expression_basic(return_node.value.right)
                    return f"    return {left} + {right};"
                elif isinstance(return_node.value.op, ast.Mult):
                    left = self._emit_expression_basic(return_node.value.left)
                    right = self._emit_expression_basic(return_node.value.right)
                    return f"    return {left} * {right};"
            elif isinstance(return_node.value, ast.Name):
                return f"    return {return_node.value.id};"
            elif isinstance(return_node.value, ast.Constant):
                val = return_node.value.value
                if isinstance(val, bytes):
                    return f"    return {val!r};"
                else:
                    return f"    return {val};"

        raise UnsupportedFeatureError("Complex function body structure not supported in basic emitter mode")

    def _emit_expression_basic(self, expr: ast.expr) -> str:
        """Basic expression generation fallback."""
        if isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.Constant):
            return str(expr.value)
        elif isinstance(expr, ast.BinOp):
            left = self._emit_expression_basic(expr.left)
            right = self._emit_expression_basic(expr.right)
            if isinstance(expr.op, ast.Add):
                return f"({left} + {right})"
            elif isinstance(expr.op, ast.Mult):
                return f"({left} * {right})"
            elif isinstance(expr.op, ast.Sub):
                return f"({left} - {right})"
            elif isinstance(expr.op, ast.Div):
                return f"({left} / {right})"
        return "0"

    def _infer_simple_types(self, func_node: ast.FunctionDef) -> dict[str, str]:
        """Infer simple types from function annotations."""
        type_context = {}

        for arg in func_node.args.args:
            if arg.annotation and isinstance(arg.annotation, ast.Name):
                type_context[arg.arg] = arg.annotation.id

        if func_node.returns and isinstance(func_node.returns, ast.Name):
            type_context["__return__"] = func_node.returns.id

        return type_context
