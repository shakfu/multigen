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

from __future__ import annotations

import ast
from typing import Any, Optional

from .string_methods import CStringMethodConverter, CFStringConverter, is_string_method
from ..converter_utils import (
    get_augmented_assignment_operator,
    get_standard_binary_operator,
    get_standard_comparison_operator,
    get_standard_unary_operator,
)
from ..errors import TypeMappingError, UnsupportedFeatureError
from ..preferences import BackendPreferences, CPreferences
from .container_codegen import ContainerCodeGenerator
from .containers import CContainerSystem
from .enhanced_type_inference import EnhancedTypeInferenceEngine, InferredType, TypeConfidence
from .ext.stc.nested_containers import NestedContainerManager


class MultiGenPythonToCConverter:
    """Enhanced Python to C converter with MultiGen runtime integration."""

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize converter with MultiGen runtime support.

        Args:
            preferences: Backend preferences for controlling code generation
        """
        # Initialize preferences
        if preferences is None:
            preferences = CPreferences()
        self.preferences = preferences

        # Initialize container code generator for generated mode
        self.container_generator = ContainerCodeGenerator()

        self.type_mapping = {
            "int": "int",
            "float": "double",
            "bool": "bool",
            "str": "char*",
            "None": "void",
            "list": "vec_int",  # Default, will be specialized
            "dict": "map_str_int",  # Default, will be specialized
            "set": "set_int",  # Default, will be specialized
        }
        self.container_system = CContainerSystem()
        self.current_function: Optional[str] = None
        self.current_function_ast: Optional[ast.FunctionDef] = None
        self.container_variables: dict[str, dict[str, Any]] = {}
        self.variable_context: dict[str, str] = {}  # var_name -> c_type
        self.defined_structs: dict[str, dict[str, Any]] = {}
        self.iterator_variables: dict[str, str] = {}
        self.includes_needed: set[str] = set()
        self.use_runtime = True

        # NEW: Enhanced type inference engine
        self.type_engine = EnhancedTypeInferenceEngine()
        self.inferred_types: dict[str, InferredType] = {}

        # NEW: Nested container support
        self.nested_container_manager = NestedContainerManager()
        self.nested_containers: set[str] = set()  # Track which variables are nested containers

        # Track function return types for better inference
        self.function_return_types: dict[str, str] = {}

        # String method converter (extracted for maintainability)
        self._string_converter = CStringMethodConverter()

    def convert_code(self, source_code: str) -> str:
        """Convert Python source code to C code."""
        try:
            tree = ast.parse(source_code)
            return self._convert_module(tree)
        except (UnsupportedFeatureError, TypeMappingError):
            # Re-raise our specific exceptions without wrapping
            raise
        except Exception as e:
            raise UnsupportedFeatureError(f"Failed to convert Python code: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to C code."""
        parts = []

        # NEW: Phase 0 - Enhanced type inference (multi-pass analysis)
        self.inferred_types = self.type_engine.analyze_module(node)

        # Get inference statistics for debugging
        self.type_engine.get_inference_statistics()
        # Debug: Could log stats here if needed

        # Check for comprehensions to enable STC support
        self.uses_comprehensions = self._uses_comprehensions(node)

        # First pass: check for string methods to populate includes_needed
        self._detect_string_methods(node)

        # First pass: detect asserts for include generation
        self.uses_asserts = self._detect_asserts(node)

        # First pass: process imports for include generation
        self._detect_imports(node)

        # First pass: detect type casts for include generation
        self._detect_type_casts(node)

        # First pass: detect container variables to generate STC declarations
        self._detect_container_variables(node)

        # Add includes
        parts.extend(self._generate_includes())
        parts.append("")

        # Check container mode preference
        container_mode = self.preferences.get("container_mode", "runtime")

        # Add container implementation (runtime includes or generated code)
        if self._uses_containers(node) or self.uses_comprehensions or self.container_variables:
            if container_mode == "generated":
                # Generate inline container implementations
                parts.extend(self._generate_inline_containers())
                parts.append("")
            else:
                # Default: Use runtime library with STC declarations
                parts.extend(self._generate_container_declarations())
                parts.append("")

        # Convert functions and classes
        for stmt in node.body:
            if isinstance(stmt, ast.Import):
                # Process import statements (may add includes)
                self._process_import(stmt)
            elif isinstance(stmt, ast.ImportFrom):
                # Process from...import statements (may add includes)
                self._process_from_import(stmt)
            elif isinstance(stmt, ast.FunctionDef):
                parts.append(self._convert_function(stmt))
                parts.append("")
            elif isinstance(stmt, ast.ClassDef):
                parts.append(self._convert_class(stmt))
                parts.append("")

        # Add main function if not present
        if not any("main" in part for part in parts):
            parts.append(self._generate_main_function())

        return "\n".join(parts)

    def _detect_string_methods(self, node: ast.AST) -> None:
        """Pre-scan AST to detect string method usage for include generation."""
        for child in ast.walk(node):
            if isinstance(child, ast.Call) and isinstance(child.func, ast.Attribute):
                method_name = child.func.attr

                # Check if this looks like a string method call
                if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                    # For pre-scan, we're more liberal - assume any call to these methods is a string method
                    self.includes_needed.add('#include "multigen_string_ops.h"')
                    break  # Only need to add it once

    def _detect_asserts(self, node: ast.AST) -> bool:
        """Check if module uses assert statements.

        Args:
            node: AST node to scan

        Returns:
            True if assert statements are found
        """
        for child in ast.walk(node):
            if isinstance(child, ast.Assert):
                return True
        return False

    def _detect_imports(self, node: ast.Module) -> None:
        """Pre-scan module to detect imports and add necessary includes.

        Args:
            node: Module node to scan
        """
        for stmt in node.body:
            if isinstance(stmt, ast.Import):
                self._process_import(stmt)
            elif isinstance(stmt, ast.ImportFrom):
                self._process_from_import(stmt)

    def _detect_type_casts(self, node: ast.Module) -> None:
        """Pre-scan module to detect type casts and add necessary includes.

        Args:
            node: Module node to scan
        """
        for child in ast.walk(node):
            if isinstance(child, ast.Call) and isinstance(child.func, ast.Name):
                func_name = child.func.id
                # Check for str() calls which need multigen_string_ops.h
                if func_name == "str":
                    self.includes_needed.add('#include "multigen_string_ops.h"')
                    # No need to continue scanning once we found it
                    break

    def _process_import(self, node: ast.Import) -> None:
        """Process import statement and add necessary C includes.

        Args:
            node: Import statement node

        Example:
            import math  →  adds #include <math.h> to includes_needed
            import typing  →  ignored (type-only import)
        """
        for alias in node.names:
            module_name = alias.name

            # Handle standard library modules that need C includes
            if module_name == "math":
                self.includes_needed.add("#include <math.h>")
            # typing, dataclasses, etc. are type-only imports - no C equivalent needed
            # Local module imports would need generated headers, but not implemented yet

    def _process_from_import(self, node: ast.ImportFrom) -> None:
        """Process from...import statement and add necessary C includes.

        Args:
            node: ImportFrom statement node

        Example:
            from math import sqrt  →  adds #include <math.h>
            from typing import NamedTuple  →  ignored (type-only)
            from dataclasses import dataclass  →  ignored (type-only)
        """
        if not node.module:
            return  # from __future__ import ... or relative imports

        module_name = node.module

        # Handle standard library modules that need C includes
        if module_name == "math":
            self.includes_needed.add("#include <math.h>")
        # typing, dataclasses, etc. are type-only imports - no C equivalent needed
        # Local module imports would need generated headers, but not implemented yet

    def _detect_container_variables(self, node: ast.AST) -> None:
        """Pre-scan AST to detect container variable declarations for STC generation."""
        # First pass: detect nested container patterns
        self._detect_nested_containers(node)

        for child in ast.walk(node):
            # Look for annotated assignments: var: list = ...
            if isinstance(child, ast.AnnAssign) and isinstance(child.target, ast.Name):
                var_name = child.target.id
                type_annotation = self._get_type_annotation(child.annotation)

                # Track list, dict, and set declarations
                if type_annotation in ["list", "dict", "set"]:
                    if var_name not in self.container_variables:
                        self.container_variables[var_name] = {
                            "type": type_annotation,
                            "element_type": "int",  # Default to int
                            "c_type": self.type_mapping.get(type_annotation, type_annotation),
                        }

    def _detect_nested_containers(self, node: ast.AST) -> None:
        """Detect patterns like matrix.append(row) or result[i][j] = value to identify 2D containers."""
        # First, collect all annotated list variables AND function parameters with list type
        list_vars = set()
        for child in ast.walk(node):
            if isinstance(child, ast.AnnAssign) and isinstance(child.target, ast.Name):
                type_annotation = self._get_type_annotation(child.annotation)
                if type_annotation == "list":
                    list_vars.add(child.target.id)
            # Also collect function parameters with list type
            elif isinstance(child, ast.FunctionDef):
                for arg in child.args.args:
                    if arg.annotation:
                        param_type = self._get_type_annotation(arg.annotation)
                        if param_type == "list":
                            list_vars.add(arg.arg)

        # Pattern 1: Look for list.append(other_list)
        for child in ast.walk(node):
            if isinstance(child, ast.Call):
                if isinstance(child.func, ast.Attribute) and child.func.attr == "append":
                    if isinstance(child.func.value, ast.Name) and len(child.args) == 1:
                        outer_var = child.func.value.id
                        arg_expr = child.args[0]

                        # Check if argument is a list variable
                        if isinstance(arg_expr, ast.Name):
                            inner_var = arg_expr.id
                            # Only mark as nested if BOTH are list variables
                            if outer_var in list_vars and inner_var in list_vars:
                                self.nested_containers.add(outer_var)

        # Pattern 2: Look for 2D subscript assignments like result[i][j] = value
        for child in ast.walk(node):
            if isinstance(child, ast.Assign):
                if isinstance(child.targets[0], ast.Subscript):
                    target = child.targets[0]
                    # Check if target is a nested subscript (e.g., result[i][j])
                    if isinstance(target.value, ast.Subscript):
                        if isinstance(target.value.value, ast.Name):
                            var_name = target.value.value.id
                            if var_name in list_vars:
                                self.nested_containers.add(var_name)

        # Pattern 3: Look for 2D subscript reads like a[i][j]
        for child in ast.walk(node):
            if isinstance(child, ast.Subscript):
                if isinstance(child.value, ast.Subscript):
                    if isinstance(child.value.value, ast.Name):
                        var_name = child.value.value.id
                        if var_name in list_vars:
                            self.nested_containers.add(var_name)

    def _generate_includes(self) -> list[str]:
        """Generate C includes with MultiGen runtime support."""
        includes = [
            "#include <stdio.h>",
            "#include <stdlib.h>",
            "#include <stdbool.h>",
        ]

        # Add assert.h if asserts are used
        if hasattr(self, 'uses_asserts') and self.uses_asserts:
            includes.append("#include <assert.h>")

        if self.use_runtime:
            includes.extend(
                [
                    '#include "multigen_error_handling.h"',
                    '#include "multigen_python_ops.h"',
                    '#include "multigen_memory_ops.h"',
                ]
            )

            if self.container_variables or self._needs_containers():
                includes.append('#include "multigen_stc_bridge.h"')

            # Check if we need vanilla C string-to-int map (only in runtime mode)
            container_mode = self.preferences.get("container_mode", "runtime")
            if self._uses_str_int_map() and container_mode == "runtime":
                includes.append('#include "multigen_str_int_map.h"')

        # Note: STC includes are handled in _generate_container_declarations()
        # with proper defines, so we don't include them here

        # Add dynamically needed includes
        includes.extend(sorted(self.includes_needed))

        return includes

    def _uses_str_int_map(self) -> bool:
        """Check if the code uses map_str_int (vanilla C string-to-int map)."""
        # Check inferred types
        for inferred in self.inferred_types.values():
            if inferred.c_type == "map_str_int":
                return True
        # Check variable context
        for c_type in self.variable_context.values():
            if c_type == "map_str_int" or c_type == "multigen_str_int_map_t*":
                return True
        return False

    def _generate_container_declarations(self) -> list[str]:
        """Generate STC container template declarations."""
        declarations = []
        declarations.append("// STC container declarations")
        declarations.append("#define STC_ENABLED")
        declarations.append("")

        # Collect all unique C types used (from both inferred types and variable context)
        c_types_used = set()

        # From inferred types
        for inferred in self.inferred_types.values():
            if inferred.c_type.startswith(("vec_", "map_", "set_")):
                c_types_used.add(inferred.c_type)

        # From variable context
        for c_type in self.variable_context.values():
            if c_type.startswith(("vec_", "map_", "set_")):
                c_types_used.add(c_type)

        # Analyze container types needed from existing variables (old approach for backward compat)
        container_types = set()
        for var_info in self.container_variables.values():
            if "element_type" in var_info:
                container_types.add(var_info["element_type"])

        # Add types for comprehensions (basic integer support for now)
        if hasattr(self, "uses_comprehensions") and self.uses_comprehensions:
            container_types.add("int")  # Most common case for comprehensions

        # Add nested container types
        if self.nested_containers:
            # Need vec_int first (inner type)
            c_types_used.add("vec_int")
            # Then vec_vec_int (outer type)
            c_types_used.add("vec_vec_int")

        # Generate declarations for each generic type (old approach)
        for element_type in container_types:
            sanitized = self._sanitize_type_name(element_type)

            # Vector declaration
            c_types_used.add(f"vec_{sanitized}")
            # Map declaration (int -> int)
            c_types_used.add(f"map_{sanitized}_{sanitized}")
            # Set declaration
            c_types_used.add(f"set_{sanitized}")

        # Now generate STC declarations for all unique types
        # Sort to ensure vec_int comes before vec_vec_int
        generated_types = set()
        for c_type in sorted(c_types_used, key=lambda x: (x.count("_"), x)):
            if c_type in generated_types:
                continue
            generated_types.add(c_type)

            if c_type.startswith("vec_"):
                # Extract element type
                element_type = c_type[4:]  # Remove "vec_"
                # Special case: vec_cstr needs cstr.h for cstr_lit() macro
                if element_type == "cstr":
                    declarations.extend(
                        [
                            "#define i_implement",  # Enable cstr implementation
                            '#include "stc/cstr.h"',
                            "#undef i_implement",
                            "",
                        ]
                    )
                declarations.extend(
                    [
                        f"#define i_type {c_type}",
                        f"#define i_key {element_type}",
                        '#include "stc/vec.h"',
                        "#undef i_type",
                        "#undef i_key",
                        "",
                    ]
                )
            elif c_type.startswith("map_"):
                # Parse key and value types from "map_key_val" format
                parts = c_type[4:].split("_", 1)  # Remove "map_" and split
                if len(parts) == 2:
                    key_type, val_type = parts
                    # Handle special case of string keys - use vanilla C implementation
                    if key_type == "str":
                        # Skip - will use multigen_str_int_map_t* directly, no STC needed
                        # The type mapping and includes are handled elsewhere
                        pass
                    else:
                        declarations.extend(
                            [
                                f"#define i_type {c_type}",
                                f"#define i_key {key_type}",
                                f"#define i_val {val_type}",
                                '#include "stc/hmap.h"',
                                "#undef i_type",
                                "#undef i_key",
                                "#undef i_val",
                                "",
                            ]
                        )
            elif c_type.startswith("set_"):
                # Extract element type
                element_type = c_type[4:]  # Remove "set_"
                declarations.extend(
                    [
                        f"#define i_type {c_type}",
                        f"#define i_key {element_type}",
                        '#include "stc/hset.h"',
                        "#undef i_type",
                        "#undef i_key",
                        "",
                    ]
                )

        return declarations

    def _generate_inline_containers(self) -> list[str]:
        """Generate inline container implementations instead of STC includes.

        This is the prototype implementation that generates complete container
        code inline, eliminating external dependencies.

        Returns:
            List of code lines with generated container implementations
        """
        code_lines = []

        # Collect all unique C types used (same logic as _generate_container_declarations)
        c_types_used = set()

        # From inferred types
        for inferred in self.inferred_types.values():
            if inferred.c_type.startswith(("vec_", "map_", "set_")):
                c_types_used.add(inferred.c_type)

        # From variable context
        for c_type in self.variable_context.values():
            if c_type.startswith(("vec_", "map_", "set_")):
                c_types_used.add(c_type)

        # Analyze container types needed from existing variables
        container_types = set()
        for var_info in self.container_variables.values():
            if "element_type" in var_info:
                container_types.add(var_info["element_type"])

        # Add types for comprehensions
        if hasattr(self, "uses_comprehensions") and self.uses_comprehensions:
            container_types.add("int")

        # Add nested container types
        if self.nested_containers:
            c_types_used.add("vec_int")
            c_types_used.add("vec_vec_int")

        # Generate declarations for each generic type (old approach)
        for element_type in container_types:
            sanitized = self._sanitize_type_name(element_type)
            c_types_used.add(f"vec_{sanitized}")
            c_types_used.add(f"map_{sanitized}_{sanitized}")
            c_types_used.add(f"set_{sanitized}")

        # Generate inline implementations for containers
        generated_containers = set()

        # Generate vec_int first if needed (vec_vec_int depends on it)
        if "vec_vec_int" in c_types_used and "vec_int" not in c_types_used:
            c_types_used.add("vec_int")

        for c_type in sorted(c_types_used):
            if c_type in generated_containers:
                continue

            # Generate supported container types
            # Note: vec_int must be generated before vec_vec_int (dependency)
            if c_type in [
                "map_str_int",
                "vec_int",
                "set_int",
                "map_int_int",
                "vec_vec_int",
                "vec_cstr",
                "vec_float",
                "vec_double",
                "map_str_str",
                "set_str",
            ]:
                generated_code = self.container_generator.generate_container(c_type)
                if generated_code:
                    code_lines.append(generated_code)
                    generated_containers.add(c_type)
            # For other containers, fall back to STC (for now)
            # Future: Generate parameterized containers for arbitrary types

        return code_lines

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert Python function to C function."""
        self.current_function = node.name
        self.current_function_ast = node

        # Build parameter list
        params = []
        for arg in node.args.args:
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "int"
            # Use enhanced type inference for complex types like list[list[int]]
            if param_type in self.type_mapping:
                c_type = self.type_mapping[param_type]
            else:
                # Use enhanced type inference engine for complex/nested types
                c_type = self.type_engine._map_python_to_c_type(param_type)

            # Check if this parameter is detected as a nested container
            if arg.arg in self.nested_containers and c_type == "vec_int":
                c_type = "vec_vec_int"

            # Special case: map_str_int uses pointer type
            if c_type == "map_str_int":
                c_type = "multigen_str_int_map_t*"

            params.append(f"{c_type} {arg.arg}")
            self.variable_context[arg.arg] = c_type

        # Get return type
        return_type = "void"
        if node.returns:
            py_return_type = self._get_type_annotation(node.returns)
            # Use enhanced type inference for complex types like list[list[int]]
            if py_return_type in self.type_mapping:
                return_type = self.type_mapping[py_return_type]
            else:
                # Use enhanced type inference engine for complex/nested types
                return_type = self.type_engine._map_python_to_c_type(py_return_type)

            # Special case: map_str_int uses pointer type
            if return_type == "map_str_int":
                return_type = "multigen_str_int_map_t*"

            # Check if any returned variable is a nested container
            if return_type == "vec_int":
                for stmt in ast.walk(node):
                    if isinstance(stmt, ast.Return) and stmt.value:
                        # Get the returned variable name
                        if isinstance(stmt.value, ast.Name):
                            returned_var = stmt.value.id
                            if returned_var in self.nested_containers:
                                return_type = "vec_vec_int"
                                break

        # Build function signature
        params_str = ", ".join(params) if params else "void"
        signature = f"{return_type} {node.name}({params_str})"

        # Store function return type for call site inference
        self.function_return_types[node.name] = return_type

        # Convert function body
        body_lines = []
        for stmt in node.body:
            converted = self._convert_statement(stmt)
            if converted:
                body_lines.extend(converted.split("\n"))

        # Format function
        body = "\n".join(f"    {line}" if line.strip() else "" for line in body_lines)

        self.current_function = None
        self.current_function_ast = None
        return f"{signature} {{\n{body}\n}}"

    def _convert_statement(self, stmt: ast.stmt) -> str:
        """Convert Python statement to C code."""
        if isinstance(stmt, ast.Return):
            return self._convert_return(stmt)
        elif isinstance(stmt, ast.Assign):
            return self._convert_assignment(stmt)
        elif isinstance(stmt, ast.AnnAssign):
            return self._convert_annotated_assignment(stmt)
        elif isinstance(stmt, ast.AugAssign):
            return self._convert_augmented_assignment(stmt)
        elif isinstance(stmt, ast.If):
            return self._convert_if(stmt)
        elif isinstance(stmt, ast.While):
            return self._convert_while(stmt)
        elif isinstance(stmt, ast.For):
            return self._convert_for(stmt)
        elif isinstance(stmt, ast.Expr):
            return self._convert_expression_statement(stmt)
        elif isinstance(stmt, ast.ClassDef):
            return self._convert_class(stmt)
        elif isinstance(stmt, ast.Assert):
            return self._convert_assert(stmt)
        elif isinstance(stmt, ast.Pass):
            return "/* pass */"
        else:
            raise UnsupportedFeatureError(f"Unsupported statement type: {type(stmt).__name__}")

    def _convert_return(self, stmt: ast.Return) -> str:
        """Convert return statement.

        Special handling for main(): In C, main() should return 0 for success.
        Python programs may return meaningful values, but Unix convention is
        0 = success, non-zero = failure.
        """
        if stmt.value is None:
            return "return;"

        # Special case: main() should always return 0 for Unix compatibility
        if self.current_function == "main":
            # If returning a value from main, just return 0 instead
            return "return 0;"

        value_expr = self._convert_expression(stmt.value)
        return f"return {value_expr};"

    def _convert_assignment(self, stmt: ast.Assign) -> str:
        """Convert assignment statement."""
        if len(stmt.targets) != 1:
            raise UnsupportedFeatureError("Multiple assignment targets not supported")

        target = stmt.targets[0]

        # Handle attribute assignment (e.g., self.attr = value or obj.attr = value)
        if isinstance(target, ast.Attribute):
            obj = self._convert_expression(target.value)
            attr_name = target.attr
            value_expr = self._convert_expression(stmt.value)

            # If this is a self reference in a method, use pointer access
            if (
                isinstance(target.value, ast.Name)
                and target.value.id == "self"
                and self.current_function
                and "_" in self.current_function
            ):
                return f"self->{attr_name} = {value_expr};"
            else:
                return f"{obj}.{attr_name} = {value_expr};"

        # Handle subscript assignment (e.g., dict[key] = value, list[i] = value, matrix[i][j] = value)
        elif isinstance(target, ast.Subscript):
            index = self._convert_expression(target.slice)
            value_expr = self._convert_expression(stmt.value)

            # Check if this is a nested subscript (e.g., result[i][j] = value)
            if isinstance(target.value, ast.Subscript):
                # Nested subscript assignment
                inner_index = self._convert_expression(target.value.slice)
                if isinstance(target.value.value, ast.Name):
                    var_name = target.value.value.id
                    c_type = None

                    # Check variable type (prioritize variable_context over inferred_types)
                    if var_name in self.variable_context:
                        c_type = self.variable_context[var_name]
                    elif var_name in self.inferred_types:
                        c_type = self.inferred_types[var_name].c_type

                    if c_type == "vec_vec_int":
                        # 2D array assignment: get pointer to inner vec, then assign to its data
                        return f"vec_vec_int_at(&{var_name}, {inner_index})->data[{index}] = {value_expr};"
                    else:
                        # Variable not properly typed as nested container
                        raise UnsupportedFeatureError(
                            f"2D subscript assignment requires variable '{var_name}' to be vec_vec_int, got: {c_type}"
                        )
                else:
                    raise UnsupportedFeatureError("Nested subscript assignment only supported for simple variables")

            # Single subscript assignment
            obj = self._convert_expression(target.value)

            # Determine container type
            if isinstance(target.value, ast.Name):
                var_name = target.value.id
                c_type = None

                # Check variable_context first (more reliable for explicit declarations)
                if var_name in self.variable_context:
                    c_type = self.variable_context[var_name]
                # Fallback to inferred types
                elif var_name in self.inferred_types:
                    c_type = self.inferred_types[var_name].c_type

                if c_type:
                    if c_type == "map_str_int":
                        # Vanilla C string map: multigen_str_int_map_insert(dict, key, value)
                        return f"multigen_str_int_map_insert({obj}, {index}, {value_expr});"
                    elif c_type.startswith("map_"):
                        # STC dictionary assignment: map_int_int_insert(&dict, key, value)
                        return f"{c_type}_insert(&{obj}, {index}, {value_expr});"
                    elif c_type.startswith("vec_"):
                        # List assignment: vec[i] = value (direct indexing)
                        return f"{obj}.data[{index}] = {value_expr};"

            raise UnsupportedFeatureError("Subscript assignment not supported for this container type")

        # Handle simple variable assignment
        elif isinstance(target, ast.Name):
            var_name = target.id
            value_expr = self._convert_expression(stmt.value)

            # Check if variable already declared (prevent shadowing)
            if var_name in self.variable_context:
                # Variable already declared - just reassign
                return f"{var_name} = {value_expr};"
            else:
                # New variable - infer type
                # NEW: Try enhanced inference first
                if var_name in self.inferred_types:
                    inferred = self.inferred_types[var_name]
                    if inferred.confidence >= TypeConfidence.MEDIUM.value:
                        inferred_type = inferred.c_type
                    else:
                        # Fallback to basic inference
                        inferred_type = self._infer_expression_type(stmt.value)
                else:
                    inferred_type = self._infer_expression_type(stmt.value)

                self.variable_context[var_name] = inferred_type
                return f"{inferred_type} {var_name} = {value_expr};"

        else:
            raise UnsupportedFeatureError("Only simple variable and attribute assignment supported")

    def _convert_annotated_assignment(self, stmt: ast.AnnAssign) -> str:
        """Convert annotated assignment (e.g., x: int = 5 or self.x: int = 5)."""
        # Handle attribute annotation (e.g., self.attr: type = value)
        if isinstance(stmt.target, ast.Attribute):
            obj = self._convert_expression(stmt.target.value)
            attr_name = stmt.target.attr
            type_annotation = self._get_type_annotation(stmt.annotation)
            c_type = self.type_mapping.get(type_annotation, type_annotation)

            if stmt.value:
                value_expr = self._convert_expression(stmt.value)
                # If this is a self reference in a method, use pointer access
                if (
                    isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                    and self.current_function
                    and "_" in self.current_function
                ):
                    return f"self->{attr_name} = {value_expr};"
                else:
                    return f"{obj}.{attr_name} = {value_expr};"
            else:
                # Just a type annotation without assignment
                return f"/* {attr_name}: {c_type} */"

        # Handle simple variable annotation
        elif isinstance(stmt.target, ast.Name):
            var_name = stmt.target.id
            type_annotation = self._get_type_annotation(stmt.annotation)

            # Check if the value expression has a more specific type than the annotation
            # (e.g., words: list = text.split() should be multigen_string_array_t*, not vec_int)
            c_type = None

            # For empty dict/list/set literals, prefer enhanced type inference over defaults
            is_empty_literal = False
            if stmt.value:
                if isinstance(stmt.value, ast.Dict) and len(stmt.value.keys) == 0:
                    is_empty_literal = True
                elif isinstance(stmt.value, ast.List) and len(stmt.value.elts) == 0:
                    is_empty_literal = True
                elif isinstance(stmt.value, ast.Set) and len(stmt.value.elts) == 0:
                    is_empty_literal = True
                elif (
                    isinstance(stmt.value, ast.Call)
                    and isinstance(stmt.value.func, ast.Name)
                    and stmt.value.func.id == "set"
                    and len(stmt.value.args) == 0
                ):
                    # set() constructor call is also an empty literal
                    is_empty_literal = True

            # For empty dict literals, scan forward to find first subscript assignment to infer type
            # Do this BEFORE enhanced type inference because it's more accurate for this case
            if isinstance(stmt.value, ast.Dict) and len(stmt.value.keys) == 0:
                inferred_dict_type = self._infer_dict_type_from_usage(var_name)
                if inferred_dict_type:
                    c_type = inferred_dict_type

            # Try enhanced type inference for empty literals if usage-based didn't work
            if not c_type and is_empty_literal and var_name in self.inferred_types:
                inferred = self.inferred_types[var_name]
                # Use inferred type if confidence is at least MEDIUM
                if inferred.confidence >= TypeConfidence.MEDIUM.value:
                    c_type = inferred.c_type

            # Otherwise, check value expression type
            if not c_type and stmt.value:
                value_type = self._infer_expression_type(stmt.value)
                # If the value type is more specific than a generic container, use it
                if value_type in ["multigen_string_array_t*", "char*"] and type_annotation == "list":
                    c_type = value_type
                elif value_type.startswith(("map_", "vec_", "set_", "multigen_")) and value_type != "vec_int":
                    # Use specific STC type or multigen custom type if not the generic default
                    c_type = value_type

            # If we didn't get a specific type from value, try enhanced type inference
            if not c_type:
                if var_name in self.inferred_types:
                    inferred = self.inferred_types[var_name]
                    # Use inferred type if confidence is high
                    if inferred.confidence >= TypeConfidence.HIGH.value:
                        c_type = inferred.c_type
                    else:
                        # Fallback to basic mapping
                        c_type = self.type_mapping.get(type_annotation, type_annotation)
                else:
                    c_type = self.type_mapping.get(type_annotation, type_annotation)

            # Check if this is a nested container (e.g., matrix: list that appends other lists)
            if var_name in self.nested_containers and type_annotation == "list":
                c_type = "vec_vec_int"

            self.variable_context[var_name] = c_type

            # Track container variables for STC declaration generation
            if type_annotation in ["list", "dict", "set"]:
                # Default to int element type for containers
                if var_name not in self.container_variables:
                    self.container_variables[var_name] = {
                        "type": type_annotation,
                        "element_type": "int",
                        "c_type": c_type,
                    }

            if stmt.value:
                # Special handling for list/dict/set literals with STC containers
                if isinstance(stmt.value, ast.List) and c_type.startswith("vec_"):
                    # Initialize list from literal: arr: list = [1, 2, 3]
                    # Generate: vec_int arr = {0}; vec_int_push(&arr, 1); vec_int_push(&arr, 2); ...
                    statements = [f"{c_type} {var_name} = {{0}};"]
                    for element in stmt.value.elts:
                        element_code = self._convert_expression(element)
                        # For vec_cstr, wrap string literals with cstr_lit()
                        if c_type == "vec_cstr" and isinstance(element, ast.Constant) and isinstance(element.value, str):
                            element_code = f"cstr_lit({element_code})"
                        statements.append(f"{c_type}_push(&{var_name}, {element_code});")
                    return "\n".join(statements)

                elif isinstance(stmt.value, ast.Dict) and c_type.startswith("map_"):
                    # Initialize dict from literal: d: dict = {"key": 1, "key2": 2}
                    if c_type == "map_str_int":
                        # Vanilla C string map
                        statements = [f"multigen_str_int_map_t* {var_name} = multigen_str_int_map_new();"]
                        for key, value in zip(stmt.value.keys, stmt.value.values):
                            if key is not None:
                                key_code = self._convert_expression(key)
                                value_code = self._convert_expression(value)
                                statements.append(f"multigen_str_int_map_insert({var_name}, {key_code}, {value_code});")
                        return "\n".join(statements)
                    else:
                        # STC dictionary
                        statements = [f"{c_type} {var_name} = {{0}};"]
                        for key, value in zip(stmt.value.keys, stmt.value.values):
                            if key is not None:
                                key_code = self._convert_expression(key)
                                value_code = self._convert_expression(value)
                                statements.append(f"{c_type}_insert(&{var_name}, {key_code}, {value_code});")
                        return "\n".join(statements)

                elif isinstance(stmt.value, ast.Set) and c_type.startswith("set_"):
                    # Initialize set from literal: s: set = {1, 2, 3}
                    statements = [f"{c_type} {var_name} = {{0}};"]
                    for element in stmt.value.elts:
                        element_code = self._convert_expression(element)
                        statements.append(f"{c_type}_insert(&{var_name}, {element_code});")
                    return "\n".join(statements)

                else:
                    # Regular assignment
                    value_expr = self._convert_expression(stmt.value)
                    # Handle empty set() constructor placeholder
                    if value_expr == "/* EMPTY_SET_LITERAL */":
                        value_expr = "{0}"
                    # Special case for map_str_int: use pointer type
                    if c_type == "map_str_int":
                        # Empty dict initialization
                        if value_expr == "{0}" or "Dict" in value_expr:
                            return f"multigen_str_int_map_t* {var_name} = multigen_str_int_map_new();"
                        else:
                            return f"multigen_str_int_map_t* {var_name} = {value_expr};"
                    return f"{c_type} {var_name} = {value_expr};"
            else:
                # Declaration without initialization
                if c_type == "map_str_int":
                    return f"multigen_str_int_map_t* {var_name} = NULL;"
                return f"{c_type} {var_name};"

        else:
            raise UnsupportedFeatureError("Only simple variable and attribute annotation supported")

    def _convert_augmented_assignment(self, stmt: ast.AugAssign) -> str:
        """Convert augmented assignment (+=, -=, etc.) to C syntax."""
        # Handle C-specific operators
        op_str: str
        if isinstance(stmt.op, ast.FloorDiv):
            op_str = "/="  # Floor division maps to regular division in C
        else:
            # Use standard augmented assignment operator mapping from converter_utils
            op_result = get_augmented_assignment_operator(stmt.op)
            if op_result is None:
                raise UnsupportedFeatureError(f"Unsupported augmented assignment operator: {type(stmt.op).__name__}")
            op_str = op_result

        # Check if target is a simple variable
        if isinstance(stmt.target, ast.Name):
            var_name = stmt.target.id
            if var_name not in self.variable_context:
                raise TypeMappingError(f"Variable '{var_name}' must be declared before augmented assignment")

            value_expr = self._convert_expression(stmt.value)
            return f"{var_name} {op_str} {value_expr};"

        # Check if target is an attribute (e.g., self.attr += value or obj.attr += value)
        elif isinstance(stmt.target, ast.Attribute):
            obj = self._convert_expression(stmt.target.value)
            attr_name = stmt.target.attr
            value_expr = self._convert_expression(stmt.value)

            # Determine correct access operator (-> for pointers, . for structs)
            if obj == "self":
                return f"self->{attr_name} {op_str} {value_expr};"
            else:
                return f"{obj}.{attr_name} {op_str} {value_expr};"

        else:
            raise UnsupportedFeatureError("Only simple variable and attribute augmented assignments supported")

    def _convert_expression(self, expr: ast.expr) -> str:
        """Convert Python expression to C expression."""
        if isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        elif isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.BinOp):
            return self._convert_binary_op(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self._convert_unary_op(expr)
        elif isinstance(expr, ast.Compare):
            return self._convert_compare(expr)
        elif isinstance(expr, ast.BoolOp):
            return self._convert_boolop(expr)
        elif isinstance(expr, ast.Call):
            return self._convert_call(expr)
        elif isinstance(expr, ast.Attribute):
            return self._convert_attribute(expr)
        elif isinstance(expr, ast.Subscript):
            return self._convert_subscript(expr)
        elif isinstance(expr, ast.ListComp):
            return self._convert_list_comprehension(expr)
        elif isinstance(expr, ast.DictComp):
            return self._convert_dict_comprehension(expr)
        elif isinstance(expr, ast.SetComp):
            return self._convert_set_comprehension(expr)
        elif isinstance(expr, ast.List):
            return self._convert_list_literal(expr)
        elif isinstance(expr, ast.Dict):
            return self._convert_dict_literal(expr)
        elif isinstance(expr, ast.Set):
            return self._convert_set_literal(expr)
        elif isinstance(expr, ast.JoinedStr):
            return self._convert_f_string(expr)
        else:
            return f"/* Unsupported expression {type(expr).__name__} */"

    def _convert_constant(self, expr: ast.Constant) -> str:
        """Convert constant values."""
        if isinstance(expr.value, str):
            return f'"{expr.value}"'
        elif isinstance(expr.value, bool):
            return "true" if expr.value else "false"
        else:
            return str(expr.value)

    def _convert_binary_op(self, expr: ast.BinOp) -> str:
        """Convert binary operations."""
        left = self._convert_expression(expr.left)
        right = self._convert_expression(expr.right)

        # Handle C-specific operators
        if isinstance(expr.op, ast.Pow):
            # Pow requires math.h
            self.includes_needed.add("#include <math.h>")
            return f"pow({left}, {right})"
        elif isinstance(expr.op, ast.FloorDiv):
            # FloorDiv maps to regular division in C (not exact for negative numbers)
            return f"({left} / {right})"
        elif isinstance(expr.op, ast.Add) and (self._is_string_type(expr.left) or self._is_string_type(expr.right)):
            # String concatenation using multigen_str_concat
            self.includes_needed.add('#include "multigen_string_ops.h"')
            return f"multigen_str_concat({left}, {right})"
        else:
            # Use standard operator mapping from converter_utils for common operators
            op = get_standard_binary_operator(expr.op)
            if op is None:
                raise UnsupportedFeatureError(f"Unsupported binary operator: {type(expr.op)}")
            return f"({left} {op} {right})"

    def _convert_unary_op(self, expr: ast.UnaryOp) -> str:
        """Convert unary operations."""
        operand = self._convert_expression(expr.operand)

        # Use standard operator mapping from converter_utils
        op = get_standard_unary_operator(expr.op)
        if op is None:
            raise UnsupportedFeatureError(f"Unsupported unary operator: {type(expr.op)}")
        return f"({op}{operand})"

    def _convert_compare(self, expr: ast.Compare) -> str:
        """Convert comparison operations."""
        if len(expr.ops) != 1 or len(expr.comparators) != 1:
            raise UnsupportedFeatureError("Only simple comparisons supported")

        # Handle 'in' and 'not in' operators for membership testing
        if isinstance(expr.ops[0], (ast.In, ast.NotIn)):
            left = self._convert_expression(expr.left)
            right_expr = expr.comparators[0]
            right = self._convert_expression(right_expr)
            is_not_in = isinstance(expr.ops[0], ast.NotIn)

            # Determine container type from variable context
            if isinstance(right_expr, ast.Name):
                var_name = right_expr.id
                c_type = None

                # Check variable_context first (more reliable for explicit declarations)
                if var_name in self.variable_context:
                    c_type = self.variable_context[var_name]
                # Fallback to inferred types
                elif var_name in self.inferred_types:
                    inferred = self.inferred_types[var_name]
                    c_type = inferred.c_type

                if c_type:
                    result = None
                    if c_type in ["str", "char*", "const char*"] or "char*" in c_type:
                        # String membership: use strstr()
                        # Need to add string.h if not already present
                        self.includes_needed.add("#include <string.h>")
                        result = f"(strstr({right}, {left}) != NULL)"
                    elif c_type == "map_str_int":
                        # Vanilla C string map
                        result = f"multigen_str_int_map_contains({right}, {left})"
                    elif c_type.startswith("map_"):
                        # STC dictionary membership: check if key exists
                        result = f"{c_type}_contains(&{right}, {left})"
                    elif c_type.startswith("set_"):
                        # Set membership
                        result = f"{c_type}_contains(&{right}, {left})"
                    elif c_type.startswith("vec_"):
                        # List membership: use linear search helper
                        # Note: STC doesn't have built-in contains for vec, need helper
                        result = f"vec_contains_{c_type}(&{right}, {left})"

                    if result:
                        if is_not_in:
                            return f"(!{result})"
                        else:
                            return result

            raise UnsupportedFeatureError("'in' operator not supported for this container type")

        left = self._convert_expression(expr.left)
        right = self._convert_expression(expr.comparators[0])

        # Check if we're comparing strings
        left_is_string = self._is_string_type(expr.left)
        right_is_string = self._is_string_type(expr.comparators[0])

        if left_is_string or right_is_string:
            # String comparison - use strcmp()
            self.includes_needed.add("#include <string.h>")

            if isinstance(expr.ops[0], ast.Eq):
                return f"(strcmp({left}, {right}) == 0)"
            elif isinstance(expr.ops[0], ast.NotEq):
                return f"(strcmp({left}, {right}) != 0)"
            elif isinstance(expr.ops[0], ast.Lt):
                return f"(strcmp({left}, {right}) < 0)"
            elif isinstance(expr.ops[0], ast.LtE):
                return f"(strcmp({left}, {right}) <= 0)"
            elif isinstance(expr.ops[0], ast.Gt):
                return f"(strcmp({left}, {right}) > 0)"
            elif isinstance(expr.ops[0], ast.GtE):
                return f"(strcmp({left}, {right}) >= 0)"
            else:
                raise UnsupportedFeatureError(f"Unsupported string comparison: {type(expr.ops[0])}")

        # Use standard operator mapping from converter_utils
        op = get_standard_comparison_operator(expr.ops[0])
        if op is None:
            raise UnsupportedFeatureError(f"Unsupported comparison: {type(expr.ops[0])}")
        return f"({left} {op} {right})"

    def _convert_boolop(self, expr: ast.BoolOp) -> str:
        """Convert boolean operations (and/or).

        Args:
            expr: BoolOp node

        Returns:
            C boolean expression

        Example:
            x > 0 and y < 10  →  ((x > 0) && (y < 10))
            a or b  →  ((a) || (b))
        """
        # Determine operator
        if isinstance(expr.op, ast.And):
            op = "&&"
        elif isinstance(expr.op, ast.Or):
            op = "||"
        else:
            raise UnsupportedFeatureError(f"Unsupported boolean operator: {type(expr.op).__name__}")

        # Convert operands and wrap each in parentheses
        operands = [f"({self._convert_expression(val)})" for val in expr.values]

        # Join with operator and wrap the whole expression
        return f"({(' ' + op + ' ').join(operands)})"

    def _convert_call(self, expr: ast.Call) -> str:
        """Convert function calls."""
        if isinstance(expr.func, ast.Name):
            func_name = expr.func.id
            args = [self._convert_expression(arg) for arg in expr.args]

            # Check if this is a class instantiation
            if func_name in self.defined_structs:
                struct_info = self.defined_structs[func_name]
                # Check if it's a dataclass
                if struct_info.get("is_dataclass"):
                    # Dataclass instantiation: ClassName() -> make_ClassName()
                    args_str = ", ".join(args)
                    return f"make_{func_name}({args_str})"
                elif struct_info.get("is_namedtuple"):
                    # NamedTuple instantiation: ClassName() -> (ClassName){...}
                    # Use C99 compound literal syntax
                    args_str = ", ".join(args)
                    return f"({func_name}){{{args_str}}}"
                else:
                    # Regular class instantiation: ClassName() -> ClassName_new()
                    args_str = ", ".join(args)
                    return f"{func_name}_new({args_str})"

            # Handle set() constructor - return placeholder that will be replaced during assignment
            elif func_name == "set" and len(args) == 0:
                # Empty set literal - return a marker that assignment handling will replace
                return "/* EMPTY_SET_LITERAL */"

            # Handle type cast built-ins (but not bool - it uses runtime)
            elif func_name in ["float", "int", "str"]:
                return self._convert_type_cast(func_name, expr.args)

            # Handle built-in functions with runtime support
            elif func_name in ["len", "bool", "abs", "min", "max", "sum", "any", "all", "print"] and self.use_runtime:
                # Pass original AST args to print for type detection
                if func_name == "print":
                    return self._convert_print_call(expr.args, args)
                return self._convert_builtin_with_runtime(func_name, args)
            else:
                args_str = ", ".join(args)
                return f"{func_name}({args_str})"

        elif isinstance(expr.func, ast.Attribute):
            # Method call: obj.method() -> ClassName_method(&obj)
            return self._convert_method_call(expr)

        else:
            raise UnsupportedFeatureError("Only simple function calls and method calls supported")

    def _convert_type_cast(self, cast_type: str, args: list[ast.expr]) -> str:
        """Convert Python type cast to C cast.

        Args:
            cast_type: Type to cast to ('float', 'int', 'str')
            args: Arguments to the cast (should be exactly 1)

        Returns:
            C cast expression

        Example:
            float(x) → (double)x
            int(x) → (int)x
            str(x) → multigen_int_to_string(x)  [if x is int]

        Note:
            bool(x) is NOT handled here - it uses multigen_bool_int() runtime function
            because Python's bool() has specific truthiness semantics.
        """
        if len(args) != 1:
            raise UnsupportedFeatureError(f"Type cast {cast_type}() expects exactly 1 argument")

        arg = self._convert_expression(args[0])

        # Map Python types to C types
        if cast_type == "float":
            # Python float is C double
            return f"(double){arg}"
        elif cast_type == "int":
            return f"(int){arg}"
        elif cast_type == "str":
            # String conversion requires runtime function
            # Add string ops header if not already present
            self.includes_needed.add('#include "multigen_string_ops.h"')
            # For now, support int to string (most common case)
            # TODO: Add support for float to string, bool to string, etc.
            return f"multigen_int_to_string({arg})"
        else:
            raise UnsupportedFeatureError(f"Type cast {cast_type}() not supported")

    def _convert_builtin_with_runtime(self, func_name: str, args: list[str]) -> str:
        """Convert built-in functions using MultiGen runtime."""
        if func_name == "len":
            # For STC containers, call size function directly
            # Determine container type from variable context
            container_name = args[0]
            container_type = None

            # Check variable_context for the container type
            if container_name in self.variable_context:
                container_type = self.variable_context[container_name]
            elif container_name in self.inferred_types:
                container_type = self.inferred_types[container_name].c_type

            # Use the appropriate size function based on container type
            # Check for multigen custom types first (before generic STC types)
            if container_type == "multigen_str_int_map_t*":
                return f"multigen_str_int_map_size({container_name})"
            elif container_type and container_type.startswith("map_"):
                # STC map types
                return f"{container_type}_size(&{container_name})"
            elif container_type and container_type.startswith("set_"):
                return f"{container_type}_size(&{container_name})"
            elif container_type and container_type.startswith("vec_"):
                return f"{container_type}_size(&{container_name})"
            elif container_type and "multigen_string_array" in container_type:
                return f"multigen_string_array_size({container_name})"
            elif container_type and container_type in ("char*", "const char*", "string"):
                # String type - use strlen()
                self.includes_needed.add("#include <string.h>")
                return f"strlen({container_name})"
            else:
                # Default to vec_int for backward compatibility
                return f"vec_int_size(&{container_name})"
        elif func_name == "bool":
            return f"multigen_bool_int({args[0]})"
        elif func_name == "abs":
            return f"multigen_abs_int({args[0]})"
        elif func_name in ["min", "max", "sum"]:
            return f"multigen_{func_name}_int_array({args[0]}, {args[1]})"  # Simplified
        else:
            return f"{func_name}({', '.join(args)})"

    def _convert_print_call(self, ast_args: list[ast.expr], converted_args: list[str]) -> str:
        """Convert print() to printf() with appropriate format specifiers."""
        if len(ast_args) == 0:
            return 'printf("\\n")'

        if len(ast_args) == 1:
            # Single argument - detect type and use appropriate format
            arg_expr = ast_args[0]
            c_arg = converted_args[0]

            # Check if it's a string literal
            if isinstance(arg_expr, ast.Constant) and isinstance(arg_expr.value, str):
                return f'printf("%s\\n", {c_arg})'

            # Check variable type from context
            if isinstance(arg_expr, ast.Name):
                var_name = arg_expr.id
                if var_name in self.variable_context:
                    var_type = self.variable_context[var_name]
                    if var_type in ["str", "char*"]:
                        return f'printf("%s\\n", {c_arg})'
                    elif var_type == "double":
                        return f'printf("%f\\n", {c_arg})'
                    elif var_type == "bool":
                        return f'printf("%s\\n", {c_arg} ? "true" : "false")'

            # Default to integer
            return f'printf("%d\\n", {c_arg})'
        else:
            # Multiple arguments - use %d for all (simplified)
            format_specs = " ".join(["%d"] * len(ast_args))
            return f'printf("{format_specs}\\n", {", ".join(converted_args)})'

    def _convert_method_call(self, expr: ast.Call) -> str:
        """Convert method calls: obj.method(args) -> ClassName_method(&obj, args)."""
        if not isinstance(expr.func, ast.Attribute):
            raise UnsupportedFeatureError("Expected attribute access for method call")

        obj_expr = expr.func.value
        method_name = expr.func.attr

        # Check if this is a module function call (e.g., math.sqrt)
        if isinstance(obj_expr, ast.Name):
            module_name = obj_expr.id
            # Handle standard library module functions
            if module_name == "math":
                # math module functions are just regular C functions from math.h
                args = [self._convert_expression(arg) for arg in expr.args]
                args_str = ", ".join(args)
                return f"{method_name}({args_str})"

        # Convert the object and arguments
        obj = self._convert_expression(obj_expr)
        args = [self._convert_expression(arg) for arg in expr.args]

        # Check if this is a string method call
        if self._is_string_type(obj_expr):
            return self._convert_string_method(obj, method_name, args)

        # Check if this is a list method call
        if self._is_list_type(obj_expr):
            return self._convert_list_method(obj, method_name, args, obj_expr, expr.args)

        # Check if this is a set method call
        if self._is_set_type(obj_expr):
            return self._convert_set_method(obj, method_name, args, obj_expr)

        # Try to determine the class type from the object
        # For now, we'll use a simple heuristic - look for known class names
        class_name = None
        if isinstance(obj_expr, ast.Name):
            var_name = obj_expr.id
            # Look in our variable context for the type
            if var_name in self.variable_context:
                var_type = self.variable_context[var_name]
                if var_type in self.defined_structs:
                    class_name = var_type

        if class_name:
            # Method call with known class type
            args_str = ", ".join([f"&{obj}"] + args)
            return f"{class_name}_{method_name}({args_str})"
        else:
            # Fallback - assume it's a simple method call
            args_str = ", ".join(args)
            return f"{obj}_{method_name}({args_str})"

    def _is_string_type(self, expr: ast.expr) -> bool:
        """Check if expression represents a string type."""
        # Check if it's a string literal
        if isinstance(expr, ast.Constant) and isinstance(expr.value, str):
            return True

        # Check if it's a variable with string type
        if isinstance(expr, ast.Name):
            var_name = expr.id
            if var_name in self.variable_context:
                var_type = self.variable_context[var_name]
                return var_type in ["str", "char*", "const char*"] or "char*" in var_type

        # Check if it's a method call that returns a string (for chaining)
        if isinstance(expr, ast.Call) and isinstance(expr.func, ast.Attribute):
            method_name = expr.func.attr
            # String methods that return strings
            string_returning_methods = {"upper", "lower", "strip", "replace", "lstrip", "rstrip"}
            if method_name in string_returning_methods:
                # Check if the object being called on is a string
                return self._is_string_type(expr.func.value)

        # Check if it's an attribute access (e.g., self.text, obj.attr)
        if isinstance(expr, ast.Attribute):
            # For attribute access, look up the attribute in class definitions
            if isinstance(expr.value, ast.Name) and expr.value.id == "self":
                # This is self.attribute - check if we know this attribute is a string
                attr_name = expr.attr
                # Look through defined structs to find this attribute's type
                for _class_name, class_info in self.defined_structs.items():
                    if attr_name in class_info.get("attributes", {}):
                        attr_type = class_info["attributes"][attr_name]
                        return attr_type in ["str", "char*"]
            else:
                # For obj.attr, try to determine the object type
                obj_expr = expr.value
                if isinstance(obj_expr, ast.Name):
                    obj_name = obj_expr.id
                    if obj_name in self.variable_context:
                        obj_type = self.variable_context[obj_name]
                        if obj_type in self.defined_structs:
                            attr_name = expr.attr
                            class_info = self.defined_structs[obj_type]
                            if attr_name in class_info.get("attributes", {}):
                                attr_type = class_info["attributes"][attr_name]
                                return attr_type in ["str", "char*"]

        return False

    def _convert_string_method(self, obj: str, method_name: str, args: list[str]) -> str:
        """Convert string method calls to appropriate C code.

        Delegates to CStringMethodConverter for the actual conversion.
        """
        result = self._string_converter.convert_string_method(obj, method_name, args)
        # Merge includes needed by string converter
        self.includes_needed.update(self._string_converter.includes_needed)
        return result

    def _is_list_type(self, expr: ast.expr) -> bool:
        """Check if expression represents a list/vector type."""
        # Check if it's a list literal
        if isinstance(expr, ast.List):
            return True

        # Check if it's a variable with list type
        if isinstance(expr, ast.Name):
            var_name = expr.id

            # NEW: Check inferred types first (Phase 1.5)
            if var_name in self.inferred_types:
                inferred = self.inferred_types[var_name]
                if inferred.confidence >= TypeConfidence.MEDIUM.value:
                    return inferred.python_type == "list" or inferred.c_type.startswith("vec_")

            if var_name in self.variable_context:
                var_type = self.variable_context[var_name]
                return var_type.startswith("vec_") or var_type == "list"

        return False

    def _convert_list_method(self, obj: str, method_name: str, args: list[str], obj_expr: ast.expr, ast_args: list[ast.expr] | None = None) -> str:
        """Convert list method calls to STC vector operations."""
        # Get the variable name to determine the vec type
        vec_type = "vec_int"  # Default
        if isinstance(obj_expr, ast.Name):
            var_name = obj_expr.id

            # Check variable_context first (most reliable for local vars)
            if var_name in self.variable_context:
                vec_type = self.variable_context[var_name]
            # Then check inferred types (Phase 1.5)
            elif var_name in self.inferred_types:
                inferred = self.inferred_types[var_name]
                if inferred.confidence >= TypeConfidence.MEDIUM.value:
                    vec_type = inferred.c_type

        if method_name == "append":
            if len(args) != 1:
                raise UnsupportedFeatureError("list.append() requires exactly one argument")

            # For vec_cstr, wrap string literals with cstr_lit()
            arg_str = args[0]
            if vec_type == "vec_cstr" and ast_args and len(ast_args) == 1:
                arg_node = ast_args[0]
                if isinstance(arg_node, ast.Constant) and isinstance(arg_node.value, str):
                    # String literal - wrap with cstr_lit()
                    arg_str = f"cstr_lit({args[0]})"

            # vec_int_push(&data, value)
            return f"{vec_type}_push(&{obj}, {arg_str})"

        elif method_name == "extend":
            if len(args) != 1:
                raise UnsupportedFeatureError("list.extend() requires exactly one argument")
            # Not implemented yet - would need vec_int_append_range or similar
            raise UnsupportedFeatureError("list.extend() is not yet supported in C backend")

        elif method_name == "pop":
            # vec_int_pop(&data) - returns and removes last element
            if len(args) > 1:
                raise UnsupportedFeatureError("list.pop() takes at most one argument")
            if len(args) == 0:
                return f"*{vec_type}_back(&{obj}); {vec_type}_pop(&{obj})"
            else:
                raise UnsupportedFeatureError("list.pop(index) is not yet supported in C backend")

        elif method_name == "clear":
            if args:
                raise UnsupportedFeatureError("list.clear() takes no arguments")
            return f"{vec_type}_clear(&{obj})"

        else:
            raise UnsupportedFeatureError(f"Unsupported list method: {method_name}")

    def _is_set_type(self, expr: ast.expr) -> bool:
        """Check if expression represents a set type."""
        # Check if it's a set literal
        if isinstance(expr, ast.Set):
            return True

        # Check if it's a variable with set type
        if isinstance(expr, ast.Name):
            var_name = expr.id

            # Check inferred types first
            if var_name in self.inferred_types:
                inferred = self.inferred_types[var_name]
                if inferred.confidence >= TypeConfidence.MEDIUM.value:
                    return inferred.python_type == "set" or inferred.c_type.startswith("set_")

            # Check variable context
            if var_name in self.variable_context:
                var_type = self.variable_context[var_name]
                return var_type.startswith("set_") or var_type == "set"

        return False

    def _convert_set_method(self, obj: str, method_name: str, args: list[str], obj_expr: ast.expr) -> str:
        """Convert set method calls to STC hset operations."""
        # Get the variable name to determine the set type
        set_type = "set_int"  # Default
        if isinstance(obj_expr, ast.Name):
            var_name = obj_expr.id

            # Check variable_context first (most reliable for local vars)
            if var_name in self.variable_context:
                set_type = self.variable_context[var_name]
            # Then check inferred types
            elif var_name in self.inferred_types:
                inferred = self.inferred_types[var_name]
                if inferred.confidence >= TypeConfidence.MEDIUM.value:
                    set_type = inferred.c_type

        if method_name == "add":
            if len(args) != 1:
                raise UnsupportedFeatureError("set.add() requires exactly one argument")
            # set_int_insert(&data, value)
            return f"{set_type}_insert(&{obj}, {args[0]})"

        elif method_name == "remove":
            if len(args) != 1:
                raise UnsupportedFeatureError("set.remove() requires exactly one argument")
            # set_int_erase(&data, value)
            return f"{set_type}_erase(&{obj}, {args[0]})"

        elif method_name == "discard":
            if len(args) != 1:
                raise UnsupportedFeatureError("set.discard() requires exactly one argument")
            # Same as remove in STC (erase doesn't fail if not present)
            return f"{set_type}_erase(&{obj}, {args[0]})"

        elif method_name == "clear":
            if args:
                raise UnsupportedFeatureError("set.clear() takes no arguments")
            return f"{set_type}_clear(&{obj})"

        else:
            raise UnsupportedFeatureError(f"Unsupported set method: {method_name}")

    def _get_type_annotation(self, annotation: ast.expr) -> str:
        """Extract type from annotation."""
        if isinstance(annotation, ast.Name):
            return annotation.id
        elif isinstance(annotation, ast.Constant):
            return str(annotation.value)
        elif isinstance(annotation, ast.Subscript):
            # Handle subscripted types like list[int], list[list[int]], dict[str, int]
            return ast.unparse(annotation)
        else:
            return "int"  # Default fallback

    def _infer_dict_type_from_usage(self, var_name: str) -> Optional[str]:
        """Infer dict type by scanning forward for subscript assignments.

        For example: data[i] = i * 3 -> infer map_int_int
        """
        if not self.current_function_ast:
            return None

        # Scan the current function's body for subscript assignments (including nested)
        for node in ast.walk(self.current_function_ast):
            if isinstance(node, ast.Assign):
                # Check if this is a subscript assignment: var[key] = value
                if len(node.targets) == 1 and isinstance(node.targets[0], ast.Subscript):
                    subscript = node.targets[0]
                    # Check if this is assigning to our variable
                    if isinstance(subscript.value, ast.Name) and subscript.value.id == var_name:
                        # Infer key and value types
                        key_type = self._infer_expression_type(subscript.slice)
                        value_type = self._infer_expression_type(node.value)

                        # Map to STC types
                        key_c_type = "int" if key_type == "int" else "str" if key_type == "char*" else "int"
                        val_c_type = "int" if value_type == "int" else "str" if value_type == "char*" else "int"

                        return f"map_{key_c_type}_{val_c_type}"
            # Also check for AugAssign (+=, -=, etc.) which might reveal the value type
            elif isinstance(node, ast.AugAssign):
                if isinstance(node.target, ast.Subscript):
                    subscript = node.target
                    if isinstance(subscript.value, ast.Name) and subscript.value.id == var_name:
                        # Infer key type from subscript
                        key_type = self._infer_expression_type(subscript.slice)
                        # For augmented assign, we can infer value type from the operation
                        value_type = self._infer_expression_type(node.value)

                        key_c_type = "int" if key_type == "int" else "str" if key_type == "char*" else "int"
                        val_c_type = "int" if value_type == "int" else "str" if value_type == "char*" else "int"

                        return f"map_{key_c_type}_{val_c_type}"

        return None

    def _infer_expression_type(self, expr: ast.expr) -> str:
        """Infer C type from Python expression."""
        if isinstance(expr, ast.Constant):
            # Note: In Python, bool is a subclass of int, so check bool first
            if isinstance(expr.value, bool):
                return "bool"
            elif isinstance(expr.value, int):
                return "int"
            elif isinstance(expr.value, float):
                return "double"
            elif isinstance(expr.value, str):
                return "char*"
        elif isinstance(expr, ast.Name):
            # Look up variable type from context
            var_name = expr.id
            if var_name in self.variable_context:
                c_type = self.variable_context[var_name]
                # If it's a container type, return the base type
                if c_type in ["char*", "int", "double", "bool", "float"]:
                    return c_type
            # Fallback to checking inferred types
            if var_name in self.inferred_types:
                return self.inferred_types[var_name].c_type
            # Default fallback
            return "int"
        elif isinstance(expr, ast.DictComp):
            # Infer dict comprehension type from key and value expressions
            key_type = self._infer_expression_type(expr.key)
            value_type = self._infer_expression_type(expr.value)

            # Map Python types to C container types
            key_c_type = "int" if key_type == "int" else "str" if key_type == "char*" else "int"
            val_c_type = "int" if value_type == "int" else "str" if value_type == "char*" else "int"

            # For string-keyed maps, use multigen custom type (not STC)
            if key_c_type == "str" and val_c_type == "int":
                return "multigen_str_int_map_t*"
            else:
                # Use STC map types for other combinations
                return f"map_{key_c_type}_{val_c_type}"
        elif isinstance(expr, ast.ListComp):
            # Infer list comprehension type from element expression
            element_type = self._infer_expression_type(expr.elt)
            if element_type == "int":
                return "vec_int"
            else:
                return "vec_int"  # Default to int for now
        elif isinstance(expr, ast.SetComp):
            # Infer set comprehension type from element expression
            element_type = self._infer_expression_type(expr.elt)
            if element_type == "int":
                return "set_int"
            else:
                return "set_int"  # Default to int for now
        elif isinstance(expr, ast.Call):
            # Check for method calls that return specific types
            if isinstance(expr.func, ast.Attribute):
                method_name = expr.func.attr
                # String method calls
                if method_name == "split":
                    return "multigen_string_array_t*"
                # String methods that return strings
                elif method_name in {"upper", "lower", "strip", "replace", "lstrip", "rstrip"}:
                    return "char*"
            # Check for function calls with known return types
            elif isinstance(expr.func, ast.Name):
                func_name = expr.func.id
                # Check for type casting functions
                if func_name == "str":
                    return "char*"
                elif func_name == "int":
                    return "int"
                elif func_name == "float":
                    return "double"
                elif func_name == "bool":
                    return "bool"
                # Check if we know this function's return type
                elif func_name in self.function_return_types:
                    return self.function_return_types[func_name]
                # Check for constructor calls
                elif func_name in ["list", "set"]:
                    return "vec_int"  # Default
                elif func_name == "dict":
                    return "map_str_int"  # Default

        return "int"  # Default fallback

    def _uses_containers(self, node: ast.AST) -> bool:
        """Check if AST uses container types."""
        for child in ast.walk(node):
            if isinstance(child, ast.Name) and child.id in ["list", "dict", "set"]:
                return True
            if isinstance(child, ast.Call):
                # Check for direct calls: list(), dict(), set()
                if isinstance(child.func, ast.Name):
                    if child.func.id in ["list", "dict", "set", "append", "extend"]:
                        return True
                # Check for method calls: numbers.append(), items.extend()
                elif isinstance(child.func, ast.Attribute):
                    if child.func.attr in ["append", "extend", "add", "remove", "pop", "clear", "update", "keys", "values", "items"]:
                        return True
        return False

    def _needs_containers(self) -> bool:
        """Check if container support is needed."""
        return len(self.container_variables) > 0

    def _uses_comprehensions(self, node: ast.AST) -> bool:
        """Check if the AST contains comprehensions."""
        for child in ast.walk(node):
            if isinstance(child, (ast.ListComp, ast.DictComp, ast.SetComp)):
                return True
        return False

    def _sanitize_type_name(self, type_name: str) -> str:
        """Sanitize type name for use in STC containers."""
        # Check for special types BEFORE doing character replacement
        if type_name == "char*":
            return "str"
        elif type_name == "const char*":
            return "cstr"
        # Replace special characters and keywords
        sanitized = type_name.replace("*", "ptr").replace(" ", "_")
        return sanitized

    def _generate_main_function(self) -> str:
        """Generate a default main function."""
        return """int main() {
    printf("Hello from MultiGen-enhanced C code!\\n");
    return 0;
}"""

    def _convert_if(self, stmt: ast.If) -> str:
        """Convert if statements."""
        condition = self._convert_expression(stmt.test)

        body = []
        for s in stmt.body:
            converted = self._convert_statement(s)
            if converted:
                body.extend(converted.split("\n"))

        result = f"if ({condition}) {{\n"
        for line in body:
            result += f"    {line}\n"
        result += "}"

        if stmt.orelse:
            # Handle elif and else
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                # This is an elif
                elif_converted = self._convert_if(stmt.orelse[0])
                result += " else " + elif_converted
            else:
                # This is an else
                result += " else {\n"
                for s in stmt.orelse:
                    converted = self._convert_statement(s)
                    if converted:
                        for line in converted.split("\n"):
                            result += f"    {line}\n"
                result += "}"

        return result

    def _convert_while(self, stmt: ast.While) -> str:
        """Convert while loop."""
        condition = self._convert_expression(stmt.test)
        body = []
        for s in stmt.body:
            converted = self._convert_statement(s)
            if converted:
                body.extend(converted.split("\n"))

        result = f"while ({condition}) {{\n"
        for line in body:
            result += f"    {line}\n"
        result += "}"
        return result

    def _convert_for(self, stmt: ast.For) -> str:
        """Convert for loop (supports range() and container iteration)."""
        if not isinstance(stmt.target, ast.Name):
            raise UnsupportedFeatureError("Only simple loop variables supported")

        var_name = stmt.target.id

        # Handle range-based iteration
        if isinstance(stmt.iter, ast.Call) and isinstance(stmt.iter.func, ast.Name) and stmt.iter.func.id == "range":
            range_args = stmt.iter.args

            if len(range_args) == 1:
                # range(n)
                start, stop, step = "0", self._convert_expression(range_args[0]), "1"
            elif len(range_args) == 2:
                # range(start, stop)
                start = self._convert_expression(range_args[0])
                stop = self._convert_expression(range_args[1])
                step = "1"
            elif len(range_args) == 3:
                # range(start, stop, step)
                start = self._convert_expression(range_args[0])
                stop = self._convert_expression(range_args[1])
                step = self._convert_expression(range_args[2])
            else:
                raise UnsupportedFeatureError("Invalid range() arguments")

            self.variable_context[var_name] = "int"

            body = []
            for s in stmt.body:
                converted = self._convert_statement(s)
                if converted:
                    body.extend(converted.split("\n"))

            result = f"for (int {var_name} = {start}; {var_name} < {stop}; {var_name} += {step}) {{\n"
            for line in body:
                result += f"    {line}\n"
            result += "}"
            return result

        # Handle dict.values(), dict.keys(), dict.items() iteration
        elif (
            isinstance(stmt.iter, ast.Call)
            and isinstance(stmt.iter.func, ast.Attribute)
            and stmt.iter.func.attr in ("values", "keys", "items")
            and isinstance(stmt.iter.func.value, ast.Name)
        ):
            dict_name = stmt.iter.func.value.id
            method = stmt.iter.func.attr

            # Determine dict type
            dict_type = None
            if dict_name in self.variable_context:
                dict_type = self.variable_context[dict_name]
            elif dict_name in self.inferred_types:
                dict_type = self.inferred_types[dict_name].c_type

            if not dict_type or not dict_type.startswith("map_"):
                dict_type = "map_int_int"  # Default

            # Generate STC iterator loop
            iter_var = self._generate_temp_var_name("iter")

            if method == "values":
                # for value in dict.values()
                self.variable_context[var_name] = "int"  # TODO: infer value type

                body = []
                for s in stmt.body:
                    converted = self._convert_statement(s)
                    if converted:
                        body.extend(converted.split("\n"))

                result = f"{dict_type}_iter {iter_var} = {dict_type}_begin(&{dict_name});\n"
                result += f"for (; {iter_var}.ref; {dict_type}_next(&{iter_var})) {{\n"
                result += f"    int {var_name} = {iter_var}.ref->second;\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"

            elif method == "keys":
                # for key in dict.keys()
                self.variable_context[var_name] = "int"  # TODO: infer key type

                body = []
                for s in stmt.body:
                    converted = self._convert_statement(s)
                    if converted:
                        body.extend(converted.split("\n"))

                result = f"{dict_type}_iter {iter_var} = {dict_type}_begin(&{dict_name});\n"
                result += f"for (; {iter_var}.ref; {dict_type}_next(&{iter_var})) {{\n"
                result += f"    int {var_name} = {iter_var}.ref->first;\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"

            else:  # items
                # for k, v in dict.items() - tuple unpacking
                if (
                    not isinstance(stmt.target, ast.Tuple)
                    or len(stmt.target.elts) != 2
                    or not isinstance(stmt.target.elts[0], ast.Name)
                    or not isinstance(stmt.target.elts[1], ast.Name)
                ):
                    raise UnsupportedFeatureError("dict.items() requires 2-tuple unpacking (for k, v in ...)")

                key_var = stmt.target.elts[0].id
                value_var = stmt.target.elts[1].id

                self.variable_context[key_var] = "int"  # TODO: infer key type
                self.variable_context[value_var] = "int"  # TODO: infer value type

                body = []
                for s in stmt.body:
                    converted = self._convert_statement(s)
                    if converted:
                        body.extend(converted.split("\n"))

                result = f"{dict_type}_iter {iter_var} = {dict_type}_begin(&{dict_name});\n"
                result += f"for (; {iter_var}.ref; {dict_type}_next(&{iter_var})) {{\n"
                result += f"    int {key_var} = {iter_var}.ref->first;\n"
                result += f"    int {value_var} = {iter_var}.ref->second;\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"

            return result

        # Handle container iteration (for x in container)
        elif isinstance(stmt.iter, ast.Name):
            container_name = stmt.iter.id
            index_var = self._generate_temp_var_name("loop_idx")

            # Determine container type
            # Prioritize variable_context over inferred_types since it reflects actual assignments
            container_type = None
            if container_name in self.variable_context:
                container_type = self.variable_context[container_name]
            elif container_name in self.inferred_types:
                container_type = self.inferred_types[container_name].c_type

            # Set loop variable type BEFORE converting body (so it's available in body statements)
            if container_type and "multigen_string_array" in container_type:
                self.variable_context[var_name] = "char*"
            elif container_type and container_type.startswith("vec_"):
                # Extract element type from vec_TYPE (e.g., vec_cstr -> cstr, vec_int -> int)
                element_type = container_type[4:]  # Remove "vec_" prefix
                self.variable_context[var_name] = element_type
            elif container_type and container_type.startswith("set_"):
                # Extract element type from set_TYPE (e.g., set_int -> int)
                element_type = container_type[4:]  # Remove "set_" prefix
                self.variable_context[var_name] = element_type
            else:
                self.variable_context[var_name] = "int"  # Default

            body = []
            for s in stmt.body:
                converted = self._convert_statement(s)
                if converted:
                    body.extend(converted.split("\n"))

            # Generate iteration code based on container type
            if container_type and "multigen_string_array" in container_type:
                # String array iteration (var_name already set above)
                result = f"for (size_t {index_var} = 0; {index_var} < multigen_string_array_size({container_name}); {index_var}++) {{\n"
                result += f"    const char* {var_name} = multigen_string_array_get({container_name}, {index_var});\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"
            elif container_type and container_type.startswith("set_"):
                # STC set iteration using iterator (var_name already set above)
                # Extract element type from set_TYPE (e.g., set_int -> int)
                element_type = container_type[4:]  # Remove "set_" prefix
                iter_var = self._generate_temp_var_name("set_iter")
                result = f"{container_type}_iter {iter_var} = {container_type}_begin(&{container_name});\n"
                result += f"for (; {iter_var}.ref; {container_type}_next(&{iter_var})) {{\n"
                result += f"    {element_type} {var_name} = *{iter_var}.ref;\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"
            elif container_type and container_type.startswith("vec_"):
                # STC vector iteration (var_name already set above)
                # Extract element type from vec_TYPE (e.g., vec_cstr -> cstr, vec_int -> int)
                element_type = container_type[4:]  # Remove "vec_" prefix
                result = f"for (size_t {index_var} = 0; {index_var} < {container_type}_size(&{container_name}); {index_var}++) {{\n"
                result += f"    {element_type} {var_name} = *{container_type}_at(&{container_name}, {index_var});\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"
            else:
                # Default: vec_int iteration (var_name already set above)
                result = (
                    f"for (size_t {index_var} = 0; {index_var} < vec_int_size(&{container_name}); {index_var}++) {{\n"
                )
                result += f"    int {var_name} = *vec_int_at(&{container_name}, {index_var});\n"
                for line in body:
                    result += f"    {line}\n"
                result += "}"
            return result

        else:
            raise UnsupportedFeatureError("Only for loops with range() or container iteration supported")

    def _convert_expression_statement(self, stmt: ast.Expr) -> str:
        """Convert expression statement."""
        # Check if this is a docstring (string literal as standalone statement)
        if isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            # Convert docstring to C comment
            docstring = stmt.value.value
            # Handle multi-line docstrings
            if "\n" in docstring:
                lines = docstring.split("\n")
                return "/* " + "\n * ".join(lines) + " */"
            else:
                return f"/* {docstring} */"

        expr = self._convert_expression(stmt.value)
        return f"{expr};"

    def _convert_assert(self, stmt: ast.Assert) -> str:
        """Convert Python assert statement to C assert() call.

        Args:
            stmt: Python assert statement node

        Returns:
            C assert() call as string

        Example:
            assert x > 0  →  assert(x > 0);
            assert result == 1, "Test failed"  →  assert(result == 1); // Test failed
        """
        # Convert the test expression
        test_expr = self._convert_expression(stmt.test)

        # Handle optional message
        if stmt.msg:
            # Convert message to string
            if isinstance(stmt.msg, ast.Constant) and isinstance(stmt.msg.value, str):
                msg = stmt.msg.value
                return f"assert({test_expr}); // {msg}"
            else:
                # Complex message expression or non-string - just add assert without comment
                return f"assert({test_expr});"
        else:
            return f"assert({test_expr});"

    def _convert_attribute(self, expr: ast.Attribute) -> str:
        """Convert attribute access."""
        obj = self._convert_expression(expr.value)

        # If this is a self reference in a method, use pointer access
        if (
            isinstance(expr.value, ast.Name)
            and expr.value.id == "self"
            and self.current_function
            and "_" in self.current_function
        ):
            return f"self->{expr.attr}"

        # Regular struct member access
        return f"{obj}.{expr.attr}"

    def _convert_subscript(self, expr: ast.Subscript) -> str:
        """Convert subscript access (including nested like a[i][j]) and slicing."""
        # Check if this is a slice operation (e.g., list[1:3])
        if isinstance(expr.slice, ast.Slice):
            return self._convert_slice(expr)

        # Handle both Python 3.8 (with ast.Index) and Python 3.9+ (without ast.Index)
        if hasattr(ast, "Index") and isinstance(expr.slice, ast.Index):  # Python < 3.9
            index = self._convert_expression(expr.slice.value)  # type: ignore
        else:  # Python >= 3.9
            index = self._convert_expression(expr.slice)

        # Check if this is a nested subscript (e.g., a[i][j])
        if isinstance(expr.value, ast.Subscript):
            # This is nested - handle the outer subscript first
            inner = self._convert_subscript(expr.value)
            # The inner subscript returns a pointer (vec_int*), so we can use it directly
            # vec_vec_int_at returns vec_int*, so: *vec_int_at(vec_vec_int_at(&a, i), j)
            return f"*vec_int_at({inner}, {index})"

        # Convert object expression
        obj = self._convert_expression(expr.value)

        # Check if this is an STC container access
        if isinstance(expr.value, ast.Name):
            var_name = expr.value.id
            c_type = None

            # Check variable_context first (more reliable for explicit declarations)
            if var_name in self.variable_context:
                c_type = self.variable_context[var_name]
            # Fallback to inferred types
            elif var_name in self.inferred_types:
                c_type = self.inferred_types[var_name].c_type

            if c_type:
                # If it's a multigen_string_array_t*, use multigen_string_array_get()
                if c_type == "multigen_string_array_t*":
                    return f"multigen_string_array_get({obj}, {index})"
                # If it's a nested vector (vec_vec_int), first access returns a vec_int*
                elif c_type == "vec_vec_int":
                    return f"vec_vec_int_at(&{obj}, {index})"
                # If it's an STC vector type, use vec_*_at() function
                elif c_type.startswith("vec_"):
                    # DEBUG
                    # print(f"DEBUG SUBSCRIPT: var={var_name}, c_type={c_type}, generating *{c_type}_at")
                    return f"*{c_type}_at(&{obj}, {index})"
                # If it's a map type, use appropriate get function
                elif c_type == "map_str_int":
                    # Vanilla C string map: returns int*, dereference it
                    return f"*multigen_str_int_map_get({obj}, {index})"
                elif c_type.startswith("map_"):
                    # STC map get returns a pointer to entry, need ->second for value
                    return f"{c_type}_get(&{obj}, {index})->second"

        # Default: use direct array subscript
        return f"{obj}[{index}]"

    def _convert_slice(self, expr: ast.Subscript) -> str:
        """Convert slice operation to C code.

        Examples:
            list[1:3] → creates new vec with elements [1, 2]
            list[1:] → creates new vec from index 1 to end
            list[:2] → creates new vec from start to index 2
            list[::2] → creates new vec with every 2nd element (step)

        Args:
            expr: ast.Subscript with ast.Slice as slice

        Returns:
            C expression that creates a new vector with sliced elements
        """
        slice_obj = expr.slice
        assert isinstance(slice_obj, ast.Slice), "Expected ast.Slice"

        # Get container name and type
        obj = self._convert_expression(expr.value)

        # Determine container type
        c_type = None
        if isinstance(expr.value, ast.Name):
            var_name = expr.value.id
            if var_name in self.variable_context:
                c_type = self.variable_context[var_name]
            elif var_name in self.inferred_types:
                c_type = self.inferred_types[var_name].c_type

        if not c_type or not c_type.startswith("vec_"):
            raise UnsupportedFeatureError(f"Slicing only supported for vec_* containers, got {c_type}")

        # Extract start, stop, step
        start = self._convert_expression(slice_obj.lower) if slice_obj.lower else "0"
        stop = self._convert_expression(slice_obj.upper) if slice_obj.upper else f"{c_type}_size(&{obj})"
        step = self._convert_expression(slice_obj.step) if slice_obj.step else "1"

        # Generate unique variable name for the slice result
        slice_var = f"slice_result_{id(expr)}"

        # Generate C code for slicing using a compound statement
        # This creates a new vector and copies elements in the range
        element_type = c_type[4:]  # Remove "vec_" prefix to get element type

        slice_code = f"""({{
        {c_type} {slice_var} = {{0}};
        for (int _i = {start}; _i < {stop}; _i += {step}) {{
            {c_type}_push(&{slice_var}, *{c_type}_at(&{obj}, _i));
        }}
        {slice_var};
    }})"""

        return slice_code

    def _convert_f_string(self, expr: ast.JoinedStr) -> str:
        """Convert f-string to C string concatenation using multigen_string_concat.

        Example:
            f"Result: {x}" -> multigen_string_concat_int("Result: ", x)
            f"Count: {len(items)}" -> multigen_string_concat_int("Count: ", vec_int_len(&items))

        Note: This generates a call to multigen runtime functions that handle string building.
        The runtime will need to implement multigen_string_concat_* functions for different types.
        """
        # For simplicity in Phase 1, we'll generate a multi-part concatenation
        # using a helper function that takes multiple arguments

        # Count format string placeholders and collect arguments
        format_parts: list[str] = []
        args: list[str] = []

        for value in expr.values:
            if isinstance(value, ast.Constant):
                # Literal string part
                if isinstance(value.value, str):
                    format_parts.append(value.value)
            elif isinstance(value, ast.FormattedValue):
                # Expression - add placeholder
                format_parts.append("%s")
                expr_code = self._convert_expression(value.value)
                # Wrap in appropriate conversion based on type
                args.append(self._to_c_string_expr(expr_code, value.value))

        format_string = "".join(format_parts)

        if len(args) == 0:
            # No interpolation, just return the string
            return f'"{format_string}"'
        else:
            # Use multigen_sprintf_string helper (needs to be in runtime)
            args_str = ", ".join(args)
            return f'multigen_sprintf_string("{format_string}", {args_str})'

    def _to_c_string_expr(self, expr_code: str, node: ast.expr) -> str:
        """Wrap an expression for string formatting in C.

        Returns an expression that can be used as a %s argument to sprintf.
        For non-strings, we need to convert them first.
        """
        # Check if it's a string literal or variable
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return expr_code  # Already a string

        if isinstance(node, ast.Name):
            var_name = node.id.lower()
            # Heuristic: check if variable name suggests it's a string
            if any(substr in var_name for substr in ["name", "text", "str", "msg", "message", "path", "file"]):
                return expr_code  # Assume string type

        # For other types, we need a temp buffer approach
        # This is a simplified version - in practice would need type inference
        # For now, assume integers and use multigen_int_to_string
        return f'multigen_int_to_string({expr_code})'

    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert Python class to C struct with associated methods."""
        class_name = node.name

        # Check if this is a dataclass or namedtuple
        is_dataclass = self._is_dataclass(node)
        is_namedtuple = self._is_namedtuple(node)

        if is_dataclass or is_namedtuple:
            # Extract fields from annotations
            fields = self._extract_dataclass_fields(node)

            # Generate struct definition
            if fields:
                field_lines = [f"    {c_type} {name};" for name, c_type in fields.items()]
                struct_def = f"typedef struct {{\n{chr(10).join(field_lines)}\n}} {class_name};"
            else:
                # Empty dataclass - fallback to dummy field
                struct_def = f"typedef struct {{\n    char _dummy;\n}} {class_name};"

            # Store struct info
            self.defined_structs[class_name] = {
                "instance_vars": fields,
                "attributes": fields,
                "is_dataclass": is_dataclass,
                "is_namedtuple": is_namedtuple,
            }

            # Generate constructor (only for dataclass, not namedtuple)
            if is_dataclass and fields:
                constructor = self._generate_dataclass_constructor(class_name, fields)
                return f"{struct_def}\n\n{constructor}"
            else:
                # NamedTuple - just struct definition
                return struct_def

        # Regular class - use existing logic
        # Analyze class to extract instance variables and methods
        instance_vars = self._extract_instance_variables(node)
        methods = self._extract_methods(node)

        parts = []

        # Generate struct definition
        struct_def = self._generate_struct_definition(class_name, instance_vars)
        parts.append(struct_def)
        parts.append("")

        # Store struct info for later use
        self.defined_structs[class_name] = {
            "instance_vars": instance_vars,
            "attributes": instance_vars,  # For compatibility with string method detection
            "methods": [m.name for m in methods],
        }

        # Generate method declarations
        for method in methods:
            if method.name == "__init__":
                # Constructor
                constructor = self._generate_constructor(class_name, method, instance_vars)
                parts.append(constructor)
            else:
                # Regular method
                method_def = self._generate_method(class_name, method)
                parts.append(method_def)
            parts.append("")

        return "\n".join(parts)

    def _extract_instance_variables(self, class_node: ast.ClassDef) -> dict[str, str]:
        """Extract instance variables from class definition."""
        instance_vars = {}

        # Look for __init__ method to find instance variable assignments
        for stmt in class_node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "__init__":
                for body_stmt in stmt.body:
                    if isinstance(body_stmt, ast.Assign):
                        # Look for self.var = value assignments
                        for target in body_stmt.targets:
                            if (
                                isinstance(target, ast.Attribute)
                                and isinstance(target.value, ast.Name)
                                and target.value.id == "self"
                            ):
                                var_name = target.attr
                                # Try to infer type from the assignment
                                var_type = self._infer_expression_type(body_stmt.value)
                                instance_vars[var_name] = var_type
                    elif isinstance(body_stmt, ast.AnnAssign):
                        # Look for self.var: type = value assignments
                        if (
                            isinstance(body_stmt.target, ast.Attribute)
                            and isinstance(body_stmt.target.value, ast.Name)
                            and body_stmt.target.value.id == "self"
                        ):
                            var_name = body_stmt.target.attr
                            if body_stmt.annotation:
                                var_type = self._get_type_annotation(body_stmt.annotation)
                                var_type = self.type_mapping.get(var_type, var_type)
                            else:
                                var_type = (
                                    self._infer_expression_type(body_stmt.value)
                                    if body_stmt.value is not None
                                    else "void*"
                                )
                            instance_vars[var_name] = var_type

        return instance_vars

    def _extract_methods(self, class_node: ast.ClassDef) -> list[ast.FunctionDef]:
        """Extract method definitions from class."""
        methods = []
        for stmt in class_node.body:
            if isinstance(stmt, ast.FunctionDef):
                methods.append(stmt)
        return methods

    def _generate_struct_definition(self, class_name: str, instance_vars: dict[str, str]) -> str:
        """Generate C struct definition for Python class."""
        lines = [f"typedef struct {class_name} {{"]

        if instance_vars:
            for var_name, var_type in instance_vars.items():
                lines.append(f"    {var_type} {var_name};")
        else:
            # Empty struct needs at least one member in C
            lines.append("    char _dummy;  // Empty struct placeholder")

        lines.append(f"}} {class_name};")

        return "\n".join(lines)

    def _generate_constructor(
        self, class_name: str, init_method: ast.FunctionDef, instance_vars: dict[str, str]
    ) -> str:
        """Generate constructor function for class."""
        # Build parameter list (skip 'self')
        params = []
        for arg in init_method.args.args[1:]:  # Skip 'self'
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "int"
            c_type = self.type_mapping.get(param_type, param_type)
            params.append(f"{c_type} {arg.arg}")

        params_str = ", ".join(params) if params else "void"
        signature = f"{class_name} {class_name}_new({params_str})"

        # Generate constructor body
        body_lines = [f"    {class_name} obj;"]

        # Add initialization code from __init__ body
        old_function = self.current_function
        self.current_function = f"{class_name}_new"

        # Set up parameter context
        for arg in init_method.args.args[1:]:
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "int"
            c_type = self.type_mapping.get(param_type, param_type)
            self.variable_context[arg.arg] = c_type

        for stmt in init_method.body:
            if isinstance(stmt, ast.Assign):
                # Convert self.var = value to obj.var = value
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        var_name = target.attr
                        value_expr = self._convert_expression(stmt.value)
                        body_lines.append(f"    obj.{var_name} = {value_expr};")
            elif isinstance(stmt, ast.AnnAssign):
                # Convert self.var: type = value to obj.var = value
                if (
                    isinstance(stmt.target, ast.Attribute)
                    and isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                ):
                    var_name = stmt.target.attr
                    if stmt.value:
                        value_expr = self._convert_expression(stmt.value)
                        body_lines.append(f"    obj.{var_name} = {value_expr};")

        body_lines.append("    return obj;")

        self.current_function = old_function

        body = "\n".join(body_lines)
        return f"{signature} {{\n{body}\n}}"

    def _generate_method(self, class_name: str, method: ast.FunctionDef) -> str:
        """Generate C function for class method."""
        method_name = f"{class_name}_{method.name}"

        # Build parameter list (convert 'self' to struct pointer)
        params = [f"{class_name}* self"]
        for arg in method.args.args[1:]:  # Skip 'self'
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "int"
            c_type = self.type_mapping.get(param_type, param_type)
            params.append(f"{c_type} {arg.arg}")
            self.variable_context[arg.arg] = c_type

        # Get return type
        return_type = "void"
        if method.returns:
            py_return_type = self._get_type_annotation(method.returns)
            return_type = self.type_mapping.get(py_return_type, py_return_type)

        params_str = ", ".join(params)
        signature = f"{return_type} {method_name}({params_str})"

        # Convert method body
        old_function = self.current_function
        self.current_function = method_name

        body_lines = []
        for stmt in method.body:
            converted = self._convert_method_statement(stmt, class_name)
            if converted:
                body_lines.extend(converted.split("\n"))

        body = "\n".join(f"    {line}" if line.strip() else "" for line in body_lines)

        self.current_function = old_function
        return f"{signature} {{\n{body}\n}}"

    def _is_dataclass(self, node: ast.ClassDef) -> bool:
        """Check if class has @dataclass decorator.

        Args:
            node: Class definition node

        Returns:
            True if class has @dataclass decorator

        Example:
            @dataclass
            class Point:
                x: int
                y: int
        """
        for decorator in node.decorator_list:
            # Handle @dataclass
            if isinstance(decorator, ast.Name) and decorator.id == "dataclass":
                return True
            # Handle @dataclass(...) with arguments
            elif isinstance(decorator, ast.Call) and isinstance(decorator.func, ast.Name):
                if decorator.func.id == "dataclass":
                    return True
        return False

    def _is_namedtuple(self, node: ast.ClassDef) -> bool:
        """Check if class inherits from NamedTuple.

        Args:
            node: Class definition node

        Returns:
            True if class inherits from NamedTuple

        Example:
            class Point(NamedTuple):
                x: int
                y: int
        """
        for base in node.bases:
            if isinstance(base, ast.Name):
                if base.id == "NamedTuple":
                    return True
            elif isinstance(base, ast.Attribute):
                if base.attr == "NamedTuple":
                    return True
        return False

    def _extract_dataclass_fields(self, node: ast.ClassDef) -> dict[str, str]:
        """Extract field names and types from dataclass.

        Args:
            node: Dataclass definition node

        Returns:
            Dictionary mapping field names to C types

        Example:
            @dataclass
            class Point:
                x: int  →  {"x": "int"}
                y: int  →  {"y": "int"}
        """
        fields = {}

        for stmt in node.body:
            if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                field_name = stmt.target.id

                # Extract type annotation
                if isinstance(stmt.annotation, ast.Name):
                    python_type = stmt.annotation.id
                    c_type = self.type_mapping.get(python_type, "int")
                    fields[field_name] = c_type
                elif isinstance(stmt.annotation, ast.Subscript):
                    # Handle generic types like list[int]
                    python_type = self._get_type_annotation(stmt.annotation)
                    c_type = self.type_mapping.get(python_type, python_type)
                    fields[field_name] = c_type

        return fields

    def _generate_dataclass_constructor(self, struct_name: str, fields: dict[str, str]) -> str:
        """Generate constructor function for dataclass.

        Args:
            struct_name: Name of the struct
            fields: Dictionary of field names to C types

        Returns:
            C constructor function as string

        Example:
            make_Point(int x, int y) { return (Point){x, y}; }
        """
        # Create parameter list
        params = [f"{c_type} {name}" for name, c_type in fields.items()]
        params_str = ", ".join(params)

        # Create field list for initialization
        field_names = ", ".join(fields.keys())

        # Generate constructor function
        constructor = f"""{struct_name} make_{struct_name}({params_str})
{{
    return ({struct_name}){{{field_names}}};
}}"""

        return constructor

    def _convert_method_statement(self, stmt: ast.stmt, class_name: str) -> str:
        """Convert statement inside a method, handling self references."""
        if isinstance(stmt, ast.Assign):
            return self._convert_method_assignment(stmt, class_name)
        elif isinstance(stmt, ast.Return):
            return self._convert_method_return(stmt, class_name)
        else:
            # For other statements, use regular conversion but handle self references
            return self._convert_statement(stmt)

    def _convert_method_assignment(self, stmt: ast.Assign, class_name: str) -> str:
        """Convert assignment in method context."""
        if len(stmt.targets) != 1:
            raise UnsupportedFeatureError("Multiple assignment targets not supported")

        target = stmt.targets[0]

        # Handle self.attr = value
        if isinstance(target, ast.Attribute) and isinstance(target.value, ast.Name) and target.value.id == "self":
            attr_name = target.attr
            value_expr = self._convert_expression(stmt.value)
            return f"self->{attr_name} = {value_expr};"

        # Regular assignment
        return self._convert_assignment(stmt)

    def _convert_method_return(self, stmt: ast.Return, class_name: str) -> str:
        """Convert return statement in method context."""
        if stmt.value is None:
            return "return;"

        value_expr = self._convert_method_expression(stmt.value, class_name)
        return f"return {value_expr};"

    def _convert_method_expression(self, expr: ast.expr, class_name: str) -> str:
        """Convert expression in method context, handling self references."""
        if isinstance(expr, ast.Attribute):
            if isinstance(expr.value, ast.Name) and expr.value.id == "self":
                # self.attr becomes self->attr
                return f"self->{expr.attr}"

        # For other expressions, use regular conversion
        return self._convert_expression(expr)

    def _convert_list_comprehension(self, node: ast.ListComp) -> str:
        """Convert list comprehension to C loop with STC list operations.

        [expr for target in iter if condition] becomes:
        vec_type result = {0};
        for (...) {
            if (condition) {
                vec_type_push(&result, expr);
            }
        }
        """
        # Generate unique temporary variable name for the result
        temp_var = self._generate_temp_var_name("comp_result")

        # Infer result element type from the expression
        result_element_type = self._infer_expression_type(node.elt)
        result_container_type = f"vec_{self._sanitize_type_name(result_element_type)}"

        # Process the single generator (comprehensions can have multiple, but we'll start simple)
        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in list comprehensions not yet supported")

        generator = node.generators[0]

        # Extract loop variable and iterable
        if not isinstance(generator.target, ast.Name):
            raise UnsupportedFeatureError("Only simple loop variables supported in comprehensions")

        loop_var = generator.target.id

        # Handle range-based iteration (most common case)
        if (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Name)
            and generator.iter.func.id == "range"
        ):
            # Generate range-based for loop
            range_args = generator.iter.args
            if len(range_args) == 1:
                start = "0"
                end = self._convert_expression(range_args[0])
                step = "1"
            elif len(range_args) == 2:
                start = self._convert_expression(range_args[0])
                end = self._convert_expression(range_args[1])
                step = "1"
            elif len(range_args) == 3:
                start = self._convert_expression(range_args[0])
                end = self._convert_expression(range_args[1])
                step = self._convert_expression(range_args[2])
            else:
                raise UnsupportedFeatureError("Invalid range() arguments in comprehension")

            loop_code = f"for (int {loop_var} = {start}; {loop_var} < {end}; {loop_var} += {step})"
            loop_var_decl = None  # No separate variable declaration needed for range

        # Handle iteration over container variables (e.g., for x in numbers)
        elif isinstance(generator.iter, ast.Name):
            container_name = generator.iter.id
            # Use vec_int as default type for now (TODO: proper type inference)
            container_size_call = f"vec_int_size(&{container_name})"
            container_at_call = f"vec_int_at(&{container_name}, __idx_{temp_var})"

            # Generate index-based iteration
            index_var = f"__idx_{temp_var}"
            loop_code = f"for (size_t {index_var} = 0; {index_var} < {container_size_call}; {index_var}++)"
            loop_var_decl = f"int {loop_var} = *{container_at_call};\n        "

        else:
            raise UnsupportedFeatureError("Non-range iterables in comprehensions not yet supported")

        # Handle conditions (if any)
        condition_code = ""
        if generator.ifs:
            if len(generator.ifs) > 1:
                raise UnsupportedFeatureError("Multiple conditions in comprehensions not yet supported")

            condition = generator.ifs[0]
            condition_expr = self._convert_expression(condition)
            condition_code = f"if ({condition_expr}) "

        # Convert the expression
        expr_str = self._convert_expression(node.elt)

        # Generate the comprehension code differently based on iteration type
        # Use GCC statement expression syntax ({ ... }) to allow use in initializers
        # Note: Last expression in statement expression must end with semicolon
        if loop_var_decl:
            # Container iteration requires extracting the element first
            comp_code = f"""({{
    {result_container_type} {temp_var} = {{0}};
    {loop_code} {{
        {loop_var_decl}{condition_code}vec_{self._sanitize_type_name(result_element_type)}_push(&{temp_var}, {expr_str});
    }}
    {temp_var};
}})"""
        else:
            # Range-based iteration is simpler
            comp_code = f"""({{
    {result_container_type} {temp_var} = {{0}};
    {loop_code} {{
        {condition_code}vec_{self._sanitize_type_name(result_element_type)}_push(&{temp_var}, {expr_str});
    }}
    {temp_var};
}})"""

        return comp_code

    def _convert_dict_comprehension(self, node: ast.DictComp) -> str:
        """Convert dictionary comprehension to C loop with STC hashmap operations.

        {key_expr: value_expr for target in iter if condition} becomes:
        map_key_value result = {0};
        for (...) {
            if (condition) {
                map_key_value_insert(&result, key_expr, value_expr);
            }
        }
        """
        # Generate unique temporary variable name
        temp_var = self._generate_temp_var_name("dict_comp_result")

        # Infer key and value types
        key_type = self._infer_expression_type(node.key)
        value_type = self._infer_expression_type(node.value)

        # Check if we need to use fallback type for string-keyed maps
        key_sanitized = self._sanitize_type_name(key_type)
        val_sanitized = self._sanitize_type_name(value_type)

        # For string-keyed maps, we use the fallback type multigen_str_int_map_t*
        if key_sanitized == "str" and val_sanitized == "int":
            result_container_type = "multigen_str_int_map_t*"
            use_fallback = True
        else:
            result_container_type = f"map_{key_sanitized}_{val_sanitized}"
            use_fallback = False

        # Process the single generator
        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in dict comprehensions not yet supported")

        generator = node.generators[0]

        # Extract loop variable and iterable
        # Support tuple unpacking for dict.items(): for k, v in dict.items()
        if isinstance(generator.target, ast.Tuple):
            if len(generator.target.elts) != 2:
                raise UnsupportedFeatureError("Only 2-element tuple unpacking supported in comprehensions")
            if not isinstance(generator.target.elts[0], ast.Name) or not isinstance(generator.target.elts[1], ast.Name):
                raise UnsupportedFeatureError("Only simple names in tuple unpacking supported")

            key_var = generator.target.elts[0].id
            value_var = generator.target.elts[1].id

            # Must be iterating over .items() method
            if (
                isinstance(generator.iter, ast.Call)
                and isinstance(generator.iter.func, ast.Attribute)
                and generator.iter.func.attr == "items"
                and isinstance(generator.iter.func.value, ast.Name)
            ):
                dict_name = generator.iter.func.value.id
                dict_type = self.variable_context.get(dict_name, "map_int_int")

                # Generate code to iterate over STC map
                iter_var = self._generate_temp_var_name("iter")
                loop_code = f"""
    {dict_type}_iter {iter_var} = {dict_type}_begin(&{dict_name});
    for (; {iter_var}.ref; {dict_type}_next(&{iter_var}))"""

                # In the loop body, extract key and value
                # For STC maps: iter.ref->first is key, iter.ref->second is value
                key_extract = f"typeof({iter_var}.ref->first) {key_var} = {iter_var}.ref->first;"
                value_extract = f"typeof({iter_var}.ref->second) {value_var} = {iter_var}.ref->second;"

                # We'll need to modify the loop body generation below
                loop_var = None  # Signal that we're using tuple unpacking
                tuple_unpacking_code = f"{key_extract}\n        {value_extract}\n        "
            else:
                raise UnsupportedFeatureError("Tuple unpacking only supported for dict.items()")
        elif isinstance(generator.target, ast.Name):
            loop_var = generator.target.id
            tuple_unpacking_code = ""

            # Handle range-based iteration
            if (
                isinstance(generator.iter, ast.Call)
                and isinstance(generator.iter.func, ast.Name)
                and generator.iter.func.id == "range"
            ):
                range_args = generator.iter.args
                if len(range_args) == 1:
                    start = "0"
                    end = self._convert_expression(range_args[0])
                    step = "1"
                elif len(range_args) == 2:
                    start = self._convert_expression(range_args[0])
                    end = self._convert_expression(range_args[1])
                    step = "1"
                elif len(range_args) == 3:
                    start = self._convert_expression(range_args[0])
                    end = self._convert_expression(range_args[1])
                    step = self._convert_expression(range_args[2])
                else:
                    raise UnsupportedFeatureError("Invalid range() arguments in comprehension")

                loop_code = f"for (int {loop_var} = {start}; {loop_var} < {end}; {loop_var} += {step})"
            else:
                raise UnsupportedFeatureError("Non-range iterables in dict comprehensions not yet supported")
        else:
            raise UnsupportedFeatureError("Only simple loop variables or 2-tuple unpacking supported in comprehensions")

        # Handle conditions (if any)
        condition_code = ""
        if generator.ifs:
            if len(generator.ifs) > 1:
                raise UnsupportedFeatureError("Multiple conditions in dict comprehensions not yet supported")

            condition = generator.ifs[0]
            condition_expr = self._convert_expression(condition)
            condition_code = f"if ({condition_expr}) "

        # Convert the key and value expressions
        key_str = self._convert_expression(node.key)
        value_str = self._convert_expression(node.value)

        # Generate the comprehension code using GCC statement expression syntax
        # If we have tuple unpacking, insert the unpacking code in the loop body
        loop_body_prefix = ""
        if loop_var is None:
            # Tuple unpacking case - add the unpacking code
            loop_body_prefix = f"{tuple_unpacking_code}"

        # Generate code based on whether we're using fallback or STC types
        if use_fallback:
            # Fallback type uses pointer and different API
            comp_code = f"""({{
    {result_container_type} {temp_var} = multigen_str_int_map_new();
    {loop_code} {{
        {loop_body_prefix}{condition_code}multigen_str_int_map_insert({temp_var}, {key_str}, {value_str});
    }}
    {temp_var};
}})"""
        else:
            # STC type uses struct and STC API
            comp_code = f"""({{
    {result_container_type} {temp_var} = {{0}};
    {loop_code} {{
        {loop_body_prefix}{condition_code}{result_container_type}_insert(&{temp_var}, {key_str}, {value_str});
    }}
    {temp_var};
}})"""

        return comp_code

    def _convert_set_comprehension(self, node: ast.SetComp) -> str:
        """Convert set comprehension to C loop with STC hset operations.

        {expr for target in iter if condition} becomes:
        set_type result = {0};
        for (...) {
            if (condition) {
                set_type_insert(&result, expr);
            }
        }
        """
        # Generate unique temporary variable name
        temp_var = self._generate_temp_var_name("set_comp_result")

        # Infer element type from the expression
        element_type = self._infer_expression_type(node.elt)
        result_container_type = f"set_{self._sanitize_type_name(element_type)}"

        # Process the single generator
        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in set comprehensions not yet supported")

        generator = node.generators[0]

        # Extract loop variable and iterable
        if not isinstance(generator.target, ast.Name):
            raise UnsupportedFeatureError("Only simple loop variables supported in set comprehensions")

        loop_var = generator.target.id

        # Track if we need to declare loop variable inside loop body
        loop_var_decl = None

        # Handle range-based iteration
        if (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Name)
            and generator.iter.func.id == "range"
        ):
            range_args = generator.iter.args
            if len(range_args) == 1:
                start = "0"
                end = self._convert_expression(range_args[0])
                step = "1"
            elif len(range_args) == 2:
                start = self._convert_expression(range_args[0])
                end = self._convert_expression(range_args[1])
                step = "1"
            elif len(range_args) == 3:
                start = self._convert_expression(range_args[0])
                end = self._convert_expression(range_args[1])
                step = self._convert_expression(range_args[2])
            else:
                raise UnsupportedFeatureError("Invalid range() arguments in comprehension")

            loop_code = f"for (int {loop_var} = {start}; {loop_var} < {end}; {loop_var} += {step})"
        # Handle container iteration (for x in set/list/dict)
        elif isinstance(generator.iter, ast.Name):
            container_name = generator.iter.id
            container_type = None

            # Check variable_context first
            if container_name in self.variable_context:
                container_type = self.variable_context[container_name]
            elif container_name in self.inferred_types:
                container_type = self.inferred_types[container_name].c_type

            if container_type and container_type.startswith("set_"):
                # Iterate over set using STC iterator
                iter_var = self._generate_temp_var_name("iter")
                self.variable_context[loop_var] = "int"  # TODO: infer element type
                loop_code = f"{container_type}_iter {iter_var} = {container_type}_begin(&{container_name});\n    for (; {iter_var}.ref; {container_type}_next(&{iter_var}))"
                # Need to extract the value from iterator
                # For sets, we'll declare the loop variable inside the loop
                loop_var_decl = f"int {loop_var} = *{iter_var}.ref"
            elif container_type and container_type.startswith("vec_"):
                # Iterate over vector
                index_var = self._generate_temp_var_name("idx")
                self.variable_context[loop_var] = "int"  # TODO: infer element type
                loop_code = f"for (size_t {index_var} = 0; {index_var} < {container_type}_size(&{container_name}); {index_var}++)"
                loop_var_decl = f"int {loop_var} = *{container_type}_at(&{container_name}, {index_var})"
            else:
                raise UnsupportedFeatureError(f"Set comprehension over {container_type} not yet supported")
        else:
            raise UnsupportedFeatureError("Non-range iterables in set comprehensions not yet supported")

        # Handle conditions (if any)
        condition_code = ""
        if generator.ifs:
            if len(generator.ifs) > 1:
                raise UnsupportedFeatureError("Multiple conditions in set comprehensions not yet supported")

            condition = generator.ifs[0]
            condition_expr = self._convert_expression(condition)
            condition_code = f"if ({condition_expr}) "

        # Convert the expression
        expr_str = self._convert_expression(node.elt)

        # Generate the comprehension code using GCC statement expression syntax
        # If we have a loop_var_decl, add it inside the loop body
        loop_body_prefix = ""
        if loop_var_decl:
            loop_body_prefix = f"{loop_var_decl};\n        "

        comp_code = f"""({{
    {result_container_type} {temp_var} = {{0}};
    {loop_code} {{
        {loop_body_prefix}{condition_code}{result_container_type}_insert(&{temp_var}, {expr_str});
    }}
    {temp_var};
}})"""

        return comp_code

    def _convert_list_literal(self, expr: ast.List) -> str:
        """Convert list literal to C STC vector initialization.

        Empty lists [] become {0}
        Non-empty lists [1, 2, 3] become initialization code
        """
        if not expr.elts:
            # Empty list - return STC zero-initialization
            return "{0}"
        else:
            # Non-empty list - generate initialization with elements
            # For now, just return {0} and let caller handle adding elements
            # TODO: Support inline initialization for simple cases
            return "{0}"

    def _convert_dict_literal(self, expr: ast.Dict) -> str:
        """Convert dict literal to C STC hashmap initialization.

        Empty dicts {} become {0}
        Non-empty dicts need element-by-element insertion
        """
        if not expr.keys:
            # Empty dict - return STC zero-initialization
            return "{0}"
        else:
            # Non-empty dict - generate initialization with key-value pairs
            # For now, return {0} and let caller handle inserting pairs
            # TODO: Support inline initialization for simple cases
            return "{0}"

    def _convert_set_literal(self, expr: ast.Set) -> str:
        """Convert set literal to C STC hashset initialization.

        Empty sets set() become {0} (handled via Call conversion)
        Non-empty sets {1, 2, 3} need element-by-element insertion
        """
        if not expr.elts:
            # Empty set - return STC zero-initialization
            return "{0}"
        else:
            # Non-empty set - generate initialization with elements
            # For now, return {0} and let caller handle adding elements
            # TODO: Support inline initialization for simple cases
            return "{0}"

    def _generate_temp_var_name(self, prefix: str) -> str:
        """Generate a unique temporary variable name."""
        import time

        return f"{prefix}_{int(time.time() * 1000000) % 1000000}"
