"""C++ code converter for MultiGen with integrated runtime libraries and sophisticated py2cpp conversion.

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
from ..converter_utils import (
    get_standard_binary_operator,
    get_standard_comparison_operator,
    get_standard_unary_operator,
)
from ..errors import TypeMappingError, UnsupportedFeatureError
from ..preferences import BackendPreferences
from ..type_inference_strategies import InferenceContext
from .factory import CppFactory
from .type_inference import create_cpp_type_inference_engine


class MultiGenPythonToCppConverter:
    """Enhanced Python to C++ converter with MultiGen STL runtime integration."""

    def __init__(self) -> None:
        """Initialize converter with MultiGen STL runtime support."""
        self.type_mapping = {
            "int": "int",
            "float": "double",
            "bool": "bool",
            "str": "std::string",
            "None": "void",
            "list": "std::vector",  # Will be specialized
            "dict": "std::unordered_map",  # Will be specialized
            "set": "std::unordered_set",  # Will be specialized
        }
        self.current_function: Optional[str] = None
        self.container_variables: dict[str, dict[str, Any]] = {}
        self.variable_context: dict[str, str] = {}  # var_name -> cpp_type
        self.defined_classes: dict[str, dict[str, Any]] = {}
        self.iterator_variables: dict[str, str] = {}
        self.includes_needed: set[str] = set()
        self.use_runtime = True
        self.append_map: dict[str, str] = {}  # container -> appended_item (from pre-pass)
        # Initialize type inference engine with C++-specific strategies
        self.type_inference_engine = create_cpp_type_inference_engine()

    def convert_code(self, source_code: str) -> str:
        """Convert Python source code to C++ code."""
        try:
            tree = ast.parse(source_code)
            return self._convert_module(tree)
        except (UnsupportedFeatureError, TypeMappingError):
            # Re-raise our specific exceptions without wrapping
            raise
        except Exception as e:
            raise UnsupportedFeatureError(f"Failed to convert Python code: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to C++ code."""
        parts = []

        # Check for advanced features to enable appropriate includes
        self.uses_comprehensions = self._uses_comprehensions(node)
        self.uses_classes = self._uses_classes(node)

        # First pass: check for string methods to populate includes_needed
        self._detect_string_methods(node)

        # Add includes
        parts.extend(self._generate_includes())
        parts.append("")

        # Add using namespace for convenience
        parts.append("using namespace std;")
        parts.append("using namespace multigen;")
        parts.append("")

        # Convert functions and classes
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef):
                parts.append(self._convert_function(stmt))
                parts.append("")
            elif isinstance(stmt, ast.ClassDef):
                parts.append(self._convert_class(stmt))
                parts.append("")
            else:
                # Handle other top-level statements if needed
                parts.append(f"// TODO: Handle {type(stmt).__name__}")

        return "\n".join(parts)

    def _generate_includes(self) -> list[str]:
        """Generate necessary C++ includes based on code analysis."""
        includes = [
            "#include <iostream>",
            "#include <string>",
            "#include <vector>",
            "#include <map>",
            "#include <unordered_map>",
            "#include <set>",
            "#include <unordered_set>",
            "#include <algorithm>",
            "#include <memory>",
            "#include <cassert>",
        ]

        # Add MultiGen runtime
        includes.append('#include "runtime/multigen_cpp_runtime.hpp"')

        # Add string operations if string methods detected
        if self.includes_needed:
            # StringOps class is included in the runtime
            pass

        return includes

    def _uses_comprehensions(self, node: ast.Module) -> bool:
        """Check if the module uses list/dict/set comprehensions."""
        for n in ast.walk(node):
            if isinstance(n, (ast.ListComp, ast.DictComp, ast.SetComp)):
                return True
        return False

    def _uses_classes(self, node: ast.Module) -> bool:
        """Check if the module uses class definitions."""
        for n in ast.walk(node):
            if isinstance(n, ast.ClassDef):
                return True
        return False

    def _detect_string_methods(self, node: ast.Module) -> None:
        """Detect string method usage to add appropriate includes."""
        for n in ast.walk(node):
            if isinstance(n, ast.Attribute):
                if n.attr in ["upper", "lower", "strip", "find", "replace", "split"]:
                    self.includes_needed.add("string_ops")

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert a Python function to C++."""
        self.current_function = node.name
        self.variable_context.clear()

        # Get return type
        return_type = self._get_return_type(node)

        # Pre-pass 0: Analyze parameter usage to detect nested containers
        nested_params = self._analyze_nested_subscripts(node.body)

        # Get parameters with types
        params = []
        for arg in node.args.args:
            param_name = arg.arg
            param_type = self._get_param_type(arg)

            # If parameter is used with nested subscripting and is bare list, make it nested
            if param_name in nested_params and param_type == "std::vector<int>":
                param_type = "std::vector<std::vector<int>>"

            params.append(f"{param_type} {param_name}")
            self.variable_context[param_name] = param_type

        # Pre-pass 1: Build append map
        self.append_map = self._analyze_append_operations(node.body)

        # Pre-pass 2: Infer all variable types (including nested containers)
        self._infer_all_variable_types(node.body)

        # Generate function body
        body_parts = []
        for stmt in node.body:
            converted = self._convert_statement(stmt)
            if converted.strip():
                body_parts.append(converted)

        body = "\n".join(body_parts)

        # Build function
        param_str = ", ".join(params)
        function = f"{return_type} {node.name}({param_str}) {{\n{self._indent_block(body)}\n}}"

        self.current_function = None
        self.append_map = {}  # Clear after function
        return function

    def _analyze_nested_subscripts(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect variables used with nested subscripts like a[i][j]."""
        nested_vars: set[str] = set()

        def check_expr(expr: ast.expr) -> None:
            # Check for nested subscript: outer[index1][index2]
            if isinstance(expr, ast.Subscript):
                # Check if the value being subscripted is itself a subscript
                if isinstance(expr.value, ast.Subscript):
                    # Get the base variable name
                    base = expr.value.value
                    if isinstance(base, ast.Name):
                        nested_vars.add(base.id)
                # Recursively check the subscripted value
                check_expr(expr.value)
                if not isinstance(expr.slice, ast.Slice):
                    check_expr(expr.slice)
            elif isinstance(expr, ast.BinOp):
                check_expr(expr.left)
                check_expr(expr.right)
            elif isinstance(expr, ast.Call):
                for arg in expr.args:
                    check_expr(arg)
            elif isinstance(expr, ast.Compare):
                check_expr(expr.left)
                for comp in expr.comparators:
                    check_expr(comp)

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    check_expr(target)
                check_expr(stmt.value)
            elif isinstance(stmt, ast.AnnAssign):
                if stmt.value:
                    check_expr(stmt.value)
            elif isinstance(stmt, ast.AugAssign):
                check_expr(stmt.target)
                check_expr(stmt.value)
            elif isinstance(stmt, ast.Expr):
                check_expr(stmt.value)
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
                if hasattr(stmt, "orelse"):
                    for s in stmt.orelse:
                        check_stmt(s)
            elif isinstance(stmt, ast.If):
                check_expr(stmt.test)
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)
            elif isinstance(stmt, ast.Return) and stmt.value:
                check_expr(stmt.value)

        for stmt in stmts:
            check_stmt(stmt)

        return nested_vars

    def _infer_all_variable_types(self, stmts: list[ast.stmt]) -> None:
        """Pre-pass to infer all variable types, including nested containers."""
        # First pass: collect initial types from annotations
        initial_types: dict[str, str] = {}

        def collect_initial_types(stmts: list[ast.stmt]) -> None:
            for stmt in stmts:
                if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                    var_name = stmt.target.id
                    var_type = self._convert_type_annotation(stmt.annotation)

                    # Infer from value if available
                    if var_type == "auto" and stmt.value:
                        inferred = self._infer_type_from_value(stmt.value)
                        if inferred != "auto":
                            var_type = inferred
                        elif isinstance(stmt.value, ast.List) and not stmt.value.elts:
                            var_type = "std::vector<int>"  # Default for now
                        elif isinstance(stmt.value, ast.Dict) and not stmt.value.keys:
                            var_type = "std::unordered_map<int, int>"

                    initial_types[var_name] = var_type

                # Recurse into loops and conditionals
                if isinstance(stmt, (ast.For, ast.While)):
                    collect_initial_types(stmt.body)
                    if hasattr(stmt, "orelse"):
                        collect_initial_types(stmt.orelse)
                elif isinstance(stmt, ast.If):
                    collect_initial_types(stmt.body)
                    collect_initial_types(stmt.orelse)

        collect_initial_types(stmts)

        # Second pass: refine types based on append operations
        # If a container has a vector appended to it, it's a vector of vectors
        for container_name, appended_var in self.append_map.items():
            if appended_var in initial_types:
                appended_type = initial_types[appended_var]
                if appended_type.startswith("std::vector<"):
                    # Upgrade container to nested type
                    initial_types[container_name] = f"std::vector<{appended_type}>"

        # Third pass: detect nested subscripting and upgrade vector<int> to vector<vector<int>>
        nested_vars = self._analyze_nested_subscripts(stmts)
        for var_name in nested_vars:
            if var_name in initial_types:
                current_type = initial_types[var_name]
                # Upgrade both vector<int> and auto to nested vectors
                if current_type == "std::vector<int>" or current_type == "auto":
                    initial_types[var_name] = "std::vector<std::vector<int>>"

        # Fourth pass: detect string-keyed dict usage
        string_keyed_dicts = self._analyze_dict_key_types(stmts)
        for var_name, key_type in string_keyed_dicts.items():
            if var_name in initial_types:
                current_type = initial_types[var_name]
                # If it's a default dict<int, int>, upgrade to dict<string, int>
                if current_type == "std::unordered_map<int, int>" and key_type == "std::string":
                    initial_types[var_name] = "std::unordered_map<std::string, int>"

        # Store in variable_context
        self.variable_context.update(initial_types)

    def _analyze_dict_key_types(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Analyze dict subscript and .count() usage to infer key types."""
        key_types: dict[str, str] = {}

        def check_for_dict_usage(expr: ast.expr) -> None:
            # Check for dict[key] subscripting
            if isinstance(expr, ast.Subscript) and isinstance(expr.value, ast.Name):
                dict_name = expr.value.id
                # Check if the index is a string literal or variable
                if isinstance(expr.slice, ast.Constant) and isinstance(expr.slice.value, str):
                    # String literal key - definitely a string-keyed dict
                    key_types[dict_name] = "std::string"
                elif isinstance(expr.slice, ast.Name):
                    # Variable key - check if it looks like a string variable
                    if expr.slice.id.endswith("word") or expr.slice.id.endswith("_word"):
                        key_types[dict_name] = "std::string"
            # Check for dict.count(key) method calls
            elif isinstance(expr, ast.Call):
                if isinstance(expr.func, ast.Attribute) and expr.func.attr == "count":
                    if isinstance(expr.func.value, ast.Name) and expr.args:
                        dict_name = expr.func.value.id
                        # Check the argument type
                        if isinstance(expr.args[0], ast.Constant) and isinstance(expr.args[0].value, str):
                            # String literal argument
                            key_types[dict_name] = "std::string"
                        elif isinstance(expr.args[0], ast.Name):
                            if expr.args[0].id.endswith("word") or expr.args[0].id.endswith("_word"):
                                key_types[dict_name] = "std::string"
                # Recursively check call arguments
                for arg in expr.args:
                    check_for_dict_usage(arg)
            # Recursively check other expression types
            elif isinstance(expr, ast.BinOp):
                check_for_dict_usage(expr.left)
                check_for_dict_usage(expr.right)
            elif isinstance(expr, ast.Compare):
                check_for_dict_usage(expr.left)
                for comp in expr.comparators:
                    check_for_dict_usage(comp)

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    check_for_dict_usage(target)
                check_for_dict_usage(stmt.value)
            elif isinstance(stmt, ast.AnnAssign):
                if stmt.value:
                    check_for_dict_usage(stmt.value)
            elif isinstance(stmt, ast.AugAssign):
                check_for_dict_usage(stmt.target)
                check_for_dict_usage(stmt.value)
            elif isinstance(stmt, ast.Expr):
                check_for_dict_usage(stmt.value)
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
                if hasattr(stmt, "orelse"):
                    for s in stmt.orelse:
                        check_stmt(s)
            elif isinstance(stmt, ast.If):
                check_for_dict_usage(stmt.test)
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)
            elif isinstance(stmt, ast.Return) and stmt.value:
                check_for_dict_usage(stmt.value)

        for stmt in stmts:
            check_stmt(stmt)

        return key_types

    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert a Python class to C++ class."""
        class_name = node.name

        # Extract instance variables and methods
        instance_vars = self._extract_instance_variables(node)
        methods = self._extract_methods(node)

        # Store class info for method resolution
        self.defined_classes[class_name] = {"attributes": instance_vars, "methods": [m.name for m in methods]}

        # Generate class parts
        parts = []
        parts.append(f"class {class_name} {{")
        parts.append("public:")

        # Add instance variables
        if instance_vars:
            parts.append("    // Instance variables")
            for var_name, var_type in instance_vars.items():
                parts.append(f"    {var_type} {var_name};")
            parts.append("")

        # Add constructor if __init__ exists
        init_method = None
        for method in methods:
            if method.name == "__init__":
                init_method = method
                break

        if init_method:
            parts.append(self._generate_constructor(class_name, init_method, instance_vars))
            parts.append("")

        # Add other methods
        for method in methods:
            if method.name != "__init__":
                parts.append(self._generate_method(class_name, method))
                parts.append("")

        parts.append("};")
        return "\n".join(parts)

    def _extract_instance_variables(self, class_node: ast.ClassDef) -> dict[str, str]:
        """Extract instance variables from __init__ method."""
        instance_vars = {}

        # Find __init__ method
        for stmt in class_node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "__init__":
                # Look for self.attribute assignments
                for init_stmt in stmt.body:
                    if isinstance(init_stmt, ast.AnnAssign) and isinstance(init_stmt.target, ast.Attribute):
                        # self.attr: type = value
                        if isinstance(init_stmt.target.value, ast.Name) and init_stmt.target.value.id == "self":
                            attr_name = init_stmt.target.attr
                            attr_type = self._convert_type_annotation(init_stmt.annotation)
                            instance_vars[attr_name] = attr_type
                    elif isinstance(init_stmt, ast.Assign):
                        # self.attr = value
                        for target in init_stmt.targets:
                            if (
                                isinstance(target, ast.Attribute)
                                and isinstance(target.value, ast.Name)
                                and target.value.id == "self"
                            ):
                                attr_name = target.attr
                                attr_type = self._infer_type_from_value(init_stmt.value)
                                instance_vars[attr_name] = attr_type

        return instance_vars

    def _extract_methods(self, class_node: ast.ClassDef) -> list[ast.FunctionDef]:
        """Extract method definitions from class."""
        methods = []
        for stmt in class_node.body:
            if isinstance(stmt, ast.FunctionDef):
                methods.append(stmt)
        return methods

    def _generate_constructor(
        self, class_name: str, init_method: ast.FunctionDef, instance_vars: dict[str, str]
    ) -> str:
        """Generate C++ constructor from Python __init__."""
        # Get constructor parameters (skip 'self')
        params = []
        for arg in init_method.args.args[1:]:  # Skip 'self'
            param_type = self._get_param_type(arg)
            param_name = arg.arg
            params.append(f"{param_type} {param_name}")
            self.variable_context[param_name] = param_type

        # Generate constructor body
        body_parts = []
        for stmt in init_method.body:
            if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Attribute):
                # self.attr: type = value
                if isinstance(stmt.target.value, ast.Name) and stmt.target.value.id == "self":
                    attr_name = stmt.target.attr
                    if stmt.value:
                        value_expr = self._convert_method_expression(stmt.value, class_name)
                        body_parts.append(f"        this->{attr_name} = {value_expr};")
            elif isinstance(stmt, ast.Assign):
                # self.attr = value
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        attr_name = target.attr
                        value_expr = self._convert_method_expression(stmt.value, class_name)
                        body_parts.append(f"        this->{attr_name} = {value_expr};")
            else:
                # Other statements in constructor - use method-aware conversion
                converted = self._convert_method_statement(stmt, class_name)
                if converted.strip():
                    body_parts.append(converted)

        # Build constructor
        param_str = ", ".join(params) if params else ""
        body = "\n".join(body_parts)
        constructor = f"    {class_name}({param_str}) {{\n{body}\n    }}"
        return constructor

    def _generate_method(self, class_name: str, method: ast.FunctionDef) -> str:
        """Generate C++ method from Python method."""
        self.current_function = f"{class_name}::{method.name}"

        # Get return type
        return_type = self._get_return_type(method)

        # Get parameters (skip 'self')
        params = []
        for arg in method.args.args[1:]:  # Skip 'self'
            param_type = self._get_param_type(arg)
            param_name = arg.arg
            params.append(f"{param_type} {param_name}")
            self.variable_context[param_name] = param_type

        # Generate method body
        body_parts = []
        for stmt in method.body:
            converted = self._convert_method_statement(stmt, class_name)
            if converted.strip():
                body_parts.append(converted)

        body = "\n".join(body_parts)

        # Build method
        param_str = ", ".join(params)
        method_def = f"    {return_type} {method.name}({param_str}) {{\n{self._indent_block(body)}\n    }}"

        self.current_function = None
        return method_def

    def _convert_method_statement(self, stmt: ast.stmt, class_name: str) -> str:
        """Convert a method statement with class context."""
        if isinstance(stmt, ast.Assign):
            return self._convert_method_assignment(stmt, class_name)
        elif isinstance(stmt, ast.AnnAssign):
            return self._convert_method_annotated_assignment(stmt, class_name)
        elif isinstance(stmt, ast.AugAssign):
            return self._convert_method_aug_assignment(stmt, class_name)
        elif isinstance(stmt, ast.Return):
            return self._convert_method_return(stmt, class_name)
        elif isinstance(stmt, ast.If):
            return self._convert_method_if(stmt, class_name)
        elif isinstance(stmt, ast.Expr):
            expr = self._convert_method_expression(stmt.value, class_name)
            return f"        {expr};"
        else:
            return self._convert_statement(stmt)

    def _convert_method_assignment(self, stmt: ast.Assign, class_name: str) -> str:
        """Convert method assignment with proper self handling."""
        value_expr = self._convert_method_expression(stmt.value, class_name)
        statements = []

        for target in stmt.targets:
            if isinstance(target, ast.Attribute):
                # self.attr = value -> this->attr = value
                if isinstance(target.value, ast.Name) and target.value.id == "self":
                    statements.append(f"        this->{target.attr} = {value_expr};")
                else:
                    # obj.attr = value
                    obj_expr = self._convert_method_expression(target.value, class_name)
                    statements.append(f"        {obj_expr}.{target.attr} = {value_expr};")
            elif isinstance(target, ast.Name):
                # Regular variable assignment
                var_type = self._infer_type_from_value(stmt.value)
                self.variable_context[target.id] = var_type
                statements.append(f"        {var_type} {target.id} = {value_expr};")

        return "\n".join(statements)

    def _convert_method_aug_assignment(self, stmt: ast.AugAssign, class_name: str) -> str:
        """Convert augmented assignment in method context with proper self handling."""
        value_expr = self._convert_method_expression(stmt.value, class_name)
        op = self._get_aug_op(stmt.op)

        if isinstance(stmt.target, ast.Attribute):
            # self.attr += value -> this->attr += value
            if isinstance(stmt.target.value, ast.Name) and stmt.target.value.id == "self":
                return f"        this->{stmt.target.attr} {op}= {value_expr};"
            else:
                # obj.attr += value
                obj_expr = self._convert_method_expression(stmt.target.value, class_name)
                return f"        {obj_expr}.{stmt.target.attr} {op}= {value_expr};"
        elif isinstance(stmt.target, ast.Name):
            # Regular variable augmented assignment
            return f"        {stmt.target.id} {op}= {value_expr};"

        return "        /* Unknown augmented assignment target */"

    def _convert_method_return(self, stmt: ast.Return, class_name: str) -> str:
        """Convert method return statement."""
        if stmt.value:
            value_expr = self._convert_method_expression(stmt.value, class_name)
            return f"        return {value_expr};"
        return "        return;"

    def _convert_method_if(self, stmt: ast.If, class_name: str) -> str:
        """Convert if statement in method context with proper self handling."""
        condition = self._convert_method_expression(stmt.test, class_name)
        then_body = self._convert_method_statements(stmt.body, class_name)
        if_part = f"        if ({condition}) {{\n{then_body}\n        }}"
        if stmt.orelse:
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                # elif chain
                else_body = self._convert_method_if(stmt.orelse[0], class_name).strip()
                return f"{if_part} else {else_body}"
            else:
                # regular else
                else_body = self._convert_method_statements(stmt.orelse, class_name)
                return f"{if_part} else {{\n{else_body}\n        }}"
        else:
            return if_part

    def _convert_method_statements(self, statements: list, class_name: str) -> str:
        """Convert a list of statements in method context."""
        converted = []
        for stmt in statements:
            converted.append(self._convert_method_statement(stmt, class_name))
        return "\n".join(converted)

    def _convert_method_expression(self, expr: ast.expr, class_name: str) -> str:
        """Convert method expression with class context."""
        if isinstance(expr, ast.Attribute):
            if isinstance(expr.value, ast.Name) and expr.value.id == "self":
                # self.attr -> this->attr
                return f"this->{expr.attr}"
            else:
                # obj.attr or obj.method()
                obj_expr = self._convert_method_expression(expr.value, class_name)
                return f"{obj_expr}.{expr.attr}"
        elif isinstance(expr, ast.Call):
            # Handle method calls within class context
            if isinstance(expr.func, ast.Attribute):
                if isinstance(expr.func.value, ast.Name) and expr.func.value.id == "self":
                    # self.method() -> handle string methods and other method calls
                    method_name = expr.func.attr
                    [self._convert_method_expression(arg, class_name) for arg in expr.args]

                    # Handle string methods on self attributes
                    if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                        obj_expr = f"this->{expr.func.attr}"  # This would be wrong for method calls
                        # Actually, this case shouldn't happen - string methods are called on attributes
                        pass

                    # Regular method calls would need more complex handling
                    return f"/* Method call: {method_name} */"
                else:
                    # Regular method/attribute calls
                    return self._convert_method_call(expr)
            else:
                return self._convert_call(expr)
        elif isinstance(expr, ast.BinOp):
            # Handle binary operations with proper self conversion
            left = self._convert_method_expression(expr.left, class_name)
            right = self._convert_method_expression(expr.right, class_name)

            if isinstance(expr.op, ast.Pow):
                return f"pow({left}, {right})"
            elif isinstance(expr.op, ast.FloorDiv):
                # C++ integer division is already floor division
                return f"({left} / {right})"

            # Use standard operator mapping from converter_utils
            op = get_standard_binary_operator(expr.op)
            if op is None:
                op = "/*UNKNOWN_OP*/"
            return f"({left} {op} {right})"
        elif isinstance(expr, ast.Compare):
            # Handle comparison operations with proper self conversion
            left = self._convert_method_expression(expr.left, class_name)
            result = left

            for op_node, comp in zip(expr.ops, expr.comparators):
                # Use standard comparison operator mapping from converter_utils
                op_str_val = get_standard_comparison_operator(op_node)

                # Handle C++-specific operators
                if op_str_val is None:
                    if isinstance(op_node, ast.Is):
                        op_str_val = "=="
                    elif isinstance(op_node, ast.IsNot):
                        op_str_val = "!="
                    else:
                        op_str_val = "/*UNKNOWN_OP*/"

                comp_expr = self._convert_method_expression(comp, class_name)
                result = f"({result} {op_str_val} {comp_expr})"

            return result
        elif isinstance(expr, ast.ListComp):
            return self._convert_method_list_comprehension(expr, class_name)
        elif isinstance(expr, ast.DictComp):
            return self._convert_method_dict_comprehension(expr, class_name)
        elif isinstance(expr, ast.SetComp):
            return self._convert_method_set_comprehension(expr, class_name)
        elif isinstance(expr, ast.Name):
            # Handle regular variable names
            return expr.id
        else:
            return self._convert_expression(expr)

    def _convert_method_list_comprehension(self, expr: ast.ListComp, class_name: str) -> str:
        """Convert list comprehensions in method context using STL and runtime helpers."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            # Range-based comprehension
            range_args = [self._convert_method_expression(arg, class_name) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"
            # Create lambda for transformation
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = (
                f"[]({target_name}) {{ return {self._convert_method_expression(element_expr, class_name)}; }}"
            )

            if conditions:
                # Comprehension with condition
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"list_comprehension({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                # Simple comprehension
                return f"list_comprehension({range_call}, {transform_lambda})"
        else:
            # Iteration over container (like words)
            container_expr = self._convert_method_expression(iter_expr, class_name)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = (
                f"[]({target_name}) {{ return {self._convert_method_expression(element_expr, class_name)}; }}"
            )

            if conditions:
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"list_comprehension({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"list_comprehension({container_expr}, {transform_lambda})"

    def _convert_method_dict_comprehension(self, expr: ast.DictComp, class_name: str) -> str:
        """Convert dict comprehensions in method context."""
        # Extract comprehension components
        key_expr = expr.key
        value_expr = expr.value
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            range_args = [self._convert_method_expression(arg, class_name) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"
            target_name = target.id if isinstance(target, ast.Name) else "x"

            # Use make_pair for key-value pairs
            key_transform = self._convert_method_expression(key_expr, class_name)
            value_transform = self._convert_method_expression(value_expr, class_name)
            transform_lambda = f"[]({target_name}) {{ return make_pair({key_transform}, {value_transform}); }}"

            if conditions:
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"dict_comprehension({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                return f"dict_comprehension({range_call}, {transform_lambda})"
        else:
            container_expr = self._convert_method_expression(iter_expr, class_name)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            key_transform = self._convert_method_expression(key_expr, class_name)
            value_transform = self._convert_method_expression(value_expr, class_name)
            transform_lambda = f"[]({target_name}) {{ return make_pair({key_transform}, {value_transform}); }}"

            if conditions:
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"dict_comprehension({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"dict_comprehension({container_expr}, {transform_lambda})"

    def _convert_method_set_comprehension(self, expr: ast.SetComp, class_name: str) -> str:
        """Convert set comprehensions in method context."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            range_args = [self._convert_method_expression(arg, class_name) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = (
                f"[]({target_name}) {{ return {self._convert_method_expression(element_expr, class_name)}; }}"
            )

            if conditions:
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"set_comprehension({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                return f"set_comprehension({range_call}, {transform_lambda})"
        else:
            container_expr = self._convert_method_expression(iter_expr, class_name)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = (
                f"[]({target_name}) {{ return {self._convert_method_expression(element_expr, class_name)}; }}"
            )

            if conditions:
                condition_lambda = (
                    f"[]({target_name}) {{ return {self._convert_method_expression(conditions[0], class_name)}; }}"
                )
                return f"set_comprehension({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"set_comprehension({container_expr}, {transform_lambda})"

    def _convert_statement(self, stmt: ast.stmt) -> str:
        """Convert a Python statement to C++."""
        if isinstance(stmt, ast.Return):
            return self._convert_return(stmt)
        elif isinstance(stmt, ast.Assign):
            return self._convert_assignment(stmt)
        elif isinstance(stmt, ast.AnnAssign):
            return self._convert_annotated_assignment(stmt)
        elif isinstance(stmt, ast.AugAssign):
            return self._convert_aug_assignment(stmt)
        elif isinstance(stmt, ast.If):
            return self._convert_if(stmt)
        elif isinstance(stmt, ast.While):
            return self._convert_while(stmt)
        elif isinstance(stmt, ast.For):
            return self._convert_for(stmt)
        elif isinstance(stmt, ast.Expr):
            return self._convert_expression_statement(stmt)
        elif isinstance(stmt, ast.Pass):
            return "        // pass"
        elif isinstance(stmt, ast.Assert):
            return self._convert_assert(stmt)
        else:
            raise UnsupportedFeatureError(f"Unsupported statement type: {type(stmt).__name__}")

    def _convert_assert(self, stmt: ast.Assert) -> str:
        """Convert Python assert statement to C++ assert() call.

        Args:
            stmt: Python assert statement node

        Returns:
            C++ assert() call as string

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
                return f"        assert({test_expr}); // {msg}"
            else:
                # Complex message expression - just add assert without comment
                return f"        assert({test_expr});"
        else:
            return f"        assert({test_expr});"

    def _convert_return(self, stmt: ast.Return) -> str:
        """Convert return statement.

        Special handling for main(): In C++, main() should return 0 for success.
        Python programs may return meaningful values, but Unix convention is
        0 = success, non-zero = failure.
        """
        # Special case: main() should always return 0 for Unix compatibility
        if stmt.value and self.current_function == "main":
            return "        return 0;"

        if stmt.value:
            value_expr = self._convert_expression(stmt.value)
            return f"        return {value_expr};"
        return "        return;"

    def _convert_assignment(self, stmt: ast.Assign) -> str:
        """Convert assignment statement."""
        value_expr = self._convert_expression(stmt.value)
        statements = []

        for target in stmt.targets:
            if isinstance(target, ast.Name):
                var_name = target.id

                # Check if variable already exists in context
                if var_name in self.variable_context:
                    # Variable already declared - just reassign
                    statements.append(f"        {var_name} = {value_expr};")
                else:
                    # New variable - declare with type
                    var_type = self._infer_type_from_value(stmt.value)
                    self.variable_context[var_name] = var_type
                    statements.append(f"        {var_type} {var_name} = {value_expr};")
            else:
                # Handle other target types (attributes, subscripts, etc.)
                target_expr = self._convert_expression(target)
                statements.append(f"        {target_expr} = {value_expr};")

        return "\n".join(statements)

    def _convert_annotated_assignment(self, stmt: ast.AnnAssign) -> str:
        """Convert annotated assignment (var: type = value)."""
        if isinstance(stmt.target, ast.Name):
            var_name = stmt.target.id

            # Check if we already inferred the type in the pre-pass
            if var_name in self.variable_context:
                # Use the pre-computed type from _infer_all_variable_types
                var_type = self.variable_context[var_name]
            else:
                # Fallback to computing the type now
                var_type = self._convert_type_annotation(stmt.annotation)

                # If annotation gives us "auto" but we have a value, try to infer concrete type
                if var_type == "auto" and stmt.value:
                    inferred_type = self._infer_type_from_value(stmt.value)
                    # Use inferred type if it's not also "auto"
                    if inferred_type != "auto":
                        var_type = inferred_type
                    # For empty containers with "auto", use default concrete types
                    elif isinstance(stmt.value, (ast.List, ast.Dict)):
                        if isinstance(stmt.value, ast.List) and not stmt.value.elts:
                            var_type = "std::vector<int>"
                        elif isinstance(stmt.value, ast.Dict) and not stmt.value.keys:
                            var_type = "std::unordered_map<int, int>"

                self.variable_context[var_name] = var_type

            if stmt.value:
                value_expr = self._convert_expression(stmt.value)
                return f"        {var_type} {var_name} = {value_expr};"
            else:
                return f"        {var_type} {var_name};"
        else:
            # Handle attribute annotations
            target_expr = self._convert_expression(stmt.target)
            if stmt.value:
                value_expr = self._convert_expression(stmt.value)
                return f"        {target_expr} = {value_expr};"
        return ""

    def _convert_method_annotated_assignment(self, stmt: ast.AnnAssign, class_name: str) -> str:
        """Convert annotated assignment in method context (var: type = value)."""
        if isinstance(stmt.target, ast.Name):
            var_name = stmt.target.id
            var_type = self._convert_type_annotation(stmt.annotation)
            self.variable_context[var_name] = var_type

            if stmt.value:
                value_expr = self._convert_method_expression(stmt.value, class_name)
                return f"        {var_type} {var_name} = {value_expr};"
            else:
                return f"        {var_type} {var_name};"
        else:
            # Handle attribute annotations
            target_expr = self._convert_method_expression(stmt.target, class_name)
            if stmt.value:
                value_expr = self._convert_method_expression(stmt.value, class_name)
                return f"        {target_expr} = {value_expr};"
        return ""

    def _convert_aug_assignment(self, stmt: ast.AugAssign) -> str:
        """Convert augmented assignment (+=, -=, etc.)."""
        target_expr = self._convert_expression(stmt.target)
        value_expr = self._convert_expression(stmt.value)
        op = self._get_aug_op(stmt.op)
        return f"        {target_expr} {op}= {value_expr};"

    def _convert_if(self, stmt: ast.If) -> str:
        """Convert if statement."""
        condition = self._convert_expression(stmt.test)
        then_body = self._convert_statements(stmt.body)

        if_part = f"        if ({condition}) {{\n{then_body}\n        }}"

        if stmt.orelse:
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                # elif chain
                else_body = self._convert_if(stmt.orelse[0]).strip()
                return f"{if_part} else {else_body}"
            else:
                # regular else
                else_body = self._convert_statements(stmt.orelse)
                return f"{if_part} else {{\n{else_body}\n        }}"

        return if_part

    def _convert_while(self, stmt: ast.While) -> str:
        """Convert while loop."""
        condition = self._convert_expression(stmt.test)
        body = self._convert_statements(stmt.body)
        return f"        while ({condition}) {{\n{body}\n        }}"

    def _convert_for(self, stmt: ast.For) -> str:
        """Convert for loop (range-based or container iteration)."""
        target_name = stmt.target.id if isinstance(stmt.target, ast.Name) else "iter_var"

        if isinstance(stmt.iter, ast.Call) and isinstance(stmt.iter.func, ast.Name) and stmt.iter.func.id == "range":
            # range() loops
            args = stmt.iter.args
            if len(args) == 1:
                # range(n)
                limit = self._convert_expression(args[0])
                init = f"int {target_name} = 0"
                condition = f"{target_name} < {limit}"
                update = f"{target_name}++"
            elif len(args) == 2:
                # range(start, stop)
                start = self._convert_expression(args[0])
                stop = self._convert_expression(args[1])
                init = f"int {target_name} = {start}"
                condition = f"{target_name} < {stop}"
                update = f"{target_name}++"
            elif len(args) == 3:
                # range(start, stop, step)
                start = self._convert_expression(args[0])
                stop = self._convert_expression(args[1])
                step = self._convert_expression(args[2])
                init = f"int {target_name} = {start}"
                condition = f"{target_name} < {stop}"
                update = f"{target_name} += {step}"
            else:
                raise UnsupportedFeatureError("Invalid range() arguments")

            body = self._convert_statements(stmt.body)
            return f"        for ({init}; {condition}; {update}) {{\n{body}\n        }}"
        else:
            # Range-based for loop for containers
            iter_expr = self._convert_expression(stmt.iter)
            body = self._convert_statements(stmt.body)
            return f"        for (auto {target_name} : {iter_expr}) {{\n{body}\n        }}"

    def _convert_expression_statement(self, stmt: ast.Expr) -> str:
        """Convert expression statement."""
        # Skip docstrings (string constants as statements)
        if isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            return f"        // {stmt.value.value}"
        expr = self._convert_expression(stmt.value)
        return f"        {expr};"

    def _convert_expression(self, expr: ast.expr) -> str:
        """Convert a Python expression to C++."""
        if isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        elif isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.BinOp):
            return self._convert_binary_op(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self._convert_unary_op(expr)
        elif isinstance(expr, ast.BoolOp):
            return self._convert_bool_op(expr)
        elif isinstance(expr, ast.Compare):
            return self._convert_compare(expr)
        elif isinstance(expr, ast.Call):
            return self._convert_call(expr)
        elif isinstance(expr, ast.Attribute):
            return self._convert_attribute(expr)
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
        elif isinstance(expr, ast.Subscript):
            return self._convert_subscript(expr)
        elif isinstance(expr, ast.JoinedStr):
            return self._convert_f_string(expr)
        else:
            raise UnsupportedFeatureError(f"Unsupported expression type: {type(expr).__name__}")

    def _convert_constant(self, expr: ast.Constant) -> str:
        """Convert constant values."""
        value = expr.value
        if isinstance(value, str):
            return f'"{value}"'
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, (int, float)):
            return str(value)
        elif value is None:
            return "nullptr"
        else:
            return str(value)

    def _convert_binary_op(self, expr: ast.BinOp) -> str:
        """Convert binary operations."""
        left = self._convert_expression(expr.left)
        right = self._convert_expression(expr.right)

        if isinstance(expr.op, ast.Pow):
            return f"pow({left}, {right})"
        elif isinstance(expr.op, ast.FloorDiv):
            # C++ integer division is already floor division
            return f"({left} / {right})"

        # Use standard operator mapping from converter_utils
        op = get_standard_binary_operator(expr.op)
        if op is None:
            op = "/*UNKNOWN_OP*/"
        return f"({left} {op} {right})"

    def _convert_unary_op(self, expr: ast.UnaryOp) -> str:
        """Convert unary operations."""
        operand = self._convert_expression(expr.operand)

        # Use standard unary operator mapping from converter_utils
        op = get_standard_unary_operator(expr.op)
        if op is None:
            op = "/*UNKNOWN_UNARY*/"
        return f"({op}{operand})"

    def _convert_bool_op(self, expr: ast.BoolOp) -> str:
        """Convert boolean operations."""
        op = " && " if isinstance(expr.op, ast.And) else " || "
        values = [self._convert_expression(v) for v in expr.values]
        return f"({op.join(values)})"

    def _convert_compare(self, expr: ast.Compare) -> str:
        """Convert comparison operations."""
        left = self._convert_expression(expr.left)
        result = left

        for op, comparator in zip(expr.ops, expr.comparators):
            right = self._convert_expression(comparator)

            # Use standard comparison operator mapping from converter_utils
            cpp_op = get_standard_comparison_operator(op)

            # Handle C++-specific operators
            if cpp_op is None:
                if isinstance(op, ast.Is):
                    cpp_op = "=="
                elif isinstance(op, ast.IsNot):
                    cpp_op = "!="
                elif isinstance(op, ast.In):
                    # Use .count() or .find() for membership testing
                    result = f"({right}.count({result}) > 0)"
                    continue
                elif isinstance(op, ast.NotIn):
                    result = f"({right}.count({result}) == 0)"
                    continue
                else:
                    cpp_op = "/*UNKNOWN_CMP*/"

            result = f"({result} {cpp_op} {right})"

        return result

    def _convert_call(self, expr: ast.Call) -> str:
        """Convert function calls."""
        if isinstance(expr.func, ast.Name):
            func_name = expr.func.id
            args = [self._convert_expression(arg) for arg in expr.args]

            # Handle empty container constructors
            if func_name == "set" and len(args) == 0:
                # set() with no args -> std::unordered_set<int>{}
                return "std::unordered_set<int>{}"
            elif func_name == "dict" and len(args) == 0:
                # dict() with no args -> std::unordered_map<int, int>{}
                return "std::unordered_map<int, int>{}"
            elif func_name == "list" and len(args) == 0:
                # list() with no args -> std::vector<int>{}
                return "std::vector<int>{}"

            # Map Python built-ins to C++ equivalents
            builtin_map = {
                "print": "cout",
                "len": "multigen::len",
                "abs": "multigen::abs",
                "min": "multigen::min",
                "max": "multigen::max",
                "sum": "multigen::sum",
                "any": "multigen::any",
                "all": "multigen::all",
                "bool": "multigen::bool_value",
                "range": "Range",
            }

            if func_name in builtin_map:
                mapped_name = builtin_map[func_name]
                if func_name == "print":
                    if args:
                        separator = ' << " " << '
                        return f"cout << {separator.join(args)} << endl"
                    else:
                        return "cout << endl"
                elif func_name == "range":
                    return f"Range({', '.join(args)})"
                else:
                    return f"{mapped_name}({', '.join(args)})"
            else:
                return f"{func_name}({', '.join(args)})"

        elif isinstance(expr.func, ast.Attribute):
            return self._convert_method_call(expr)
        else:
            return "/* Complex function call */"

    def _convert_method_call(self, expr: ast.Call) -> str:
        """Convert method calls including string methods."""
        if isinstance(expr.func, ast.Attribute):
            # Check if this is a method call on 'self' attribute
            if (
                isinstance(expr.func.value, ast.Attribute)
                and isinstance(expr.func.value.value, ast.Name)
                and expr.func.value.value.id == "self"
            ):
                # self.attr.method() -> handle string methods on instance variables
                obj_expr = f"this->{expr.func.value.attr}"
            else:
                obj_expr = self._convert_expression(expr.func.value)

            method_name = expr.func.attr
            args = [self._convert_expression(arg) for arg in expr.args]

            # Handle string methods
            if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                if method_name == "upper":
                    return f"StringOps::upper({obj_expr})"
                elif method_name == "lower":
                    return f"StringOps::lower({obj_expr})"
                elif method_name == "strip":
                    if args:
                        return f"StringOps::strip({obj_expr}, {args[0]})"
                    else:
                        return f"StringOps::strip({obj_expr})"
                elif method_name == "find":
                    return f"StringOps::find({obj_expr}, {args[0]})"
                elif method_name == "replace":
                    return f"StringOps::replace({obj_expr}, {args[0]}, {args[1]})"
                elif method_name == "split":
                    if args:
                        return f"StringOps::split({obj_expr}, {args[0]})"
                    else:
                        return f"StringOps::split({obj_expr})"

            # Handle container methods - map Python names to C++ names
            if method_name == "append":
                # Python's append -> C++'s push_back
                # Also check if we need to update the container type to handle nested containers
                if isinstance(expr.func.value, ast.Name) and expr.args:
                    container_name = expr.func.value.id
                    # Check if the argument is a variable with a known type
                    if isinstance(expr.args[0], ast.Name):
                        appended_var = expr.args[0].id
                        if appended_var in self.variable_context:
                            appended_type = self.variable_context[appended_var]
                            # If appending a vector to a container that's currently vector<int>,
                            # upgrade it to vector<vector<...>>
                            if container_name in self.variable_context:
                                current_type = self.variable_context[container_name]
                                if appended_type.startswith("std::vector<") and current_type == "std::vector<int>":
                                    # This is likely a case where we defaulted to vector<int> but it's really nested
                                    # Update the type in our context
                                    self.variable_context[container_name] = f"std::vector<{appended_type}>"

                return f"{obj_expr}.push_back({', '.join(args)})"

            # Handle dict.items() - in C++, we just iterate over the map directly
            # Range-based for automatically gives us pairs
            if method_name == "items":
                return obj_expr  # Just return the map itself

            # Handle dict.values() - extract values from map
            # We'll use a lambda-based values extractor helper
            if method_name == "values":
                # Use a helper function from runtime to extract values
                return f"multigen::values({obj_expr})"

            # Handle set methods - map Python names to C++ names
            if method_name == "add":
                # Python's set.add() -> C++'s insert()
                return f"{obj_expr}.insert({', '.join(args)})"
            elif method_name == "remove":
                # Python's set.remove() -> C++'s erase()
                return f"{obj_expr}.erase({', '.join(args)})"
            elif method_name == "discard":
                # Python's set.discard() -> C++'s erase() (doesn't throw if missing)
                # In C++, erase() doesn't throw, so it matches discard() semantics
                return f"{obj_expr}.erase({', '.join(args)})"

            # Regular method calls
            if args:
                return f"{obj_expr}.{method_name}({', '.join(args)})"
            else:
                return f"{obj_expr}.{method_name}()"

        return "/* Unknown method call */"

    def _convert_attribute(self, expr: ast.Attribute) -> str:
        """Convert attribute access."""
        obj_expr = self._convert_expression(expr.value)
        return f"{obj_expr}.{expr.attr}"

    def _convert_list_comprehension(self, expr: ast.ListComp) -> str:
        """Convert list comprehensions using STL and runtime helpers."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            # Range-based comprehension
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"

            # Create lambda for transformation
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(element_expr)}; }}"

            if conditions:
                # With condition
                condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                return f"list_comprehension({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                # No condition
                return f"list_comprehension({range_call}, {transform_lambda})"
        else:
            # Container iteration (like iterating over a vector)
            container_expr = self._convert_expression(iter_expr)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(element_expr)}; }}"

            if conditions:
                condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                return f"list_comprehension({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"list_comprehension({container_expr}, {transform_lambda})"

    def _convert_dict_comprehension(self, expr: ast.DictComp) -> str:
        """Convert dictionary comprehensions using STL and runtime helpers."""
        # Extract comprehension components
        key_expr = expr.key
        value_expr = expr.value
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            # Range-based comprehension
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"

            # Create lambda for key-value pairs
            target_name = target.id if isinstance(target, ast.Name) else "x"
            key_val_lambda = f"[](auto {target_name}) {{ return std::make_pair({self._convert_expression(key_expr)}, {self._convert_expression(value_expr)}); }}"

            if conditions:
                # With condition
                condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                return f"dict_comprehension({range_call}, {key_val_lambda}, {condition_lambda})"
            else:
                # No condition
                return f"dict_comprehension({range_call}, {key_val_lambda})"
        else:
            # Container iteration (like iterating over a vector or map.items())
            container_expr = self._convert_expression(iter_expr)

            # Check if target is a tuple (e.g., for k, v in dict.items())
            if isinstance(target, ast.Tuple) and len(target.elts) == 2:
                # Tuple unpacking for pairs - use temporary pair and .first/.second
                # Note: C++17 doesn't support structured bindings in lambda parameters
                k_name = target.elts[0].id if isinstance(target.elts[0], ast.Name) else "k"
                v_name = target.elts[1].id if isinstance(target.elts[1], ast.Name) else "v"
                # Use pair.first and pair.second instead of structured bindings
                key_val_lambda = f"[](const auto& __pair) {{ const auto& {k_name} = __pair.first; const auto& {v_name} = __pair.second; return std::make_pair({self._convert_expression(key_expr)}, {self._convert_expression(value_expr)}); }}"

                if conditions:
                    condition_lambda = f"[](const auto& __pair) {{ const auto& {k_name} = __pair.first; const auto& {v_name} = __pair.second; return {self._convert_expression(conditions[0])}; }}"
                    return f"dict_comprehension({container_expr}, {key_val_lambda}, {condition_lambda})"
                else:
                    return f"dict_comprehension({container_expr}, {key_val_lambda})"
            else:
                # Simple target
                target_name = target.id if isinstance(target, ast.Name) else "x"
                key_val_lambda = f"[](auto {target_name}) {{ return std::make_pair({self._convert_expression(key_expr)}, {self._convert_expression(value_expr)}); }}"

                if conditions:
                    condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                    return f"dict_comprehension({container_expr}, {key_val_lambda}, {condition_lambda})"
                else:
                    return f"dict_comprehension({container_expr}, {key_val_lambda})"

    def _convert_set_comprehension(self, expr: ast.SetComp) -> str:
        """Convert set comprehensions using STL and runtime helpers."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            # Range-based comprehension
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"Range({', '.join(range_args)})"

            # Create lambda for transformation
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(element_expr)}; }}"

            if conditions:
                # With condition
                condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                return f"set_comprehension({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                # No condition
                return f"set_comprehension({range_call}, {transform_lambda})"
        else:
            # Container iteration (like iterating over a vector)
            container_expr = self._convert_expression(iter_expr)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(element_expr)}; }}"

            if conditions:
                condition_lambda = f"[](auto {target_name}) {{ return {self._convert_expression(conditions[0])}; }}"
                return f"set_comprehension({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"set_comprehension({container_expr}, {transform_lambda})"

    def _convert_list_literal(self, expr: ast.List) -> str:
        """Convert list literal to C++ initializer list."""
        if not expr.elts:
            return "{}"  # Empty initializer list
        elements = [self._convert_expression(elt) for elt in expr.elts]
        return "{" + ", ".join(elements) + "}"

    def _convert_dict_literal(self, expr: ast.Dict) -> str:
        """Convert dict literal to C++ initializer list."""
        if not expr.keys:
            return "{}"  # Empty initializer list
        pairs = []
        for key, value in zip(expr.keys, expr.values):
            if key is None:
                # Skip unpacking operations (e.g., **kwargs)
                continue
            key_str = self._convert_expression(key)
            val_str = self._convert_expression(value)
            pairs.append(f"{{{key_str}, {val_str}}}")
        return "{" + ", ".join(pairs) + "}"

    def _convert_set_literal(self, expr: ast.Set) -> str:
        """Convert set literal to C++ initializer list."""
        if not expr.elts:
            return "{}"  # Empty initializer list
        elements = [self._convert_expression(elt) for elt in expr.elts]
        return "{" + ", ".join(elements) + "}"

    def _convert_subscript(self, expr: ast.Subscript) -> str:
        """Convert subscript operation to C++ array access."""
        value_expr = self._convert_expression(expr.value)

        if isinstance(expr.slice, ast.Slice):
            # Handle slicing (not fully supported in C++, would need custom implementation)
            raise UnsupportedFeatureError("Slice operations not supported in C++ backend")
        else:
            # Simple subscript
            index_expr = self._convert_expression(expr.slice)
            return f"{value_expr}[{index_expr}]"

    def _convert_f_string(self, expr: ast.JoinedStr) -> str:
        """Convert f-string to C++ string concatenation.

        Example:
            f"Result: {x}" -> ("Result: " + std::to_string(x))
            f"Count: {len(items)} items" -> ("Count: " + std::to_string(multigen::len(items)) + " items")
        """
        parts: list[str] = []
        for value in expr.values:
            if isinstance(value, ast.Constant):
                # Literal string part
                if isinstance(value.value, str):
                    parts.append(f'"{value.value}"')
            elif isinstance(value, ast.FormattedValue):
                # Expression to be converted to string
                expr_code = self._convert_expression(value.value)
                parts.append(self._to_string_cpp(expr_code, value.value))

        if len(parts) == 0:
            return '""'
        elif len(parts) == 1:
            return parts[0]
        else:
            return "(" + " + ".join(parts) + ")"

    def _to_string_cpp(self, expr_code: str, expr_node: ast.expr) -> str:
        """Convert an expression to string in C++.

        Args:
            expr_code: The C++ code for the expression
            expr_node: The original AST node (for type inference)

        Returns:
            C++ code that converts the expression to a string
        """
        # Try to infer the type of the expression
        # For simple names, check variable context
        if isinstance(expr_node, ast.Name):
            var_name = expr_node.id
            if var_name in self.variable_context:
                cpp_type = self.variable_context[var_name]
                if cpp_type == "std::string":
                    return expr_code  # Already a string
                elif cpp_type == "bool":
                    # Convert bool to string ("true" or "false")
                    return f'({expr_code} ? "true" : "false")'

        # Check if it's already a string literal
        if isinstance(expr_node, ast.Constant) and isinstance(expr_node.value, str):
            return expr_code  # Already a string

        # For boolean values
        if isinstance(expr_node, ast.Constant) and isinstance(expr_node.value, bool):
            # Convert bool to string ("true" or "false")
            return f'({expr_code} ? "true" : "false")'

        # Check for string method calls or operations that return strings
        if isinstance(expr_node, ast.Call):
            if isinstance(expr_node.func, ast.Attribute):
                # String methods return strings
                attr_name = expr_node.func.attr
                if attr_name in {"lower", "upper", "strip", "replace", "join"}:
                    return expr_code  # Already returns string

        # Default: use std::to_string for numbers
        return f"std::to_string({expr_code})"

    def _convert_statements(self, statements: list[ast.stmt]) -> str:
        """Convert multiple statements."""
        converted_statements = []
        for stmt in statements:
            converted = self._convert_statement(stmt)
            if converted.strip():
                converted_statements.append(converted)
        return "\n".join(converted_statements)

    def _indent_block(self, text: str) -> str:
        """Add indentation to a block of text."""
        if not text.strip():
            return text
        lines = text.split("\n")
        indented_lines = []
        for line in lines:
            if line.strip():  # Don't indent empty lines
                indented_lines.append(f"    {line}")
            else:
                indented_lines.append(line)
        return "\n".join(indented_lines)

    def _get_return_type(self, func_node: ast.FunctionDef) -> str:
        """Get return type from function annotation."""
        if func_node.returns:
            if isinstance(func_node.returns, ast.Constant) and func_node.returns.value is None:
                return "void"
            return self._convert_type_annotation(func_node.returns)

        # Try to infer from return statements
        has_return_stmt = False
        for stmt in ast.walk(func_node):
            if isinstance(stmt, ast.Return) and stmt.value:
                has_return_stmt = True
                inferred_type = self._infer_type_from_value(stmt.value)
                # If we get an incomplete type like "std::vector", default to int specialization
                if inferred_type == "std::vector":
                    return "std::vector<int>"
                elif inferred_type == "std::unordered_map":
                    return "std::unordered_map<int, int>"  # Default to int keys
                elif inferred_type == "std::unordered_set":
                    return "std::unordered_set<int>"
                elif inferred_type and inferred_type != "auto":
                    return inferred_type

        # If we found return statements but couldn't infer type, use auto
        if has_return_stmt:
            return "auto"

        return "void"  # No return statements, so return void

    def _get_param_type(self, arg: ast.arg) -> str:
        """Get parameter type from annotation."""
        if arg.annotation:
            # For function parameters, use concrete types for bare containers
            if isinstance(arg.annotation, ast.Name) and arg.annotation.id in ("list", "dict", "set"):
                if arg.annotation.id == "list":
                    return "std::vector<int>"
                elif arg.annotation.id == "dict":
                    return "std::unordered_map<int, int>"  # Default to int keys
                elif arg.annotation.id == "set":
                    return "std::unordered_set<int>"
            return self._convert_type_annotation(arg.annotation)
        return "auto"

    def _analyze_append_operations(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Analyze append operations to detect what types are appended to containers.

        Returns a mapping of container_name -> appended_item_name.
        """
        append_map: dict[str, str] = {}

        def analyze_stmts(stmts: list[ast.stmt]) -> None:
            for stmt in stmts:
                # Recursively analyze nested statements (loops, ifs, etc.)
                if isinstance(stmt, (ast.For, ast.While)):
                    analyze_stmts(stmt.body)
                    if hasattr(stmt, "orelse"):
                        analyze_stmts(stmt.orelse)
                elif isinstance(stmt, ast.If):
                    analyze_stmts(stmt.body)
                    analyze_stmts(stmt.orelse)
                elif isinstance(stmt, ast.Expr):
                    # Check for append method calls: container.append(item)
                    if isinstance(stmt.value, ast.Call):
                        if isinstance(stmt.value.func, ast.Attribute) and stmt.value.func.attr == "append":
                            if isinstance(stmt.value.func.value, ast.Name) and stmt.value.args:
                                container_name = stmt.value.func.value.id
                                # Record what's being appended
                                append_arg = stmt.value.args[0]
                                if isinstance(append_arg, ast.Name):
                                    # Store the mapping: container -> appended_variable
                                    append_map[container_name] = append_arg.id

        analyze_stmts(stmts)
        return append_map

    def _convert_type_annotation(self, annotation: ast.expr) -> str:
        """Convert Python type annotation to C++ type (for return types and variables)."""
        if isinstance(annotation, ast.Name):
            # For bare container types without template args, use auto for return types
            if annotation.id in ("list", "dict", "set"):
                return "auto"
            return self.type_mapping.get(annotation.id, annotation.id)
        elif isinstance(annotation, ast.Constant):
            if annotation.value is None:
                return "void"
            return "auto"
        elif isinstance(annotation, ast.Subscript):
            # Generic types like List[int], Dict[str, int]
            base_type = annotation.value.id if isinstance(annotation.value, ast.Name) else "unknown"
            if base_type == "list":
                element_type = self._convert_type_annotation(annotation.slice)
                return f"std::vector<{element_type}>"
            elif base_type == "dict":
                if isinstance(annotation.slice, ast.Tuple) and len(annotation.slice.elts) == 2:
                    key_type = self._convert_type_annotation(annotation.slice.elts[0])
                    val_type = self._convert_type_annotation(annotation.slice.elts[1])
                    return f"std::unordered_map<{key_type}, {val_type}>"
            elif base_type == "set":
                element_type = self._convert_type_annotation(annotation.slice)
                return f"std::unordered_set<{element_type}>"
        return "auto"

    def _map_type(self, python_type: str) -> str:
        """Map Python type to C++ type.

        Args:
            python_type: Python type name (e.g., "int", "str", "list")

        Returns:
            C++ type name (e.g., "int", "std::string", "std::vector<int>")
        """
        mapped = self.type_mapping.get(python_type, "auto")

        # If we get an incomplete container type, add default template arguments
        if mapped == "std::vector":
            return "std::vector<int>"
        elif mapped == "std::unordered_map":
            return "std::unordered_map<int, int>"
        elif mapped == "std::unordered_set":
            return "std::unordered_set<int>"

        return mapped

    def _infer_type_from_value(self, value: ast.expr) -> str:
        """Infer C++ type from Python value using Strategy pattern.

        This method has been refactored to use the TypeInferenceEngine,
        reducing complexity from 53 to ~8.

        Before refactoring: 93 lines, complexity 53
        After refactoring: 10 lines, complexity ~8
        """
        # Create inference context with C++-specific type mapper
        context = InferenceContext(
            type_mapper=self._map_type,
            variable_types=self.variable_context,
        )

        # Delegate to type inference engine
        return self.type_inference_engine.infer_type(value, context)

    def _get_aug_op(self, op: ast.operator) -> str:
        """Get augmented assignment operator (just the operator part, without =)."""
        # Handle FloorDiv specially for C++
        if isinstance(op, ast.FloorDiv):
            return "/"

        # Use standard binary operator mapping from converter_utils
        # (augmented assignment operators are the same as binary operators)
        op_str = get_standard_binary_operator(op)
        if op_str is None:
            return "/*UNKNOWN_OP*/"
        return op_str


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
