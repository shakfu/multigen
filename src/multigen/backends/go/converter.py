"""Enhanced Go code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import Any, Optional

from ..converter_utils import (
    get_augmented_assignment_operator,
    get_standard_binary_operator,
    get_standard_comparison_operator,
)
from ..errors import TypeMappingError, UnsupportedFeatureError
from ..type_inference_strategies import InferenceContext


class MultiGenPythonToGoConverter:
    """Sophisticated Python-to-Go converter with comprehensive language support."""

    def __init__(self) -> None:
        """Initialize the converter."""
        self.type_map = {
            "int": "int",
            "float": "float64",
            "bool": "bool",
            "str": "string",
            "list": "[]int",  # Default to int elements for unsubscripted list
            "dict": "map[int]int",  # Default to int keys/values for unsubscripted dict
            "set": "map[int]bool",  # Default to int keys for unsubscripted set
            "void": "",
            "None": "",
        }
        self.struct_info: dict[str, dict[str, Any]] = {}  # Track struct definitions for classes
        self.current_function: Optional[str] = None  # Track current function context
        self.declared_vars: set[str] = set()  # Track declared variables in current function
        self.function_return_types: dict[str, str] = {}  # Track function return types
        self.variable_types: dict[str, str] = {}  # Track variable types in current function scope
        self._type_inference_engine: Optional[Any] = None  # Lazy-initialized type inference engine

    @property
    def type_inference_engine(self) -> Any:
        """Lazily initialize and return the type inference engine."""
        if self._type_inference_engine is None:
            from .type_inference import create_go_type_inference_engine

            self._type_inference_engine = create_go_type_inference_engine(self)
        return self._type_inference_engine

    def _map_type(self, python_type: str) -> str:
        """Map Python type to Go type.

        Args:
            python_type: Python type name (e.g., "int", "str", "list")

        Returns:
            Go type name (e.g., "int", "string", "[]int")
        """
        return self.type_map.get(python_type, "interface{}")

    def _to_camel_case(self, snake_str: str) -> str:
        """Convert snake_case to CamelCase."""
        components = snake_str.split("_")
        return "".join(word.capitalize() for word in components)

    def _to_go_method_name(self, method_name: str) -> str:
        """Convert Python method name to Go method name (proper CamelCase)."""
        # Handle special cases like get_increment -> GetIncrement
        return self._to_camel_case(method_name)

    def convert_code(self, python_code: str) -> str:
        """Convert Python code to Go."""
        try:
            tree = ast.parse(python_code)
            return self._convert_module(tree)
        except Exception as e:
            raise TypeMappingError(f"Failed to convert Python code: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to Go."""
        parts = []

        # Package declaration
        parts.append("package main")
        parts.append("")

        # Imports
        imports = self._collect_required_imports(node)
        for imp in imports:
            parts.append(f'import "{imp}"')
        parts.append("")

        # Convert classes first (they become struct definitions)
        for item in node.body:
            if isinstance(item, ast.ClassDef):
                struct_def = self._convert_class(item)
                parts.append(struct_def)
                parts.append("")

        # First pass: collect function return types
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                # Extract return type without converting the whole function
                if item.name == "main":
                    self.function_return_types[item.name] = ""
                elif item.returns:
                    mapped_type = self._map_type_annotation(item.returns)
                    self.function_return_types[item.name] = mapped_type if mapped_type else ""
                else:
                    # Default to int if no annotation
                    self.function_return_types[item.name] = "int"

        # Convert functions
        functions = []
        has_main = False
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "main":
                    has_main = True
                func_code = self._convert_function(item)
                functions.append(func_code)

        # Add functions to parts
        parts.extend(functions)

        # Add main function if not present
        if not has_main:
            main_func = 'func main() {\n    multigen.Print("Generated Go code executed successfully")\n}'
            parts.append("")
            parts.append(main_func)

        return "\n".join(parts)

    def _collect_required_imports(self, node: ast.Module) -> list[str]:
        """Collect required imports based on code features."""
        imports = ["multigenproject/multigen"]  # Always import our runtime
        # All required functionality is in the multigen runtime package
        return imports

    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert Python class to Go struct with methods."""
        class_name = node.name

        # Extract instance variables from __init__ method
        init_method = None
        other_methods = []

        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "__init__":
                    init_method = item
                else:
                    other_methods.append(item)

        # Build struct definition
        struct_lines = [f"type {class_name} struct {{"]

        if init_method:
            # Extract instance variables from __init__
            for stmt in init_method.body:
                if isinstance(stmt, ast.Assign):
                    for target in stmt.targets:
                        if (
                            isinstance(target, ast.Attribute)
                            and isinstance(target.value, ast.Name)
                            and target.value.id == "self"
                        ):
                            field_name = self._to_camel_case(target.attr)  # Capitalize for Go visibility
                            field_type = self._infer_type_from_assignment(stmt)
                            struct_lines.append(f"    {field_name} {field_type}")
                elif isinstance(stmt, ast.AnnAssign):
                    if (
                        isinstance(stmt.target, ast.Attribute)
                        and isinstance(stmt.target.value, ast.Name)
                        and stmt.target.value.id == "self"
                    ):
                        field_name = self._to_camel_case(stmt.target.attr)
                        field_type = self._map_type_annotation(stmt.annotation)
                        struct_lines.append(f"    {field_name} {field_type}")

        struct_lines.append("}")

        # Store struct info for method generation
        self.struct_info[class_name] = {"fields": self._extract_struct_fields(init_method) if init_method else []}

        # Generate constructor
        constructor_lines = []
        if init_method:
            constructor_lines.append(self._convert_constructor(class_name, init_method))

        # Generate methods
        method_lines = []
        for method in other_methods:
            method_lines.append(self._convert_method(class_name, method))

        # Combine all parts
        result_parts = ["\n".join(struct_lines)]
        if constructor_lines:
            result_parts.extend(constructor_lines)
        if method_lines:
            result_parts.extend(method_lines)

        return "\n\n".join(result_parts)

    def _convert_constructor(self, class_name: str, init_method: ast.FunctionDef) -> str:
        """Generate Go constructor function from Python __init__."""
        # Build parameter list (skip 'self')
        params = []
        for arg in init_method.args.args[1:]:  # Skip self
            param_type = self._infer_parameter_type(arg, init_method)
            params.append(f"{arg.arg} {param_type}")

        params_str = ", ".join(params)

        # Generate constructor body
        body_lines = [f"    obj := {class_name}{{}}"]

        for stmt in init_method.body:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        field_name = self._to_camel_case(target.attr)
                        value_expr = self._convert_expression(stmt.value)
                        body_lines.append(f"    obj.{field_name} = {value_expr}")
            elif isinstance(stmt, ast.AnnAssign):
                if (
                    isinstance(stmt.target, ast.Attribute)
                    and isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                ):
                    field_name = self._to_camel_case(stmt.target.attr)
                    if stmt.value:
                        value_expr = self._convert_expression(stmt.value)
                        body_lines.append(f"    obj.{field_name} = {value_expr}")

        body_lines.append("    return obj")

        # Build function signature
        func_signature = f"func New{class_name}({params_str}) {class_name}"

        return f"{func_signature} {{\n" + "\n".join(body_lines) + "\n}"

    def _convert_method(self, class_name: str, method: ast.FunctionDef) -> str:
        """Convert Python instance method to Go method."""
        # Build parameter list (convert 'self' to receiver)
        params = []
        for arg in method.args.args[1:]:  # Skip self
            param_type = self._infer_parameter_type(arg, method)
            params.append(f"{arg.arg} {param_type}")

        params_str = ", ".join(params)

        # Get return type
        return_type = ""
        if method.returns:
            mapped_type = self._map_type_annotation(method.returns)
            if mapped_type:  # Only add space if type is not empty
                return_type = " " + mapped_type

        # Build method signature with receiver
        receiver = f"obj *{class_name}"
        if params_str:
            func_signature = f"func ({receiver}) {self._to_go_method_name(method.name)}({params_str}){return_type}"
        else:
            func_signature = f"func ({receiver}) {self._to_go_method_name(method.name)}(){return_type}"

        # Convert method body
        self.current_function = method.name
        body = self._convert_method_statements(method.body, class_name)
        self.current_function = None

        return func_signature + " {\n" + body + "\n}"

    def _convert_method_statements(self, statements: list[ast.stmt], class_name: str) -> str:
        """Convert method statements with class context."""
        converted = []
        for stmt in statements:
            converted.append(self._convert_method_statement(stmt, class_name))
        return "\n".join(converted)

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
            return f"    {expr}"
        else:
            return self._convert_statement(stmt)

    def _convert_method_assignment(self, stmt: ast.Assign, class_name: str) -> str:
        """Convert method assignment with proper obj handling."""
        value_expr = self._convert_method_expression(stmt.value, class_name)
        statements = []

        for target in stmt.targets:
            if isinstance(target, ast.Name):
                # Local variable assignment
                self._infer_type_from_value(stmt.value)
                statements.append(f"    {target.id} := {value_expr}")
            elif isinstance(target, ast.Attribute):
                if isinstance(target.value, ast.Name) and target.value.id == "self":
                    # Instance variable assignment: self.attr = value -> obj.Attr = value
                    field_name = self._to_camel_case(target.attr)
                    statements.append(f"    obj.{field_name} = {value_expr}")
                else:
                    # Regular attribute assignment
                    obj_expr = self._convert_method_expression(target.value, class_name)
                    statements.append(f"    {obj_expr}.{self._to_camel_case(target.attr)} = {value_expr}")

        return "\n".join(statements)

    def _convert_method_annotated_assignment(self, stmt: ast.AnnAssign, class_name: str) -> str:
        """Convert method annotated assignment with proper obj handling."""
        if stmt.value:
            value_expr = self._convert_method_expression(stmt.value, class_name)
        else:
            # Default value based on type
            type_name = self._map_type_annotation(stmt.annotation)
            value_expr = self._get_default_value(type_name)

        if isinstance(stmt.target, ast.Name):
            # Local variable with type annotation
            var_type = self._map_type_annotation(stmt.annotation)
            return f"    var {stmt.target.id} {var_type} = {value_expr}"
        elif isinstance(stmt.target, ast.Attribute):
            if isinstance(stmt.target.value, ast.Name) and stmt.target.value.id == "self":
                # Instance variable: self.attr: type = value -> obj.Attr = value
                field_name = self._to_camel_case(stmt.target.attr)
                return f"    obj.{field_name} = {value_expr}"

        raise UnsupportedFeatureError(f"Complex annotated assignment not supported: {ast.unparse(stmt)}")

    def _convert_method_aug_assignment(self, stmt: ast.AugAssign, class_name: str) -> str:
        """Convert method augmented assignment with proper obj handling."""
        value_expr = self._convert_method_expression(stmt.value, class_name)

        # Get augmented assignment operator from converter_utils
        op = get_augmented_assignment_operator(stmt.op)
        if op is None:
            # Handle Go-specific operators
            if isinstance(stmt.op, ast.FloorDiv):
                op = "/="  # Go integer division is already floor division
            else:
                op = "/*UNKNOWN_OP*/"

        if isinstance(stmt.target, ast.Name):
            return f"    {stmt.target.id} {op} {value_expr}"
        elif isinstance(stmt.target, ast.Attribute):
            if isinstance(stmt.target.value, ast.Name) and stmt.target.value.id == "self":
                field_name = self._to_camel_case(stmt.target.attr)
                return f"    obj.{field_name} {op} {value_expr}"

        raise UnsupportedFeatureError(f"Complex augmented assignment not supported: {ast.unparse(stmt)}")

    def _convert_method_return(self, stmt: ast.Return, class_name: str) -> str:
        """Convert method return statement."""
        if stmt.value:
            value_expr = self._convert_method_expression(stmt.value, class_name)
            return f"    return {value_expr}"
        return "    return"

    def _convert_method_if(self, stmt: ast.If, class_name: str) -> str:
        """Convert if statement in method context."""
        condition = self._convert_method_expression(stmt.test, class_name)
        then_body = self._convert_method_statements(stmt.body, class_name)
        if_part = f"    if {condition} {{\n{then_body}\n    }}"

        if stmt.orelse:
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                # elif chain
                else_body = self._convert_method_if(stmt.orelse[0], class_name).strip()
                return f"{if_part} else {else_body}"
            else:
                # regular else
                else_body = self._convert_method_statements(stmt.orelse, class_name)
                return if_part + " else {\n" + else_body + "\n    }"
        else:
            return if_part

    def _convert_method_expression(self, expr: ast.expr, class_name: str) -> str:
        """Convert method expression with class context."""
        if isinstance(expr, ast.Attribute):
            if isinstance(expr.value, ast.Name) and expr.value.id == "self":
                # self.attr -> obj.Attr
                return f"obj.{self._to_camel_case(expr.attr)}"
            else:
                # obj.attr or obj.method()
                obj_expr = self._convert_method_expression(expr.value, class_name)
                return f"{obj_expr}.{self._to_camel_case(expr.attr)}"
        elif isinstance(expr, ast.Call):
            return self._convert_method_call(expr, class_name)
        elif isinstance(expr, ast.BinOp):
            # Handle binary operations with proper obj conversion
            left = self._convert_method_expression(expr.left, class_name)
            right = self._convert_method_expression(expr.right, class_name)

            # Handle Go-specific operators
            if isinstance(expr.op, ast.Pow):
                return f"math.Pow({left}, {right})"
            elif isinstance(expr.op, ast.FloorDiv):
                # Go integer division is already floor division
                return f"({left} / {right})"

            # Use standard operator mapping from converter_utils
            op = get_standard_binary_operator(expr.op)
            if op is None:
                op = "/*UNKNOWN_OP*/"
            return f"({left} {op} {right})"
        elif isinstance(expr, ast.Compare):
            return self._convert_method_compare(expr, class_name)
        elif isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        else:
            return self._convert_expression(expr)

    def _convert_method_call(self, expr: ast.Call, class_name: str) -> str:
        """Convert method calls with class context."""
        if isinstance(expr.func, ast.Attribute):
            if isinstance(expr.func.value, ast.Name) and expr.func.value.id == "self":
                # self.method() -> obj.Method()
                method_name = self._to_go_method_name(expr.func.attr)
                args = [self._convert_method_expression(arg, class_name) for arg in expr.args]
                args_str = ", ".join(args)
                return f"obj.{method_name}({args_str})"
            else:
                # Handle string methods and other attribute calls
                obj_expr = self._convert_method_expression(expr.func.value, class_name)
                method_name = expr.func.attr
                args = [self._convert_method_expression(arg, class_name) for arg in expr.args]

                # Handle string methods
                if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                    if method_name == "upper":
                        return f"multigen.StrOps.Upper({obj_expr})"
                    elif method_name == "lower":
                        return f"multigen.StrOps.Lower({obj_expr})"
                    elif method_name == "strip":
                        if args:
                            return f"multigen.StrOps.StripChars({obj_expr}, {args[0]})"
                        else:
                            return f"multigen.StrOps.Strip({obj_expr})"
                    elif method_name == "find":
                        return f"multigen.StrOps.Find({obj_expr}, {args[0]})"
                    elif method_name == "replace":
                        return f"multigen.StrOps.Replace({obj_expr}, {args[0]}, {args[1]})"
                    elif method_name == "split":
                        if args:
                            return f"multigen.StrOps.SplitSep({obj_expr}, {args[0]})"
                        else:
                            return f"multigen.StrOps.Split({obj_expr})"

                # Regular method call
                args_str = ", ".join(args)
                return f"{obj_expr}.{self._to_go_method_name(method_name)}({args_str})"
        else:
            # Handle regular function calls like len() with method context
            if isinstance(expr.func, ast.Name):
                func_name = expr.func.id
                args = [self._convert_method_expression(arg, class_name) for arg in expr.args]

                # Handle built-in functions with generics
                if func_name == "len":
                    arg_type = self._infer_type_from_value(expr.args[0])
                    if arg_type.startswith("[]"):
                        elem_type = arg_type[2:]
                        return f"multigen.Len[{elem_type}]({args[0]})"
                    elif arg_type.startswith("map["):
                        return f"multigen.LenMap({args[0]})"
                    elif arg_type == "string":
                        return f"multigen.LenString({args[0]})"
                    else:
                        # Default to generic slice
                        return f"multigen.Len({args[0]})"
                elif func_name == "abs":
                    arg_type = self._infer_type_from_value(expr.args[0])
                    if arg_type == "float64" or arg_type == "float32":
                        return f"multigen.AbsFloat({args[0]})"
                    else:
                        return f"multigen.AbsInt({args[0]})"
                elif func_name == "min":
                    arg_type = self._infer_type_from_value(expr.args[0])
                    elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                    return f"multigen.Min[{elem_type}]({args[0]})"
                elif func_name == "max":
                    arg_type = self._infer_type_from_value(expr.args[0])
                    elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                    return f"multigen.Max[{elem_type}]({args[0]})"
                elif func_name == "sum":
                    arg_type = self._infer_type_from_value(expr.args[0])
                    elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                    return f"multigen.Sum[{elem_type}]({args[0]})"
                elif func_name == "bool":
                    return f"multigen.ToBool({args[0]})"
                elif func_name == "int":
                    return f"multigen.ToInt({args[0]})"
                elif func_name == "float":
                    return f"multigen.ToFloat({args[0]})"
                elif func_name == "str":
                    return f"multigen.ToStr({args[0]})"
                elif func_name == "range":
                    range_args = ", ".join(args)
                    return f"multigen.NewRange({range_args})"
                else:
                    # Check if this is a class constructor
                    if func_name in self.struct_info:
                        args_str = ", ".join(args)
                        return f"New{func_name}({args_str})"
                    else:
                        args_str = ", ".join(args)
                        return f"{func_name}({args_str})"
            else:
                return self._convert_call(expr)

    def _convert_method_compare(self, expr: ast.Compare, class_name: str) -> str:
        """Convert comparison expressions in method context."""
        left = self._convert_method_expression(expr.left, class_name)
        result = left

        for op, comp in zip(expr.ops, expr.comparators):
            # Use standard comparison operator mapping from converter_utils
            op_str = get_standard_comparison_operator(op)

            # Handle Go-specific operators
            if op_str is None:
                if isinstance(op, ast.Is):
                    op_str = "=="
                elif isinstance(op, ast.IsNot):
                    op_str = "!="
                else:
                    op_str = "/*UNKNOWN_OP*/"

            comp_expr = self._convert_method_expression(comp, class_name)
            result = f"({result} {op_str} {comp_expr})"

        return result

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert Python function to Go function."""
        # Pre-pass: Analyze nested subscripts to detect 2D arrays
        nested_vars = self._analyze_nested_subscripts(node.body)
        append_map = self._analyze_append_operations(node.body)

        # Build parameter list
        params = []
        for arg in node.args.args:
            param_type = self._infer_parameter_type(arg, node)

            # If parameter is used with nested subscripting and is bare slice, make it 2D
            if arg.arg in nested_vars and param_type == "[]int":
                param_type = "[][]int"

            params.append(f"{arg.arg} {param_type}")

        params_str = ", ".join(params)

        # Get return type
        # Special case: Go's main function must not have a return type
        return_type = ""
        if node.name != "main":
            if node.returns:
                mapped_type = self._map_type_annotation(node.returns)
                # Check if return type should be nested based on usage
                if mapped_type == "[]int":
                    # Check if any variable in body that could be returned is nested
                    for stmt in node.body:
                        if isinstance(stmt, ast.Return) and stmt.value and isinstance(stmt.value, ast.Name):
                            if stmt.value.id in nested_vars or stmt.value.id in append_map:
                                mapped_type = "[][]int"
                                break
                if mapped_type:  # Only add space if type is not empty
                    return_type = " " + mapped_type
            else:
                # Infer return type from function body if no annotation
                inferred_type = self._infer_return_type(node)
                if inferred_type:
                    return_type = " " + inferred_type

        # Build function signature
        func_signature = f"func {node.name}({params_str}){return_type}"

        # Convert function body
        self.current_function = node.name
        self.declared_vars = set()  # Reset for new function
        self.variable_types = {}  # Reset variable type tracking for new function
        self.nested_vars = nested_vars  # Store for use in type inference
        self.append_map = append_map

        # Add parameters to variable types first
        for arg in node.args.args:
            param_type = self._infer_parameter_type(arg, node)
            # Apply nested upgrade to parameter types
            if arg.arg in nested_vars and param_type == "[]int":
                param_type = "[][]int"
            self.variable_types[arg.arg] = param_type

        # Pre-pass: infer all variable types including nested container upgrades
        self._pre_infer_variable_types(node.body)

        # After pre-pass, check if return type needs upgrade based on inferred variable types
        if return_type and ("map[int]" in return_type or "[]int" == return_type.strip()):
            for stmt in node.body:
                if isinstance(stmt, ast.Return) and stmt.value and isinstance(stmt.value, ast.Name):
                    returned_var = stmt.value.id
                    if returned_var in self.variable_types:
                        var_type = self.variable_types[returned_var]
                        # Upgrade return type if variable was upgraded
                        if var_type != return_type.strip():
                            return_type = " " + var_type
                            func_signature = f"func {node.name}({params_str}){return_type}"
                            break

        # Update function_return_types with the final return type
        if node.name != "main" and return_type:
            self.function_return_types[node.name] = return_type.strip()

        # Add parameters to declared variables
        for arg in node.args.args:
            self.declared_vars.add(arg.arg)

        body = self._convert_statements(node.body)

        # Detect unused variables and mark them with _ = variable
        unused_vars = self._detect_unused_variables(node.body)
        if unused_vars:
            # Add _ = var statements at the end of the function body before return
            unused_statements = []
            for var in sorted(unused_vars):  # Sort for consistent output
                unused_statements.append(f"    _ = {var}")

            # Insert unused variable markers at the end, before any return statement
            if unused_statements:
                # Find the last non-return statement position
                body_lines = body.rstrip().split("\n")
                insert_pos = len(body_lines)

                # Find the last return statement
                for i in range(len(body_lines) - 1, -1, -1):
                    if "return" in body_lines[i]:
                        insert_pos = i
                        break

                # Insert the unused markers
                body_lines[insert_pos:insert_pos] = unused_statements
                body = "\n".join(body_lines) + "\n"

        self.current_function = None
        self.nested_vars = set()  # Clear
        self.append_map = {}

        return func_signature + " {\n" + body + "\n}"

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

    def _analyze_append_operations(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Analyze append operations to detect when vectors are appended to vectors."""
        append_map: dict[str, str] = {}

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Expr):
                if isinstance(stmt.value, ast.Call):
                    if isinstance(stmt.value.func, ast.Attribute) and stmt.value.func.attr == "append":
                        if isinstance(stmt.value.func.value, ast.Name) and stmt.value.args:
                            container_name = stmt.value.func.value.id
                            if isinstance(stmt.value.args[0], ast.Name):
                                appended_var = stmt.value.args[0].id
                                append_map[container_name] = appended_var
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
            elif isinstance(stmt, ast.If):
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)

        for stmt in stmts:
            check_stmt(stmt)

        return append_map

    def _detect_unused_variables(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect variables that are declared but never used."""
        declared: set[str] = set()
        used: set[str] = set()

        def collect_declared(stmt: ast.stmt) -> None:
            """Collect variable declarations."""
            if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                declared.add(stmt.target.id)
            elif isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if isinstance(target, ast.Name):
                        declared.add(target.id)
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    collect_declared(s)
                if hasattr(stmt, "orelse"):
                    for s in stmt.orelse:
                        collect_declared(s)
            elif isinstance(stmt, ast.If):
                for s in stmt.body:
                    collect_declared(s)
                for s in stmt.orelse:
                    collect_declared(s)

        def collect_used(node: ast.AST) -> None:
            """Collect variable uses (but not in assignment targets)."""
            if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Load):
                used.add(node.id)
            elif isinstance(node, ast.expr):
                for child in ast.walk(node):
                    if isinstance(child, ast.Name) and isinstance(child.ctx, ast.Load):
                        used.add(child.id)

        def traverse_stmt(stmt: ast.stmt) -> None:
            """Traverse statements to find uses."""
            if isinstance(stmt, ast.Assign):
                # Check the value being assigned (RHS)
                collect_used(stmt.value)
            elif isinstance(stmt, ast.AnnAssign):
                if stmt.value:
                    collect_used(stmt.value)
            elif isinstance(stmt, ast.Expr):
                collect_used(stmt.value)
            elif isinstance(stmt, ast.Return):
                if stmt.value:
                    collect_used(stmt.value)
            elif isinstance(stmt, ast.If):
                collect_used(stmt.test)
                for s in stmt.body:
                    traverse_stmt(s)
                for s in stmt.orelse:
                    traverse_stmt(s)
            elif isinstance(stmt, (ast.For, ast.While)):
                if hasattr(stmt, "iter"):
                    collect_used(stmt.iter)
                if hasattr(stmt, "test"):
                    collect_used(stmt.test)
                for s in stmt.body:
                    traverse_stmt(s)
                if hasattr(stmt, "orelse"):
                    for s in stmt.orelse:
                        traverse_stmt(s)

        # Collect declarations and uses
        for stmt in stmts:
            collect_declared(stmt)
            traverse_stmt(stmt)

        # Return variables that are declared but never used
        return declared - used

    def _analyze_map_key_types(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect maps that are accessed with string keys."""
        string_keyed_maps: set[str] = set()

        def check_expr(expr: ast.expr) -> None:
            if isinstance(expr, ast.Subscript):
                # Check if the subscripted value is a map and the key is a string
                if isinstance(expr.value, ast.Name):
                    map_name = expr.value.id
                    # Check if key is a string literal or string variable
                    if isinstance(expr.slice, ast.Constant) and isinstance(expr.slice.value, str):
                        string_keyed_maps.add(map_name)
                    elif isinstance(expr.slice, ast.Name):
                        # Check if the key variable is a string
                        if expr.slice.id in self.variable_types and self.variable_types[expr.slice.id] == "string":
                            string_keyed_maps.add(map_name)
                # Recursively check
                check_expr(expr.value)
                if not isinstance(expr.slice, ast.Slice):
                    check_expr(expr.slice)
            elif isinstance(expr, ast.BinOp):
                check_expr(expr.left)
                check_expr(expr.right)
            elif isinstance(expr, ast.Call):
                if isinstance(expr.func, ast.Attribute):
                    check_expr(expr.func.value)
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

        return string_keyed_maps

    def _analyze_map_value_types(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Detect map value types from subscript assignments."""
        map_value_types: dict[str, str] = {}

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Assign):
                # Check for subscript assignment like map[key] = value
                for target in stmt.targets:
                    if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
                        map_name = target.value.id
                        # Infer value type from assigned value
                        value_type = self._infer_type_from_value(stmt.value)
                        if map_name not in map_value_types:
                            map_value_types[map_name] = value_type
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
                if hasattr(stmt, "orelse"):
                    for s in stmt.orelse:
                        check_stmt(s)
            elif isinstance(stmt, ast.If):
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)

        for stmt in stmts:
            check_stmt(stmt)

        return map_value_types

    def _pre_infer_variable_types(self, stmts: list[ast.stmt]) -> None:
        """Pre-pass to infer all variable types before code generation."""

        # First pass: collect base types from annotations and initializations
        def collect_types(stmts: list[ast.stmt]) -> None:
            for stmt in stmts:
                if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                    var_name = stmt.target.id
                    var_type = self._map_type_annotation(stmt.annotation)
                    if var_name not in self.variable_types:  # Don't override parameters
                        self.variable_types[var_name] = var_type
                elif isinstance(stmt, ast.Assign):
                    for target in stmt.targets:
                        if isinstance(target, ast.Name) and target.id not in self.variable_types:
                            var_type = self._infer_type_from_value(stmt.value)
                            self.variable_types[target.id] = var_type
                elif isinstance(stmt, (ast.For, ast.While)):
                    collect_types(stmt.body)
                    if hasattr(stmt, "orelse"):
                        collect_types(stmt.orelse)
                elif isinstance(stmt, ast.If):
                    collect_types(stmt.body)
                    collect_types(stmt.orelse)

        collect_types(stmts)

        # Second pass: upgrade types based on append operations
        for container_name, appended_var in self.append_map.items():
            if appended_var in self.variable_types and container_name in self.variable_types:
                appended_type = self.variable_types[appended_var]
                if appended_type.startswith("[]"):
                    self.variable_types[container_name] = f"[]{appended_type}"

        # Third pass: upgrade types based on nested subscript usage
        for var_name in self.nested_vars:
            if var_name in self.variable_types and self.variable_types[var_name] == "[]int":
                self.variable_types[var_name] = "[][]int"

        # Fourth pass: detect string-keyed maps
        string_keyed_maps = self._analyze_map_key_types(stmts)
        for var_name in string_keyed_maps:
            if var_name in self.variable_types:
                current_type = self.variable_types[var_name]
                # Upgrade map[int]int to map[string]int, map[int]bool to map[string]bool
                if current_type == "map[int]int":
                    self.variable_types[var_name] = "map[string]int"
                elif current_type == "map[int]bool":
                    self.variable_types[var_name] = "map[string]bool"

        # Fifth pass: detect map value types from subscript assignments
        map_value_types = self._analyze_map_value_types(stmts)
        for var_name, value_type in map_value_types.items():
            if var_name in self.variable_types:
                current_type = self.variable_types[var_name]
                # Only upgrade if the new value type is more specific (not interface{})
                if value_type != "interface{}" and current_type.startswith("map[") and current_type.endswith("]int"):
                    # Extract key type and update value type
                    key_part = current_type[4:-4]  # Remove "map[" and "]int"
                    self.variable_types[var_name] = f"map[{key_part}]{value_type}"

    def _convert_statements(self, statements: list[ast.stmt]) -> str:
        """Convert a list of statements."""
        converted = []
        for stmt in statements:
            converted.append(self._convert_statement(stmt))
        return "\n".join(converted)

    def _convert_statement(self, stmt: ast.stmt) -> str:
        """Convert a Python statement to Go."""
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
            return "    // pass"
        elif isinstance(stmt, ast.Assert):
            return self._convert_assert(stmt)
        else:
            raise UnsupportedFeatureError(f"Unsupported statement type: {type(stmt).__name__}")

    def _convert_assert(self, stmt: ast.Assert) -> str:
        """Convert Python assert statement to Go panic on failure.

        Args:
            stmt: Python assert statement node

        Returns:
            Go if statement that panics on assertion failure

        Example:
            assert x > 0  →  if !(x > 0) { panic("assertion failed") }
            assert result == 1, "Test failed"  →  if !(result == 1) { panic("Test failed") }
        """
        # Convert the test expression
        test_expr = self._convert_expression(stmt.test)

        # Handle optional message
        if stmt.msg:
            # Convert message to string
            if isinstance(stmt.msg, ast.Constant) and isinstance(stmt.msg.value, str):
                msg = stmt.msg.value
                return f'    if !({test_expr}) {{ panic("{msg}") }}'
            else:
                # Complex message expression - just add default panic
                return f'    if !({test_expr}) {{ panic("assertion failed") }}'
        else:
            return f'    if !({test_expr}) {{ panic("assertion failed") }}'

    def _convert_return(self, stmt: ast.Return) -> str:
        """Convert return statement."""
        # Special case: in main(), ignore return statements
        if self.current_function == "main":
            return ""

        if stmt.value:
            value_expr = self._convert_expression(stmt.value)
            return f"    return {value_expr}"
        return "    return"

    def _convert_assignment(self, stmt: ast.Assign) -> str:
        """Convert assignment statement."""
        value_expr = self._convert_expression(stmt.value)
        statements = []

        for target in stmt.targets:
            if isinstance(target, ast.Name):
                if target.id in self.declared_vars:
                    # Variable already declared, use assignment
                    statements.append(f"    {target.id} = {value_expr}")
                else:
                    # First declaration of variable
                    self.declared_vars.add(target.id)

                    # For function calls, always use := to let Go infer the correct type
                    if isinstance(stmt.value, ast.Call):
                        # Update variable_types if we have a pre-computed type
                        if target.id in self.variable_types:
                            # Already pre-computed, just use :=
                            pass
                        elif isinstance(stmt.value.func, ast.Name) and stmt.value.func.id in self.function_return_types:
                            # Get return type from function
                            self.variable_types[target.id] = self.function_return_types[stmt.value.func.id]
                        statements.append(f"    {target.id} := {value_expr}")
                    else:
                        # Use pre-computed type if available, otherwise infer
                        if target.id in self.variable_types:
                            var_type = self.variable_types[target.id]
                        else:
                            var_type = self._infer_type_from_value(stmt.value)
                            # Check if this variable should be nested based on usage analysis
                            if hasattr(self, "nested_vars") and target.id in self.nested_vars and var_type == "[]int":
                                var_type = "[][]int"
                            # Check if this variable has a vector appended to it
                            if hasattr(self, "append_map") and target.id in self.append_map:
                                appended_var = self.append_map[target.id]
                                if appended_var in self.variable_types and self.variable_types[appended_var].startswith(
                                    "[]"
                                ):
                                    var_type = f"[]{self.variable_types[appended_var]}"
                            self.variable_types[target.id] = var_type

                        # Use := for constructor calls and interface{} for cleaner code
                        if var_type == "interface{}" or self._is_constructor_call(stmt.value):
                            statements.append(f"    {target.id} := {value_expr}")
                        else:
                            statements.append(f"    var {target.id} {var_type} = {value_expr}")
            elif isinstance(target, ast.Subscript):
                # Handle subscript assignment: container[index] = value
                container_expr = self._convert_expression(target.value)
                index_expr = self._convert_expression(target.slice)
                statements.append(f"    {container_expr}[{index_expr}] = {value_expr}")

        return "\n".join(statements)

    def _convert_annotated_assignment(self, stmt: ast.AnnAssign) -> str:
        """Convert annotated assignment."""
        # Use pre-computed type if available, otherwise map from annotation
        if isinstance(stmt.target, ast.Name) and stmt.target.id in self.variable_types:
            var_type = self.variable_types[stmt.target.id]
        else:
            # Always use annotation if present - Python annotations are explicit type declarations
            var_type = self._map_type_annotation(stmt.annotation)

            # Check if this variable should be nested based on usage analysis
            if isinstance(stmt.target, ast.Name):
                if hasattr(self, "nested_vars") and stmt.target.id in self.nested_vars and var_type == "[]int":
                    var_type = "[][]int"
                # Check if this variable has a vector appended to it
                if hasattr(self, "append_map") and stmt.target.id in self.append_map:
                    appended_var = self.append_map[stmt.target.id]
                    if appended_var in self.variable_types and self.variable_types[appended_var].startswith("[]"):
                        var_type = f"[]{self.variable_types[appended_var]}"

        if stmt.value:
            # For empty dict, use the upgraded type
            if isinstance(stmt.value, ast.Dict) and not stmt.value.keys:
                value_expr = f"make({var_type})"
            # For function calls, use := to let Go infer the type correctly (except make())
            elif isinstance(stmt.value, ast.Call):
                # Special case: make() should use the annotated/upgraded type
                if isinstance(stmt.value.func, ast.Name) and stmt.value.func.id == "make":
                    # Use the upgraded type for make()
                    value_expr = f"make({var_type})"
                else:
                    value_expr = self._convert_expression(stmt.value)
                    target_id = stmt.target.id if isinstance(stmt.target, ast.Name) else str(stmt.target)
                    if isinstance(stmt.target, ast.Name):
                        self.declared_vars.add(stmt.target.id)
                        # Update variable_types with the correct return type
                        if isinstance(stmt.value.func, ast.Name) and stmt.value.func.id in self.function_return_types:
                            self.variable_types[stmt.target.id] = self.function_return_types[stmt.value.func.id]
                        else:
                            self.variable_types[stmt.target.id] = var_type
                    return f"    {target_id} := {value_expr}"
            # For empty lists, use the correct type
            elif isinstance(stmt.value, ast.List) and not stmt.value.elts:
                value_expr = f"{var_type}{{}}"
            else:
                value_expr = self._convert_expression(stmt.value)
        else:
            value_expr = None

        if stmt.value:
            target_id = stmt.target.id if isinstance(stmt.target, ast.Name) else str(stmt.target)
            # Track this variable as declared with the inferred type
            if isinstance(stmt.target, ast.Name):
                self.declared_vars.add(stmt.target.id)
                self.variable_types[stmt.target.id] = var_type
            return f"    var {target_id} {var_type} = {value_expr}"
        else:
            default_value = self._get_default_value(var_type)
            target_id = stmt.target.id if isinstance(stmt.target, ast.Name) else str(stmt.target)
            # Track this variable as declared and its type
            if isinstance(stmt.target, ast.Name):
                self.declared_vars.add(stmt.target.id)
                self.variable_types[stmt.target.id] = var_type
            return f"    var {target_id} {var_type} = {default_value}"

    def _convert_aug_assignment(self, stmt: ast.AugAssign) -> str:
        """Convert augmented assignment."""
        value_expr = self._convert_expression(stmt.value)

        # Get augmented assignment operator from converter_utils
        op = get_augmented_assignment_operator(stmt.op)
        if op is None:
            # Handle Go-specific operators
            if isinstance(stmt.op, ast.FloorDiv):
                op = "/="  # Go integer division is already floor division
            else:
                op = "/*UNKNOWN_OP*/"

        if isinstance(stmt.target, ast.Name):
            return f"    {stmt.target.id} {op} {value_expr}"

        raise UnsupportedFeatureError(f"Complex augmented assignment target not supported: {ast.unparse(stmt.target)}")

    def _convert_if(self, stmt: ast.If) -> str:
        """Convert if statement."""
        condition = self._convert_expression(stmt.test)
        then_body = self._convert_statements(stmt.body)
        if_part = f"    if {condition} {{\n{then_body}\n    }}"

        if stmt.orelse:
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                # elif chain
                else_body = self._convert_if(stmt.orelse[0]).strip()
                return f"{if_part} else {else_body}"
            else:
                # regular else
                else_body = self._convert_statements(stmt.orelse)
                return if_part + " else {\n" + else_body + "\n    }"
        else:
            return if_part

    def _convert_while(self, stmt: ast.While) -> str:
        """Convert while loop."""
        condition = self._convert_expression(stmt.test)
        body = self._convert_statements(stmt.body)
        return "    for " + condition + " {\n" + body + "\n    }"

    def _convert_for(self, stmt: ast.For) -> str:
        """Convert for loop."""
        if isinstance(stmt.iter, ast.Call) and isinstance(stmt.iter.func, ast.Name) and stmt.iter.func.id == "range":
            # Range-based for loop
            range_args = [self._convert_expression(arg) for arg in stmt.iter.args]
            target_name = stmt.target.id if isinstance(stmt.target, ast.Name) else "i"

            if len(range_args) == 1:
                # range(n)
                stop = range_args[0]
                body = self._convert_statements(stmt.body)
                return f"    for {target_name} := 0; {target_name} < {stop}; {target_name}++ {{\n{body}\n    }}"
            elif len(range_args) == 2:
                # range(start, stop)
                start, stop = range_args
                body = self._convert_statements(stmt.body)
                return f"    for {target_name} := {start}; {target_name} < {stop}; {target_name}++ {{\n{body}\n    }}"
            elif len(range_args) == 3:
                # range(start, stop, step)
                start, stop, step = range_args
                body = self._convert_statements(stmt.body)
                return f"    for {target_name} := {start}; {target_name} < {stop}; {target_name} += {step} {{\n{body}\n    }}"
            else:
                # Invalid range arguments
                body = self._convert_statements(stmt.body)
                return (
                    f"    for {target_name} := 0; {target_name} < 0; {target_name}++ {{\n{body}\n    }}"  # Empty loop
                )
        else:
            # Iteration over container
            container_expr = self._convert_expression(stmt.iter)
            target_name = stmt.target.id if isinstance(stmt.target, ast.Name) else "item"
            body = self._convert_statements(stmt.body)
            return f"    for _, {target_name} := range {container_expr} {{\n{body}\n    }}"

    def _convert_expression_statement(self, stmt: ast.Expr) -> str:
        """Convert expression statement."""
        # Skip docstrings (string constants used as statements)
        if isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            return f"    // {stmt.value.value}"
        expr = self._convert_expression(stmt.value)

        # Handle append operations - convert to assignment
        if "__APPEND__" in expr:
            # Extract the parts: __APPEND__<obj>__ARGS__<args>__END__
            parts = expr.split("__")
            if len(parts) >= 5 and parts[1] == "APPEND" and parts[3] == "ARGS" and parts[5] == "END":
                obj = parts[2]
                args = parts[4]
                return f"    {obj} = append({obj}, {args})"

        return f"    {expr}"

    def _convert_expression(self, expr: ast.expr) -> str:
        """Convert Python expression to Go."""
        if isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        elif isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.BinOp):
            return self._convert_binop(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self._convert_unaryop(expr)
        elif isinstance(expr, ast.Compare):
            return self._convert_compare(expr)
        elif isinstance(expr, ast.Call):
            return self._convert_call(expr)
        elif isinstance(expr, ast.Attribute):
            return self._convert_attribute(expr)
        elif isinstance(expr, ast.List):
            return self._convert_list_literal(expr)
        elif isinstance(expr, ast.Dict):
            return self._convert_dict_literal(expr)
        elif isinstance(expr, ast.Set):
            return self._convert_set_literal(expr)
        elif isinstance(expr, ast.ListComp):
            return self._convert_list_comprehension(expr)
        elif isinstance(expr, ast.DictComp):
            return self._convert_dict_comprehension(expr)
        elif isinstance(expr, ast.SetComp):
            return self._convert_set_comprehension(expr)
        elif isinstance(expr, ast.Subscript):
            return self._convert_subscript(expr)
        elif isinstance(expr, ast.JoinedStr):
            return self._convert_f_string(expr)
        else:
            raise UnsupportedFeatureError(f"Unsupported expression type: {type(expr).__name__}")

    def _convert_constant(self, expr: ast.Constant) -> str:
        """Convert constant values."""
        if isinstance(expr.value, str):
            return f'"{expr.value}"'
        elif isinstance(expr.value, bool):
            return "true" if expr.value else "false"
        elif expr.value is None:
            return "nil"
        elif isinstance(expr.value, float):
            # Convert whole floats to ints for cleaner code (1.0 -> 1)
            if expr.value.is_integer():
                return str(int(expr.value))
            return str(expr.value)
        else:
            return str(expr.value)

    def _convert_binop(self, expr: ast.BinOp) -> str:
        """Convert binary operations."""
        left = self._convert_expression(expr.left)
        right = self._convert_expression(expr.right)

        # Handle Go-specific operators
        if isinstance(expr.op, ast.Pow):
            return f"math.Pow({left}, {right})"
        elif isinstance(expr.op, ast.FloorDiv):
            # Go integer division is already floor division
            return f"({left} / {right})"

        # Use standard operator mapping from converter_utils
        op = get_standard_binary_operator(expr.op)
        if op is None:
            op = "/*UNKNOWN_OP*/"
        return f"({left} {op} {right})"

    def _convert_unaryop(self, expr: ast.UnaryOp) -> str:
        """Convert unary operations."""
        operand = self._convert_expression(expr.operand)

        op_map = {ast.UAdd: "+", ast.USub: "-", ast.Not: "!", ast.Invert: "^"}

        op = op_map.get(type(expr.op), "/*UNKNOWN_OP*/")
        return f"({op}{operand})"

    def _convert_compare(self, expr: ast.Compare) -> str:
        """Convert comparison operations."""
        left = self._convert_expression(expr.left)
        result = left

        for op, comp in zip(expr.ops, expr.comparators):
            # Use standard comparison operator mapping from converter_utils
            op_str = get_standard_comparison_operator(op)

            # Handle Go-specific operators
            if op_str is None:
                if isinstance(op, ast.Is):
                    op_str = "=="
                    comp_expr = self._convert_expression(comp)
                    result = f"({result} {op_str} {comp_expr})"
                elif isinstance(op, ast.IsNot):
                    op_str = "!="
                    comp_expr = self._convert_expression(comp)
                    result = f"({result} {op_str} {comp_expr})"
                elif isinstance(op, ast.In):
                    # Use map membership check with comma-ok idiom
                    comp_expr = self._convert_expression(comp)
                    result = f"func() bool {{ _, ok := {comp_expr}[{result}]; return ok }}()"
                elif isinstance(op, ast.NotIn):
                    comp_expr = self._convert_expression(comp)
                    result = f"func() bool {{ _, ok := {comp_expr}[{result}]; return !ok }}()"
                else:
                    op_str = "/*UNKNOWN_OP*/"
                    comp_expr = self._convert_expression(comp)
                    result = f"({result} {op_str} {comp_expr})"
            else:
                comp_expr = self._convert_expression(comp)
                result = f"({result} {op_str} {comp_expr})"

        return result

    def _convert_call(self, expr: ast.Call) -> str:
        """Convert function calls."""
        if isinstance(expr.func, ast.Name):
            func_name = expr.func.id
            args = [self._convert_expression(arg) for arg in expr.args]

            # Handle empty container constructors
            if func_name == "list" and len(args) == 0:
                # list() with no args -> []int{} (default to int)
                return "[]int{}"
            elif func_name == "dict" and len(args) == 0:
                # dict() with no args -> make(map[int]int) (default to int keys/values)
                return "make(map[int]int)"
            elif func_name == "set" and len(args) == 0:
                # set() with no args -> make(map[int]bool) (default to int keys)
                return "make(map[int]bool)"

            # Handle built-in functions
            if func_name == "print":
                args_str = ", ".join(args)
                return f"multigen.Print({args_str})"
            elif func_name == "len":
                arg_type = self._infer_type_from_value(expr.args[0])
                if arg_type.startswith("[]"):
                    elem_type = arg_type[2:]
                    return f"multigen.Len[{elem_type}]({args[0]})"
                elif arg_type.startswith("map["):
                    return f"multigen.LenMap({args[0]})"
                elif arg_type == "string":
                    return f"multigen.LenString({args[0]})"
                else:
                    return f"multigen.Len({args[0]})"
            elif func_name == "abs":
                arg_type = self._infer_type_from_value(expr.args[0])
                if arg_type == "float64" or arg_type == "float32":
                    return f"multigen.AbsFloat({args[0]})"
                else:
                    return f"multigen.AbsInt({args[0]})"
            elif func_name == "min":
                arg_type = self._infer_type_from_value(expr.args[0])
                elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                return f"multigen.Min[{elem_type}]({args[0]})"
            elif func_name == "max":
                arg_type = self._infer_type_from_value(expr.args[0])
                elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                return f"multigen.Max[{elem_type}]({args[0]})"
            elif func_name == "sum":
                arg_type = self._infer_type_from_value(expr.args[0])
                elem_type = arg_type[2:] if arg_type.startswith("[]") else "int"
                return f"multigen.Sum[{elem_type}]({args[0]})"
            elif func_name == "any":
                return f"multigen.Any({args[0]})"
            elif func_name == "all":
                return f"multigen.All({args[0]})"
            elif func_name == "bool":
                # BoolValue still needs to use interface{} since it handles many types
                return f"multigen.ToBool({args[0]})"
            elif func_name == "str":
                return f"multigen.ToStr({args[0]})"
            elif func_name == "range":
                range_args = ", ".join(args)
                return f"multigen.NewRange({range_args})"
            else:
                # Check if this is a class constructor
                if func_name in self.struct_info:
                    args_str = ", ".join(args)
                    return f"New{func_name}({args_str})"
                else:
                    args_str = ", ".join(args)
                    return f"{func_name}({args_str})"

        elif isinstance(expr.func, ast.Attribute):
            return self._convert_method_call_expression(expr)
        else:
            return "/* Complex function call */"

    def _convert_method_call_expression(self, expr: ast.Call) -> str:
        """Convert method calls on objects."""
        if isinstance(expr.func, ast.Attribute):
            obj_expr = self._convert_expression(expr.func.value)
            method_name = expr.func.attr
            args = [self._convert_expression(arg) for arg in expr.args]

            # Handle string methods
            if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                if method_name == "upper":
                    return f"multigen.StrOps.Upper({obj_expr})"
                elif method_name == "lower":
                    return f"multigen.StrOps.Lower({obj_expr})"
                elif method_name == "strip":
                    if args:
                        return f"multigen.StrOps.StripChars({obj_expr}, {args[0]})"
                    else:
                        return f"multigen.StrOps.Strip({obj_expr})"
                elif method_name == "find":
                    return f"multigen.StrOps.Find({obj_expr}, {args[0]})"
                elif method_name == "replace":
                    return f"multigen.StrOps.Replace({obj_expr}, {args[0]}, {args[1]})"
                elif method_name == "split":
                    if args:
                        return f"multigen.StrOps.SplitSep({obj_expr}, {args[0]})"
                    else:
                        return f"multigen.StrOps.Split({obj_expr})"

            # Handle container methods - convert append to Go's builtin
            # Note: This generates an expression that should be used in assignment
            if method_name == "append":
                # Python's list.append() -> Go's append() builtin
                # This needs special handling because append returns the new slice
                args_str = ", ".join(args)
                # Return a marker that the statement converter can detect
                return f"__APPEND__{obj_expr}__ARGS__{args_str}__END__"

            # Handle dict methods - translate to Go map iteration
            elif method_name == "items":
                # Python's dict.items() - Convert to slice of key-value pairs for comprehensions
                # multigen.MapItems() returns []multigen.KV[K, V]
                return f"multigen.MapItems({obj_expr})"
            elif method_name == "values":
                # Python's dict.values() - need to extract values from map
                # This would need runtime support, for now use map directly
                return f"multigen.MapValues({obj_expr})"
            elif method_name == "keys":
                # Python's dict.keys() - need to extract keys from map
                return f"multigen.MapKeys({obj_expr})"

            # Regular method call
            args_str = ", ".join(args)
            return f"{obj_expr}.{self._to_go_method_name(method_name)}({args_str})"

        return "/* Complex method call */"

    def _convert_attribute(self, expr: ast.Attribute) -> str:
        """Convert attribute access."""
        obj_expr = self._convert_expression(expr.value)
        return f"{obj_expr}.{self._to_camel_case(expr.attr)}"

    def _convert_list_literal(self, expr: ast.List) -> str:
        """Convert list literal to Go slice literal."""
        if not expr.elts:
            # Empty list - default to []int{}
            return "[]int{}"

        # Try to infer a common type for all elements
        element_types = [self._infer_type_from_value(elt) for elt in expr.elts]
        if element_types and all(t == element_types[0] and t != "interface{}" for t in element_types):
            # All elements have the same specific type
            element_type = element_types[0]
            elements = [self._convert_expression(elt) for elt in expr.elts]
            elements_str = ", ".join(elements)
            return f"[]{element_type}{{{elements_str}}}"
        else:
            # Mixed types or interface{}, use interface{}
            elements = [self._convert_expression(elt) for elt in expr.elts]
            elements_str = ", ".join(elements)
            return f"[]interface{{{elements_str}}}"

    def _convert_dict_literal(self, expr: ast.Dict) -> str:
        """Convert dict literal to Go map literal."""
        if not expr.keys:
            # Empty dict - default to map[int]int
            return "make(map[int]int)"

        # Check for None keys (dictionary unpacking with **)
        has_unpacking = any(key is None for key in expr.keys)

        if has_unpacking:
            # Dictionary unpacking present, use interface{} for safety
            pairs = []
            for key, value in zip(expr.keys, expr.values):
                if key is not None:
                    key_str = self._convert_expression(key)
                    value_str = self._convert_expression(value)
                    pairs.append(f"{key_str}: {value_str}")
                # Note: actual unpacking (**dict) would need runtime handling
            pairs_str = ", ".join(pairs)
            return f"map[interface{{}}]interface{{}}{{{{{pairs_str}}}}}"

        # Try to infer common types for keys and values (all keys are non-None)
        key_types = [self._infer_type_from_value(key) for key in expr.keys if key is not None]
        value_types = [self._infer_type_from_value(value) for value in expr.values]

        if (
            key_types
            and all(t == key_types[0] and t != "interface{}" for t in key_types)
            and value_types
            and all(t == value_types[0] and t != "interface{}" for t in value_types)
        ):
            # All keys and values have specific types
            key_type = key_types[0]
            value_type = value_types[0]
            pairs = []
            for key, value in zip(expr.keys, expr.values):
                if key is not None:
                    key_str = self._convert_expression(key)
                    value_str = self._convert_expression(value)
                    pairs.append(f"{key_str}: {value_str}")
            pairs_str = ", ".join(pairs)
            return f"map[{key_type}]{value_type}{{{{{pairs_str}}}}}"
        else:
            # Mixed types or interface{}, use interface{}
            pairs = []
            for key, value in zip(expr.keys, expr.values):
                if key is not None:
                    key_str = self._convert_expression(key)
                    value_str = self._convert_expression(value)
                    pairs.append(f"{key_str}: {value_str}")
            pairs_str = ", ".join(pairs)
            return f"map[interface{{}}]interface{{}}{{{{{pairs_str}}}}}"

    def _convert_set_literal(self, expr: ast.Set) -> str:
        """Convert set literal to Go map literal (sets as map[T]bool)."""
        if not expr.elts:
            # Empty set - default to map[int]bool
            return "make(map[int]bool)"
        elements = []
        for elt in expr.elts:
            elt_str = self._convert_expression(elt)
            elements.append(f"{elt_str}: true")
        elements_str = ", ".join(elements)
        return f"map[interface{{}}]bool{{{{{elements_str}}}}}"

    def _convert_list_comprehension(self, expr: ast.ListComp) -> str:
        """Convert list comprehensions using Go 1.18+ generics."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter
        conditions = expr.generators[0].ifs

        # Infer result type from element expression with loop variable context
        loop_var_types = self._infer_loop_variable_type(expr.generators[0])
        result_type = self._infer_comprehension_element_type(element_expr, loop_var_types)

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            # Range-based comprehension
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"multigen.NewRange({', '.join(range_args)})"

            # Create transform function with proper types
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_expr = self._convert_expression(element_expr)
            transform_lambda = f"func({target_name} int) {result_type} {{ return {transform_expr} }}"

            if conditions:
                # With condition
                condition_expr = self._convert_expression(conditions[0])
                condition_lambda = f"func({target_name} int) bool {{ return {condition_expr} }}"
                return f"multigen.ListComprehensionFromRangeWithFilter[{result_type}]({range_call}, {transform_lambda}, {condition_lambda})"
            else:
                # No condition
                return f"multigen.ListComprehensionFromRange[{result_type}]({range_call}, {transform_lambda})"
        else:
            # Container iteration - need to infer source type
            source_type = self._infer_type_from_value(iter_expr)
            # Extract element type from slice type (e.g., []int -> int)
            element_type = source_type[2:] if source_type.startswith("[]") else "interface{}"

            container_expr = self._convert_expression(iter_expr)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_expr = self._convert_expression(element_expr)
            transform_lambda = f"func({target_name} {element_type}) {result_type} {{ return {transform_expr} }}"

            if conditions:
                condition_expr = self._convert_expression(conditions[0])
                condition_lambda = f"func({target_name} {element_type}) bool {{ return {condition_expr} }}"
                return f"multigen.ListComprehensionWithFilter[{element_type}, {result_type}]({container_expr}, {transform_lambda}, {condition_lambda})"
            else:
                return f"multigen.ListComprehension[{element_type}, {result_type}]({container_expr}, {transform_lambda})"

    def _convert_dict_comprehension(self, expr: ast.DictComp) -> str:
        """Convert dictionary comprehensions using Go 1.18+ generics."""
        # Extract comprehension components
        key_expr = expr.key
        value_expr = expr.value
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter

        # Infer key and value types with loop variable context
        loop_var_types = self._infer_loop_variable_type(expr.generators[0])
        key_type = self._infer_comprehension_element_type(key_expr, loop_var_types)
        value_type = self._infer_comprehension_element_type(value_expr, loop_var_types)

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"multigen.NewRange({', '.join(range_args)})"
            target_name = target.id if isinstance(target, ast.Name) else "x"

            key_transform = self._convert_expression(key_expr)
            value_transform = self._convert_expression(value_expr)
            transform_lambda = (
                f"func({target_name} int) ({key_type}, {value_type}) {{ return {key_transform}, {value_transform} }}"
            )

            return f"multigen.DictComprehensionFromRange[{key_type}, {value_type}]({range_call}, {transform_lambda})"
        else:
            # Handle tuple unpacking for dict iteration: {k: v for k, v in dict.items()}
            if isinstance(target, ast.Tuple) and len(target.elts) == 2:
                # Tuple unpacking from .items()
                key_var = target.elts[0].id if isinstance(target.elts[0], ast.Name) else "k"
                value_var = target.elts[1].id if isinstance(target.elts[1], ast.Name) else "v"

                # Convert dict.items() to multigen.MapItems() call that returns []KV struct
                container_expr = self._convert_expression(iter_expr)
                key_transform = self._convert_expression(key_expr)
                value_transform = self._convert_expression(value_expr)

                # For Go, we need to convert map to slice of key-value pairs
                # The multigen.MapItems() function will handle this
                transform_lambda = f"func(kv multigen.KV[{key_type}, {value_type}]) ({key_type}, {value_type}) {{ {key_var}, {value_var} := kv.Key, kv.Value; return {key_transform}, {value_transform} }}"

                # Check if we need to handle filtering
                conditions = expr.generators[0].ifs
                if conditions:
                    condition_expr = self._convert_expression(conditions[0])
                    # Detect which variables are used in the condition
                    key_used = key_var in condition_expr
                    value_used = value_var in condition_expr
                    key_assign = key_var if key_used else "_"
                    value_assign = value_var if value_used else "_"
                    filter_lambda = f"func(kv multigen.KV[{key_type}, {value_type}]) bool {{ {key_assign}, {value_assign} := kv.Key, kv.Value; return {condition_expr} }}"
                    return f"multigen.DictComprehensionWithFilter[multigen.KV[{key_type}, {value_type}], {key_type}, {value_type}]({container_expr}, {transform_lambda}, {filter_lambda})"
                else:
                    return f"multigen.DictComprehension[multigen.KV[{key_type}, {value_type}], {key_type}, {value_type}]({container_expr}, {transform_lambda})"
            else:
                # Non-tuple unpacking case
                source_type = self._infer_type_from_value(iter_expr)
                element_type = source_type[2:] if source_type.startswith("[]") else "interface{}"

                container_expr = self._convert_expression(iter_expr)
                target_name = target.id if isinstance(target, ast.Name) else "x"
                key_transform = self._convert_expression(key_expr)
                value_transform = self._convert_expression(value_expr)
                transform_lambda = f"func({target_name} {element_type}) ({key_type}, {value_type}) {{ return {key_transform}, {value_transform} }}"

                return f"multigen.DictComprehension[{element_type}, {key_type}, {value_type}]({container_expr}, {transform_lambda})"

    def _convert_set_comprehension(self, expr: ast.SetComp) -> str:
        """Convert set comprehensions using Go 1.18+ generics."""
        # Extract comprehension components
        element_expr = expr.elt
        target = expr.generators[0].target
        iter_expr = expr.generators[0].iter

        # Infer element type for the set with loop variable context
        loop_var_types = self._infer_loop_variable_type(expr.generators[0])
        element_type = self._infer_comprehension_element_type(element_expr, loop_var_types)

        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            range_args = [self._convert_expression(arg) for arg in iter_expr.args]
            range_call = f"multigen.NewRange({', '.join(range_args)})"
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_expr = self._convert_expression(element_expr)
            transform_lambda = f"func({target_name} int) {element_type} {{ return {transform_expr} }}"

            return f"multigen.SetComprehensionFromRange[{element_type}]({range_call}, {transform_lambda})"
        else:
            source_type = self._infer_type_from_value(iter_expr)
            container_expr = self._convert_expression(iter_expr)
            target_name = target.id if isinstance(target, ast.Name) else "x"
            transform_expr = self._convert_expression(element_expr)

            # Extract element type from source (handle both slices and sets)
            if source_type.startswith("[]"):
                # Slice type: []int → int
                source_element_type = source_type[2:]
                transform_lambda = (
                    f"func({target_name} {source_element_type}) {element_type} {{ return {transform_expr} }}"
                )
                return f"multigen.SetComprehension[{source_element_type}, {element_type}]({container_expr}, {transform_lambda})"
            elif source_type.startswith("map[") and source_type.endswith("]bool"):
                # Set type: map[int]bool → int - use SetComprehensionFromSet
                source_element_type = source_type[4:-5]  # Remove "map[" prefix and "]bool" suffix
                transform_lambda = (
                    f"func({target_name} {source_element_type}) {element_type} {{ return {transform_expr} }}"
                )

                # Check if there's a filter condition
                if expr.generators[0].ifs:
                    # Has filter - use SetComprehensionFromSetWithFilter
                    filter_conditions = " && ".join(
                        [self._convert_expression(if_expr) for if_expr in expr.generators[0].ifs]
                    )
                    filter_lambda = f"func({target_name} {source_element_type}) bool {{ return {filter_conditions} }}"
                    return f"multigen.SetComprehensionFromSetWithFilter[{source_element_type}, {element_type}]({container_expr}, {filter_lambda}, {transform_lambda})"
                else:
                    # No filter - use SetComprehensionFromSet
                    return f"multigen.SetComprehensionFromSet[{source_element_type}, {element_type}]({container_expr}, {transform_lambda})"
            else:
                source_element_type = "interface{}"
                transform_lambda = (
                    f"func({target_name} {source_element_type}) {element_type} {{ return {transform_expr} }}"
                )
                return f"multigen.SetComprehension[{source_element_type}, {element_type}]({container_expr}, {transform_lambda})"

    def _convert_subscript(self, expr: ast.Subscript) -> str:
        """Convert subscript operation to Go array/map access."""
        value_expr = self._convert_expression(expr.value)

        if isinstance(expr.slice, ast.Slice):
            # Handle slicing
            raise UnsupportedFeatureError("Slice operations not supported in Go backend")
        else:
            # Simple subscript
            index_expr = self._convert_expression(expr.slice)
            return f"{value_expr}[{index_expr}]"

    def _convert_f_string(self, expr: ast.JoinedStr) -> str:
        """Convert f-string to Go fmt.Sprintf.

        Example:
            f"Result: {x}" -> fmt.Sprintf("Result: %v", x)
            f"Count: {len(items)} items" -> fmt.Sprintf("Count: %v items", len(items))
        """
        # Build format string and arguments list
        format_parts: list[str] = []
        args: list[str] = []

        for value in expr.values:
            if isinstance(value, ast.Constant):
                # Literal string part - escape % signs
                if isinstance(value.value, str):
                    literal = value.value.replace("%", "%%")
                    format_parts.append(literal)
            elif isinstance(value, ast.FormattedValue):
                # Expression to be formatted - use %v (value in default format)
                format_parts.append("%v")
                expr_code = self._convert_expression(value.value)
                args.append(expr_code)

        format_string = "".join(format_parts)

        if len(args) == 0:
            # No expressions, just return string literal
            return f'"{format_string}"'
        else:
            # Use fmt.Sprintf
            args_str = ", ".join(args)
            return f'fmt.Sprintf("{format_string}", {args_str})'

    # Helper methods for type inference and mapping

    def _map_type_annotation(self, annotation: ast.expr) -> str:
        """Map Python type annotation to Go type."""
        if isinstance(annotation, ast.Name):
            return self.type_map.get(annotation.id, "interface{}")
        elif isinstance(annotation, ast.Subscript):
            # Handle subscripted types like list[int], dict[str, int], etc.
            if isinstance(annotation.value, ast.Name):
                container_type = annotation.value.id
                if container_type == "list":
                    # list[int] -> []int, list[list[int]] -> [][]int
                    if isinstance(annotation.slice, ast.Name):
                        element_type = self.type_map.get(annotation.slice.id, annotation.slice.id)
                        return f"[]{element_type}"
                    elif isinstance(annotation.slice, ast.Subscript):
                        # Recursively handle nested lists like list[list[int]]
                        element_type = self._map_type_annotation(annotation.slice)
                        return f"[]{element_type}"
                    return "[]interface{}"
                elif container_type == "dict":
                    # dict[str, int] -> map[string]int
                    if isinstance(annotation.slice, ast.Tuple) and len(annotation.slice.elts) == 2:
                        key_type = self._map_type_annotation(annotation.slice.elts[0])
                        value_type = self._map_type_annotation(annotation.slice.elts[1])
                        return f"map[{key_type}]{value_type}"
                    return "map[interface{}]interface{}"
                elif container_type == "set":
                    # set[int] -> map[int]bool
                    if isinstance(annotation.slice, ast.Name):
                        element_type = self.type_map.get(annotation.slice.id, annotation.slice.id)
                        return f"map[{element_type}]bool"
                    return "map[interface{}]bool"
            return "interface{}"
        elif isinstance(annotation, ast.Constant):
            if annotation.value is None:
                return ""  # None type should be empty return type
            return str(annotation.value)
        else:
            return "interface{}"

    def _infer_type_from_value(self, value: ast.expr) -> str:
        """Infer Go type from Python value using Strategy pattern.

        This method has been refactored to use the TypeInferenceEngine,
        reducing complexity from 31 to ~8.

        Before refactoring: 75 lines, complexity 31
        After refactoring: 13 lines, complexity ~8
        """
        # Create inference context with Go-specific type mapper
        context = InferenceContext(
            type_mapper=self._map_type,
            variable_types=self.variable_types,
        )

        # Delegate to type inference engine
        return self.type_inference_engine.infer_type(value, context)

    def _infer_loop_variable_type(self, generator: ast.comprehension) -> dict[str, str]:
        """Infer the type of the loop variable in a comprehension."""
        target = generator.target
        iter_expr = generator.iter

        loop_var_types = {}
        if isinstance(target, ast.Name):
            # Infer type from iterator
            if (
                isinstance(iter_expr, ast.Call)
                and isinstance(iter_expr.func, ast.Name)
                and iter_expr.func.id == "range"
            ):
                loop_var_types[target.id] = "int"
            else:
                # Iterating over a container
                iter_type = self._infer_type_from_value(iter_expr)
                # Extract element type from slice or set
                if iter_type.startswith("[]"):
                    # Slice type: []int → int
                    element_type = iter_type[2:]
                    loop_var_types[target.id] = element_type
                elif iter_type.startswith("map[") and iter_type.endswith("]bool"):
                    # Set type: map[int]bool → int
                    element_type = iter_type[4:-5]
                    loop_var_types[target.id] = element_type
                else:
                    loop_var_types[target.id] = "interface{}"
        return loop_var_types

    def _infer_comprehension_element_type(self, expr: ast.expr, loop_var_types: dict[str, str]) -> str:
        """Infer the type of elements produced by a comprehension expression."""
        if isinstance(expr, ast.Constant):
            if isinstance(expr.value, bool):
                return "bool"
            elif isinstance(expr.value, int):
                return "int"
            elif isinstance(expr.value, float):
                return "float64"
            elif isinstance(expr.value, str):
                return "string"
        elif isinstance(expr, ast.Name):
            # Check if it's a loop variable with known type
            if expr.id in loop_var_types:
                return loop_var_types[expr.id]
            # Check if it's a regular variable
            if expr.id in self.variable_types:
                return self.variable_types[expr.id]
            # Default to int for range-based comprehensions
            return "int"
        elif isinstance(expr, ast.BinOp):
            # For binary operations, try to infer from operands
            left_type = self._infer_comprehension_element_type(expr.left, loop_var_types)
            right_type = self._infer_comprehension_element_type(expr.right, loop_var_types)
            # If both are the same type, use that
            if left_type == right_type:
                return left_type
            # If one is float and one is int, result is float
            if {left_type, right_type} == {"int", "float64"}:
                return "float64"
            return "int"  # Default to int for arithmetic
        elif isinstance(expr, ast.Call):
            # Handle function calls in comprehensions
            if isinstance(expr.func, ast.Attribute):
                # Method calls like str.upper() return string
                attr_name = expr.func.attr
                if attr_name in ("upper", "lower", "strip", "replace"):
                    return "string"
            return "int"  # Default

        return "int"  # Default to int

    def _infer_type_from_assignment(self, stmt: ast.Assign) -> str:
        """Infer type from assignment statement."""
        return self._infer_type_from_value(stmt.value)

    def _infer_parameter_type(self, arg: ast.arg, func: ast.FunctionDef) -> str:
        """Infer parameter type from annotation or context."""
        if arg.annotation:
            return self._map_type_annotation(arg.annotation)
        return "interface{}"

    def _infer_return_type(self, func: ast.FunctionDef) -> str:
        """Infer return type from function body."""
        for stmt in func.body:
            if isinstance(stmt, ast.Return) and stmt.value:
                # Found return statement, infer type
                return "interface{}"  # Default to interface{} for complex expressions
        return ""  # No return statement found

    def _is_constructor_call(self, value: ast.expr) -> bool:
        """Check if the expression is a constructor call."""
        return isinstance(value, ast.Call) and isinstance(value.func, ast.Name) and value.func.id in self.struct_info

    def _extract_struct_fields(self, init_method: ast.FunctionDef) -> list[str]:
        """Extract struct field names from __init__ method."""
        fields = []
        for stmt in init_method.body:
            if isinstance(stmt, (ast.Assign, ast.AnnAssign)):
                if isinstance(stmt.target if hasattr(stmt, "target") else stmt.targets[0], ast.Attribute):
                    target = stmt.target if hasattr(stmt, "target") else stmt.targets[0]
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        fields.append(target.attr)
        return fields

    def _get_default_value(self, go_type: str) -> str:
        """Get default value for Go type."""
        defaults = {
            "int": "0",
            "float64": "0.0",
            "bool": "false",
            "string": '""',
            "[]interface{}": "[]interface{}{}",
            "map[interface{}]interface{}": "make(map[interface{}]interface{})",
            "map[interface{}]bool": "make(map[interface{}]bool)",
        }
        # Handle specific slice types like []int, []string, etc.
        if go_type.startswith("[]") and go_type != "[]interface{}":
            return f"{go_type}{{}}"
        # Handle specific map types
        if go_type.startswith("map["):
            return f"make({go_type})"
        return defaults.get(go_type, "nil")
