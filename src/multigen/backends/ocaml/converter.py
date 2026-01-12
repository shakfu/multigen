"""Enhanced OCaml code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import TYPE_CHECKING, Any, Optional, Union

from ..converter_utils import (
    get_standard_binary_operator,
    get_standard_comparison_operator,
)
from ..errors import UnsupportedFeatureError
from ..loop_conversion_strategies import LoopContext
from ..preferences import BackendPreferences

if TYPE_CHECKING:
    from ..loop_conversion_strategies import ForLoopConverter


class MultiGenPythonToOCamlConverter:
    """Sophisticated Python-to-OCaml converter with comprehensive language support."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the converter with optional preferences."""
        self.preferences = preferences
        self.type_map = {
            "int": "int",
            "float": "float",
            "bool": "bool",
            "str": "string",
            "list": "'a list",  # Generic list type
            "dict": "(string * 'a) list",  # Association list
            "set": "'a list",  # List-based set
            "Any": "'a",
            "None": "unit",
            "void": "unit",
        }

        # Track class definitions and variables
        self.classes: dict[str, Any] = {}
        self.variables: dict[str, str] = {}
        self.current_class: Optional[str] = None
        self.mutable_vars: set[str] = set()  # Variables that need to be refs

        # Lazy-initialized loop converter
        self._loop_converter: Optional[ForLoopConverter] = None

    def convert_code(self, python_code: str) -> str:
        """Convert Python source code to OCaml."""
        try:
            tree = ast.parse(python_code)
            return self._convert_module(tree)
        except SyntaxError as e:
            raise UnsupportedFeatureError(f"Python syntax error: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to OCaml."""
        # Include runtime library
        ocaml_code = ["(* Generated OCaml code from Python *)"]
        ocaml_code.append("")
        ocaml_code.append("open Multigen_runtime")
        ocaml_code.append("")

        # Track if main function exists
        has_main = False
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "main":
                has_main = True
                break

        # Convert all statements
        for stmt in node.body:
            converted = self._convert_statement(stmt)
            if converted:
                if isinstance(converted, list):
                    ocaml_code.extend(converted)
                else:
                    ocaml_code.append(converted)
                ocaml_code.append("")

        # Add main execution
        ocaml_code.append("(* Main execution *)")
        if has_main:
            ocaml_code.append("let () = ignore (main ())")
        else:
            ocaml_code.append('let () = print_value "Generated OCaml code executed successfully"')

        return "\n".join(ocaml_code)

    def _convert_statement(self, node: ast.AST) -> Union[str, list[str]]:
        """Convert a Python statement to OCaml."""
        if isinstance(node, ast.FunctionDef):
            return self._convert_function_def(node)
        elif isinstance(node, ast.ClassDef):
            return self._convert_class_def(node)
        elif isinstance(node, ast.Assign):
            return self._convert_assignment(node)
        elif isinstance(node, ast.AnnAssign):
            return self._convert_annotated_assignment(node)
        elif isinstance(node, ast.AugAssign):
            return self._convert_augmented_assignment(node)
        elif isinstance(node, ast.Expr):
            return self._convert_expression_statement(node)
        elif isinstance(node, ast.Return):
            return self._convert_return(node)
        elif isinstance(node, ast.If):
            return self._convert_if_statement(node)
        elif isinstance(node, ast.While):
            return self._convert_while_statement(node)
        elif isinstance(node, ast.For):
            return self._convert_for_statement(node)
        elif isinstance(node, ast.ImportFrom):
            # Ignore ImportFrom statements (like "from __future__ import annotations")
            # These are Python-specific directives that don't need translation
            return "(* Import statement ignored *)"
        elif isinstance(node, ast.Assert):
            return self._convert_assert_statement(node)
        else:
            raise UnsupportedFeatureError(f"Unsupported statement: {type(node).__name__}")

    def _convert_assert_statement(self, node: ast.Assert) -> str:
        """Convert Python assert statement to OCaml assert.

        Args:
            node: Python assert statement node

        Returns:
            OCaml assert statement as string

        Example:
            assert x > 0  →  assert (x > 0);
            assert result == 1, "Test failed"  →  assert (result = 1); (* Test failed *)
        """
        # Convert the test expression
        test_expr = self._convert_expression(node.test)

        # Handle optional message
        # Note: Don't add trailing semicolon - it's added by function body assembly
        if node.msg:
            # Convert message to string
            if isinstance(node.msg, ast.Constant) and isinstance(node.msg.value, str):
                msg = node.msg.value
                return f"assert ({test_expr}) (* {msg} *)"
            else:
                # Complex message expression - just add assert without comment
                return f"assert ({test_expr})"
        else:
            return f"assert ({test_expr})"

    def _convert_function_def(self, node: ast.FunctionDef) -> list[str]:
        """Convert Python function definition to OCaml."""
        self._to_ocaml_var_name(node.name)

        # Extract parameter information
        params = []
        for arg in node.args.args:
            param_name = self._to_ocaml_var_name(arg.arg)
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "'a"
            params.append((param_name, param_type))

        # Get return type
        return_type = self._get_type_annotation(node.returns) if node.returns else "'a"

        # Handle methods vs functions
        if self.current_class:
            # This is a method
            if node.name == "__init__":
                return self._convert_constructor(node, params)
            else:
                return self._convert_method(node, params, return_type)
        else:
            # This is a regular function
            return self._convert_regular_function(node, params, return_type)

    def _is_recursive_function(self, node: ast.FunctionDef, func_name: str) -> bool:
        """Check if a function is recursive by looking for calls to itself."""
        for child in ast.walk(node):
            if isinstance(child, ast.Call):
                if isinstance(child.func, ast.Name) and child.func.id == func_name:
                    return True
        return False

    def _find_mutable_variables(self, node: ast.FunctionDef) -> set[str]:
        """Find all variables that are mutated in a function.

        This includes:
        - Augmented assignment: x += 1
        - Subscript assignment in loops: arr[i] = value
        - Append calls in loops: data.append(x)
        - Conditional assignment: if cond: x = value (inside if statements)
        """
        mutable = set()
        for child in ast.walk(node):
            # Augmented assignment
            if isinstance(child, ast.AugAssign) and isinstance(child.target, ast.Name):
                mutable.add(child.target.id)
            # For loops with mutations - use _has_mutations helper to recursively check
            elif isinstance(child, ast.For):
                mutable.update(self._has_mutations(child.body))
            # If statements with regular assignments (conditional mutations)
            elif isinstance(child, ast.If):
                for stmt in ast.walk(child):
                    if isinstance(stmt, ast.Assign):
                        for target in stmt.targets:
                            if isinstance(target, ast.Name):
                                mutable.add(target.id)
        return mutable

    def _convert_regular_function(self, node: ast.FunctionDef, params: list[tuple], return_type: str) -> list[str]:
        """Convert a regular function definition."""
        func_name = self._to_ocaml_var_name(node.name)

        # Find mutable variables for this function
        self.mutable_vars = self._find_mutable_variables(node)

        # Exclude function parameters from mutable vars (they're passed normally, not as refs)
        param_names = {name for name, _ in params}
        self.mutable_vars = self.mutable_vars - param_names

        # Check if function is recursive
        is_recursive = self._is_recursive_function(node, node.name)

        # Function signature
        rec_keyword = "rec " if is_recursive else ""
        if params:
            param_list = " ".join([name for name, _ in params])
            signature = f"let {rec_keyword}{func_name} {param_list} ="
        else:
            signature = f"let {rec_keyword}{func_name} () ="

        lines = [signature]

        # Filter out docstrings first
        filtered_body = []
        for stmt in node.body:
            if (
                isinstance(stmt, ast.Expr)
                and isinstance(stmt.value, ast.Constant)
                and isinstance(stmt.value.value, str)
            ):
                continue
            filtered_body.append(stmt)

        # Check for early return pattern: if cond: return X; return Y
        if (
            len(filtered_body) == 2
            and isinstance(filtered_body[0], ast.If)
            and len(filtered_body[0].body) == 1
            and isinstance(filtered_body[0].body[0], ast.Return)
            and isinstance(filtered_body[1], ast.Return)
            and not filtered_body[0].orelse
        ):
            # Convert directly to if-expression
            condition = self._convert_expression(filtered_body[0].test)
            then_expr = filtered_body[0].body[0].value
            else_expr = filtered_body[1].value
            if then_expr and else_expr:
                then_value = self._convert_expression(then_expr)
                else_value = self._convert_expression(else_expr)
                lines.append(f"  if {condition} then {then_value} else {else_value}")
            else:
                # Fall back to normal conversion if return values are None
                then_value = self._convert_expression(then_expr) if then_expr else "()"
                else_value = self._convert_expression(else_expr) if else_expr else "()"
                lines.append(f"  if {condition} then {then_value} else {else_value}")
        elif len(filtered_body) == 1 and isinstance(filtered_body[0], ast.Return):
            # Single return statement
            expr = self._convert_expression(filtered_body[0].value) if filtered_body[0].value else "()"
            lines.append(f"  {expr}")
        else:
            # Multiple statements - use let expressions
            body_lines = []
            for _i, stmt in enumerate(filtered_body):
                if isinstance(stmt, ast.Return):
                    if stmt.value:
                        body_lines.append(self._convert_expression(stmt.value))
                    else:
                        body_lines.append("()")
                else:
                    converted = self._convert_statement(stmt)
                    if isinstance(converted, list):
                        body_lines.extend(converted)
                    else:
                        body_lines.append(converted)

            # Properly sequence statements with semicolons where needed
            if body_lines:
                for i, line in enumerate(body_lines):
                    if i < len(body_lines) - 1:  # Not the last statement
                        if not line.rstrip().endswith(" in"):
                            lines.append(f"  {line};")
                        else:
                            lines.append(f"  {line}")
                    else:
                        # Last statement - if it ends with 'in', add ()
                        if line.rstrip().endswith(" in"):
                            lines.append(f"  {line}")
                            lines.append("  ()")
                        else:
                            lines.append(f"  {line}")
            else:
                lines.append("  ()")

        return lines

    def _convert_class_def(self, node: ast.ClassDef) -> list[str]:
        """Convert Python class to OCaml record type and functions."""
        class_name = self._to_ocaml_type_name(node.name)
        self.current_class = class_name

        # Extract fields from __init__ method
        init_method = None
        methods = []

        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "__init__":
                    init_method = item
                else:
                    methods.append(item)

        lines = []

        # Generate record type
        if init_method:
            fields = self._extract_class_fields(init_method)
            if fields:
                lines.append(f"type {class_name.lower()} = {{")
                for field_name, field_type in fields:
                    lines.append(f"  {field_name} : {field_type};")
                lines.append("}")
                lines.append("")
        else:
            # Empty class
            lines.append(f"type {class_name.lower()} = unit")
            lines.append("")

        # Generate constructor function
        if init_method:
            constructor_lines = self._convert_constructor(init_method, [])
            lines.extend(constructor_lines)
            lines.append("")

        # Generate methods
        for method in methods:
            method_lines = self._convert_method(method, [], "'a")
            lines.extend(method_lines)
            lines.append("")

        self.current_class = None
        return lines

    def _extract_class_fields(self, init_method: ast.FunctionDef) -> list[tuple]:
        """Extract field definitions from __init__ method."""
        fields = []

        for stmt in init_method.body:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        field_name = self._to_ocaml_var_name(target.attr)
                        field_type = self._infer_type_from_value(stmt.value)
                        fields.append((field_name, field_type))
            elif isinstance(stmt, ast.AnnAssign):
                if (
                    isinstance(stmt.target, ast.Attribute)
                    and isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                ):
                    field_name = self._to_ocaml_var_name(stmt.target.attr)
                    field_type = self._get_type_annotation(stmt.annotation) if stmt.annotation else "'a"
                    fields.append((field_name, field_type))

        return fields

    def _convert_constructor(self, node: ast.FunctionDef, params: list[tuple]) -> list[str]:
        """Convert __init__ method to constructor function."""
        if self.current_class is None:
            raise ValueError("Constructor called outside of class context")
        class_name = self.current_class.lower()

        # Extract constructor parameters (excluding self)
        constructor_params = []
        for arg in node.args.args[1:]:  # Skip 'self'
            param_name = self._to_ocaml_var_name(arg.arg)
            param_type = self._get_type_annotation(arg.annotation) if arg.annotation else "'a"
            constructor_params.append((param_name, param_type))

        # Constructor function signature
        if constructor_params:
            param_list = " ".join([name for name, _ in constructor_params])
            signature = f"let create_{class_name} {param_list} ="
        else:
            signature = f"let create_{class_name} () ="

        lines = [signature]

        # Extract field assignments
        field_assignments = []
        for stmt in node.body:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        field_name = self._to_ocaml_var_name(target.attr)
                        field_value = self._convert_expression(stmt.value)
                        field_assignments.append(f"    {field_name} = {field_value};")
            elif isinstance(stmt, ast.AnnAssign):
                if (
                    isinstance(stmt.target, ast.Attribute)
                    and isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                ):
                    field_name = self._to_ocaml_var_name(stmt.target.attr)
                    if stmt.value:
                        field_value = self._convert_expression(stmt.value)
                    else:
                        # Use default value based on type
                        field_type = self._get_type_annotation(stmt.annotation) if stmt.annotation else "int"
                        field_value = self._get_default_value(field_type)
                    field_assignments.append(f"    {field_name} = {field_value};")

        if field_assignments:
            lines.append("  {")
            lines.extend(field_assignments)
            lines.append("  }")
        else:
            lines.append("  ()")

        return lines

    def _convert_method(self, node: ast.FunctionDef, params: list[tuple], return_type: str) -> list[str]:
        """Convert class method to OCaml function."""
        if self.current_class is None:
            raise ValueError("Method called outside of class context")
        method_name = self._to_ocaml_var_name(node.name)
        class_name = self.current_class.lower()

        # Method parameters (first is always the object)
        method_params = [f"({class_name}_obj : {class_name})"]
        for arg in node.args.args[1:]:  # Skip 'self'
            param_name = self._to_ocaml_var_name(arg.arg)
            method_params.append(param_name)

        # Function signature
        param_list = " ".join(method_params)
        signature = f"let {class_name}_{method_name} {param_list} ="

        lines = [signature]

        # Convert method body (simplified - methods often just return default values)
        if len(node.body) == 1 and isinstance(node.body[0], ast.Return):
            if node.body[0].value:
                expr = self._convert_expression(node.body[0].value)
                lines.append(f"  {expr}")
            else:
                lines.append("  ()")
        else:
            # For complex methods, provide a placeholder
            if return_type == "unit":
                lines.append("  ()")
            elif return_type == "int":
                lines.append("  0")
            elif return_type == "string":
                lines.append('  ""')
            elif return_type == "bool":
                lines.append("  false")
            else:
                lines.append("  (* Method implementation simplified *)")
                lines.append('  failwith "Method not implemented"')

        return lines

    def _convert_expression(self, node: ast.AST) -> str:
        """Convert Python expression to OCaml."""
        if isinstance(node, ast.Constant):
            return self._convert_constant(node)
        elif isinstance(node, ast.Name):
            return self._convert_name(node)
        elif isinstance(node, ast.BinOp):
            return self._convert_binary_operation(node)
        elif isinstance(node, ast.UnaryOp):
            return self._convert_unary_operation(node)
        elif isinstance(node, ast.Compare):
            return self._convert_comparison(node)
        elif isinstance(node, ast.Call):
            return self._convert_function_call(node)
        elif isinstance(node, ast.Attribute):
            return self._convert_attribute_access(node)
        elif isinstance(node, ast.Subscript):
            return self._convert_subscript(node)
        elif isinstance(node, ast.List):
            return self._convert_list_literal(node)
        elif isinstance(node, ast.Dict):
            return self._convert_dict_literal(node)
        elif isinstance(node, ast.Set):
            return self._convert_set_literal(node)
        elif isinstance(node, ast.ListComp):
            return self._convert_list_comprehension(node)
        elif isinstance(node, ast.DictComp):
            return self._convert_dict_comprehension(node)
        elif isinstance(node, ast.SetComp):
            return self._convert_set_comprehension(node)
        elif isinstance(node, ast.IfExp):
            return self._convert_ternary_expression(node)
        elif isinstance(node, ast.JoinedStr):
            return self._convert_f_string(node)
        else:
            raise UnsupportedFeatureError(f"Unsupported expression: {type(node).__name__}")

    def _convert_constant(self, node: ast.Constant) -> str:
        """Convert Python constant to OCaml."""
        if isinstance(node.value, bool):
            return "true" if node.value else "false"
        elif isinstance(node.value, int):
            return str(node.value)
        elif isinstance(node.value, float):
            return str(node.value)
        elif isinstance(node.value, str):
            # Escape quotes and backslashes
            escaped = node.value.replace("\\", "\\\\").replace('"', '\\"')
            return f'"{escaped}"'
        elif node.value is None:
            return "()"
        else:
            raise UnsupportedFeatureError(f"Unsupported constant type: {type(node.value)}")

    def _convert_name(self, node: ast.Name) -> str:
        """Convert Python name to OCaml variable name."""
        var_name = self._to_ocaml_var_name(node.id)

        # If this is a mutable variable (ref), dereference it for reading
        if node.id in self.mutable_vars:
            return f"!{var_name}"
        else:
            return var_name

    def _convert_binary_operation(self, node: ast.BinOp) -> str:
        """Convert Python binary operation to OCaml."""
        left = self._convert_expression(node.left)
        right = self._convert_expression(node.right)

        # Handle OCaml-specific operators
        if isinstance(node.op, ast.FloorDiv):
            op = "/"  # OCaml doesn't have floor division
        elif isinstance(node.op, ast.Mod):
            op = "mod"
        elif isinstance(node.op, ast.Pow):
            op = "**"
        elif isinstance(node.op, ast.BitOr):
            op = "lor"
        elif isinstance(node.op, ast.BitXor):
            op = "lxor"
        elif isinstance(node.op, ast.BitAnd):
            op = "land"
        elif isinstance(node.op, ast.LShift):
            op = "lsl"
        elif isinstance(node.op, ast.RShift):
            op = "lsr"
        else:
            # Use standard operator mapping from converter_utils for common operators
            op_result = get_standard_binary_operator(node.op)
            if op_result is None:
                raise UnsupportedFeatureError(f"Unsupported binary operator: {type(node.op).__name__}")
            op = op_result

        return f"({left} {op} {right})"

    def _convert_unary_operation(self, node: ast.UnaryOp) -> str:
        """Convert Python unary operation to OCaml."""
        operand = self._convert_expression(node.operand)

        if isinstance(node.op, ast.UAdd):
            return f"(+{operand})"
        elif isinstance(node.op, ast.USub):
            return f"(-{operand})"
        elif isinstance(node.op, ast.Not):
            return f"(not {operand})"
        elif isinstance(node.op, ast.Invert):
            return f"(lnot {operand})"
        else:
            raise UnsupportedFeatureError(f"Unsupported unary operator: {type(node.op).__name__}")

    def _convert_comparison(self, node: ast.Compare) -> str:
        """Convert Python comparison to OCaml."""
        if len(node.ops) != 1 or len(node.comparators) != 1:
            raise UnsupportedFeatureError("Chained comparisons not supported")

        left = self._convert_expression(node.left)
        right = self._convert_expression(node.comparators[0])
        op = node.ops[0]

        # Handle OCaml-specific comparison operators
        if isinstance(op, ast.NotEq):
            ocaml_op = "<>"  # OCaml uses <> instead of !=
            return f"({left} {ocaml_op} {right})"
        elif isinstance(op, ast.In):
            # Use List.mem_assoc for membership in association lists (dicts)
            return f"(List.mem_assoc {left} {right})"
        elif isinstance(op, ast.NotIn):
            # Use not with List.mem_assoc
            return f"(not (List.mem_assoc {left} {right}))"
        else:
            # Use standard operator mapping from converter_utils for common operators
            op_result = get_standard_comparison_operator(op)
            if op_result is None:
                raise UnsupportedFeatureError(f"Unsupported comparison operator: {type(op).__name__}")
            # OCaml uses = for equality (same as standard)
            ocaml_op = op_result
            return f"({left} {ocaml_op} {right})"

    def _convert_function_call(self, node: ast.Call) -> str:
        """Convert Python function call to OCaml."""
        if isinstance(node.func, ast.Name):
            func_name = node.func.id

            # Handle built-in functions
            if func_name in ["abs", "bool", "len", "min", "max", "sum"]:
                return self._convert_builtin_call(func_name, node.args)
            elif func_name == "range":
                return self._convert_range_call(node.args)
            elif func_name == "print":
                if node.args:
                    arg_node = node.args[0]
                    arg = self._convert_expression(arg_node)

                    # Determine the conversion function based on type
                    conversion_func: Optional[str] = "string_of_int"  # Default to int

                    # Check if argument is a variable with known type
                    if isinstance(arg_node, ast.Name):
                        var_name = self._to_ocaml_var_name(arg_node.id)
                        if var_name in self.variables:
                            var_type = self.variables[var_name]
                            if var_type == "string" or var_type == "str":
                                conversion_func = None  # No conversion needed
                            elif var_type == "float":
                                conversion_func = "string_of_float"
                            elif var_type == "bool":
                                conversion_func = "Conversions.string_of_bool"
                    # Check if argument is a string literal
                    elif isinstance(arg_node, ast.Constant) and isinstance(arg_node.value, str):
                        conversion_func = None  # No conversion needed

                    if conversion_func:
                        return f"print_value ({conversion_func} {arg})"
                    else:
                        return f"print_value {arg}"
                else:
                    return 'print_value ""'
            else:
                # Regular function call
                args = [self._convert_expression(arg) for arg in node.args]
                if args:
                    return f"{self._to_ocaml_var_name(func_name)} {' '.join(args)}"
                else:
                    return f"{self._to_ocaml_var_name(func_name)} ()"
        elif isinstance(node.func, ast.Attribute):
            return self._convert_method_call(node)
        else:
            raise UnsupportedFeatureError(f"Unsupported function call: {type(node.func).__name__}")

    def _convert_builtin_call(self, func_name: str, args: list[ast.expr]) -> str:
        """Convert built-in function calls."""
        if not args:
            raise UnsupportedFeatureError(f"Function {func_name} requires arguments")

        arg = self._convert_expression(args[0])

        # Special handling for len() - detect if argument is an array
        if func_name == "len":
            # Default to len' (works for lists, dicts, sets)
            # Only use len_array when we're certain it's a Python list (→ OCaml array)
            use_array_len = False

            if isinstance(args[0], ast.Name):
                var_name = self._to_ocaml_var_name(args[0].id)
                var_type = self.variables.get(var_name, "")

                # Explicitly check for Python list type
                # Python lists become OCaml arrays, but are stored with OCaml type syntax like "int list"
                if var_type:
                    # Python list becomes OCaml array - check for bare "list" or OCaml type like "int list"
                    if var_type == "list" or (var_type.endswith(" list") and " * " not in var_type):
                        use_array_len = True
                    # Sets and dicts use regular OCaml lists (association lists)
                    elif var_type == "set" or var_type == "dict":
                        use_array_len = False
                    # Tuple/dict association lists (OCaml type strings like "(int * string) list")
                    elif " * " in var_type:
                        use_array_len = False
                    # Array type annotations
                    elif "array" in var_type or var_type.endswith("[]"):
                        use_array_len = True
                    # For safety: unknown types use len' (works for both)
                    else:
                        use_array_len = False

            # The arg might have been converted with dereferencing (!)
            # Check if it starts with ! and adjust the function accordingly
            if use_array_len:
                return f"len_array {arg}"
            else:
                return f"len' {arg}"

        builtin_map = {
            "abs": f"abs' {arg}",
            "bool": f"bool' {arg}",
            "min": f"min' {arg} {self._convert_expression(args[1]) if len(args) > 1 else arg}",
            "max": f"max' {arg} {self._convert_expression(args[1]) if len(args) > 1 else arg}",
            "sum": f"sum' {arg}",
            "any": f"any' {arg}",
            "all": f"all' {arg}",
        }

        return builtin_map.get(func_name, f"{func_name}' {arg}")

    def _convert_range_call(self, args: list[ast.expr]) -> str:
        """Convert range() call to OCaml."""
        if len(args) == 1:
            stop = self._convert_expression(args[0])
            return f"range_list (range {stop})"
        elif len(args) == 2:
            start = self._convert_expression(args[0])
            stop = self._convert_expression(args[1])
            return f"range_list (range2 {start} {stop})"
        elif len(args) == 3:
            start = self._convert_expression(args[0])
            stop = self._convert_expression(args[1])
            step = self._convert_expression(args[2])
            return f"range_list (range3 {start} {stop} {step})"
        else:
            raise UnsupportedFeatureError("range() requires 1-3 arguments")

    def _convert_method_call(self, node: ast.Call) -> str:
        """Convert method call to OCaml function call."""
        if isinstance(node.func, ast.Attribute):
            method_name = node.func.attr

            # Handle chained method calls: obj.method1().method2()
            is_chained = False
            if isinstance(node.func.value, ast.Call):
                # The value is itself a method call, so convert it first
                obj_expr = self._convert_method_call(node.func.value)
                is_chained = True
            elif isinstance(node.func.value, ast.Name):
                obj_expr = self._to_ocaml_var_name(node.func.value.id)
            elif isinstance(node.func.value, ast.Attribute):
                # Attribute access like obj.attr.method()
                obj_expr = self._convert_expression(node.func.value)
            else:
                raise UnsupportedFeatureError(f"Unsupported method call pattern: {type(node.func.value).__name__}")

            # Wrap in parentheses if it's a chained call (complex expression)
            if is_chained:
                obj_expr = f"({obj_expr})"

            # Handle string methods
            if method_name in ["upper", "lower", "strip", "find", "replace", "split"]:
                return self._convert_string_method(obj_expr, method_name, node.args)
            # Handle dict methods
            elif method_name == "items":
                # For association lists, items() is just the identity function
                return obj_expr
            elif method_name == "keys":
                # Extract keys from association list
                return f"List.map fst {obj_expr}"
            elif method_name == "values":
                # Extract values from association list
                return f"List.map snd {obj_expr}"
            # Handle list/array methods
            elif method_name == "append":
                # Append to array (mutation, so this should be a statement)
                args = [self._convert_expression(arg) for arg in node.args]
                if args:
                    return f"array_append {obj_expr} {args[0]}"
                else:
                    raise UnsupportedFeatureError("append() requires an argument")
            else:
                # Object method call
                args = [self._convert_expression(arg) for arg in node.args]
                if args:
                    return f"{obj_expr}_{method_name} {obj_expr} {' '.join(args)}"
                else:
                    return f"{obj_expr}_{method_name} {obj_expr}"
        else:
            raise UnsupportedFeatureError("Complex method calls not supported")

    def _convert_string_method(self, obj_name: str, method_name: str, args: list[ast.expr]) -> str:
        """Convert string method calls."""
        if method_name == "upper":
            return f"upper {obj_name}"
        elif method_name == "lower":
            return f"lower {obj_name}"
        elif method_name == "strip":
            return f"strip {obj_name}"
        elif method_name == "find":
            if args:
                substr = self._convert_expression(args[0])
                return f"find {obj_name} {substr}"
            else:
                raise UnsupportedFeatureError("find() requires an argument")
        elif method_name == "replace":
            if len(args) >= 2:
                old_str = self._convert_expression(args[0])
                new_str = self._convert_expression(args[1])
                return f"replace {obj_name} {old_str} {new_str}"
            else:
                raise UnsupportedFeatureError("replace() requires two arguments")
        elif method_name == "split":
            if args:
                delimiter = self._convert_expression(args[0])
                return f"split {obj_name} {delimiter}"
            else:
                return f'split {obj_name} " "'  # Default to space
        else:
            raise UnsupportedFeatureError(f"Unsupported string method: {method_name}")

    def _convert_attribute_access(self, node: ast.Attribute) -> str:
        """Convert attribute access to OCaml field access."""
        if isinstance(node.value, ast.Name):
            obj_name = self._to_ocaml_var_name(node.value.id)
            attr_name = self._to_ocaml_var_name(node.attr)
            return f"{obj_name}.{attr_name}"
        else:
            raise UnsupportedFeatureError("Complex attribute access not supported")

    def _convert_list_literal(self, node: ast.List) -> str:
        """Convert Python list literal to OCaml array.

        Note: We use arrays instead of lists because Python lists are mutable
        and support subscript assignment, which OCaml lists don't support.
        """
        elements = [self._convert_expression(elt) for elt in node.elts]
        return "[|" + "; ".join(elements) + "|]"

    def _convert_dict_literal(self, node: ast.Dict) -> str:
        """Convert Python dict literal to OCaml association list."""
        pairs = []
        for key, value in zip(node.keys, node.values):
            if key is None:
                raise UnsupportedFeatureError("Dictionary unpacking (**) not supported")
            key_expr = self._convert_expression(key)
            value_expr = self._convert_expression(value)
            pairs.append(f"({key_expr}, {value_expr})")
        return "[" + "; ".join(pairs) + "]"

    def _convert_set_literal(self, node: ast.Set) -> str:
        """Convert Python set literal to OCaml list (sets represented as lists)."""
        elements = [self._convert_expression(elt) for elt in node.elts]
        return "[" + "; ".join(elements) + "]"

    def _convert_list_comprehension(self, node: ast.ListComp) -> str:
        """Convert Python list comprehension to OCaml."""
        expr = self._convert_expression(node.elt)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]
        target = self._to_ocaml_var_name(gen.target.id) if isinstance(gen.target, ast.Name) else "x"
        iterable = self._convert_expression(gen.iter)

        # Wrap iterable in parentheses if it contains spaces (function calls)
        if " " in iterable and not iterable.startswith("("):
            iterable = f"({iterable})"

        if gen.ifs:
            conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
            condition = " && ".join(conditions)
            return f"list_comprehension_with_filter {iterable} (fun {target} -> {condition}) (fun {target} -> {expr})"
        else:
            return f"list_comprehension {iterable} (fun {target} -> {expr})"

    def _convert_dict_comprehension(self, node: ast.DictComp) -> str:
        """Convert Python dict comprehension to OCaml."""
        key_expr = self._convert_expression(node.key)
        value_expr = self._convert_expression(node.value)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]

        # Handle tuple unpacking: for k, v in dict.items()
        if isinstance(gen.target, ast.Tuple):
            # For dict comprehensions with unpacking, we expect (k, v) pattern
            if len(gen.target.elts) == 2:
                key_var = (
                    self._to_ocaml_var_name(gen.target.elts[0].id) if isinstance(gen.target.elts[0], ast.Name) else "k"
                )
                value_var = (
                    self._to_ocaml_var_name(gen.target.elts[1].id) if isinstance(gen.target.elts[1], ast.Name) else "v"
                )
                target = f"({key_var}, {value_var})"
            else:
                raise UnsupportedFeatureError("Dict comprehension with tuple unpacking requires exactly 2 elements")
        elif isinstance(gen.target, ast.Name):
            target = self._to_ocaml_var_name(gen.target.id)
        else:
            target = "x"

        iterable = self._convert_expression(gen.iter)

        # Wrap iterable in parentheses if it contains spaces (function calls)
        if " " in iterable and not iterable.startswith("("):
            iterable = f"({iterable})"

        if gen.ifs:
            conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
            condition = " && ".join(conditions)
            return f"dict_comprehension_with_filter {iterable} (fun {target} -> {condition}) (fun {target} -> {key_expr}) (fun {target} -> {value_expr})"
        else:
            return f"dict_comprehension {iterable} (fun {target} -> {key_expr}) (fun {target} -> {value_expr})"

    def _convert_set_comprehension(self, node: ast.SetComp) -> str:
        """Convert Python set comprehension to OCaml."""
        expr = self._convert_expression(node.elt)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]
        target = self._to_ocaml_var_name(gen.target.id) if isinstance(gen.target, ast.Name) else "x"
        iterable = self._convert_expression(gen.iter)

        # Wrap iterable in parentheses if it contains spaces (function calls)
        if " " in iterable and not iterable.startswith("("):
            iterable = f"({iterable})"

        if gen.ifs:
            conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
            condition = " && ".join(conditions)
            return f"set_comprehension_with_filter {iterable} (fun {target} -> {condition}) (fun {target} -> {expr})"
        else:
            return f"set_comprehension {iterable} (fun {target} -> {expr})"

    def _convert_ternary_expression(self, node: ast.IfExp) -> str:
        """Convert Python ternary expression to OCaml if-then-else."""
        test = self._convert_expression(node.test)
        body = self._convert_expression(node.body)
        orelse = self._convert_expression(node.orelse)
        return f"(if {test} then {body} else {orelse})"

    def _convert_assignment(self, node: ast.Assign) -> str:
        """Convert Python assignment to OCaml let binding."""
        if len(node.targets) != 1:
            raise UnsupportedFeatureError("Multiple assignment targets not supported")

        target = node.targets[0]
        value = self._convert_expression(node.value)

        if isinstance(target, ast.Name):
            var_name = self._to_ocaml_var_name(target.id)
            # Check if this is a mutable variable (ref) - use := instead of let
            if target.id in self.mutable_vars:
                return f"{var_name} := {value}"
            else:
                return f"let {var_name} = {value} in"
        elif isinstance(target, ast.Subscript):
            # Handle subscript assignment: container[index] = value
            index = self._convert_expression(target.slice)

            # Get base variable name (without dereferencing for refs)
            base_var_name = None
            if isinstance(target.value, ast.Name):
                base_var_name = self._to_ocaml_var_name(target.value.id)

            # Determine if this is dict or array assignment
            is_dict = False

            # Check if the base variable has a tracked dict type
            if base_var_name:
                var_type = self.variables.get(base_var_name, "")
                if "dict" in var_type or "string * " in var_type:
                    is_dict = True

            # Also check if the slice is a string constant
            if isinstance(target.slice, ast.Constant) and isinstance(target.slice.value, str):
                is_dict = True

            # Check if the base variable is a ref (mutable)
            is_ref = isinstance(target.value, ast.Name) and target.value.id in self.mutable_vars

            if is_dict:
                # Dictionary assignment
                if is_ref:
                    # Use ref assignment for mutable dicts
                    return f"{base_var_name} := update_assoc_list !{base_var_name} {index} {value}"
                else:
                    container = self._convert_expression(target.value)
                    return f"let {container} = update_assoc_list {container} {index} {value} in"
            else:
                # Array assignment
                if is_ref:
                    # For array refs, just modify the element (no need to reassign the ref)
                    return f"let _ = !{base_var_name}.({index}) <- {value} in"
                else:
                    container = self._convert_expression(target.value)
                    return f"let _ = {container}.({index}) <- {value} in"
        else:
            raise UnsupportedFeatureError("Complex assignment targets not supported")

    def _convert_annotated_assignment(self, node: ast.AnnAssign) -> str:
        """Convert Python annotated assignment to OCaml let binding.

        Type annotations are tracked for type-aware code generation.
        """
        target = node.target

        if not node.value:
            # Annotation without value - not supported in OCaml
            raise UnsupportedFeatureError("Annotated assignment without value not supported")

        value = self._convert_expression(node.value)

        if isinstance(target, ast.Name):
            var_name = self._to_ocaml_var_name(target.id)

            # Track variable type for type-aware code generation
            # Store the Python type name (not OCaml type) for accurate type checking
            if node.annotation:
                if isinstance(node.annotation, ast.Name):
                    # Store the original Python type name
                    self.variables[var_name] = node.annotation.id
                else:
                    # For complex annotations, use the converted OCaml type
                    type_name = self._get_type_annotation(node.annotation)
                    self.variables[var_name] = type_name

            # Check if this variable is mutable (will be mutated later)
            if target.id in self.mutable_vars:
                return f"let {var_name} = ref ({value}) in"
            else:
                return f"let {var_name} = {value} in"
        elif isinstance(target, ast.Subscript):
            # Handle subscript assignment: container[index] = value
            index = self._convert_expression(target.slice)

            # Get base variable name (without dereferencing for refs)
            base_var_name = None
            if isinstance(target.value, ast.Name):
                base_var_name = self._to_ocaml_var_name(target.value.id)

            # Determine if this is dict or array assignment
            is_dict = False

            # Check if the base variable has a tracked dict type
            if base_var_name:
                var_type = self.variables.get(base_var_name, "")
                if "dict" in var_type or "string * " in var_type:
                    is_dict = True

            # Also check if the slice is a string constant
            if isinstance(target.slice, ast.Constant) and isinstance(target.slice.value, str):
                is_dict = True

            # Check if the base variable is a ref (mutable)
            is_ref = isinstance(target.value, ast.Name) and target.value.id in self.mutable_vars

            if is_dict:
                # Dictionary assignment
                if is_ref:
                    # Use ref assignment for mutable dicts
                    return f"{base_var_name} := update_assoc_list !{base_var_name} {index} {value}"
                else:
                    container = self._convert_expression(target.value)
                    return f"let {container} = update_assoc_list {container} {index} {value} in"
            else:
                # Array assignment
                if is_ref:
                    # For array refs, just modify the element (no need to reassign the ref)
                    return f"let _ = !{base_var_name}.({index}) <- {value} in"
                else:
                    container = self._convert_expression(target.value)
                    return f"let _ = {container}.({index}) <- {value} in"
        else:
            raise UnsupportedFeatureError("Complex assignment targets not supported")

    def _convert_operator(self, op_node: ast.operator) -> str:
        """Convert AST operator to OCaml operator string."""
        if isinstance(op_node, ast.FloorDiv):
            return "/"  # OCaml doesn't have floor division
        elif isinstance(op_node, ast.Mod):
            return "mod"
        elif isinstance(op_node, ast.Pow):
            return "**"
        elif isinstance(op_node, ast.BitOr):
            return "lor"
        elif isinstance(op_node, ast.BitXor):
            return "lxor"
        elif isinstance(op_node, ast.BitAnd):
            return "land"
        elif isinstance(op_node, ast.LShift):
            return "lsl"
        elif isinstance(op_node, ast.RShift):
            return "lsr"
        else:
            # Use standard operator mapping from converter_utils for common operators
            op_result = get_standard_binary_operator(op_node)
            if op_result is None:
                raise UnsupportedFeatureError(f"Unsupported operator: {type(op_node).__name__}")
            return op_result

    def _convert_augmented_assignment(self, node: ast.AugAssign) -> str:
        """Convert Python augmented assignment to OCaml."""
        if isinstance(node.target, ast.Name):
            var_name = self._to_ocaml_var_name(node.target.id)
            value = self._convert_expression(node.value)
            op = self._convert_operator(node.op)

            # If this is a mutable variable (ref), use := for assignment
            if node.target.id in self.mutable_vars:
                return f"{var_name} := (!{var_name}) {op} ({value})"
            else:
                return f"let {var_name} = {var_name} {op} {value} in"
        else:
            target = self._convert_expression(node.target)
            value = self._convert_expression(node.value)
            op = self._convert_operator(node.op)
            return f"let {target} = {target} {op} {value} in"

    def _convert_expression_statement(self, node: ast.Expr) -> str:
        """Convert expression statement."""
        # Ignore docstrings (string constants)
        if isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
            return ""  # Ignore docstrings

        # Special handling for .append() method calls - treat as assignment
        if (
            isinstance(node.value, ast.Call)
            and isinstance(node.value.func, ast.Attribute)
            and node.value.func.attr == "append"
            and isinstance(node.value.func.value, ast.Name)
        ):
            var_name = self._to_ocaml_var_name(node.value.func.value.id)
            args = [self._convert_expression(arg) for arg in node.value.args]
            if args:
                # If this is a mutable variable (ref), use := assignment
                if node.value.func.value.id in self.mutable_vars:
                    return f"{var_name} := array_append !{var_name} {args[0]}"
                else:
                    # data.append(x) -> let data = array_append data x in
                    return f"let {var_name} = array_append {var_name} {args[0]} in"
            else:
                raise UnsupportedFeatureError("append() requires an argument")

        expr = self._convert_expression(node.value)
        return f"let _ = {expr} in"

    def _convert_return(self, node: ast.Return) -> str:
        """Convert return statement."""
        if node.value:
            return self._convert_expression(node.value)
        else:
            return "()"

    def _convert_if_statement(self, node: ast.If) -> str:
        """Convert if statement to OCaml match or if expression."""
        condition = self._convert_expression(node.test)

        # Convert then branch
        then_stmts = []
        for stmt in node.body:
            converted = self._convert_statement(stmt)
            if converted:
                if isinstance(converted, list):
                    then_stmts.extend(converted)
                else:
                    then_stmts.append(converted)

        # Build proper OCaml statement sequences
        def sequence_statements(stmts: list[str]) -> str:
            """Properly sequence OCaml statements with semicolons where needed."""
            if not stmts:
                return "()"
            if len(stmts) == 1:
                # Single statement - if it ends with 'in', add ()
                if stmts[0].rstrip().endswith(" in"):
                    return stmts[0] + "\n    ()"
                return stmts[0]

            # Add semicolons between statements that don't end with 'in'
            result = []
            for i, stmt in enumerate(stmts):
                if i < len(stmts) - 1:  # Not the last statement
                    # If this statement doesn't end with 'in', add semicolon
                    if not stmt.rstrip().endswith(" in"):
                        result.append(stmt + ";")
                    else:
                        result.append(stmt)
                else:
                    # Last statement - if it ends with 'in', add ()
                    if stmt.rstrip().endswith(" in"):
                        result.append(stmt)
                        result.append("()")
                    else:
                        result.append(stmt)

            return "(\n    " + "\n    ".join(result) + "\n  )"

        then_part = sequence_statements(then_stmts)

        # Convert else branch
        if node.orelse:
            else_stmts = []
            for stmt in node.orelse:
                converted = self._convert_statement(stmt)
                if converted:
                    if isinstance(converted, list):
                        else_stmts.extend(converted)
                    else:
                        else_stmts.append(converted)

            else_part = sequence_statements(else_stmts)
        else:
            else_part = "()"

        return f"if {condition} then {then_part} else {else_part}"

    def _convert_while_statement(self, node: ast.While) -> str:
        """Convert while statement (simplified)."""
        condition = self._convert_expression(node.test)
        return f"(* while {condition} do ... done *)"

    def _has_mutations(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect variables that are mutated in a list of statements."""
        mutated = set()
        for stmt in stmts:
            # Augmented assignment: x += 1
            if isinstance(stmt, ast.AugAssign) and isinstance(stmt.target, ast.Name):
                mutated.add(stmt.target.id)
            # Subscript assignment: arr[i] = value
            elif isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
                        mutated.add(target.value.id)
            # Append method call: data.append(x)
            elif isinstance(stmt, ast.Expr):
                if isinstance(stmt.value, ast.Call) and isinstance(stmt.value.func, ast.Attribute):
                    if stmt.value.func.attr == "append" and isinstance(stmt.value.func.value, ast.Name):
                        mutated.add(stmt.value.func.value.id)
            # Recurse into if statements
            elif isinstance(stmt, ast.If):
                mutated.update(self._has_mutations(stmt.body))
                mutated.update(self._has_mutations(stmt.orelse))
        return mutated

    @property
    def loop_converter(self) -> "ForLoopConverter":
        """Lazily initialize and return the loop converter."""
        if self._loop_converter is None:
            from .loop_strategies import create_ocaml_loop_converter

            self._loop_converter = create_ocaml_loop_converter()
        return self._loop_converter

    def _convert_for_statement(self, node: ast.For) -> list[str]:
        """Convert Python for loop to OCaml using Strategy pattern.

        Before refactoring: ~84 lines, complexity ~15-20
        After refactoring: ~10 lines for delegation, fallback for edge cases

        Uses loop conversion strategies for common patterns:
        - Simple assignment (no mutations)
        - Accumulation (single mutation with augmented assignment)
        - General loop (List.iter for complex cases)
        """
        # Try strategy-based conversion first
        context = LoopContext(converter=self)
        result = self.loop_converter.convert(node, context)
        if result is not None:
            return [result]

        # Fallback for edge cases not covered by strategies
        return self._convert_for_statement_fallback(node)

    def _convert_for_statement_fallback(self, node: ast.For) -> list[str]:
        """Fallback for-loop conversion for edge cases not yet in strategies.

        This method handles complex patterns that don't fit the standard strategies.
        Since OCamlGeneralLoopStrategy is a catch-all, this should rarely be called.
        """
        # Complex for loop targets not supported
        if not isinstance(node.target, ast.Name):
            return ["(* Complex for loop target not supported *)"]

        # Empty loops
        if not node.body:
            return ["(* Empty for loop *)"]

        # This should rarely execute since OCamlGeneralLoopStrategy handles most cases
        return ["(* Unsupported for loop pattern *)"]

    def _to_ocaml_var_name(self, name: str) -> str:
        """Convert Python variable name to OCaml style."""
        # Handle naming convention preferences
        if self.preferences and self.preferences.get("naming_convention") == "camelCase":
            # Convert snake_case to camelCase
            components = name.split("_")
            if len(components) > 1:
                return components[0] + "".join(word.capitalize() for word in components[1:])

        # Default: keep snake_case but ensure it's valid OCaml
        ocaml_name = name.replace("__", "_").lower()

        # Handle OCaml keywords
        keywords = {
            "and",
            "as",
            "assert",
            "begin",
            "class",
            "constraint",
            "do",
            "done",
            "downto",
            "else",
            "end",
            "exception",
            "external",
            "false",
            "for",
            "fun",
            "function",
            "functor",
            "if",
            "in",
            "include",
            "inherit",
            "initializer",
            "lazy",
            "let",
            "match",
            "method",
            "module",
            "mutable",
            "new",
            "object",
            "of",
            "open",
            "or",
            "private",
            "rec",
            "sig",
            "struct",
            "then",
            "to",
            "true",
            "try",
            "type",
            "val",
            "virtual",
            "when",
            "while",
            "with",
        }

        if ocaml_name in keywords:
            return f"{ocaml_name}_"

        return ocaml_name

    def _to_ocaml_type_name(self, name: str) -> str:
        """Convert Python class name to OCaml type name."""
        # Capitalize first letter for OCaml type names
        return name[0].upper() + name[1:] if name else name

    def _get_type_annotation(self, annotation: ast.AST) -> str:
        """Convert Python type annotation to OCaml type."""
        if isinstance(annotation, ast.Name):
            return self.type_map.get(annotation.id, annotation.id.lower())
        elif isinstance(annotation, ast.Constant) and annotation.value is None:
            return "unit"
        elif isinstance(annotation, ast.Subscript):
            # Handle subscripted types like list[int], list[list[int]], dict[str, int]
            if isinstance(annotation.value, ast.Name):
                container_type = annotation.value.id
                if container_type == "list":
                    # list[int] -> int list, list[list[int]] -> int list list
                    if isinstance(annotation.slice, ast.Name):
                        element_type = self.type_map.get(annotation.slice.id, annotation.slice.id.lower())
                        return f"{element_type} list"
                    elif isinstance(annotation.slice, ast.Subscript):
                        # Recursively handle nested lists like list[list[int]]
                        element_type = self._get_type_annotation(annotation.slice)
                        return f"{element_type} list"
                    return "'a list"  # Default to 'a list
                elif container_type == "dict":
                    # dict[str, int] -> (string * int) list
                    if isinstance(annotation.slice, ast.Tuple) and len(annotation.slice.elts) == 2:
                        key_type = self._get_type_annotation(annotation.slice.elts[0])
                        value_type = self._get_type_annotation(annotation.slice.elts[1])
                        return f"({key_type} * {value_type}) list"
                    return "('a * 'b) list"  # Default
                elif container_type == "set":
                    # set[int] -> int list (OCaml doesn't have built-in sets in basic list operations)
                    if isinstance(annotation.slice, ast.Name):
                        element_type = self.type_map.get(annotation.slice.id, annotation.slice.id.lower())
                        return f"{element_type} list"
                    return "'a list"  # Default
            return "'a"  # Fallback
        else:
            # Complex type annotations - simplified
            return "'a"

    def _infer_type_from_value(self, value: ast.AST) -> str:
        """Infer OCaml type from Python value."""
        if isinstance(value, ast.Constant):
            if isinstance(value.value, bool):
                return "bool"
            elif isinstance(value.value, int):
                return "int"
            elif isinstance(value.value, float):
                return "float"
            elif isinstance(value.value, str):
                return "string"
            elif value.value is None:
                return "unit"
        elif isinstance(value, ast.List):
            return "'a list"
        elif isinstance(value, ast.Dict):
            return "(string * 'a) list"

        return "'a"

    def _get_default_value(self, type_name: str) -> str:
        """Get default value for a type."""
        defaults = {
            "int": "0",
            "float": "0.0",
            "bool": "false",
            "string": '""',
            "unit": "()",
            "'a list": "[]",
            "(string * 'a) list": "[]",
        }
        return defaults.get(type_name, 'failwith "default value not implemented"')

    def _convert_subscript(self, node: ast.Subscript) -> str:
        """Convert subscript access (e.g., list[0], dict[key])."""
        value = self._convert_expression(node.value)
        slice_expr = self._convert_expression(node.slice)

        # Determine if this is dict or array access
        is_dict = False

        # Check if the base variable has a tracked dict type
        if isinstance(node.value, ast.Name):
            var_name = self._to_ocaml_var_name(node.value.id)
            var_type = self.variables.get(var_name, "")
            if "dict" in var_type or "string * " in var_type:
                is_dict = True

        # Also check if the slice is a string constant (clear indicator of dict)
        if isinstance(node.slice, ast.Constant) and isinstance(node.slice.value, str):
            is_dict = True

        if is_dict:
            # Dictionary access - use List.assoc for association lists
            return f"List.assoc {slice_expr} {value}"
        else:
            # Array access - use array indexing notation
            return f"{value}.({slice_expr})"

    def _convert_f_string(self, node: ast.JoinedStr) -> str:
        """Convert f-string to OCaml string concatenation.

        Example:
            f"Result: {x}" -> "Result: " ^ string_of_int x
            f"Count: {len(items)} items" -> "Count: " ^ string_of_int (List.length items) ^ " items"
        """
        parts: list[str] = []

        for value in node.values:
            if isinstance(value, ast.Constant):
                # Literal string part - escape properly
                if isinstance(value.value, str):
                    escaped = value.value.replace("\\", "\\\\").replace('"', '\\"')
                    parts.append(f'"{escaped}"')
            elif isinstance(value, ast.FormattedValue):
                # Expression to be converted to string
                expr_code = self._convert_expression(value.value)
                # Determine the type conversion function needed
                parts.append(self._to_string_ocaml(expr_code, value.value))

        if len(parts) == 0:
            return '""'
        elif len(parts) == 1:
            return parts[0]
        else:
            return "(" + " ^ ".join(parts) + ")"

    def _to_string_ocaml(self, expr_code: str, node: ast.expr) -> str:
        """Convert an expression to string in OCaml.

        Args:
            expr_code: The OCaml code for the expression
            node: The original AST node for type inference

        Returns:
            OCaml code that converts the expression to a string
        """
        # String literals
        if isinstance(node, ast.Constant):
            if isinstance(node.value, str):
                return expr_code  # Already a string
            elif isinstance(node.value, bool):
                return f'(string_of_bool {expr_code})'
            elif isinstance(node.value, int):
                return f'(string_of_int {expr_code})'
            elif isinstance(node.value, float):
                return f'(string_of_float {expr_code})'

        # Variable names - heuristic based on name
        if isinstance(node, ast.Name):
            var_name = node.id.lower()
            # Common string variable names
            if any(substr in var_name for substr in ["name", "text", "str", "msg", "message", "path", "file"]):
                return expr_code  # Assume it's a string
            # Otherwise, assume int (common default)
            return f'(string_of_int {expr_code})'

        # String method calls return strings
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
            method_name = node.func.attr
            if method_name in {"lower", "upper", "strip", "replace"}:
                return expr_code  # Already returns string

        # Default to string_of_int (most common case)
        return f'(string_of_int {expr_code})'
