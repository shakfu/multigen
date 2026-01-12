"""Enhanced Haskell code emitter for MultiGen with comprehensive Python language support."""

import ast
from typing import TYPE_CHECKING, Any, Optional

from ..converter_utils import (
    get_standard_binary_operator,
    get_standard_comparison_operator,
)
from ..errors import TypeMappingError, UnsupportedFeatureError
from ..loop_conversion_strategies import LoopContext
from ..preferences import BackendPreferences
from .function_converter import convert_function_with_visitor

if TYPE_CHECKING:
    from ..loop_conversion_strategies import ForLoopConverter


class MultiGenPythonToHaskellConverter:
    """Sophisticated Python-to-Haskell converter with comprehensive language support."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the converter with optional preferences."""
        self.preferences = preferences
        self.type_map = {
            "int": "Int",
            "float": "Double",
            "bool": "Bool",
            "str": "String",
            "list": "[a]",  # Generic list type
            "dict": "Dict String a",  # Dictionary with String keys
            "set": "Set a",  # Generic set type
            "void": "()",
            "None": "()",
            "NoneType": "()",
        }
        self.data_types: dict[str, Any] = {}  # Track data type definitions for classes
        self.current_function: Optional[str] = None  # Track current function context
        self.declared_vars: set[str] = set()  # Track declared variables in current function
        self.needed_imports: set[str] = set()  # Track which imports are needed
        self._loop_converter: Optional[ForLoopConverter] = None  # Lazy-initialized loop converter

    @property
    def loop_converter(self) -> "ForLoopConverter":
        """Lazily initialize and return the loop converter."""
        if self._loop_converter is None:
            from .loop_strategies import create_haskell_loop_converter

            self._loop_converter = create_haskell_loop_converter()
        return self._loop_converter

    def _to_camel_case(self, snake_str: str) -> str:
        """Convert snake_case to CamelCase."""
        # If it's already CamelCase, keep it as is
        if snake_str[0].isupper():
            return snake_str
        components = snake_str.split("_")
        return "".join(word.capitalize() for word in components)

    def _to_haskell_function_name(self, function_name: str) -> str:
        """Convert Python function name to Haskell function name (camelCase)."""
        if function_name == "main":
            return "main"

        # Check for Haskell reserved keywords and built-in functions to avoid shadowing
        haskell_keywords = {
            # Reserved keywords
            "case",
            "class",
            "data",
            "default",
            "deriving",
            "do",
            "else",
            "foreign",
            "if",
            "import",
            "in",
            "infix",
            "infixl",
            "infixr",
            "instance",
            "let",
            "module",
            "newtype",
            "of",
            "then",
            "type",
            "where",
            "as",
            "qualified",
            "hiding",
            # Built-in functions to avoid shadowing
            "words",  # String splitting function used by text.split()
            "lines",  # String splitting by newlines
            "unwords",  # Join words with spaces
            "unlines",  # Join lines with newlines
        }

        components = function_name.split("_")
        if len(components) == 1:
            base_name = function_name
        else:
            base_name = components[0] + "".join(word.capitalize() for word in components[1:])

        # If it's a reserved keyword, append an underscore
        if base_name.lower() in haskell_keywords:
            return base_name + "_"
        return base_name

    def _to_haskell_var_name(self, var_name: str) -> str:
        """Convert Python variable name to Haskell variable name (camelCase)."""
        return self._to_haskell_function_name(var_name)

    def convert_code(self, python_code: str) -> str:
        """Convert Python code to Haskell."""
        try:
            tree = ast.parse(python_code)
            return self._convert_module(tree)
        except UnsupportedFeatureError:
            # Re-raise UnsupportedFeatureError without wrapping
            raise
        except Exception as e:
            raise TypeMappingError(f"Failed to convert Python code: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to Haskell."""
        parts = []

        # Reset needed imports for this module
        self.needed_imports = set()

        # Scan for needed imports
        self._scan_for_imports(node)

        # Add language extensions if needed
        extensions = []
        if any(isinstance(item, ast.ClassDef) for item in node.body):
            extensions.append("{-# LANGUAGE OverloadedStrings #-}")
        if self.needed_imports:
            extensions.append("{-# LANGUAGE FlexibleInstances #-}")

        if extensions:
            parts.extend(extensions)
            parts.append("")

        # Add module declaration
        parts.append("module Main where")
        parts.append("")

        # Add imports
        base_imports = [
            "import MultiGenRuntime",
            "import Control.Monad (foldM)",
            "import qualified Data.Map as Map",
            "import qualified Data.Set as Set",
            "import Data.Map (Map)",
            "import Data.Set (Set)",
        ]

        for imp in base_imports:
            parts.append(imp)
        parts.append("")

        # Convert classes first (they become data type definitions)
        for item in node.body:
            if isinstance(item, ast.ClassDef):
                data_def = self._convert_class(item)
                parts.append(data_def)
                parts.append("")

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
            main_func = '''main :: IO ()
main = printValue "Generated Haskell code executed successfully"'''
            parts.append(main_func)

        return "\n".join(parts)

    def _scan_for_imports(self, node: ast.AST) -> None:
        """Scan AST for features that require imports."""
        for child in ast.walk(node):
            if isinstance(child, ast.Call):
                if isinstance(child.func, ast.Attribute):
                    # String methods, dict methods, etc.
                    self.needed_imports.add("string_ops")
                elif isinstance(child.func, ast.Name):
                    if child.func.id in ["len", "abs", "min", "max", "sum"]:
                        self.needed_imports.add("builtins")
            elif isinstance(child, (ast.ListComp, ast.DictComp, ast.SetComp)):
                self.needed_imports.add("comprehensions")
            elif isinstance(child, ast.AugAssign):
                self.needed_imports.add("augassign")

    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert Python class to Haskell data type with functions."""
        class_name = self._to_camel_case(node.name)

        # Extract fields from __init__ method
        fields = []
        constructor = None
        methods = []

        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "__init__":
                    constructor = item
                    # Extract field assignments from constructor
                    for stmt in item.body:
                        if isinstance(stmt, ast.Assign):
                            for target in stmt.targets:
                                if (
                                    isinstance(target, ast.Attribute)
                                    and isinstance(target.value, ast.Name)
                                    and target.value.id == "self"
                                ):
                                    field_name = self._to_haskell_var_name(target.attr)
                                    field_type = self._infer_type_from_node(stmt.value)
                                    fields.append(f"    {field_name} :: {field_type}")
                        elif isinstance(stmt, ast.AnnAssign):
                            if (
                                isinstance(stmt.target, ast.Attribute)
                                and isinstance(stmt.target.value, ast.Name)
                                and stmt.target.value.id == "self"
                            ):
                                field_name = self._to_haskell_var_name(stmt.target.attr)
                                field_type = self._convert_type_annotation(stmt.annotation)
                                fields.append(f"    {field_name} :: {field_type}")
                else:
                    methods.append(item)

        # Handle empty class
        if not fields:
            fields = ["    dummy :: ()"]

        # Store class info for method generation
        self.data_types[class_name] = {
            "fields": [field.strip().split(" :: ")[0] for field in fields],
            "constructor": constructor,
            "methods": methods,
        }

        # Generate data type definition
        parts = []
        parts.append(f"data {class_name} = {class_name}")
        parts.append("  { " + "\n  , ".join(field.strip() for field in fields))
        parts.append("  } deriving (Show, Eq)")

        # Generate constructor function
        if constructor:
            constructor_func = self._convert_constructor(class_name, constructor)
            parts.append("")
            parts.append(constructor_func)

        # Generate methods
        for method in methods:
            method_func = self._convert_method(class_name, method)
            parts.append("")
            parts.append(method_func)

        return "\n".join(parts)

    def _convert_constructor(self, class_name: str, constructor: ast.FunctionDef) -> str:
        """Convert Python __init__ method to Haskell constructor function."""
        func_name = f"new{class_name}"

        # Extract parameters (skip self)
        params = []
        for arg in constructor.args.args[1:]:  # Skip 'self'
            param_name = self._to_haskell_var_name(arg.arg)
            param_type = "a"  # Default generic type
            if arg.annotation:
                param_type = self._convert_type_annotation(arg.annotation)
            params.append(f"{param_name} :: {param_type}")

        param_names = [param.split(" :: ")[0] for param in params]
        param_types = " -> ".join([param.split(" :: ")[1] for param in params]) if params else ""

        if param_types:
            signature = f"{func_name} :: {param_types} -> {class_name}"
        else:
            signature = f"{func_name} :: {class_name}"

        # Generate constructor body
        field_assignments = []
        for stmt in constructor.body:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if (
                        isinstance(target, ast.Attribute)
                        and isinstance(target.value, ast.Name)
                        and target.value.id == "self"
                    ):
                        field_name = self._to_haskell_var_name(target.attr)
                        value = self._convert_expression(stmt.value)
                        field_assignments.append(f"{field_name} = {value}")
            elif isinstance(stmt, ast.AnnAssign):
                if (
                    isinstance(stmt.target, ast.Attribute)
                    and isinstance(stmt.target.value, ast.Name)
                    and stmt.target.value.id == "self"
                ):
                    field_name = self._to_haskell_var_name(stmt.target.attr)
                    if stmt.value:
                        value = self._convert_expression(stmt.value)
                    else:
                        value = "undefined"  # Default value
                    field_assignments.append(f"{field_name} = {value}")

        if not field_assignments:
            field_assignments = ["dummy = ()"]

        if param_names:
            param_pattern = " " + " ".join(param_names)
        else:
            param_pattern = ""
        body = f"{func_name}{param_pattern} = {class_name} {{ {', '.join(field_assignments)} }}"

        return f"{signature}\n{body}"

    def _convert_method(self, class_name: str, method: ast.FunctionDef) -> str:
        """Convert Python instance method to Haskell function."""
        method_name = self._to_haskell_function_name(method.name)

        # Extract parameters (skip self)
        params = []
        for arg in method.args.args[1:]:  # Skip 'self'
            param_name = self._to_haskell_var_name(arg.arg)
            param_type = "a"  # Default generic type
            if arg.annotation:
                param_type = self._convert_type_annotation(arg.annotation)
            params.append((param_name, param_type))

        # Determine return type
        return_type = "()"
        if method.returns:
            return_type = self._convert_type_annotation(method.returns)

        # Build function signature
        param_types = [param[1] for param in params]
        all_types = [class_name] + param_types + [return_type]
        signature = f"{method_name} :: " + " -> ".join(all_types)

        # Convert function body - for now, return undefined for methods with side effects
        # In Haskell, we can't modify object state directly due to immutability
        if return_type == "()":
            body = "()"  # Void methods return unit
        else:
            body = "undefined"  # Non-void methods need proper implementation

        param_names = ["obj"] + [param[0] for param in params]
        param_pattern = " " + " ".join(param_names)

        implementation = f"{method_name}{param_pattern} = {body}"

        return f"{signature}\n{implementation}"

    def _mutates_array_parameter(self, node: ast.FunctionDef) -> tuple[bool, set[str]]:
        """Detect if function mutates array parameters via subscript assignment.
        Returns (is_mutating, set_of_mutated_param_names).
        """
        param_names = {arg.arg for arg in node.args.args}
        mutated_params = set()

        for stmt in ast.walk(node):
            # Look for arr[i] = value where arr is a parameter
            if isinstance(stmt, ast.Assign):
                if len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Subscript):
                    # Check if the subscripted object is a parameter
                    if isinstance(stmt.targets[0].value, ast.Name):
                        var_name = stmt.targets[0].value.id
                        if var_name in param_names:
                            mutated_params.add(var_name)

        return len(mutated_params) > 0, mutated_params

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert Python function to Haskell using visitor pattern.

        Delegates to convert_function_with_visitor for cleaner, more maintainable code.
        """
        return convert_function_with_visitor(self, node)

    def _convert_statement(self, node: ast.stmt) -> str:
        """Convert Python statement to Haskell."""
        if isinstance(node, ast.Return):
            if node.value:
                return self._convert_expression(node.value)
            else:
                return "()"

        elif isinstance(node, ast.Assign):
            if len(node.targets) == 1:
                target = node.targets[0]
                if isinstance(target, ast.Name):
                    var_name = self._to_haskell_var_name(target.id)
                    value = self._convert_expression(node.value)
                    return f"{var_name} = {value}"
                else:
                    return "-- Complex assignment target"
            else:
                return "-- Multiple assignment targets"

        elif isinstance(node, ast.AnnAssign):
            if isinstance(node.target, ast.Name):
                var_name = self._to_haskell_var_name(node.target.id)
                if node.value:
                    value = self._convert_expression(node.value)
                    return f"{var_name} = {value}"
                else:
                    return f"{var_name} = undefined"  # Type annotation without value
            else:
                return "-- Complex annotated assignment"

        elif isinstance(node, ast.Expr):
            # Check if this is a mutation that needs to be converted to reassignment
            if isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Attribute):
                method_name = node.value.func.attr

                # Handle list.append() - convert to reassignment
                if method_name == "append" and isinstance(node.value.func.value, ast.Name):
                    list_var = self._to_haskell_var_name(node.value.func.value.id)
                    if node.value.args:
                        append_expr = self._convert_expression(node.value.args[0])
                        # list.append(x) -> list = list ++ [x]
                        return f"{list_var} = {list_var} ++ [{append_expr}]"

            # Regular expression statements
            expr = self._convert_expression(node.value)
            return expr

        elif isinstance(node, ast.AugAssign):
            return self._convert_augmented_assignment(node)

        elif isinstance(node, ast.If):
            return self._convert_if_statement(node)

        elif isinstance(node, ast.While):
            return self._convert_while_statement(node)

        elif isinstance(node, ast.For):
            return self._convert_for_statement(node)

        elif isinstance(node, ast.Assert):
            return self._convert_assert_statement(node)

        else:
            raise UnsupportedFeatureError(f"Unsupported statement type: {type(node).__name__}")

    def _convert_assert_statement(self, node: ast.Assert) -> str:
        """Convert Python assert statement to Haskell error on failure.

        Args:
            node: Python assert statement node

        Returns:
            Haskell if/then/else expression that errors on assertion failure

        Example:
            assert x > 0  →  if not (x > 0) then error "assertion failed" else ()
            assert result == 1, "Test failed"  →  if not (result == 1) then error "Test failed" else ()
            In IO context (main): uses 'return ()' instead of '()'
        """
        # Convert the test expression
        test_expr = self._convert_expression(node.test)

        # Determine the success value based on context
        # In IO context (main function), use 'return ()', otherwise use '()'
        success_value = "return ()" if self.current_function == "main" else "()"

        # Handle optional message
        if node.msg:
            # Convert message to string
            if isinstance(node.msg, ast.Constant) and isinstance(node.msg.value, str):
                msg = node.msg.value
                return f'if not ({test_expr}) then error "{msg}" else {success_value}'
            else:
                # Complex message expression - just add default error
                return f'if not ({test_expr}) then error "assertion failed" else {success_value}'
        else:
            return f'if not ({test_expr}) then error "assertion failed" else {success_value}'

    def _convert_expression(self, node: ast.expr) -> str:
        """Convert Python expression to Haskell."""
        if isinstance(node, ast.Constant):
            return self._convert_constant(node)

        elif isinstance(node, ast.Name):
            return self._to_haskell_var_name(node.id)

        elif isinstance(node, ast.BinOp):
            return self._convert_binary_operation(node)

        elif isinstance(node, ast.UnaryOp):
            return self._convert_unary_operation(node)

        elif isinstance(node, ast.Compare):
            return self._convert_comparison(node)

        elif isinstance(node, ast.BoolOp):
            return self._convert_bool_operation(node)

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
            raise UnsupportedFeatureError(f"Unsupported expression type: {type(node).__name__}")

    def _convert_constant(self, node: ast.Constant) -> str:
        """Convert Python constant to Haskell."""
        if isinstance(node.value, bool):
            return "True" if node.value else "False"
        elif isinstance(node.value, int):
            return str(node.value)
        elif isinstance(node.value, float):
            return str(node.value)
        elif isinstance(node.value, str):
            return f'"{node.value}"'
        elif node.value is None:
            return "()"
        else:
            return str(node.value)

    def _convert_binary_operation(self, node: ast.BinOp) -> str:
        """Convert Python binary operation to Haskell."""
        left = self._convert_expression(node.left)
        right = self._convert_expression(node.right)

        # Handle list concatenation: use ++ instead of + for lists
        if isinstance(node.op, ast.Add):
            # Check if either operand is a list
            left_type = self._infer_type_from_node(node.left)
            right_type = self._infer_type_from_node(node.right)

            if left_type.startswith("[") or right_type.startswith("["):
                return f"({left} ++ {right})"

        # Handle Haskell-specific operators
        if isinstance(node.op, ast.FloorDiv):
            return f"({left} `div` {right})"
        elif isinstance(node.op, ast.Mod):
            return f"({left} `mod` {right})"
        elif isinstance(node.op, ast.Pow):
            return f"({left} ** {right})"
        elif isinstance(node.op, ast.BitOr):
            return f"({left} .|. {right})"
        elif isinstance(node.op, ast.BitXor):
            return f"({left} `xor` {right})"
        elif isinstance(node.op, ast.BitAnd):
            return f"({left} .&. {right})"
        elif isinstance(node.op, ast.LShift):
            return f"({left} `shiftL` {right})"
        elif isinstance(node.op, ast.RShift):
            return f"({left} `shiftR` {right})"

        # Use standard operator mapping from converter_utils for common operators
        op = get_standard_binary_operator(node.op)
        if op is not None:
            return f"({left} {op} {right})"
        else:
            raise UnsupportedFeatureError(f"Unsupported binary operator: {type(node.op).__name__}")

    def _convert_unary_operation(self, node: ast.UnaryOp) -> str:
        """Convert Python unary operation to Haskell."""
        operand = self._convert_expression(node.operand)

        if isinstance(node.op, ast.UAdd):
            return f"(+{operand})"
        elif isinstance(node.op, ast.USub):
            return f"(-{operand})"
        elif isinstance(node.op, ast.Not):
            return f"(not {operand})"
        elif isinstance(node.op, ast.Invert):
            return f"(complement {operand})"
        else:
            raise UnsupportedFeatureError(f"Unsupported unary operator: {type(node.op).__name__}")

    def _convert_comparison(self, node: ast.Compare) -> str:
        """Convert Python comparison to Haskell."""
        left = self._convert_expression(node.left)

        if len(node.ops) == 1 and len(node.comparators) == 1:
            op = node.ops[0]
            right = self._convert_expression(node.comparators[0])

            # Handle Haskell-specific operators
            if isinstance(op, ast.NotEq):
                # Haskell uses /= for inequality
                return f"({left} /= {right})"
            elif isinstance(op, ast.In):
                # Use Map.member for maps (assuming right is a map)
                return f"(Map.member {left} {right})"
            elif isinstance(op, ast.NotIn):
                # Use not . Map.member for maps
                return f"(not (Map.member {left} {right}))"

            # Use standard comparison operator mapping from converter_utils
            haskell_op = get_standard_comparison_operator(op)
            if haskell_op is not None:
                return f"({left} {haskell_op} {right})"
            else:
                raise UnsupportedFeatureError(f"Unsupported comparison operator: {type(op).__name__}")
        else:
            raise UnsupportedFeatureError("Complex comparison chains not supported")

    def _convert_bool_operation(self, node: ast.BoolOp) -> str:
        """Convert Python boolean operation to Haskell."""
        values = [self._convert_expression(value) for value in node.values]

        if isinstance(node.op, ast.And):
            return "(" + " && ".join(values) + ")"
        elif isinstance(node.op, ast.Or):
            return "(" + " || ".join(values) + ")"
        else:
            raise UnsupportedFeatureError(f"Unsupported boolean operator: {type(node.op).__name__}")

    def _convert_function_call(self, node: ast.Call) -> str:
        """Convert Python function call to Haskell."""
        if isinstance(node.func, ast.Name):
            func_name = node.func.id
            args = [self._convert_expression(arg) for arg in node.args]

            # Handle built-in functions
            if func_name == "print":
                if args:
                    return f"printValue {args[0]}"
                else:
                    return 'printValue ""'
            elif func_name == "len":
                if args:
                    return f"len' {args[0]}"
                else:
                    return "0"  # Default len value
            elif func_name == "abs":
                if args:
                    return f"abs' {args[0]}"
                else:
                    return "abs' 0"  # Default abs value
            elif func_name == "min":
                if args:
                    return f"min' {args[0]}"
                else:
                    return "0"  # Default min value
            elif func_name == "max":
                if args:
                    return f"max' {args[0]}"
                else:
                    return "0"  # Default max value
            elif func_name == "sum":
                if args:
                    return f"sum' {args[0]}"
                else:
                    return "0"  # Default sum value
            elif func_name == "any":
                if args:
                    return f"or {args[0]}"
                else:
                    return "False"
            elif func_name == "all":
                if args:
                    return f"and {args[0]}"
                else:
                    return "True"
            elif func_name == "bool":
                if args:
                    return f"bool' {args[0]}"
                else:
                    return "False"  # Default bool value
            elif func_name == "str":
                if args:
                    return f"toString {args[0]}"
                else:
                    return 'toString ""'  # Empty string conversion
            elif func_name == "range":
                if len(args) == 1:
                    return f"rangeList (range {args[0]})"
                elif len(args) == 2:
                    return f"rangeList (range2 {args[0]} {args[1]})"
                elif len(args) == 3:
                    return f"rangeList (range3 {args[0]} {args[1]} {args[2]})"
                else:
                    return "rangeList (range 0)"  # Fallback for invalid range args
            elif func_name == "set":
                # Python's set() creates empty set
                if args:
                    # set(iterable) - convert iterable to set
                    return f"Set.fromList {args[0]}"
                else:
                    # set() - empty set
                    return "Set.empty"
            elif func_name == "dict":
                # Python's dict() creates empty dict
                if args:
                    raise UnsupportedFeatureError("dict(iterable) not yet supported")
                else:
                    # dict() - empty dict
                    return "Map.empty"
            else:
                # Check if it's a class constructor call
                camel_func_name = self._to_camel_case(func_name)
                if camel_func_name in self.data_types:
                    constructor_name = f"new{camel_func_name}"
                    if args:
                        return f"{constructor_name} " + " ".join(args)
                    else:
                        return constructor_name
                else:
                    # Regular function call
                    haskell_func_name = self._to_haskell_function_name(func_name)
                    if args:
                        return f"{haskell_func_name} " + " ".join(args)
                    else:
                        return haskell_func_name

        elif isinstance(node.func, ast.Attribute):
            return self._convert_method_call(node)

        else:
            raise UnsupportedFeatureError("Complex function calls not supported")

    def _convert_method_call(self, node: ast.Call) -> str:
        """Convert Python method call to Haskell function call."""
        if isinstance(node.func, ast.Attribute):
            obj = self._convert_expression(node.func.value)
            method_name = node.func.attr
            args = [self._convert_expression(arg) for arg in node.args]

            # Handle string methods - use qualified names to avoid shadowing
            if method_name == "upper":
                return f"(MultiGenRuntime.upper {obj})"
            elif method_name == "lower":
                return f"(MultiGenRuntime.lower {obj})"
            elif method_name == "strip":
                return f"(MultiGenRuntime.strip {obj})"
            elif method_name == "find":
                if args:
                    return f"(MultiGenRuntime.find {obj} {args[0]})"
            elif method_name == "replace":
                if len(args) >= 2:
                    return f"(MultiGenRuntime.replace {obj} {args[0]} {args[1]})"
            elif method_name == "split":
                if args:
                    return f"(MultiGenRuntime.split {obj} {args[0]})"
                else:
                    # Python's split() with no args splits on whitespace
                    # Haskell's words function does the same
                    return f"(words {obj})"
            # Handle dict methods
            elif method_name == "items":
                return f"(items {obj})"
            elif method_name == "values":
                return f"(values {obj})"
            elif method_name == "keys":
                return f"(keys {obj})"
            else:
                # Regular method call - convert to function call
                haskell_method_name = self._to_haskell_function_name(method_name)
                all_args = [obj] + args
                return f"({haskell_method_name} " + " ".join(all_args) + ")"

        raise UnsupportedFeatureError("Complex method calls not supported")

    def _convert_attribute_access(self, node: ast.Attribute) -> str:
        """Convert Python attribute access to Haskell record access."""
        obj = self._convert_expression(node.value)
        attr_name = self._to_haskell_var_name(node.attr)
        return f"({attr_name} {obj})"

    def _convert_subscript(self, node: ast.Subscript) -> str:
        """Convert Python subscript to Haskell list/map access."""
        obj = self._convert_expression(node.value)

        # Handle slice notation: arr[start:end]
        if isinstance(node.slice, ast.Slice):
            lower = self._convert_expression(node.slice.lower) if node.slice.lower else "0"
            upper = self._convert_expression(node.slice.upper) if node.slice.upper else f"(len' {obj})"

            # arr[1:] -> drop 1 arr
            if node.slice.lower and not node.slice.upper:
                return f"(drop {lower} {obj})"
            # arr[:n] -> take n arr
            elif node.slice.upper and not node.slice.lower:
                return f"(take {upper} {obj})"
            # arr[i:j] -> take (j-i) (drop i arr)
            else:
                return f"(take ({upper} - {lower}) (drop {lower} {obj}))"

        index = self._convert_expression(node.slice)

        # Heuristic: if index is a string literal, it's likely a dict access
        # For maps: obj Map.! index
        # For lists: obj !! index
        if isinstance(node.slice, ast.Constant) and isinstance(node.slice.value, str):
            return f"({obj} Map.! {index})"
        else:
            return f"({obj} !! {index})"

    def _convert_f_string(self, node: ast.JoinedStr) -> str:
        """Convert f-string to Haskell string concatenation with show.

        Example:
            f"Result: {x}" -> "Result: " ++ show x
            f"Count: {len(items)} items" -> "Count: " ++ show (length items) ++ " items"
        """
        parts: list[str] = []

        for value in node.values:
            if isinstance(value, ast.Constant):
                # Literal string part
                if isinstance(value.value, str):
                    parts.append(f'"{value.value}"')
            elif isinstance(value, ast.FormattedValue):
                # Expression to be converted to string
                expr_code = self._convert_expression(value.value)
                # Use show to convert to string, but check if already a string
                if self._is_string_expression(value.value):
                    parts.append(expr_code)
                else:
                    parts.append(f"(show {expr_code})")

        if len(parts) == 0:
            return '""'
        elif len(parts) == 1:
            return parts[0]
        else:
            return "(" + " ++ ".join(parts) + ")"

    def _is_string_expression(self, node: ast.expr) -> bool:
        """Check if an expression already returns a String."""
        # String literals
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return True
        # Variable names - we can't definitively know the type without more context,
        # but we can make a best guess. In practice, the type checker should catch issues.
        # For now, assume variables ending in common string suffixes or with string-like names
        # are strings. This is a heuristic that works for the common case where f-strings
        # are used with string parameters.
        if isinstance(node, ast.Name):
            var_name = node.id.lower()
            # Common string variable names
            if any(substr in var_name for substr in ["name", "text", "str", "msg", "message", "path", "file"]):
                return True
        # String method calls that return strings
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
            method_name = node.func.attr
            if method_name in {"lower", "upper", "strip", "replace"}:
                return True
        return False

    def _convert_list_literal(self, node: ast.List) -> str:
        """Convert Python list literal to Haskell list."""
        elements = [self._convert_expression(elt) for elt in node.elts]
        return "[" + ", ".join(elements) + "]"

    def _convert_dict_literal(self, node: ast.Dict) -> str:
        """Convert Python dict literal to Haskell Map."""
        pairs = []
        for key, value in zip(node.keys, node.values):
            key_expr = self._convert_expression(key) if key is not None else "undefined"
            value_expr = self._convert_expression(value) if value is not None else "undefined"
            pairs.append(f"({key_expr}, {value_expr})")

        # Use Map.empty for empty dicts to avoid type ambiguity
        if not pairs:
            return "Map.empty"
        return f"Map.fromList [{', '.join(pairs)}]"

    def _convert_set_literal(self, node: ast.Set) -> str:
        """Convert Python set literal to Haskell Set."""
        elements = [self._convert_expression(elt) for elt in node.elts]
        # Use Set.empty for empty sets to avoid type ambiguity
        if not elements:
            return "Set.empty"
        return f"Set.fromList [{', '.join(elements)}]"

    def _convert_list_comprehension(self, node: ast.ListComp) -> str:
        """Convert Python list comprehension to Haskell."""
        # Check preferences for comprehension style
        use_native = False
        if self.preferences and self.preferences.get("use_native_comprehensions", False):
            use_native = True

        expr = self._convert_expression(node.elt)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]
        target = self._to_haskell_var_name(gen.target.id) if isinstance(gen.target, ast.Name) else "x"
        iterable = self._convert_expression(gen.iter)

        if use_native:
            # Use native Haskell list comprehension syntax
            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"[{expr} | {target} <- {iterable}, {condition}]"
            else:
                return f"[{expr} | {target} <- {iterable}]"
        else:
            # Use runtime library functions (default for consistency)
            # Add parentheses if iterable is a function call (contains spaces)
            if " " in iterable and not iterable.startswith("("):
                iterable = f"({iterable})"
            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"listComprehensionWithFilter {iterable} (\\{target} -> {condition}) (\\{target} -> {expr})"
            else:
                return f"listComprehension {iterable} (\\{target} -> {expr})"

    def _convert_dict_comprehension(self, node: ast.DictComp) -> str:
        """Convert Python dict comprehension to Haskell."""
        # Check preferences for comprehension style
        use_native = False
        if self.preferences and self.preferences.get("use_native_comprehensions", False):
            use_native = True

        key_expr = self._convert_expression(node.key)
        value_expr = self._convert_expression(node.value)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]

        # Handle tuple unpacking in target: (k, v) in items()
        if isinstance(gen.target, ast.Tuple):
            # Tuple target - create pattern for each element
            tuple_vars = [
                self._to_haskell_var_name(elt.id) if isinstance(elt, ast.Name) else "x" for elt in gen.target.elts
            ]
            target = f"({', '.join(tuple_vars)})"
            target_pattern = target  # For pattern matching
        elif isinstance(gen.target, ast.Name):
            target = self._to_haskell_var_name(gen.target.id)
            target_pattern = target
        else:
            target = "x"
            target_pattern = "x"

        iterable = self._convert_expression(gen.iter)

        if use_native:
            # Use native Haskell with Map.fromList
            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"Map.fromList [({key_expr}, {value_expr}) | {target} <- {iterable}, {condition}]"
            else:
                return f"Map.fromList [({key_expr}, {value_expr}) | {target} <- {iterable}]"
        else:
            # Use runtime library functions (default for consistency)
            # Add parentheses if iterable is a function call (contains spaces)
            if " " in iterable and not iterable.startswith("("):
                iterable = f"({iterable})"
            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"dictComprehensionWithFilter {iterable} (\\{target_pattern} -> {condition}) (\\{target_pattern} -> {key_expr}) (\\{target_pattern} -> {value_expr})"
            else:
                return f"dictComprehension {iterable} (\\{target_pattern} -> {key_expr}) (\\{target_pattern} -> {value_expr})"

    def _convert_set_comprehension(self, node: ast.SetComp) -> str:
        """Convert Python set comprehension to Haskell."""
        # Check preferences for comprehension style
        use_native = False
        if self.preferences and self.preferences.get("use_native_comprehensions", False):
            use_native = True

        expr = self._convert_expression(node.elt)

        if len(node.generators) != 1:
            raise UnsupportedFeatureError("Multiple generators in comprehensions not supported")

        gen = node.generators[0]
        target = self._to_haskell_var_name(gen.target.id) if isinstance(gen.target, ast.Name) else "x"
        iterable = self._convert_expression(gen.iter)

        if use_native:
            # Use native Haskell with Set.fromList
            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"Set.fromList [{expr} | {target} <- {iterable}, {condition}]"
            else:
                return f"Set.fromList [{expr} | {target} <- {iterable}]"
        else:
            # Use runtime library functions (default for consistency)
            # Add parentheses if iterable is a function call (contains spaces)
            if " " in iterable and not iterable.startswith("("):
                iterable = f"({iterable})"

            # If iterating over a set, convert to list first
            # Heuristic: check if iterable looks like a Set expression
            if "setComprehension" in iterable or "Set." in iterable or isinstance(gen.iter, ast.Name):
                # Wrap in Set.toList for safety - it's a no-op if already a list
                # Actually, we need to be more careful - only wrap if we know it's a Set
                # For now, check if the iterable expression suggests it's a Set
                if "setComprehension" in iterable:
                    iterable = f"(Set.toList {iterable})"

            if gen.ifs:
                conditions = [self._convert_expression(if_clause) for if_clause in gen.ifs]
                condition = " && ".join(conditions)
                return f"setComprehensionWithFilter {iterable} (\\{target} -> {condition}) (\\{target} -> {expr})"
            else:
                return f"setComprehension {iterable} (\\{target} -> {expr})"

    def _convert_ternary_expression(self, node: ast.IfExp) -> str:
        """Convert Python ternary expression to Haskell if-then-else."""
        test = self._convert_expression(node.test)
        body = self._convert_expression(node.body)
        orelse = self._convert_expression(node.orelse)
        return f"(if {test} then {body} else {orelse})"

    def _convert_operator(self, op_node: ast.operator) -> str:
        """Convert AST operator to Haskell operator string."""
        if isinstance(op_node, ast.FloorDiv):
            return "`div`"
        elif isinstance(op_node, ast.Mod):
            return "`mod`"
        elif isinstance(op_node, ast.Pow):
            return "**"
        elif isinstance(op_node, ast.BitOr):
            return ".|."
        elif isinstance(op_node, ast.BitXor):
            return "`xor`"
        elif isinstance(op_node, ast.BitAnd):
            return ".&."
        elif isinstance(op_node, ast.LShift):
            return "`shiftL`"
        elif isinstance(op_node, ast.RShift):
            return "`shiftR`"
        else:
            # Use standard operator mapping from converter_utils for common operators
            op_result = get_standard_binary_operator(op_node)
            if op_result is None:
                raise UnsupportedFeatureError(f"Unsupported operator: {type(op_node).__name__}")
            return op_result

    def _convert_augmented_assignment(self, node: ast.AugAssign) -> str:
        """Convert Python augmented assignment to Haskell."""
        target = self._convert_expression(node.target)
        value = self._convert_expression(node.value)
        op = self._convert_operator(node.op)
        return f"{target} = ({target} {op} {value})"

    def _convert_if_statement(self, node: ast.If) -> str:
        """Convert Python if statement to Haskell."""
        condition = self._convert_expression(node.test)

        # Special case: if cond: var = expr (no else) in main function
        # Convert to: let var = if cond then expr else initial_value
        if (
            self.current_function == "main"
            and len(node.body) == 1
            and isinstance(node.body[0], ast.Assign)
            and not node.orelse
        ):
            assign_stmt = node.body[0]
            if len(assign_stmt.targets) == 1 and isinstance(assign_stmt.targets[0], ast.Name):
                var_name = self._to_haskell_var_name(assign_stmt.targets[0].id)
                then_expr = self._convert_expression(assign_stmt.value)

                # Default else value
                else_value = "0"  # Default for numeric types

                # Return as let binding with conditional expression
                return f"{var_name} = if {condition} then {then_expr} else {else_value}"

        then_stmts = []
        for stmt in node.body:
            converted = self._convert_statement(stmt)
            if converted:
                then_stmts.append(converted)

        else_stmts = []
        for stmt in node.orelse:
            converted = self._convert_statement(stmt)
            if converted:
                else_stmts.append(converted)

        then_body = " >> ".join(then_stmts) if then_stmts else "()"
        else_body = " >> ".join(else_stmts) if else_stmts else "()"

        if else_stmts:
            return f"if {condition} then {then_body} else {else_body}"
        else:
            return f"if {condition} then {then_body} else ()"

    def _convert_while_statement(self, node: ast.While) -> str:
        """Convert Python while loop to Haskell."""
        # Haskell doesn't have while loops - we'd need to use recursion
        raise UnsupportedFeatureError("While loops not directly supported in Haskell")

    def _convert_for_statement(self, node: ast.For) -> str:
        """Convert Python for loop to Haskell using Strategy pattern.

        Before refactoring: ~251 lines, complexity ~40-50
        After refactoring: ~20 lines for common cases, fallback for complex patterns

        Uses loop conversion strategies for common patterns:
        - Nested list building (2D lists)
        - Simple list.append()
        - Accumulation (augmented assignment)
        - Assignment in main (IO)
        """
        # Try strategy-based conversion first
        context = LoopContext(converter=self, current_function=self.current_function)
        result = self.loop_converter.convert(node, context)
        if result is not None:
            return result

        # Fallback to original complex logic for patterns not yet covered by strategies
        # (Triple-nested matrix multiplication, word count pattern, etc.)
        return self._convert_for_statement_fallback(node)

    def _convert_for_statement_fallback(self, node: ast.For) -> str:
        """Fallback for-loop conversion for complex patterns not yet in strategies.

        This method contains the original complex logic for patterns like:
        - Triple-nested matrix multiplication
        - Word count pattern (transform + dict update)
        - Other complex nested patterns

        Over time, these can be extracted into additional strategies.
        """
        if isinstance(node.target, ast.Name):
            var_name = self._to_haskell_var_name(node.target.id)
            iterable = self._convert_expression(node.iter)

            # Detect triple-nested loop pattern for matrix multiplication
            # Pattern: for i in range(size):
            #              for j in range(size):
            #                  sum_val = 0
            #                  for k in range(size):
            #                      sum_val += expr
            #                  result[i][j] = sum_val
            if len(node.body) == 1 and isinstance(node.body[0], ast.For):
                j_loop = node.body[0]
                if (
                    isinstance(j_loop.target, ast.Name)
                    and len(j_loop.body) == 3
                    and isinstance(j_loop.body[0], (ast.Assign, ast.AnnAssign))
                    and isinstance(j_loop.body[1], ast.For)
                    and isinstance(j_loop.body[2], ast.Assign)
                ):
                    sum_init = j_loop.body[0]
                    k_loop = j_loop.body[1]
                    result_assign = j_loop.body[2]

                    # Check if sum_val is initialized to 0
                    sum_var = None
                    if isinstance(sum_init, ast.Assign):
                        if (
                            len(sum_init.targets) == 1
                            and isinstance(sum_init.targets[0], ast.Name)
                            and isinstance(sum_init.value, ast.Constant)
                            and sum_init.value.value == 0
                        ):
                            sum_var = sum_init.targets[0].id
                    elif isinstance(sum_init, ast.AnnAssign):
                        if (
                            isinstance(sum_init.target, ast.Name)
                            and isinstance(sum_init.value, ast.Constant)
                            and sum_init.value.value == 0
                        ):
                            sum_var = sum_init.target.id

                    # Check if k_loop accumulates into sum_var
                    if (
                        sum_var
                        and isinstance(k_loop.target, ast.Name)
                        and len(k_loop.body) == 1
                        and isinstance(k_loop.body[0], ast.AugAssign)
                        and isinstance(k_loop.body[0].target, ast.Name)
                        and k_loop.body[0].target.id == sum_var
                    ):
                        # Check if result[i][j] = sum_val
                        if (
                            isinstance(result_assign.targets[0], ast.Subscript)
                            and isinstance(result_assign.targets[0].value, ast.Subscript)
                            and isinstance(result_assign.targets[0].value.value, ast.Name)
                            and isinstance(result_assign.value, ast.Name)
                            and result_assign.value.id == sum_var
                        ):
                            # Extract all variables
                            j_var = self._to_haskell_var_name(j_loop.target.id)
                            k_var = self._to_haskell_var_name(k_loop.target.id)
                            j_iterable = self._convert_expression(j_loop.iter)
                            k_iterable = self._convert_expression(k_loop.iter)
                            accum_expr = self._convert_expression(k_loop.body[0].value)
                            result_var = self._to_haskell_var_name(result_assign.targets[0].value.value.id)

                            # Generate triple-nested comprehension with sum
                            # result = [[sum [expr | k <- range] | j <- range] | i <- range]
                            if self.current_function != "main":
                                return f"{result_var} = [[sum' [{accum_expr} | {k_var} <- {k_iterable}] | {j_var} <- {j_iterable}] | {var_name} <- {iterable}]"
                            else:
                                # In IO context, generate using foldM
                                return f"{result_var} <- foldM (\\acc {var_name} -> return (acc ++ [[sum' [{accum_expr} | {k_var} <- {k_iterable}] | {j_var} <- {j_iterable}]])) {result_var} ({iterable})"

            # Detect word count pattern: for item in list: transform, then update dict
            if (
                len(node.body) == 2
                and isinstance(node.body[0], (ast.Assign, ast.AnnAssign))
                and isinstance(node.body[1], ast.If)
            ):
                # Extract transformation variable and expression
                transform_stmt = node.body[0]
                if isinstance(transform_stmt, ast.Assign):
                    if len(transform_stmt.targets) == 1 and isinstance(transform_stmt.targets[0], ast.Name):
                        key_var = self._to_haskell_var_name(transform_stmt.targets[0].id)
                        key_expr = self._convert_expression(transform_stmt.value)
                    else:
                        key_var = None
                elif isinstance(transform_stmt, ast.AnnAssign):
                    if isinstance(transform_stmt.target, ast.Name):
                        key_var = self._to_haskell_var_name(transform_stmt.target.id)
                        if transform_stmt.value:
                            key_expr = self._convert_expression(transform_stmt.value)
                        else:
                            key_expr = "undefined"
                    else:
                        key_var = None
                else:
                    key_var = None

                # Check if the if-statement updates a dictionary
                if_stmt = node.body[1]
                dict_var = None

                # Pattern: if key in dict: dict[key] = dict[key] + 1 else: dict[key] = 1
                if (
                    isinstance(if_stmt.test, ast.Compare)
                    and len(if_stmt.test.ops) == 1
                    and isinstance(if_stmt.test.ops[0], ast.In)
                    and len(if_stmt.body) == 1
                    and isinstance(if_stmt.body[0], ast.Assign)
                    and len(if_stmt.orelse) == 1
                    and isinstance(if_stmt.orelse[0], ast.Assign)
                ):
                    then_stmt = if_stmt.body[0]
                    if_stmt.orelse[0]

                    # Extract dictionary variable from the assignments
                    if isinstance(then_stmt.targets[0], ast.Subscript) and isinstance(
                        then_stmt.targets[0].value, ast.Name
                    ):
                        dict_var = self._to_haskell_var_name(then_stmt.targets[0].value.id)

                    # Generate fold for word count pattern
                    if dict_var and key_var and self.current_function != "main":
                        # Pure context - use foldl with Map operations
                        # Use Map.empty as initial accumulator (assuming dict was initialized to empty)
                        return (
                            f"{dict_var} = foldl (\\acc {var_name} -> let {key_var} = {key_expr} in "
                            f"Map.insertWith (+) {key_var} 1 acc) Map.empty ({iterable})"
                        )

            body_stmts = []
            for body_stmt in node.body:
                converted = self._convert_statement(body_stmt)
                if converted:
                    body_stmts.append(converted)

            body = " >> ".join(body_stmts) if body_stmts else "()"

            # Use mapM_ for side effects - only in IO context (main function)
            if self.current_function == "main":
                return f"mapM_ (\\{var_name} -> {body}) {iterable}"
            else:
                # In pure context, can't use mapM_ - need to handle differently
                # For now, skip side-effect-only loops in pure functions
                return "-- for loop with side effects (not converted in pure function)"
        else:
            raise UnsupportedFeatureError("Complex for loop targets not supported")

    def _convert_type_annotation(self, node: ast.expr) -> str:
        """Convert Python type annotation to Haskell type."""
        if isinstance(node, ast.Name):
            python_type = node.id
            if python_type == "None":
                return "()"
            return self.type_map.get(python_type, python_type)
        elif isinstance(node, ast.Constant):
            if node.value is None:
                return "()"
            return str(node.value)
        elif isinstance(node, ast.Subscript):
            # Handle subscripted types like list[int], list[list[int]], dict[str, int]
            if isinstance(node.value, ast.Name):
                container_type = node.value.id
                if container_type == "list":
                    # list[int] -> [Int], list[list[int]] -> [[Int]]
                    if isinstance(node.slice, ast.Name):
                        element_type = self.type_map.get(node.slice.id, node.slice.id)
                        return f"[{element_type}]"
                    elif isinstance(node.slice, ast.Subscript):
                        # Recursively handle nested lists like list[list[int]]
                        element_type = self._convert_type_annotation(node.slice)
                        return f"[{element_type}]"
                    return "[a]"  # Default to [a]
                elif container_type == "dict":
                    # dict[str, int] -> Map String Int
                    if isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 2:
                        key_type = self._convert_type_annotation(node.slice.elts[0])
                        value_type = self._convert_type_annotation(node.slice.elts[1])
                        return f"Map {key_type} {value_type}"
                    return "Map a b"  # Default
                elif container_type == "set":
                    # set[int] -> Set Int
                    if isinstance(node.slice, ast.Name):
                        element_type = self.type_map.get(node.slice.id, node.slice.id)
                        return f"Set {element_type}"
                    return "Set a"  # Default
            return "a"  # Fallback
        else:
            return "a"  # Default generic type

    def _param_used_as_2d_list(self, func_node: ast.FunctionDef, param_name: str) -> bool:
        """Check if a parameter is used as a 2D list (nested subscript access)."""
        # Look for patterns like param[i][j]
        for node in ast.walk(func_node):
            if isinstance(node, ast.Subscript):
                # Check if this is a nested subscript: param[i][j]
                if isinstance(node.value, ast.Subscript):
                    # Check if the innermost subscript is accessing the parameter
                    if isinstance(node.value.value, ast.Name) and node.value.value.id == param_name:
                        return True
        return False

    def _returns_2d_list(self, func_node: ast.FunctionDef) -> bool:
        """Check if function returns a 2D list (nested list comprehension or nested loops)."""
        # Look for return statements that return nested list comprehensions
        for stmt in ast.walk(func_node):
            if isinstance(stmt, ast.Return) and stmt.value:
                # Check if return value is a list comprehension
                if isinstance(stmt.value, ast.ListComp):
                    # Check if the comprehension expression is another list comprehension
                    if isinstance(stmt.value.elt, ast.ListComp):
                        return True
                # Check if return value is a variable that was assigned a nested list comp
                elif isinstance(stmt.value, ast.Name):
                    var_name = stmt.value.id
                    # Look for assignments to this variable with nested list comps
                    for assign_stmt in ast.walk(func_node):
                        if isinstance(assign_stmt, (ast.Assign, ast.AnnAssign)):
                            if isinstance(assign_stmt, ast.Assign):
                                if (
                                    len(assign_stmt.targets) == 1
                                    and isinstance(assign_stmt.targets[0], ast.Name)
                                    and assign_stmt.targets[0].id == var_name
                                ):
                                    if isinstance(assign_stmt.value, ast.ListComp):
                                        if isinstance(assign_stmt.value.elt, ast.ListComp):
                                            return True
                            elif isinstance(assign_stmt, ast.AnnAssign):
                                if (
                                    isinstance(assign_stmt.target, ast.Name)
                                    and assign_stmt.target.id == var_name
                                    and assign_stmt.value
                                ):
                                    if isinstance(assign_stmt.value, ast.ListComp):
                                        if isinstance(assign_stmt.value.elt, ast.ListComp):
                                            return True

            # Also check for nested loop patterns that build 2D lists
            # Pattern: for i in range: row = []; for j in range: row.append(...); matrix.append(row)
            if isinstance(stmt, ast.For):
                if len(stmt.body) == 3:
                    if (
                        isinstance(stmt.body[0], (ast.Assign, ast.AnnAssign))
                        and isinstance(stmt.body[1], ast.For)
                        and isinstance(stmt.body[2], ast.Expr)
                    ):
                        # This is a 2D list building pattern
                        return True
                # Also check for triple-nested loop pattern (matrix multiplication)
                # Pattern: for i: for j: sum_val = 0; for k: sum_val += ...; result[i][j] = sum_val
                if len(stmt.body) == 1 and isinstance(stmt.body[0], ast.For):
                    j_loop = stmt.body[0]
                    if len(j_loop.body) == 3:
                        if (
                            isinstance(j_loop.body[0], (ast.Assign, ast.AnnAssign))
                            and isinstance(j_loop.body[1], ast.For)
                            and isinstance(j_loop.body[2], ast.Assign)
                        ):
                            # This is a triple-nested loop building a 2D result
                            return True

        return False

    def _infer_dict_value_type(self, func_node: ast.FunctionDef) -> Optional[str]:
        """Infer dictionary value type from function body operations."""
        # Look for patterns that indicate value type:
        # 1. dict[key] = int_literal -> Int
        # 2. dict[key] = dict[key] + 1 -> Int
        # 3. Map.insertWith (+) key 1 -> Int (numeric operations)

        for stmt in ast.walk(func_node):
            # Pattern: dict[key] = 1 or dict[key] = dict[key] + 1
            if isinstance(stmt, ast.Assign):
                if len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Subscript):
                    # Check if value is an int literal
                    if isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, int):
                        return "Int"
                    # Check if value is a binary operation with int
                    if isinstance(stmt.value, ast.BinOp):
                        if isinstance(stmt.value.right, ast.Constant) and isinstance(stmt.value.right.value, int):
                            return "Int"

            # Pattern: if key in dict: dict[key] = dict[key] + 1 else: dict[key] = 1
            if isinstance(stmt, ast.If):
                # Check the then branch for dict[key] = ... + int
                for then_stmt in stmt.body:
                    if isinstance(then_stmt, ast.Assign) and len(then_stmt.targets) == 1:
                        if isinstance(then_stmt.targets[0], ast.Subscript):
                            if isinstance(then_stmt.value, ast.BinOp):
                                if isinstance(then_stmt.value.right, ast.Constant) and isinstance(
                                    then_stmt.value.right.value, int
                                ):
                                    return "Int"
                # Check the else branch
                for else_stmt in stmt.orelse:
                    if isinstance(else_stmt, ast.Assign) and len(else_stmt.targets) == 1:
                        if isinstance(else_stmt.targets[0], ast.Subscript):
                            if isinstance(else_stmt.value, ast.Constant) and isinstance(else_stmt.value.value, int):
                                return "Int"

        return None

    def _infer_type_from_node(self, node: ast.expr) -> str:
        """Infer Haskell type from Python expression node."""
        if isinstance(node, ast.Constant):
            if isinstance(node.value, bool):
                return "Bool"
            elif isinstance(node.value, int):
                return "Int"
            elif isinstance(node.value, float):
                return "Double"
            elif isinstance(node.value, str):
                return "String"
        elif isinstance(node, ast.List):
            return "[a]"
        elif isinstance(node, ast.Dict):
            return "Dict String a"
        elif isinstance(node, ast.Set):
            return "Set a"
        elif isinstance(node, ast.BinOp):
            # If it's a BinOp with Add, recursively check operands
            if isinstance(node.op, ast.Add):
                left_type = self._infer_type_from_node(node.left)
                right_type = self._infer_type_from_node(node.right)
                # If either side is a list, the result is a list
                if left_type.startswith("[") or right_type.startswith("["):
                    return "[a]"
        elif isinstance(node, ast.Call):
            # Check if the function name suggests it returns a list
            if isinstance(node.func, ast.Name):
                # Functions like quicksort, filter, map typically return lists
                if node.func.id in ("quicksort", "filter", "map", "sorted"):
                    return "[a]"

        return "a"  # Default generic type
