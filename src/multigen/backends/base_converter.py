"""Base converter class providing common AST traversal logic for all language backends.

This module extracts common Python-to-X conversion patterns to reduce code duplication
across backend implementations. Each language backend inherits from BaseConverter and
implements language-specific formatting via abstract methods.
"""

import ast
from abc import ABC, abstractmethod
from typing import Any, Optional

# Import error classes (backward compatibility)
from .errors import TypeMappingError, UnsupportedFeatureError


class BaseConverter(ABC):
    """Abstract base converter providing common AST traversal patterns.

    This class implements the structure of Python AST traversal while delegating
    language-specific formatting to abstract methods that subclasses must implement.

    Common patterns handled:
    - Module-level structure and organization
    - Function and class traversal
    - Statement type dispatching
    - Expression type dispatching
    - Type annotation processing
    - Error handling

    Language-specific behavior (abstract):
    - Literal formatting (strings, bools, None, numbers)
    - Type name mapping
    - Operator symbols
    - Built-in function names
    - Control flow syntax
    - Container syntax
    """

    def __init__(self) -> None:
        """Initialize the base converter with common tracking structures."""
        self.type_map: dict[str, str] = {}  # Populated by subclasses
        self.classes: dict[str, Any] = {}  # Track class definitions
        self.current_class: Optional[str] = None  # Current class context
        self.variable_types: dict[str, str] = {}  # Track variable types

    # ============================================================================
    # Main Entry Points (to be called by subclasses)
    # ============================================================================

    def convert_code(self, source_code: str) -> str:
        """Convert Python source code to target language.

        Args:
            source_code: Python source code string

        Returns:
            Converted code in target language

        Raises:
            UnsupportedFeatureError: If unsupported Python feature encountered
            TypeMappingError: If type conversion fails
        """
        try:
            tree = ast.parse(source_code)
            return self._convert_module(tree)
        except UnsupportedFeatureError:
            raise
        except Exception as e:
            raise TypeMappingError(f"Failed to convert Python code: {e}") from e

    # ============================================================================
    # Abstract Methods - Must be implemented by subclasses
    # ============================================================================

    @abstractmethod
    def _format_string_literal(self, value: str) -> str:
        """Format a string literal for the target language.

        Args:
            value: Python string value

        Returns:
            Formatted string literal (e.g., '"value"', '"value".to_string()', etc.)
        """
        pass

    @abstractmethod
    def _format_bool_literal(self, value: bool) -> str:
        """Format a boolean literal for the target language.

        Args:
            value: Python boolean value

        Returns:
            Formatted boolean (e.g., 'true'/'false', 'True'/'False', etc.)
        """
        pass

    @abstractmethod
    def _format_none_literal(self) -> str:
        """Format None/null for the target language.

        Returns:
            Formatted null value (e.g., 'nullptr', 'None', 'nil', '()', etc.)
        """
        pass

    @abstractmethod
    def _format_binary_operator(self, op: ast.operator) -> str:
        """Format a binary operator for the target language.

        Args:
            op: Python AST operator node

        Returns:
            Formatted operator symbol

        Raises:
            UnsupportedFeatureError: If operator not supported
        """
        pass

    @abstractmethod
    def _format_unary_operator(self, op: ast.unaryop) -> str:
        """Format a unary operator for the target language.

        Args:
            op: Python AST unary operator node

        Returns:
            Formatted operator symbol

        Raises:
            UnsupportedFeatureError: If operator not supported
        """
        pass

    @abstractmethod
    def _format_comparison_operator(self, op: ast.cmpop) -> str:
        """Format a comparison operator for the target language.

        Args:
            op: Python AST comparison operator node

        Returns:
            Formatted operator symbol

        Raises:
            UnsupportedFeatureError: If operator not supported
        """
        pass

    @abstractmethod
    def _format_function_definition(
        self,
        name: str,
        params: list[tuple[str, str]],
        return_type: str,
        body: str,
    ) -> str:
        """Format a complete function definition.

        Args:
            name: Function name in target language format
            params: List of (param_name, param_type) tuples
            return_type: Return type in target language
            body: Converted function body code

        Returns:
            Complete function definition code
        """
        pass

    @abstractmethod
    def _format_variable_declaration(self, name: str, var_type: str, value: Optional[str]) -> str:
        """Format a variable declaration/assignment.

        Args:
            name: Variable name in target language format
            var_type: Variable type in target language
            value: Optional initial value expression

        Returns:
            Variable declaration/assignment statement
        """
        pass

    @abstractmethod
    def _format_return_statement(self, value: Optional[str]) -> str:
        """Format a return statement.

        Args:
            value: Optional return value expression

        Returns:
            Return statement code
        """
        pass

    @abstractmethod
    def _format_if_statement(self, condition: str, then_block: str, else_block: Optional[str]) -> str:
        """Format an if statement.

        Args:
            condition: Condition expression
            then_block: Code for then branch
            else_block: Optional code for else branch

        Returns:
            If statement code
        """
        pass

    @abstractmethod
    def _format_while_loop(self, condition: str, body: str) -> str:
        """Format a while loop.

        Args:
            condition: Loop condition expression
            body: Loop body code

        Returns:
            While loop code
        """
        pass

    @abstractmethod
    def _format_for_loop(self, var: str, iterable: str, body: str) -> str:
        """Format a for loop.

        Args:
            var: Loop variable name
            iterable: Iterable expression
            body: Loop body code

        Returns:
            For loop code
        """
        pass

    @abstractmethod
    def _format_function_call(self, func_name: str, args: list[str]) -> str:
        """Format a function call.

        Args:
            func_name: Function name
            args: List of argument expressions

        Returns:
            Function call expression
        """
        pass

    @abstractmethod
    def _format_method_call(self, object_expr: str, method_name: str, args: list[str]) -> str:
        """Format a method call.

        Args:
            object_expr: Object expression
            method_name: Method name
            args: List of argument expressions

        Returns:
            Method call expression
        """
        pass

    @abstractmethod
    def _map_type(self, python_type: str) -> str:
        """Map Python type to target language type.

        Args:
            python_type: Python type name

        Returns:
            Target language type name
        """
        pass

    # ============================================================================
    # Common AST Traversal - Concrete implementations
    # ============================================================================

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to target language.

        This method provides common structure and delegates specifics to subclasses.

        Args:
            node: Python AST Module node

        Returns:
            Converted module code
        """
        # Subclasses should override this to add imports, module structure, etc.
        # Default implementation just converts all statements
        parts = []

        # Convert classes first
        for item in node.body:
            if isinstance(item, ast.ClassDef):
                parts.append(self._convert_class(item))

        # Convert functions
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                parts.append(self._convert_function(item))

        return "\n\n".join(parts)

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert a Python function definition.

        Args:
            node: Python AST FunctionDef node

        Returns:
            Converted function definition
        """
        # Extract function name
        func_name = node.name

        # Extract parameters with types
        params = []
        for arg in node.args.args:
            param_name = arg.arg
            param_type = self._convert_type_annotation(arg.annotation) if arg.annotation else self._map_type("Any")
            params.append((param_name, param_type))

        # Extract return type
        return_type = self._convert_type_annotation(node.returns) if node.returns else self._map_type("None")

        # Convert function body
        body_statements = [self._convert_statement(stmt) for stmt in node.body]
        body = "\n".join(stmt for stmt in body_statements if stmt)

        # Format using subclass-specific formatter
        return self._format_function_definition(func_name, params, return_type, body)

    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert a Python class definition.

        Subclasses should override this for language-specific class handling.

        Args:
            node: Python AST ClassDef node

        Returns:
            Converted class definition
        """
        # This is a complex operation that varies significantly by language
        # Subclasses should override with language-specific implementation
        raise UnsupportedFeatureError(f"Class conversion not implemented in {self.__class__.__name__}")

    def _convert_statement(self, stmt: ast.stmt) -> str:
        """Convert a Python statement to target language.

        This dispatcher handles all statement types.

        Args:
            stmt: Python AST statement node

        Returns:
            Converted statement code

        Raises:
            UnsupportedFeatureError: If statement type not supported
        """
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
        else:
            raise UnsupportedFeatureError(f"Unsupported statement: {type(stmt).__name__}")

    def _convert_return(self, stmt: ast.Return) -> str:
        """Convert return statement.

        Args:
            stmt: Python AST Return node

        Returns:
            Converted return statement
        """
        value = self._convert_expression(stmt.value) if stmt.value else None
        return self._format_return_statement(value)

    def _convert_assignment(self, stmt: ast.Assign) -> str:
        """Convert assignment statement.

        Args:
            stmt: Python AST Assign node

        Returns:
            Converted assignment statement

        Raises:
            UnsupportedFeatureError: If multiple targets not supported
        """
        if len(stmt.targets) != 1:
            raise UnsupportedFeatureError("Multiple assignment targets not supported")

        target = stmt.targets[0]
        if not isinstance(target, ast.Name):
            raise UnsupportedFeatureError("Only simple name assignments supported")

        var_name = target.id
        value_expr = self._convert_expression(stmt.value)

        # Infer type from value
        var_type = self._infer_type_from_value(stmt.value)

        # Track variable type
        self.variable_types[var_name] = var_type

        return self._format_variable_declaration(var_name, var_type, value_expr)

    def _convert_annotated_assignment(self, stmt: ast.AnnAssign) -> str:
        """Convert annotated assignment statement.

        Args:
            stmt: Python AST AnnAssign node

        Returns:
            Converted annotated assignment statement
        """
        if not isinstance(stmt.target, ast.Name):
            raise UnsupportedFeatureError("Only simple name assignments supported")

        var_name = stmt.target.id
        var_type = self._convert_type_annotation(stmt.annotation)
        value_expr = self._convert_expression(stmt.value) if stmt.value else None

        # Track variable type
        self.variable_types[var_name] = var_type

        return self._format_variable_declaration(var_name, var_type, value_expr)

    def _convert_aug_assignment(self, stmt: ast.AugAssign) -> str:
        """Convert augmented assignment (+=, -=, etc.).

        Default implementation: converts to regular assignment with binary op.
        Subclasses can override for language-specific augmented assignment syntax.

        Args:
            stmt: Python AST AugAssign node

        Returns:
            Converted augmented assignment statement
        """
        if not isinstance(stmt.target, ast.Name):
            raise UnsupportedFeatureError("Only simple name augmented assignments supported")

        var_name = stmt.target.id
        operator = self._format_binary_operator(stmt.op)
        value = self._convert_expression(stmt.value)

        # Convert to: var = var op value
        var_type = self.variable_types.get(var_name, self._map_type("Any"))
        combined_expr = f"({var_name} {operator} {value})"

        return self._format_variable_declaration(var_name, var_type, combined_expr)

    def _convert_if(self, stmt: ast.If) -> str:
        """Convert if statement.

        Args:
            stmt: Python AST If node

        Returns:
            Converted if statement
        """
        condition = self._convert_expression(stmt.test)

        then_stmts = [self._convert_statement(s) for s in stmt.body]
        then_block = "\n".join(s for s in then_stmts if s)

        else_block = None
        if stmt.orelse:
            else_stmts = [self._convert_statement(s) for s in stmt.orelse]
            else_block = "\n".join(s for s in else_stmts if s)

        return self._format_if_statement(condition, then_block, else_block)

    def _convert_while(self, stmt: ast.While) -> str:
        """Convert while loop.

        Args:
            stmt: Python AST While node

        Returns:
            Converted while loop
        """
        condition = self._convert_expression(stmt.test)
        body_stmts = [self._convert_statement(s) for s in stmt.body]
        body = "\n".join(s for s in body_stmts if s)

        return self._format_while_loop(condition, body)

    def _convert_for(self, stmt: ast.For) -> str:
        """Convert for loop.

        Args:
            stmt: Python AST For node

        Returns:
            Converted for loop

        Raises:
            UnsupportedFeatureError: If complex loop targets not supported
        """
        if not isinstance(stmt.target, ast.Name):
            raise UnsupportedFeatureError("Complex for loop targets not supported")

        var = stmt.target.id
        iterable = self._convert_expression(stmt.iter)
        body_stmts = [self._convert_statement(s) for s in stmt.body]
        body = "\n".join(s for s in body_stmts if s)

        return self._format_for_loop(var, iterable, body)

    def _convert_expression_statement(self, stmt: ast.Expr) -> str:
        """Convert expression statement.

        Args:
            stmt: Python AST Expr node

        Returns:
            Converted expression statement
        """
        return self._convert_expression(stmt.value)

    def _convert_expression(self, expr: ast.expr) -> str:
        """Convert a Python expression to target language.

        This dispatcher handles all expression types.

        Args:
            expr: Python AST expression node

        Returns:
            Converted expression code

        Raises:
            UnsupportedFeatureError: If expression type not supported
        """
        if isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        elif isinstance(expr, ast.Name):
            return expr.id  # Variable name - usually same across languages
        elif isinstance(expr, ast.BinOp):
            return self._convert_binop(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self._convert_unaryop(expr)
        elif isinstance(expr, ast.Compare):
            return self._convert_compare(expr)
        elif isinstance(expr, ast.BoolOp):
            return self._convert_boolop(expr)
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
        elif isinstance(expr, ast.IfExp):
            return self._convert_ternary(expr)
        else:
            raise UnsupportedFeatureError(f"Unsupported expression: {type(expr).__name__}")

    def _convert_constant(self, expr: ast.Constant) -> str:
        """Convert constant literal.

        Args:
            expr: Python AST Constant node

        Returns:
            Converted constant value
        """
        value = expr.value

        if isinstance(value, str):
            return self._format_string_literal(value)
        elif isinstance(value, bool):
            return self._format_bool_literal(value)
        elif value is None:
            return self._format_none_literal()
        elif isinstance(value, (int, float)):
            return str(value)
        else:
            return str(value)

    def _convert_binop(self, expr: ast.BinOp) -> str:
        """Convert binary operation.

        Args:
            expr: Python AST BinOp node

        Returns:
            Converted binary operation expression
        """
        left = self._convert_expression(expr.left)
        operator = self._format_binary_operator(expr.op)
        right = self._convert_expression(expr.right)

        return f"({left} {operator} {right})"

    def _convert_unaryop(self, expr: ast.UnaryOp) -> str:
        """Convert unary operation.

        Args:
            expr: Python AST UnaryOp node

        Returns:
            Converted unary operation expression
        """
        operator = self._format_unary_operator(expr.op)
        operand = self._convert_expression(expr.operand)

        return f"({operator}{operand})"

    def _convert_compare(self, expr: ast.Compare) -> str:
        """Convert comparison operation.

        Args:
            expr: Python AST Compare node

        Returns:
            Converted comparison expression

        Raises:
            UnsupportedFeatureError: If multiple comparisons not supported
        """
        if len(expr.ops) != 1 or len(expr.comparators) != 1:
            raise UnsupportedFeatureError("Multiple comparisons not supported")

        left = self._convert_expression(expr.left)
        operator = self._format_comparison_operator(expr.ops[0])
        right = self._convert_expression(expr.comparators[0])

        return f"({left} {operator} {right})"

    def _convert_boolop(self, expr: ast.BoolOp) -> str:
        """Convert boolean operation (and/or).

        Args:
            expr: Python AST BoolOp node

        Returns:
            Converted boolean operation expression
        """
        operator = "&&" if isinstance(expr.op, ast.And) else "||"
        operands = [self._convert_expression(v) for v in expr.values]

        return f"({f' {operator} '.join(operands)})"

    def _convert_call(self, expr: ast.Call) -> str:
        """Convert function/method call.

        Args:
            expr: Python AST Call node

        Returns:
            Converted call expression
        """
        # Check if it's a method call
        if isinstance(expr.func, ast.Attribute):
            object_expr = self._convert_expression(expr.func.value)
            method_name = expr.func.attr
            args = [self._convert_expression(arg) for arg in expr.args]
            return self._format_method_call(object_expr, method_name, args)
        # Regular function call
        elif isinstance(expr.func, ast.Name):
            func_name = expr.func.id
            args = [self._convert_expression(arg) for arg in expr.args]
            return self._format_function_call(func_name, args)
        else:
            raise UnsupportedFeatureError("Complex function calls not supported")

    def _convert_attribute(self, expr: ast.Attribute) -> str:
        """Convert attribute access.

        Subclasses should override for language-specific attribute syntax.

        Args:
            expr: Python AST Attribute node

        Returns:
            Converted attribute access expression
        """
        obj = self._convert_expression(expr.value)
        attr = expr.attr
        return f"{obj}.{attr}"

    def _convert_list_literal(self, expr: ast.List) -> str:
        """Convert list literal.

        Subclasses should override for language-specific list syntax.

        Args:
            expr: Python AST List node

        Returns:
            Converted list literal
        """
        elements = [self._convert_expression(elt) for elt in expr.elts]
        return f"[{', '.join(elements)}]"

    def _convert_dict_literal(self, expr: ast.Dict) -> str:
        """Convert dict literal.

        Subclasses should override for language-specific dict syntax.

        Args:
            expr: Python AST Dict node

        Returns:
            Converted dict literal
        """
        pairs = []
        for key, value in zip(expr.keys, expr.values):
            key_expr = self._convert_expression(key) if key else ""
            value_expr = self._convert_expression(value) if value else ""
            pairs.append(f"{key_expr}: {value_expr}")
        return f"{{{', '.join(pairs)}}}"

    def _convert_set_literal(self, expr: ast.Set) -> str:
        """Convert set literal.

        Subclasses should override for language-specific set syntax.

        Args:
            expr: Python AST Set node

        Returns:
            Converted set literal
        """
        elements = [self._convert_expression(elt) for elt in expr.elts]
        return f"{{{', '.join(elements)}}}"

    def _convert_list_comprehension(self, expr: ast.ListComp) -> str:
        """Convert list comprehension.

        Subclasses should override for language-specific comprehension syntax.

        Args:
            expr: Python AST ListComp node

        Returns:
            Converted list comprehension

        Raises:
            UnsupportedFeatureError: Default implementation doesn't support comprehensions
        """
        raise UnsupportedFeatureError(f"List comprehensions not implemented in {self.__class__.__name__}")

    def _convert_dict_comprehension(self, expr: ast.DictComp) -> str:
        """Convert dict comprehension.

        Subclasses should override for language-specific comprehension syntax.

        Args:
            expr: Python AST DictComp node

        Returns:
            Converted dict comprehension

        Raises:
            UnsupportedFeatureError: Default implementation doesn't support comprehensions
        """
        raise UnsupportedFeatureError(f"Dict comprehensions not implemented in {self.__class__.__name__}")

    def _convert_set_comprehension(self, expr: ast.SetComp) -> str:
        """Convert set comprehension.

        Subclasses should override for language-specific comprehension syntax.

        Args:
            expr: Python AST SetComp node

        Returns:
            Converted set comprehension

        Raises:
            UnsupportedFeatureError: Default implementation doesn't support comprehensions
        """
        raise UnsupportedFeatureError(f"Set comprehensions not implemented in {self.__class__.__name__}")

    def _convert_ternary(self, expr: ast.IfExp) -> str:
        """Convert ternary expression (if-else expression).

        Args:
            expr: Python AST IfExp node

        Returns:
            Converted ternary expression
        """
        test = self._convert_expression(expr.test)
        body = self._convert_expression(expr.body)
        orelse = self._convert_expression(expr.orelse)

        # Default C-style ternary format, subclasses can override
        return f"({test} ? {body} : {orelse})"

    def _convert_type_annotation(self, annotation: ast.expr) -> str:
        """Convert type annotation.

        Args:
            annotation: Python AST expression representing type

        Returns:
            Converted type name
        """
        if isinstance(annotation, ast.Name):
            return self._map_type(annotation.id)
        elif isinstance(annotation, ast.Subscript):
            # Handle generic types like List[int], Dict[str, int]
            if isinstance(annotation.value, ast.Name):
                container = annotation.value.id
                # Subclasses should handle this properly
                return self._map_type(container)
        elif isinstance(annotation, ast.Constant) and annotation.value is None:
            return self._map_type("None")

        return self._map_type("Any")

    def _infer_type_from_value(self, value: ast.expr) -> str:
        """Infer type from value expression.

        Args:
            value: Python AST expression

        Returns:
            Inferred type name in target language
        """
        if isinstance(value, ast.Constant):
            if isinstance(value.value, bool):
                return self._map_type("bool")
            elif isinstance(value.value, int):
                return self._map_type("int")
            elif isinstance(value.value, float):
                return self._map_type("float")
            elif isinstance(value.value, str):
                return self._map_type("str")
            elif value.value is None:
                return self._map_type("None")
        elif isinstance(value, ast.List):
            return self._map_type("list")
        elif isinstance(value, ast.Dict):
            return self._map_type("dict")
        elif isinstance(value, ast.Set):
            return self._map_type("set")
        elif isinstance(value, ast.Call):
            # Try to infer from function name
            if isinstance(value.func, ast.Name):
                func_name = value.func.id
                if func_name in self.classes:
                    return func_name  # Constructor call

        return self._map_type("Any")
