"""Z3 Formula Generation from Python AST.

Converts Python AST nodes into Z3 formulas for formal verification.
"""

import ast
from typing import Any, Optional

try:
    import z3  # type: ignore[import-untyped,import-not-found]

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    z3 = None  # type: ignore[assignment]


class Z3FormulaGenerator:
    """Generates Z3 formulas from Python AST nodes."""

    def __init__(self) -> None:
        """Initialize the formula generator."""
        self.z3_available = Z3_AVAILABLE
        self.variables: dict[str, Any] = {}  # name -> z3 variable
        self.constraints: list[Any] = []

    def get_or_create_var(self, name: str, var_type: str = "int") -> Any:
        """Get or create a Z3 variable.

        Args:
            name: Variable name
            var_type: Variable type ("int", "bool", etc.)

        Returns:
            Z3 variable or None if Z3 is not available
        """
        if not self.z3_available:
            return None

        if name not in self.variables:
            if var_type == "int":
                self.variables[name] = z3.Int(name)
            elif var_type == "bool":
                if hasattr(z3, "Bool"):
                    self.variables[name] = z3.Bool(name)
                else:
                    self.variables[name] = z3.Int(name)  # Fallback for mock
            else:
                self.variables[name] = z3.Int(name)  # Default to int
        return self.variables[name]

    def generate_array_bounds_formula(
        self, array_name: str, index_expr: ast.expr, array_len_name: str
    ) -> Optional[Any]:
        """Generate Z3 formula for array bounds safety.

        Args:
            array_name: Name of the array
            index_expr: AST expression for the index
            array_len_name: Name of variable holding array length

        Returns:
            Z3 formula asserting bounds safety
        """
        if not self.z3_available:
            return None

        # Get array length variable
        arr_len = self.get_or_create_var(array_len_name, "int")

        # Convert index expression to Z3
        index_z3 = self._expr_to_z3(index_expr)
        if index_z3 is None:
            return None

        # Generate bounds check: 0 <= index < arr_len
        return z3.And(index_z3 >= 0, index_z3 < arr_len)

    def generate_loop_bounds_formula(
        self, loop_var: str, start: int, end_expr: ast.expr, array_len_name: str
    ) -> Optional[Any]:
        """Generate Z3 formula for loop bounds safety.

        For: for i in range(start, end)
        Prove: all accesses arr[i] are safe

        Args:
            loop_var: Loop variable name (e.g., "i")
            start: Loop start value
            end_expr: AST expression for loop end
            array_len_name: Name of variable holding array length

        Returns:
            Z3 formula asserting all loop iterations are safe
        """
        if not self.z3_available:
            return None

        i = self.get_or_create_var(loop_var, "int")
        arr_len = self.get_or_create_var(array_len_name, "int")

        # Convert end expression to Z3
        end_z3 = self._expr_to_z3(end_expr)
        if end_z3 is None:
            return None

        # For all i in [start, end), prove: start <= i < end => 0 <= i < arr_len
        loop_range = z3.And(i >= start, i < end_z3)
        bounds_safe = z3.And(i >= 0, i < arr_len)

        return z3.ForAll([i], z3.Implies(loop_range, bounds_safe))

    def generate_preconditions(self, array_len_name: str, min_len_expr: ast.expr) -> Optional[Any]:
        """Generate preconditions for array safety.

        Args:
            array_len_name: Name of variable holding array length
            min_len_expr: AST expression for minimum required length

        Returns:
            Z3 formula for preconditions
        """
        if not self.z3_available:
            return None

        arr_len = self.get_or_create_var(array_len_name, "int")
        min_len = self._expr_to_z3(min_len_expr)

        if min_len is None:
            return None

        # Preconditions: arr_len >= min_len and arr_len >= 0
        return z3.And(arr_len >= min_len, arr_len >= 0)

    def _expr_to_z3(self, expr: ast.expr) -> Optional[Any]:
        """Convert Python AST expression to Z3 expression.

        Args:
            expr: Python AST expression node

        Returns:
            Z3 expression or None if conversion fails
        """
        if not self.z3_available:
            return None

        if isinstance(expr, ast.Constant):
            # Constant value
            if isinstance(expr.value, int):
                return expr.value
            return None

        elif isinstance(expr, ast.Name):
            # Variable reference
            return self.get_or_create_var(expr.id, "int")

        elif isinstance(expr, ast.BinOp):
            # Binary operation
            left = self._expr_to_z3(expr.left)
            right = self._expr_to_z3(expr.right)

            if left is None or right is None:
                return None

            if isinstance(expr.op, ast.Add):
                return left + right
            elif isinstance(expr.op, ast.Sub):
                return left - right
            elif isinstance(expr.op, ast.Mult):
                return left * right
            elif isinstance(expr.op, ast.Div):
                # Integer division - use Z3 division if available
                if hasattr(left, "__truediv__"):
                    return left / right
                return None
            else:
                return None

        elif isinstance(expr, ast.Compare):
            # Comparison operation
            if len(expr.ops) != 1 or len(expr.comparators) != 1:
                return None  # Only handle simple comparisons

            left = self._expr_to_z3(expr.left)
            right = self._expr_to_z3(expr.comparators[0])

            if left is None or right is None:
                return None

            op = expr.ops[0]
            if isinstance(op, ast.Lt):
                return left < right
            elif isinstance(op, ast.LtE):
                return left <= right
            elif isinstance(op, ast.Gt):
                return left > right
            elif isinstance(op, ast.GtE):
                return left >= right
            elif isinstance(op, ast.Eq):
                return left == right
            elif isinstance(op, ast.NotEq):
                return left != right
            else:
                return None

        elif isinstance(expr, ast.UnaryOp):
            # Unary operation
            operand = self._expr_to_z3(expr.operand)
            if operand is None:
                return None

            if isinstance(expr.op, ast.USub):
                return -operand
            elif isinstance(expr.op, ast.UAdd):
                return operand
            else:
                return None

        else:
            # Unsupported expression type
            return None

    def add_constraint(self, constraint: Any) -> None:
        """Add a constraint to the formula.

        Args:
            constraint: Z3 constraint
        """
        if constraint is not None:
            self.constraints.append(constraint)

    def get_all_constraints(self) -> list[Any]:
        """Get all accumulated constraints.

        Returns:
            List of Z3 constraints
        """
        return self.constraints

    def clear_constraints(self) -> None:
        """Clear all constraints."""
        self.constraints = []

    def clear_variables(self) -> None:
        """Clear all variables."""
        self.variables = {}
