"""Haskell-specific for-loop conversion strategies.

Extends the base loop conversion system with Haskell-specific patterns
like foldl/foldM, list comprehensions, and do-notation.
"""

import ast
from typing import TYPE_CHECKING

from ..loop_conversion_strategies import ForLoopStrategy, LoopContext

if TYPE_CHECKING:
    from .converter import MultiGenPythonToHaskellConverter


class HaskellNestedListBuildingStrategy(ForLoopStrategy):
    """Strategy for nested loop pattern building 2D lists.

    Pattern:
        for i in range(rows):
            row = []
            for j in range(cols):
                row.append(value)
            matrix.append(row)

    Converts to:
        matrix = [[value | j <- range] | i <- range]  (pure)
        matrix <- foldM (...) matrix range  (IO)
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for nested list building pattern."""
        if len(node.body) != 3:
            return False

        # Check structure: assignment, inner loop, append
        if not (
            isinstance(node.body[0], (ast.Assign, ast.AnnAssign))
            and isinstance(node.body[1], ast.For)
            and isinstance(node.body[2], ast.Expr)
        ):
            return False

        # Check if first statement initializes empty list
        init_stmt = node.body[0]
        if isinstance(init_stmt, ast.Assign):
            if not (
                len(init_stmt.targets) == 1
                and isinstance(init_stmt.targets[0], ast.Name)
                and isinstance(init_stmt.value, ast.List)
                and len(init_stmt.value.elts) == 0
            ):
                return False
            row_var = init_stmt.targets[0].id
        elif isinstance(init_stmt, ast.AnnAssign):
            if not (
                isinstance(init_stmt.target, ast.Name)
                and isinstance(init_stmt.value, ast.List)
                and len(init_stmt.value.elts) == 0
            ):
                return False
            row_var = init_stmt.target.id
        else:
            return False

        # Check if inner loop appends to row
        inner_loop = node.body[1]
        if not (
            len(inner_loop.body) == 1
            and isinstance(inner_loop.body[0], ast.Expr)
            and isinstance(inner_loop.body[0].value, ast.Call)
        ):
            return False

        inner_call = inner_loop.body[0].value
        if not (
            isinstance(inner_call.func, ast.Attribute)
            and inner_call.func.attr == "append"
            and isinstance(inner_call.func.value, ast.Name)
            and inner_call.func.value.id == row_var
            and len(inner_call.args) == 1
        ):
            return False

        # Check if outer append adds row to matrix
        outer_append = node.body[2]
        if not (
            isinstance(outer_append.value, ast.Call)
            and isinstance(outer_append.value.func, ast.Attribute)
            and outer_append.value.func.attr == "append"
            and isinstance(outer_append.value.func.value, ast.Name)
            and len(outer_append.value.args) == 1
            and isinstance(outer_append.value.args[0], ast.Name)
            and outer_append.value.args[0].id == row_var
        ):
            return False

        return True

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert nested list building to Haskell list comprehension."""
        converter: MultiGenPythonToHaskellConverter = context.converter  # type: ignore

        # Extract components
        var_name = converter._to_haskell_var_name(node.target.id) if isinstance(node.target, ast.Name) else "i"
        iterable = converter._convert_expression(node.iter)

        inner_loop = node.body[1]
        assert isinstance(inner_loop, ast.For)
        outer_append = node.body[2]
        assert isinstance(outer_append, ast.Expr)
        inner_stmt = inner_loop.body[0]
        assert isinstance(inner_stmt, ast.Expr) and isinstance(inner_stmt.value, ast.Call)
        inner_call = inner_stmt.value

        matrix_var = converter._to_haskell_var_name(outer_append.value.func.value.id)  # type: ignore
        inner_var = (
            converter._to_haskell_var_name(inner_loop.target.id) if isinstance(inner_loop.target, ast.Name) else "j"
        )
        inner_iterable = converter._convert_expression(inner_loop.iter)
        assert len(inner_call.args) > 0
        append_expr = converter._convert_expression(inner_call.args[0])

        # Generate nested list comprehension
        if context.current_function != "main":
            return f"{matrix_var} = [[{append_expr} | {inner_var} <- {inner_iterable}] | {var_name} <- {iterable}]"
        else:
            # In IO context, use foldM
            return f"{matrix_var} <- foldM (\\acc {var_name} -> return (acc ++ [[{append_expr} | {inner_var} <- {inner_iterable}]])) {matrix_var} ({iterable})"


class HaskellListAppendStrategy(ForLoopStrategy):
    """Strategy for simple list.append() pattern.

    Pattern:
        for i in iter:
            list.append(expr)

    Converts to:
        list = foldl (\\acc i -> acc ++ [expr]) list iter  (pure)
        list <- foldM (\\acc i -> return (acc ++ [expr])) list iter  (IO)
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for list append pattern."""
        if len(node.body) != 1:
            return False

        stmt = node.body[0]
        if not (isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call)):
            return False

        call = stmt.value
        return (
            isinstance(call.func, ast.Attribute)
            and call.func.attr == "append"
            and isinstance(call.func.value, ast.Name)
            and len(call.args) == 1
        )

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert list append to foldl/foldM."""
        converter: MultiGenPythonToHaskellConverter = context.converter  # type: ignore

        var_name = converter._to_haskell_var_name(node.target.id) if isinstance(node.target, ast.Name) else "i"
        iterable = converter._convert_expression(node.iter)

        stmt = node.body[0]
        assert isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call)
        call = stmt.value
        list_var = converter._to_haskell_var_name(call.func.value.id)  # type: ignore
        assert len(call.args) > 0
        append_expr = converter._convert_expression(call.args[0])

        if context.current_function != "main":
            return f"{list_var} = foldl (\\acc {var_name} -> acc ++ [{append_expr}]) {list_var} ({iterable})"
        else:
            return f"{list_var} <- foldM (\\acc {var_name} -> return (acc ++ [{append_expr}])) {list_var} ({iterable})"


class HaskellAccumulationStrategy(ForLoopStrategy):
    """Strategy for accumulation pattern with augmented assignment.

    Pattern:
        for i in iter:
            var += expr

    Converts to:
        var = foldl (\\acc i -> acc + expr) var iter  (pure)
        var <- foldM (\\acc i -> return (acc + expr)) var iter  (IO)
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for accumulation pattern."""
        if len(node.body) != 1:
            return False

        stmt = node.body[0]
        return isinstance(stmt, ast.AugAssign) and isinstance(stmt.target, ast.Name)

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert accumulation to foldl/foldM."""
        converter: MultiGenPythonToHaskellConverter = context.converter  # type: ignore

        var_name = converter._to_haskell_var_name(node.target.id) if isinstance(node.target, ast.Name) else "i"
        iterable = converter._convert_expression(node.iter)

        stmt = node.body[0]
        assert isinstance(stmt, ast.AugAssign)
        var_name_target = converter._to_haskell_var_name(stmt.target.id)  # type: ignore
        value_expr = converter._convert_expression(stmt.value)
        op = converter._convert_operator(stmt.op)

        if context.current_function != "main":
            return f"{var_name_target} = foldl (\\acc {var_name} -> acc {op} ({value_expr})) {var_name_target} ({iterable})"
        else:
            return f"{var_name_target} <- foldM (\\acc {var_name} -> return (acc {op} ({value_expr}))) {var_name_target} ({iterable})"


class HaskellAssignmentInMainStrategy(ForLoopStrategy):
    """Strategy for simple assignment in main (IO context).

    Pattern (in main):
        for i in iter:
            var = expr

    Converts to:
        var <- foldM (\\acc i -> return (expr)) var iter
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for assignment in main pattern."""
        if context.current_function != "main":
            return False

        if len(node.body) != 1:
            return False

        stmt = node.body[0]
        return isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name)

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert assignment to foldM."""
        converter: MultiGenPythonToHaskellConverter = context.converter  # type: ignore

        var_name = converter._to_haskell_var_name(node.target.id) if isinstance(node.target, ast.Name) else "i"
        iterable = converter._convert_expression(node.iter)

        stmt = node.body[0]
        assert isinstance(stmt, ast.Assign)
        updated_var = converter._to_haskell_var_name(stmt.targets[0].id)  # type: ignore
        value_expr = converter._convert_expression(stmt.value)

        return f"{updated_var} <- foldM (\\acc {var_name} -> return ({value_expr})) {updated_var} ({iterable})"


def create_haskell_loop_converter() -> "ForLoopConverter":  # type: ignore[name-defined]
    """Create ForLoopConverter configured for Haskell.

    Returns:
        ForLoopConverter with Haskell-specific strategies
    """
    from ..loop_conversion_strategies import ForLoopConverter

    strategies = [
        HaskellNestedListBuildingStrategy(),
        HaskellListAppendStrategy(),
        HaskellAccumulationStrategy(),
        HaskellAssignmentInMainStrategy(),
        # More strategies can be added here for other patterns
    ]

    return ForLoopConverter(strategies)


__all__ = [
    "HaskellNestedListBuildingStrategy",
    "HaskellListAppendStrategy",
    "HaskellAccumulationStrategy",
    "HaskellAssignmentInMainStrategy",
    "create_haskell_loop_converter",
]
