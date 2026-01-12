"""OCaml-specific for-loop conversion strategies.

Extends the base loop conversion system with OCaml-specific patterns
like List.fold_left and mutable ref handling.
"""

import ast
from typing import TYPE_CHECKING

from ..loop_conversion_strategies import ForLoopStrategy, LoopContext

if TYPE_CHECKING:
    from .converter import MultiGenPythonToOCamlConverter


class OCamlSimpleAssignmentStrategy(ForLoopStrategy):
    """Strategy for simple assignment pattern.

    Pattern:
        for i in range(n):
            var = expr

    Converts to:
        var := List.fold_left (fun _ i -> expr) !var iter  (if mutable)
        let var = List.fold_left (fun _ i -> expr) var iter in  (if immutable)
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for simple assignment pattern."""
        converter: MultiGenPythonToOCamlConverter = context.converter  # type: ignore

        if len(node.body) != 1:
            return False

        # Check for no mutations (simple case)
        mutated_vars = converter._has_mutations(node.body)
        if mutated_vars:
            return False

        stmt = node.body[0]
        return isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name)

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert simple assignment to List.fold_left."""
        converter: MultiGenPythonToOCamlConverter = context.converter  # type: ignore

        if not isinstance(node.target, ast.Name):
            return "(* Complex for loop target not supported *)"

        target = converter._to_ocaml_var_name(node.target.id)
        iter_expr = converter._convert_expression(node.iter)

        stmt = node.body[0]
        assert isinstance(stmt, ast.Assign)
        updated_var = converter._to_ocaml_var_name(stmt.targets[0].id)  # type: ignore
        value_expr = converter._convert_expression(stmt.value)

        # Check if this is a mutable variable (ref)
        if stmt.targets[0].id in converter.mutable_vars:  # type: ignore
            # Use ref assignment
            return f"{updated_var} := List.fold_left (fun _ {target} -> {value_expr}) !{updated_var} ({iter_expr})"
        else:
            # Use let-binding for non-ref variables
            return f"let {updated_var} = List.fold_left (fun _ {target} -> {value_expr}) {updated_var} ({iter_expr}) in"


class OCamlAccumulationStrategy(ForLoopStrategy):
    """Strategy for accumulation pattern with augmented assignment.

    Pattern:
        for i in range(n):
            var += expr

    Converts to:
        var := List.fold_left (fun acc i -> acc + expr) !var iter  (if mutable)
        let var = List.fold_left (fun acc i -> acc + expr) var iter in  (if immutable)
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """Check for accumulation pattern."""
        converter: MultiGenPythonToOCamlConverter = context.converter  # type: ignore

        if len(node.body) != 1:
            return False

        # Check for single mutation (the accumulation variable)
        mutated_vars = converter._has_mutations(node.body)
        if len(mutated_vars) != 1:
            return False

        stmt = node.body[0]
        return isinstance(stmt, ast.AugAssign) and isinstance(stmt.target, ast.Name)

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert accumulation to List.fold_left."""
        converter: MultiGenPythonToOCamlConverter = context.converter  # type: ignore

        if not isinstance(node.target, ast.Name):
            return "(* Complex for loop target not supported *)"

        target = converter._to_ocaml_var_name(node.target.id)
        iter_expr = converter._convert_expression(node.iter)

        stmt = node.body[0]
        assert isinstance(stmt, ast.AugAssign)
        updated_var = converter._to_ocaml_var_name(stmt.target.id)  # type: ignore
        value_expr = converter._convert_expression(stmt.value)
        op = converter._convert_operator(stmt.op)

        # Check if this is a mutable variable (ref)
        if stmt.target.id in converter.mutable_vars:  # type: ignore
            # Use ref assignment
            return f"{updated_var} := List.fold_left (fun acc {target} -> acc {op} ({value_expr})) !{updated_var} ({iter_expr})"
        else:
            # Use let-binding for non-ref variables
            return f"let {updated_var} = List.fold_left (fun acc {target} -> acc {op} ({value_expr})) {updated_var} ({iter_expr}) in"


class OCamlGeneralLoopStrategy(ForLoopStrategy):
    """Strategy for general loop pattern using List.iter.

    Pattern:
        for i in iter:
            <multiple statements>

    Converts to:
        List.iter (fun i -> stmt1; stmt2; ...) iter
    """

    def can_handle(self, node: ast.For, context: LoopContext) -> bool:
        """This strategy handles all remaining cases."""
        return True  # Catch-all strategy

    def convert(self, node: ast.For, context: LoopContext) -> str:
        """Convert general loop to List.iter."""
        converter: MultiGenPythonToOCamlConverter = context.converter  # type: ignore

        if not isinstance(node.target, ast.Name):
            return "(* Complex for loop target not supported *)"

        target = converter._to_ocaml_var_name(node.target.id)
        iter_expr = converter._convert_expression(node.iter)

        # Convert body to let expressions
        body_lines = []
        for stmt in node.body:
            converted = converter._convert_statement(stmt)
            if isinstance(converted, list):
                body_lines.extend(converted)
            else:
                body_lines.append(converted)

        if body_lines:
            # Properly sequence statements with semicolons where needed
            sequenced = []
            for i, line in enumerate(body_lines):
                if i < len(body_lines) - 1:  # Not the last statement
                    if not line.rstrip().endswith(" in"):
                        sequenced.append(line + ";")
                    else:
                        sequenced.append(line)
                else:
                    # Last statement - if it ends with 'in', add ()
                    if line.rstrip().endswith(" in"):
                        sequenced.append(line)
                        sequenced.append("()")
                    else:
                        sequenced.append(line)

            body_str = " ".join(sequenced)
            return f"List.iter (fun {target} -> {body_str}) ({iter_expr})"
        else:
            return "(* Empty for loop *)"


def create_ocaml_loop_converter() -> "ForLoopConverter":  # type: ignore[name-defined]
    """Create ForLoopConverter configured for OCaml.

    Returns:
        ForLoopConverter with OCaml-specific strategies
    """
    from ..loop_conversion_strategies import ForLoopConverter

    strategies = [
        OCamlSimpleAssignmentStrategy(),
        OCamlAccumulationStrategy(),
        OCamlGeneralLoopStrategy(),  # Must be last (catch-all)
    ]

    return ForLoopConverter(strategies)


__all__ = [
    "OCamlSimpleAssignmentStrategy",
    "OCamlAccumulationStrategy",
    "OCamlGeneralLoopStrategy",
    "create_ocaml_loop_converter",
]
