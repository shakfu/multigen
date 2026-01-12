"""Statement Visitor Pattern for Haskell Code Generation.

This module implements the visitor pattern to convert Python statements to Haskell,
reducing complexity and improving maintainability of the Haskell converter.
"""

import ast
from abc import ABC, abstractmethod
from typing import Optional


class HaskellStatementVisitor(ABC):
    """Abstract visitor for converting Python statements to Haskell."""

    def __init__(self, converter: "MultiGenPythonToHaskellConverter") -> None:  # type: ignore[name-defined]
        """Initialize visitor with reference to converter for helper methods.

        Args:
            converter: Reference to MultiGenPythonToHaskellConverter for accessing helper methods
        """
        self.converter = converter

    @abstractmethod
    def visit_assign(self, node: ast.Assign) -> Optional[str]:
        """Visit assignment statement."""
        pass

    @abstractmethod
    def visit_ann_assign(self, node: ast.AnnAssign) -> Optional[str]:
        """Visit annotated assignment statement."""
        pass

    @abstractmethod
    def visit_if(self, node: ast.If) -> Optional[str]:
        """Visit if statement."""
        pass

    @abstractmethod
    def visit_return(self, node: ast.Return) -> Optional[str]:
        """Visit return statement."""
        pass

    @abstractmethod
    def visit_expr(self, node: ast.Expr) -> Optional[str]:
        """Visit expression statement."""
        pass

    def visit(self, stmt: ast.stmt) -> Optional[str]:
        """Dispatch statement to appropriate visitor method.

        Args:
            stmt: Python AST statement node

        Returns:
            Converted Haskell code or None
        """
        if isinstance(stmt, ast.Assign):
            return self.visit_assign(stmt)
        elif isinstance(stmt, ast.AnnAssign):
            return self.visit_ann_assign(stmt)
        elif isinstance(stmt, ast.If):
            return self.visit_if(stmt)
        elif isinstance(stmt, ast.Return):
            return self.visit_return(stmt)
        elif isinstance(stmt, ast.Expr):
            return self.visit_expr(stmt)
        else:
            # Fallback to converter's _convert_statement
            return self.converter._convert_statement(stmt)


class MainFunctionVisitor(HaskellStatementVisitor):
    """Visitor for main() function with IO do-notation."""

    def __init__(self, converter: "HaskellConverter") -> None:  # type: ignore[name-defined]
        super().__init__(converter)
        self.skip_initial_bindings: set[int] = set()

    def set_skip_bindings(self, skip: set[int]) -> None:
        """Set indices of bindings to skip (overridden by conditionals)."""
        self.skip_initial_bindings = skip

    def visit_assign(self, node: ast.Assign) -> Optional[str]:
        """Convert assignment to let binding in do-notation."""
        if len(node.targets) != 1:
            return None

        target = node.targets[0]
        if not isinstance(target, ast.Name):
            return None

        var_name = self.converter._to_haskell_var_name(target.id)
        if node.value:
            value = self.converter._convert_expression(node.value)
            return f"{var_name} = {value}"
        return None

    def visit_ann_assign(self, node: ast.AnnAssign) -> Optional[str]:
        """Convert annotated assignment to let binding with optional type annotation."""
        if not isinstance(node.target, ast.Name):
            return None

        var_name = self.converter._to_haskell_var_name(node.target.id)
        if node.value:
            value = self.converter._convert_expression(node.value)

            # Add inline type annotation if the type is concrete (not generic)
            if node.annotation:
                haskell_type = self.converter._convert_type_annotation(node.annotation)
                # Only add annotation for concrete types (not polymorphic "a" or "[a]")
                if haskell_type and "a" not in haskell_type:
                    # Use inline type annotation: let var :: Type = value
                    return f"{var_name} :: {haskell_type} = {value}"

            return f"{var_name} = {value}"
        return None

    def visit_if(self, node: ast.If) -> Optional[str]:
        """Convert if statement, handling conditional assignments specially."""
        # Check if this generates a binding (var = if ... then ... else ...)
        converted = self.converter._convert_statement(node)
        if converted and "=" in converted and not converted.startswith("if "):
            # It's a binding from conditional assignment
            return converted
        else:
            # Regular if-statement
            return converted

    def visit_return(self, node: ast.Return) -> Optional[str]:
        """Ignore return statements in main (Haskell main returns void)."""
        return None

    def visit_expr(self, node: ast.Expr) -> Optional[str]:
        """Convert expression statement."""
        # Skip docstrings
        if isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
            return None
        return self.converter._convert_statement(node)


class PureFunctionVisitor(HaskellStatementVisitor):
    """Visitor for pure functions (non-main)."""

    def __init__(self, converter: "HaskellConverter") -> None:  # type: ignore[name-defined]
        super().__init__(converter)
        self.var_init_values: dict[str, str] = {}

    def visit_assign(self, node: ast.Assign) -> Optional[str]:
        """Convert assignment to where-clause or let-expression."""
        if len(node.targets) != 1:
            return None

        target = node.targets[0]
        if isinstance(target, ast.Name):
            var_name = self.converter._to_haskell_var_name(target.id)
            if node.value:
                init_value = self.converter._convert_expression(node.value)
                self.var_init_values[var_name] = init_value
                return f"{var_name} = {init_value}"
        return None

    def visit_ann_assign(self, node: ast.AnnAssign) -> Optional[str]:
        """Convert annotated assignment."""
        if isinstance(node.target, ast.Name):
            var_name = self.converter._to_haskell_var_name(node.target.id)
            if node.value:
                init_value = self.converter._convert_expression(node.value)
                self.var_init_values[var_name] = init_value
                return f"{var_name} = {init_value}"
        return None

    def visit_if(self, node: ast.If) -> Optional[str]:
        """Convert if statement."""
        return self.converter._convert_statement(node)

    def visit_return(self, node: ast.Return) -> Optional[str]:
        """Convert return statement (final expression in Haskell)."""
        return self.converter._convert_statement(node)

    def visit_expr(self, node: ast.Expr) -> Optional[str]:
        """Convert expression statement."""
        # Skip docstrings
        if isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
            return None
        return self.converter._convert_statement(node)


class FunctionBodyAnalyzer:
    """Analyzer for function body patterns and optimizations."""

    @staticmethod
    def find_conditional_assignment_pairs(stmts: list[ast.stmt]) -> set[int]:
        """Find indices of initial bindings that are overridden by conditional assignments.

        Pattern: var = init_value; if cond: var = other_value

        Args:
            stmts: List of Python AST statements

        Returns:
            Set of indices to skip (initial bindings)
        """
        skip_indices = set()

        for i, stmt in enumerate(stmts):
            if isinstance(stmt, (ast.Assign, ast.AnnAssign)):
                # Check if followed by an if-statement that reassigns same variable
                if i + 1 < len(stmts):
                    next_stmt = stmts[i + 1]
                    if isinstance(next_stmt, ast.If):
                        # Check if if-body has single assignment to same variable
                        if (
                            len(next_stmt.body) == 1
                            and isinstance(next_stmt.body[0], ast.Assign)
                            and not next_stmt.orelse
                        ):
                            # Get variable names
                            init_var = FunctionBodyAnalyzer._get_assignment_target(stmt)
                            if_var = FunctionBodyAnalyzer._get_assignment_target(next_stmt.body[0])

                            # If same variable, skip the initial binding
                            if init_var and if_var and init_var == if_var:
                                skip_indices.add(i)

        return skip_indices

    @staticmethod
    def _get_assignment_target(stmt: ast.stmt) -> Optional[str]:
        """Extract variable name from assignment statement."""
        if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1:
            if isinstance(stmt.targets[0], ast.Name):
                return stmt.targets[0].id
        elif isinstance(stmt, ast.AnnAssign):
            if isinstance(stmt.target, ast.Name):
                return stmt.target.id
        return None

    @staticmethod
    def detect_early_return_pattern(stmts: list[ast.stmt]) -> Optional[tuple[ast.If, ast.Return]]:
        """Detect early return pattern: if cond: return X; ...; return Y.

        Args:
            stmts: Filtered body statements (no docstrings)

        Returns:
            Tuple of (if_stmt, final_return) if pattern detected, None otherwise
        """
        # Pattern 1: if cond: return X; return Y (exactly 2 statements)
        if (
            len(stmts) == 2
            and isinstance(stmts[0], ast.If)
            and len(stmts[0].body) == 1
            and isinstance(stmts[0].body[0], ast.Return)
            and isinstance(stmts[1], ast.Return)
            and not stmts[0].orelse
        ):
            return (stmts[0], stmts[1])

        # Pattern 2: if cond: return X; ...(bindings)...; return Y (>= 2 statements)
        if (
            len(stmts) >= 2
            and isinstance(stmts[0], ast.If)
            and len(stmts[0].body) == 1
            and isinstance(stmts[0].body[0], ast.Return)
            and isinstance(stmts[-1], ast.Return)
            and not stmts[0].orelse
        ):
            # Check that all middle statements are bindings (assignments)
            for stmt in stmts[1:-1]:
                if not isinstance(stmt, (ast.Assign, ast.AnnAssign)):
                    return None  # Not all bindings, can't use this pattern
            return (stmts[0], stmts[-1])

        return None

    @staticmethod
    def eliminate_duplicate_bindings(body_stmts: list[Optional[str]], var_init_values: dict[str, str]) -> list[str]:
        """Eliminate duplicate bindings (Haskell doesn't allow variable redefinition).

        Args:
            body_stmts: List of converted statement strings
            var_init_values: Mapping of variable names to initial values

        Returns:
            Filtered list with duplicates removed
        """
        import re

        seen_vars: dict[str, list[int]] = {}

        # Find all bindings
        for i, stmt_str in enumerate(body_stmts):
            if stmt_str and "=" in stmt_str and not stmt_str.startswith("--"):
                if not any(op in stmt_str for op in ["if", "then", "<=", ">=", "==", "/="]):
                    parts = stmt_str.split("=", 1)
                    if len(parts) == 2:
                        var_name = parts[0].strip()
                        if var_name and var_name.replace("_", "").isalnum():
                            if var_name not in seen_vars:
                                seen_vars[var_name] = []
                            seen_vars[var_name].append(i)

        # Handle duplicates
        for var_name, indices in seen_vars.items():
            if len(indices) > 1:
                accumulated_value = var_init_values.get(var_name, var_name)

                for i, idx in enumerate(indices):
                    stmt_str = body_stmts[idx]
                    if stmt_str and "=" in stmt_str:
                        parts = stmt_str.split("=", 1)
                        value_expr = parts[1].strip()

                        # Replace self-references with accumulated value
                        pattern = rf"\b{re.escape(var_name)}\b"
                        safe_replacement = accumulated_value.replace("\\", r"\\")
                        value_expr = re.sub(pattern, safe_replacement, value_expr)

                        # Replace variable references in folds
                        for v, init_val in var_init_values.items():
                            if v != var_name and f") {v} (" in value_expr:
                                value_expr = value_expr.replace(f") {v} (", f") {init_val} (")

                        accumulated_value = value_expr

                        # Mark all but last for removal
                        if i < len(indices) - 1:
                            body_stmts[idx] = None
                        else:
                            body_stmts[idx] = f"{var_name} = {value_expr}"
            else:
                # Single binding - replace init values in folds
                idx = indices[0]
                stmt_str = body_stmts[idx]
                if stmt_str and "=" in stmt_str:
                    parts = stmt_str.split("=", 1)
                    value_expr = parts[1].strip()
                    for v, init_val in var_init_values.items():
                        if f") {v} (" in value_expr:
                            value_expr = value_expr.replace(f") {v} (", f") {init_val} (")
                    body_stmts[idx] = f"{parts[0].strip()} = {value_expr}"

        # Filter out removed statements
        return [stmt for stmt in body_stmts if stmt is not None]


__all__ = [
    "HaskellStatementVisitor",
    "MainFunctionVisitor",
    "PureFunctionVisitor",
    "FunctionBodyAnalyzer",
]
