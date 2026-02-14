"""Refactored function converter using visitor pattern for Haskell backend.

This module provides a cleaner implementation of _convert_function using the visitor pattern,
reducing complexity from 69 to ~15-20.
"""

import ast
from typing import TYPE_CHECKING, Optional

from .statement_visitor import FunctionBodyAnalyzer, MainFunctionVisitor, PureFunctionVisitor

if TYPE_CHECKING:
    from .converter import MultiGenPythonToHaskellConverter


def _detect_needed_constraints(node: ast.FunctionDef) -> set[str]:
    """Detect which type constraints are needed for a function.

    Returns:
        Set of constraint names (e.g., {'Ord', 'Num'})
    """
    constraints = set()

    class ConstraintVisitor(ast.NodeVisitor):
        def visit_Compare(self, node: ast.Compare) -> None:
            # Comparison operators require Ord or Eq
            for op in node.ops:
                if isinstance(op, (ast.Lt, ast.LtE, ast.Gt, ast.GtE)):
                    constraints.add("Ord")
                # Note: Eq is implied by Ord, so we don't need to add it separately
            self.generic_visit(node)

    visitor = ConstraintVisitor()
    visitor.visit(node)
    return constraints


def convert_function_with_visitor(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef) -> str:
    """Convert Python function to Haskell using visitor pattern.

    This is a refactored version of _convert_function that delegates complexity
    to specialized visitor classes.

    Args:
        converter: Reference to MultiGenPythonToHaskellConverter
        node: Python function definition AST node

    Returns:
        Converted Haskell function code
    """
    func_name = converter._to_haskell_function_name(node.name)

    # Check for array parameter mutations (Haskell constraint)
    is_mutating, mutated_params = _mutates_array_parameter(converter, node)
    if is_mutating and node.name != "main":
        params_list = ", ".join(sorted(mutated_params))
        from ..errors import UnsupportedFeatureError

        raise UnsupportedFeatureError(
            f"Function '{node.name}' mutates array parameter(s): {params_list}. "
            f"In-place array mutations cannot be directly translated to pure Haskell "
            f"(which lacks mutable arrays in its standard library). "
            f"Consider rewriting the algorithm in functional style (e.g., for sorting, "
            f"use filter-based quicksort: qsort (p:xs) = qsort [x|x<-xs,x<p] ++ [p] ++ qsort [x|x<-xs,x>=p])"
        )

    # Detect generator functions (contain yield)
    is_generator = any(isinstance(n, (ast.Yield, ast.YieldFrom)) for n in ast.walk(node))

    # Handle main function specially
    if node.name == "main":
        return _convert_main_function(converter, node)

    # Handle generator functions
    if is_generator:
        return _convert_generator_function(converter, node, func_name)

    # Handle regular pure functions
    return _convert_pure_function(converter, node, func_name)


def _mutates_array_parameter(
    converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef
) -> tuple[bool, set[str]]:
    """Check if function mutates array parameters.

    Args:
        converter: Reference to converter
        node: Function definition node

    Returns:
        Tuple of (is_mutating, set of mutated parameter names)
    """
    param_names = {arg.arg for arg in node.args.args}
    mutated_params = set()

    for stmt in ast.walk(node):
        if isinstance(stmt, ast.Assign):
            if len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Subscript):
                if isinstance(stmt.targets[0].value, ast.Name):
                    var_name = stmt.targets[0].value.id
                    if var_name in param_names:
                        mutated_params.add(var_name)

    return len(mutated_params) > 0, mutated_params


def _convert_main_function(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef) -> str:
    """Convert main function with IO do-notation.

    Args:
        converter: Reference to converter
        node: Function definition node

    Returns:
        Converted Haskell main function
    """
    converter.current_function = "main"
    converter.declared_vars = set()

    # Analyze for conditional assignment patterns
    skip_indices = FunctionBodyAnalyzer.find_conditional_assignment_pairs(node.body)

    # Create visitor for main function
    visitor = MainFunctionVisitor(converter)
    visitor.set_skip_bindings(skip_indices)

    do_lines = []
    for idx, stmt in enumerate(node.body):
        # Skip initial bindings that are overridden
        if idx in skip_indices:
            continue

        # Skip docstrings (handled in visitor)
        if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            continue

        # Skip returns (handled in visitor)
        if isinstance(stmt, ast.Return):
            continue

        converted_stmt = visitor.visit(stmt)
        if not converted_stmt:
            continue

        # Categorize and add to do notation
        if isinstance(stmt, (ast.Assign, ast.AnnAssign)):
            do_lines.append(f"  let {converted_stmt}")
        elif isinstance(stmt, ast.If):
            if "=" in converted_stmt and not converted_stmt.startswith("if "):
                do_lines.append(f"  let {converted_stmt}")
            else:
                do_lines.append(f"  {converted_stmt}")
        else:
            do_lines.append(f"  {converted_stmt}")

    # Build main body
    signature = "main :: IO ()"

    if not do_lines:
        body = 'main = printValue "No statements"'
    elif len(do_lines) == 1 and not do_lines[0].strip().startswith("let"):
        body = f"main = {do_lines[0].strip()}"
    else:
        body = "main = do\n" + "\n".join(do_lines)

    converter.current_function = None
    converter.declared_vars = set()
    return f"{signature}\n{body}"


def _convert_pure_function(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, func_name: str) -> str:
    """Convert pure (non-main) function.

    Args:
        converter: Reference to converter
        node: Function definition node
        func_name: Converted Haskell function name

    Returns:
        Converted Haskell function
    """
    # Extract parameters
    params = []
    for arg in node.args.args:
        param_name = converter._to_haskell_var_name(arg.arg)
        param_type = "a"  # Default generic type
        if arg.annotation:
            param_type = converter._convert_type_annotation(arg.annotation)
            # Check for 2D list usage
            if param_type == "[a]":
                if _param_used_as_2d_list(converter, node, arg.arg):
                    param_type = "[[Int]]"
        params.append((param_name, param_type))

    # Determine return type
    return_type = "a"  # Default generic type
    if node.returns:
        return_type = converter._convert_type_annotation(node.returns)
        # Infer dict value type if possible
        if return_type == "Dict String a":
            inferred_value_type = converter._infer_dict_value_type(node)
            if inferred_value_type:
                return_type = f"Dict String {inferred_value_type}"
        # Check for 2D list return
        elif return_type == "[a]":
            if _returns_2d_list(converter, node):
                return_type = "[[Int]]"

    # Build function signature
    if params:
        param_types = [param[1] for param in params]
        all_types = param_types + [return_type]
        base_signature = " -> ".join(all_types)
    else:
        base_signature = return_type

    # Detect needed type constraints
    constraints = _detect_needed_constraints(node)

    # Add constraints if function uses polymorphic types ([a], a, etc.)
    has_polymorphic = any("a" in t for t in ([return_type] + ([p[1] for p in params] if params else [])))

    if constraints and has_polymorphic:
        constraint_list = [f"{c} a" for c in sorted(constraints)]
        constraint_str = ", ".join(constraint_list)
        signature = f"{func_name} :: ({constraint_str}) => {base_signature}"
    else:
        signature = f"{func_name} :: {base_signature}"

    # Convert function body
    converter.current_function = func_name
    converter.declared_vars = set()

    # Filter docstrings
    filtered_body = [
        stmt
        for stmt in node.body
        if not (
            isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str)
        )
    ]

    # Check for early return pattern
    early_return = FunctionBodyAnalyzer.detect_early_return_pattern(filtered_body)
    if early_return:
        if_stmt, final_return = early_return
        condition = converter._convert_expression(if_stmt.test)
        # The pattern guarantees if_stmt.body[0] is ast.Return
        then_return = if_stmt.body[0]
        assert isinstance(then_return, ast.Return), "Early return pattern must have Return in if body"
        then_expr = then_return.value
        else_expr = final_return.value
        if then_expr is not None and else_expr is not None:
            then_value = converter._convert_expression(then_expr)
            else_value = converter._convert_expression(else_expr)

            # Handle bindings between the if and final return
            bindings: list[str] = []
            if len(filtered_body) > 2:
                # Convert the middle statements (bindings)
                visitor = PureFunctionVisitor(converter)
                for stmt in filtered_body[1:-1]:
                    converted_stmt = visitor.visit(stmt)
                    if converted_stmt:
                        bindings.append(converted_stmt)

            # Build function body with conditional
            body = f"if {condition} then {then_value} else {else_value}"
            if bindings:
                body = f"{body}\n  where\n    " + "\n    ".join(bindings)

            param_names = " ".join([p[0] for p in params])
            converter.current_function = None
            converter.declared_vars = set()
            param_pattern = f" {param_names}" if param_names else ""
            return f"{signature}\n{func_name}{param_pattern} = {body}"

    # Normal conversion using visitor
    visitor = PureFunctionVisitor(converter)
    body_stmts: list[Optional[str]] = []

    for stmt in filtered_body:
        converted_stmt = visitor.visit(stmt)
        if converted_stmt:
            body_stmts.append(converted_stmt)

    # Eliminate duplicate bindings
    final_stmts = FunctionBodyAnalyzer.eliminate_duplicate_bindings(body_stmts, visitor.var_init_values)

    # Build function definition
    param_names = " ".join([p[0] for p in params])

    if not final_stmts:
        body = "undefined"
    elif len(final_stmts) == 1:
        # Check if it's a binding (var = expr) vs a plain expression
        # A binding has the pattern: identifier = expression
        stmt_str = final_stmts[0]
        if "=" in stmt_str and not stmt_str.startswith(("if ", "Map.", "Set.", "(", "[")):
            # Check if it looks like a binding (starts with identifier)
            parts = stmt_str.split("=", 1)
            if parts[0].strip() and parts[0].strip().replace("_", "").isalnum():
                body = parts[1].strip()
            else:
                body = stmt_str
        else:
            body = stmt_str
    else:
        # Multi-statement: use where clause or let-in
        last_stmt = final_stmts[-1]
        bindings = final_stmts[:-1]

        if "=" in last_stmt:
            # Last is also a binding
            body = last_stmt.split("=", 1)[1].strip()
            if bindings:
                body = f"{body}\n  where\n    " + "\n    ".join(bindings + [last_stmt])
        else:
            # Last is an expression
            body = last_stmt
            if bindings:
                body = f"{last_stmt}\n  where\n    " + "\n    ".join(bindings)

    converter.current_function = None
    converter.declared_vars = set()
    param_pattern = f" {param_names}" if param_names else ""
    return f"{signature}\n{func_name}{param_pattern} = {body}"


def _param_used_as_2d_list(
    converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, param_name: str
) -> bool:
    """Check if parameter is used as 2D list."""
    return converter._param_used_as_2d_list(node, param_name)


def _returns_2d_list(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef) -> bool:
    """Check if function returns 2D list."""
    return converter._returns_2d_list(node)


def _convert_generator_function(
    converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, func_name: str
) -> str:
    """Convert generator function to Haskell using accumulation pattern.

    Generators are converted to functions that return a list by building
    up results via concatenation.
    """
    # Extract parameters
    params = []
    for arg in node.args.args:
        param_name = converter._to_haskell_var_name(arg.arg)
        param_type = "a"
        if arg.annotation:
            param_type = converter._convert_type_annotation(arg.annotation)
        params.append((param_name, param_type))

    # Determine element type from return annotation
    element_type = "Int"
    if node.returns:
        element_type = converter._convert_type_annotation(node.returns)

    # Return type is a list of the element type
    return_type = f"[{element_type}]"

    # Build type signature
    if params:
        param_types = [p[1] for p in params]
        all_types = param_types + [return_type]
        base_signature = " -> ".join(all_types)
    else:
        base_signature = return_type
    signature = f"{func_name} :: {base_signature}"

    # Convert function body using accumulation pattern
    # We generate a helper that accumulates results
    converter.current_function = func_name

    # For simple while-loop generators, generate a recursive helper
    # Pattern: def gen(n): i=0; while i<n: yield i; i+=1
    # -> gen n = genHelper 0 [] where genHelper i acc = if i < n then genHelper (i+1) (acc ++ [i]) else acc
    body = _convert_generator_body(converter, node, func_name, params)

    converter.current_function = None
    converter.declared_vars = set()

    param_names = " ".join([p[0] for p in params])
    param_pattern = f" {param_names}" if param_names else ""
    return f"{signature}\n{func_name}{param_pattern} = {body}"


def _convert_generator_body(
    converter: "MultiGenPythonToHaskellConverter",
    node: ast.FunctionDef,
    func_name: str,
    params: list[tuple[str, str]],
) -> str:
    """Convert generator body to Haskell accumulation pattern."""
    # Analyze the body to find while-loop or for-loop generators
    filtered_body = [
        stmt
        for stmt in node.body
        if not (
            isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str)
        )
    ]

    # Look for simple while-loop pattern:
    # var = init; while cond: yield expr; var += step
    init_stmts: list[ast.stmt] = []
    loop_stmt: Optional[ast.stmt] = None
    for stmt in filtered_body:
        if isinstance(stmt, ast.While):
            loop_stmt = stmt
            break
        elif isinstance(stmt, ast.For):
            loop_stmt = stmt
            break
        else:
            init_stmts.append(stmt)

    if loop_stmt is not None and isinstance(loop_stmt, ast.While):
        return _convert_while_generator(converter, init_stmts, loop_stmt, params)
    elif loop_stmt is not None and isinstance(loop_stmt, ast.For):
        return _convert_for_generator(converter, init_stmts, loop_stmt, params)

    # Fallback: collect all yield/yield from expressions into a list
    yield_parts = []
    for stmt in filtered_body:
        if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Yield) and stmt.value.value:
            yield_parts.append("[" + converter._convert_expression(stmt.value.value) + "]")
        elif isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.YieldFrom):
            yield_parts.append(converter._convert_expression(stmt.value.value))
    if yield_parts:
        return " ++ ".join(yield_parts)
    return "[]"


def _convert_while_generator(
    converter: "MultiGenPythonToHaskellConverter",
    init_stmts: list[ast.stmt],
    while_stmt: ast.While,
    params: list[tuple[str, str]],
) -> str:
    """Convert while-loop generator to recursive helper."""
    # Extract initial variable values
    init_vars: dict[str, str] = {}
    for stmt in init_stmts:
        if isinstance(stmt, (ast.Assign, ast.AnnAssign)):
            target = stmt.targets[0] if isinstance(stmt, ast.Assign) else stmt.target
            if isinstance(target, ast.Name) and hasattr(stmt, "value") and stmt.value:
                var_name = converter._to_haskell_var_name(target.id)
                init_vars[var_name] = converter._convert_expression(stmt.value)

    # Convert condition
    condition = converter._convert_expression(while_stmt.test)

    # Analyze loop body for yields and updates
    yield_exprs = []
    update_vars: dict[str, str] = {}

    for stmt in while_stmt.body:
        if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Yield) and stmt.value.value:
            yield_exprs.append(converter._convert_expression(stmt.value.value))
        elif isinstance(stmt, ast.AugAssign) and isinstance(stmt.target, ast.Name):
            var_name = converter._to_haskell_var_name(stmt.target.id)
            value = converter._convert_expression(stmt.value)
            op = stmt.op
            if isinstance(op, ast.Add):
                update_vars[var_name] = f"({var_name} + {value})"
            elif isinstance(op, ast.Sub):
                update_vars[var_name] = f"({var_name} - {value})"
            elif isinstance(op, ast.Mult):
                update_vars[var_name] = f"({var_name} * {value})"
            else:
                update_vars[var_name] = f"({var_name} + {value})"
        elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
            var_name = converter._to_haskell_var_name(stmt.targets[0].id)
            update_vars[var_name] = converter._convert_expression(stmt.value)

    # Handle conditional yield (yield inside if)
    for stmt in while_stmt.body:
        if isinstance(stmt, ast.If):
            for sub in stmt.body:
                if isinstance(sub, ast.Expr) and isinstance(sub.value, ast.Yield) and sub.value.value:
                    yield_exprs.append(converter._convert_expression(sub.value.value))

    if not yield_exprs:
        return "[]"

    # Build recursive helper
    helper_vars = list(init_vars.keys())
    helper_params = " ".join(helper_vars + ["acc"])
    init_args = " ".join(init_vars.values()) + " []"

    # Build recursive call arguments
    rec_args = []
    for var in helper_vars:
        if var in update_vars:
            rec_args.append(update_vars[var])
        else:
            rec_args.append(var)
    rec_args_str = " ".join(rec_args)

    yield_expr = yield_exprs[0]  # Use first yield expression
    return (
        f"helper {init_args}\n"
        f"  where\n"
        f"    helper {helper_params} = if {condition} then helper {rec_args_str} (acc ++ [{yield_expr}]) else acc"
    )


def _convert_for_generator(
    converter: "MultiGenPythonToHaskellConverter",
    init_stmts: list[ast.stmt],
    for_stmt: ast.For,
    params: list[tuple[str, str]],
) -> str:
    """Convert for-loop generator to list operation."""
    # Handle range-based for loop
    if isinstance(for_stmt.iter, ast.Call) and isinstance(for_stmt.iter.func, ast.Name):
        if for_stmt.iter.func.id == "range" and isinstance(for_stmt.target, ast.Name):
            loop_var = converter._to_haskell_var_name(for_stmt.target.id)
            args = for_stmt.iter.args

            # Build range expression
            if len(args) == 1:
                range_expr = f"[0..({converter._convert_expression(args[0])} - 1)]"
            elif len(args) == 2:
                start = converter._convert_expression(args[0])
                end = converter._convert_expression(args[1])
                range_expr = f"[{start}..({end} - 1)]"
            else:
                range_expr = "[0..0]"

            # Find yield expression in body
            yield_exprs = []
            for stmt in for_stmt.body:
                if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Yield) and stmt.value.value:
                    yield_exprs.append(converter._convert_expression(stmt.value.value))
                elif isinstance(stmt, ast.If):
                    for sub in stmt.body:
                        if isinstance(sub, ast.Expr) and isinstance(sub.value, ast.Yield) and sub.value.value:
                            cond = converter._convert_expression(stmt.test)
                            expr = converter._convert_expression(sub.value.value)
                            yield_exprs.append(f"if {cond} then [{expr}] else []")

            if yield_exprs:
                yield_expr = yield_exprs[0]
                # Check for conditional yield
                if "if " in yield_expr and "then [" in yield_expr:
                    return f"concatMap (\\{loop_var} -> {yield_expr}) {range_expr}"
                return f"map (\\{loop_var} -> {yield_expr}) {range_expr}"

    return "[]"


__all__ = ["convert_function_with_visitor"]
