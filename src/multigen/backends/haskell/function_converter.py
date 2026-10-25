"""Refactored function converter using visitor pattern for Haskell backend.

This module provides a cleaner implementation of _convert_function using the visitor pattern,
reducing complexity from 69 to ~15-20.
"""

import ast
import re
from typing import TYPE_CHECKING, Optional

from ..errors import UnsupportedFeatureError
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


def _is_dict_annotation(annotation: Optional[ast.expr]) -> bool:
    """Whether an annotation names dict, bare or subscripted."""
    if isinstance(annotation, ast.Subscript):
        annotation = annotation.value
    return isinstance(annotation, ast.Name) and annotation.id in ("dict", "Dict")


def _dict_variables(node: ast.FunctionDef) -> set[str]:
    """Return names the function annotates as dict or binds to a dict literal or comprehension."""
    names = {arg.arg for arg in node.args.args if _is_dict_annotation(arg.annotation)}
    for sub in ast.walk(node):
        if isinstance(sub, ast.AnnAssign) and isinstance(sub.target, ast.Name):
            if _is_dict_annotation(sub.annotation) or isinstance(sub.value, (ast.Dict, ast.DictComp)):
                names.add(sub.target.id)
        elif isinstance(sub, ast.Assign) and len(sub.targets) == 1 and isinstance(sub.targets[0], ast.Name):
            if isinstance(sub.value, (ast.Dict, ast.DictComp)):
                names.add(sub.targets[0].id)
    return names


def _scalar_variables(node: ast.FunctionDef) -> dict[str, str]:
    """Map names annotated int, float or bool, and range loop targets, to that type; drop conflicts."""
    found: list[tuple[str, str]] = []
    annotated = [(arg.arg, arg.annotation) for arg in node.args.args]
    for sub in ast.walk(node):
        if isinstance(sub, ast.AnnAssign) and isinstance(sub.target, ast.Name):
            annotated.append((sub.target.id, sub.annotation))
        elif (
            isinstance(sub, ast.For)
            and isinstance(sub.target, ast.Name)
            and isinstance(sub.iter, ast.Call)
            and isinstance(sub.iter.func, ast.Name)
            and sub.iter.func.id == "range"
        ):
            found.append((sub.target.id, "int"))
    for name, annotation in annotated:
        if isinstance(annotation, ast.Name) and annotation.id in ("int", "float", "bool"):
            found.append((name, annotation.id))
    types: dict[str, str] = {}
    conflicts: set[str] = set()
    for name, type_name in found:
        if types.setdefault(name, type_name) != type_name:
            conflicts.add(name)
    return {name: t for name, t in types.items() if name not in conflicts}


def convert_function_with_visitor(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef) -> str:
    """Convert a Python function, with its dict and scalar variables known while converting it."""
    converter.dict_vars = _dict_variables(node)
    converter.scalar_vars = _scalar_variables(node)
    try:
        return _convert_function(converter, node)
    finally:
        converter.dict_vars = set()
        converter.scalar_vars = {}


def _convert_function(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef) -> str:
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
    try:
        return _convert_pure_function(converter, node, func_name)
    except UnsupportedFeatureError:
        # Loops and rebindings the where-clause form cannot order; translate statement by statement
        sequential = _convert_sequential_function(converter, node, func_name)
        if sequential is None:
            raise
        return sequential


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


def _simple_target(stmt: ast.stmt) -> Optional[str]:
    """Return the variable an assignment statement binds, if it is a plain name."""
    if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
        return stmt.targets[0].id
    if isinstance(stmt, (ast.AnnAssign, ast.AugAssign)) and isinstance(stmt.target, ast.Name):
        return stmt.target.id
    return None


def _item_update(converter: "MultiGenPythonToHaskellConverter", target: ast.expr, value: str) -> tuple[str, str]:
    """Return the variable an item assignment rebinds and its updated value; nested targets compose."""
    if isinstance(target, ast.Name):
        return converter._to_haskell_var_name(target.id), value
    if not isinstance(target, ast.Subscript) or isinstance(target.slice, ast.Slice):
        raise UnsupportedFeatureError(f"Haskell backend does not support assignment to {ast.unparse(target)}")
    container = converter._convert_expression(target.value)
    key = converter._convert_expression(target.slice)
    if isinstance(target.value, ast.Name) and target.value.id in converter.dict_vars:
        updated = f"Map.insert ({key}) ({value}) {container}"
    else:
        updated = f"updateAt ({key}) ({value}) ({container})"
    return _item_update(converter, target.value, updated)


def _rebinding_line(converter: "MultiGenPythonToHaskellConverter", stmt: ast.stmt) -> Optional[tuple[str, str]]:
    """Convert an item assignment in main to a bind, returning the name bound and the line.

    Returns None for a plain-name assignment, which the visitor converts.
    """
    if not isinstance(stmt, ast.Assign) or len(stmt.targets) != 1 or isinstance(stmt.targets[0], ast.Name):
        return None
    name, updated = _item_update(converter, stmt.targets[0], converter._convert_expression(stmt.value))
    return name, f"{name} <- return ({updated})"


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
    bound: set[str] = set()
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

        rebinding = _rebinding_line(converter, stmt)
        if rebinding is not None:
            name, line = rebinding
            bound.add(name)
            do_lines.append(f"  {line}")
            continue

        converted_stmt = visitor.visit(stmt)
        if not converted_stmt:
            continue

        target = _simple_target(stmt)
        if target is not None:
            name = converter._to_haskell_var_name(target)
            if isinstance(stmt, ast.AugAssign) or name in bound:
                # let is recursive, so `let x = x + 1` never terminates. A bind
                # shadows x with a value computed from the previous x.
                value = converted_stmt.split("=", 1)[1].strip()
                do_lines.append(f"  {name} <- return ({value})")
                continue
            bound.add(name)

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

    # A do block cannot end in a let; Python discards main's return value.
    if do_lines and do_lines[-1].strip().startswith("let"):
        do_lines.append("  return ()")

    # Build main body
    signature = "main :: IO ()"

    if not do_lines:
        body = "main = return ()"
    elif len(do_lines) == 1 and not do_lines[0].strip().startswith("let"):
        body = f"main = {do_lines[0].strip()}"
    else:
        body = "main = do\n" + "\n".join(do_lines)

    converter.current_function = None
    converter.declared_vars = set()
    return f"{signature}\n{body}"


def _pure_signature(
    converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, func_name: str
) -> tuple[str, list[tuple[str, str]]]:
    """Return a pure function's type signature line and its (name, type) parameters."""
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
    # Match the type variable `a` as a word; a substring test also matches "Map".
    has_polymorphic = any(re.search(r"\ba\b", t) for t in ([return_type] + ([p[1] for p in params] if params else [])))

    if constraints and has_polymorphic:
        constraint_list = [f"{c} a" for c in sorted(constraints)]
        constraint_str = ", ".join(constraint_list)
        signature = f"{func_name} :: ({constraint_str}) => {base_signature}"
    else:
        signature = f"{func_name} :: {base_signature}"
    return signature, params


def _convert_pure_function(converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, func_name: str) -> str:
    """Convert pure (non-main) function.

    Args:
        converter: Reference to converter
        node: Function definition node
        func_name: Converted Haskell function name

    Returns:
        Converted Haskell function
    """
    signature, params = _pure_signature(converter, node, func_name)

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

    # A where clause cannot rebind a name, so an if that assigns needs the sequential translation
    for stmt in filtered_body:
        if isinstance(stmt, ast.If) and _assigned_names(stmt.body + stmt.orelse):
            raise UnsupportedFeatureError(f"if at line {stmt.lineno} assigns in a pure function")

    # Leading `if c: raise E(m)` guards wrap the body as `if c then throw (E m) else ...`
    guards: list[ast.If] = []
    while len(filtered_body) > 1 and _is_guard_raise(filtered_body[0]):
        guard = filtered_body.pop(0)
        assert isinstance(guard, ast.If)
        guards.append(guard)

    # Check for early return pattern
    # Bindings before an early return join its where clause, if each name is bound once
    lead = 0
    while lead < len(filtered_body) and _bound_name(filtered_body[lead]):
        lead += 1
    early_return = FunctionBodyAnalyzer.detect_early_return_pattern(filtered_body[lead:])
    binding_stmts = [*filtered_body[:lead], *filtered_body[lead + 1 : -1]]
    names = [_bound_name(stmt) for stmt in binding_stmts]
    if early_return and len(set(names)) < len(names):
        early_return = None
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

            # Bindings before the if and between the if and the final return
            bindings: list[str] = []
            visitor = PureFunctionVisitor(converter)
            for stmt in binding_stmts:
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
            return f"{signature}\n{func_name}{param_pattern} = {_with_guards(converter, guards, body)}"

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

    body = _with_guards(converter, guards, body)
    converter.current_function = None
    converter.declared_vars = set()
    param_pattern = f" {param_names}" if param_names else ""
    return f"{signature}\n{func_name}{param_pattern} = {body}"


def _bound_name(stmt: ast.stmt) -> Optional[str]:
    """Return the name a plain `x = v` or `x: T = v` statement binds, else None."""
    if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
        return stmt.targets[0].id
    if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name) and stmt.value is not None:
        return stmt.target.id
    return None


def _is_guard_raise(stmt: ast.stmt) -> bool:
    """Match `if cond: raise E(...)` with no else branch."""
    return isinstance(stmt, ast.If) and not stmt.orelse and len(stmt.body) == 1 and isinstance(stmt.body[0], ast.Raise)


def _with_guards(converter: "MultiGenPythonToHaskellConverter", guards: list[ast.If], body: str) -> str:
    """Wrap a function body in its guards; a `where` clause stays attached to the equation."""
    head, sep, where = body.partition("\n  where")
    for guard in reversed(guards):
        raise_stmt = guard.body[0]
        assert isinstance(raise_stmt, ast.Raise)
        condition = converter._convert_expression(guard.test)
        head = f"if {condition} then {converter._convert_raise_statement(raise_stmt)} else {head}"
    return head + sep + where


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


def _convert_sequential_function(
    converter: "MultiGenPythonToHaskellConverter", node: ast.FunctionDef, func_name: str
) -> Optional[str]:
    """Translate a pure function as one expression that runs its statements in order.

    Each assignment becomes `case e of x -> rest`: `case` binds without recursion, so
    `x = x * 2` refers to the old x, as in Python. A `for` loop becomes a foldl over the
    variables its body updates. Returns None when the body holds a statement this cannot express.
    """
    signature, params = _pure_signature(converter, node, func_name)
    converter.current_function = func_name
    converter.declared_vars = set()
    try:
        body = _sequential_body(converter, node.body, {arg.arg for arg in node.args.args}, None)
    except UnsupportedFeatureError:
        return None
    finally:
        converter.current_function = None
        converter.declared_vars = set()
    param_names = " ".join(name for name, _ in params)
    return f"{signature}\n{func_name}{' ' + param_names if param_names else ''} =\n  {body}"


def _sequential_body(
    converter: "MultiGenPythonToHaskellConverter", stmts: list[ast.stmt], defined: set[str], out: Optional[list[str]]
) -> str:
    """Translate statements into one expression.

    `out` names the variables a loop or if branch hands back as a tuple when its
    statements run out; None means the function body, which must end in return.
    """
    if not stmts:
        if out is None:
            raise UnsupportedFeatureError("function body can end without return")
        return _state(converter, out)
    stmt, rest = stmts[0], stmts[1:]
    name = converter._to_haskell_var_name

    def bind(var: str, value: str) -> str:
        return f"(case {value} of {name(var)} -> {_sequential_body(converter, rest, defined | {var}, out)})"

    if isinstance(stmt, (ast.Return, ast.Raise)):
        if out is not None:
            raise UnsupportedFeatureError("return or raise inside a loop or branch that falls through")
        if isinstance(stmt, ast.Raise):
            return str(converter._convert_raise_statement(stmt))
        return converter._convert_expression(stmt.value) if stmt.value else "()"
    if isinstance(stmt, ast.Pass) or (isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant)):
        return _sequential_body(converter, rest, defined, out)
    target = _store_target(stmt)
    if target is not None and isinstance(target, ast.Name) and isinstance(stmt, (ast.Assign, ast.AnnAssign)):
        assert stmt.value is not None
        return bind(target.id, converter._convert_expression(stmt.value))
    if isinstance(stmt, ast.AugAssign) and isinstance(stmt.target, ast.Name):
        load = ast.Name(id=stmt.target.id, ctx=ast.Load())
        return bind(stmt.target.id, converter._convert_expression(ast.BinOp(left=load, op=stmt.op, right=stmt.value)))
    if target is not None and isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
        base = target.value.id
        index = converter._convert_expression(target.slice)
        if isinstance(stmt, ast.AugAssign):
            current = ast.Subscript(value=ast.Name(id=base, ctx=ast.Load()), slice=target.slice, ctx=ast.Load())
            value = converter._convert_expression(ast.BinOp(left=current, op=stmt.op, right=stmt.value))
        else:
            assert isinstance(stmt, (ast.Assign, ast.AnnAssign)) and stmt.value is not None
            value = converter._convert_expression(stmt.value)
        update = "Map.insert" if base in converter.dict_vars else "updateAt"
        return bind(base, f"({update} ({index}) ({value}) {name(base)})")
    appended = _appended_list(stmt)
    if appended is not None:
        assert isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call)
        item = converter._convert_expression(stmt.value.args[0])
        return bind(appended, f"({name(appended)} ++ [{item}])")
    if isinstance(stmt, ast.If):
        return _sequential_if(converter, stmt, rest, defined, out)
    if isinstance(stmt, ast.For) and not stmt.orelse:
        return _sequential_for(converter, stmt, rest, defined, out)
    if isinstance(stmt, ast.While) and not stmt.orelse:
        return _sequential_while(converter, stmt, rest, defined, out)
    raise UnsupportedFeatureError(f"{type(stmt).__name__} at line {stmt.lineno} in a sequential translation")


def _sequential_if(
    converter: "MultiGenPythonToHaskellConverter",
    stmt: ast.If,
    rest: list[ast.stmt],
    defined: set[str],
    out: Optional[list[str]],
) -> str:
    """An if that ends in return takes the rest as its else; otherwise both branches hand back what they update."""
    condition = converter._convert_expression(stmt.test)
    if _ends_function(stmt.body):
        then = _sequential_body(converter, stmt.body, defined, out)
        return f"(if {condition} then {then} else {_sequential_body(converter, stmt.orelse + rest, defined, out)})"
    if _has_exit(stmt.body + stmt.orelse):
        raise UnsupportedFeatureError(f"return inside an if at line {stmt.lineno} that can fall through")
    updated = sorted(_assigned_names(stmt.body + stmt.orelse) & defined)
    if not updated:
        return _sequential_body(converter, rest, defined, out)
    then = _sequential_body(converter, stmt.body, defined, updated)
    other = _sequential_body(converter, stmt.orelse, defined, updated)
    following = _sequential_body(converter, rest, defined, out)
    return f"(case (if {condition} then {then} else {other}) of {_state(converter, updated)} -> {following})"


def _sequential_for(
    converter: "MultiGenPythonToHaskellConverter",
    stmt: ast.For,
    rest: list[ast.stmt],
    defined: set[str],
    out: Optional[list[str]],
) -> str:
    """A for loop folds the variables its body updates over the iterable."""
    if _has_exit(stmt.body):
        raise UnsupportedFeatureError(f"return, break or continue in the loop at line {stmt.lineno}")
    if isinstance(stmt.target, ast.Name):
        loop_names = {stmt.target.id}
    elif isinstance(stmt.target, ast.Tuple) and all(isinstance(e, ast.Name) for e in stmt.target.elts):
        loop_names = {e.id for e in stmt.target.elts if isinstance(e, ast.Name)}
    else:
        raise UnsupportedFeatureError(f"loop target at line {stmt.lineno}")
    updated = sorted(_assigned_names(stmt.body) & defined)
    if not updated:
        return _sequential_body(converter, rest, defined, out)
    iterable = converter._convert_expression(stmt.iter)
    # Iterating a dict yields its keys; folding the Map itself would visit values
    if isinstance(stmt.iter, ast.Name) and stmt.iter.id in converter.dict_vars:
        iterable = f"Map.keys {iterable}"
    state = _state(converter, updated)
    body = _sequential_body(converter, stmt.body, defined | loop_names, updated)
    target = converter._convert_expression(stmt.target)
    folded = f"(foldl (\\{state} {target} -> {body}) {state} ({iterable}))"
    return f"(case {folded} of {state} -> {_sequential_body(converter, rest, defined, out)})"


def _sequential_while(
    converter: "MultiGenPythonToHaskellConverter",
    stmt: ast.While,
    rest: list[ast.stmt],
    defined: set[str],
    out: Optional[list[str]],
) -> str:
    """A while loop is a local recursive function over the variables its body updates."""
    if _has_exit(stmt.body):
        raise UnsupportedFeatureError(f"return, break or continue in the loop at line {stmt.lineno}")
    updated = sorted(_assigned_names(stmt.body) & defined)
    if not updated:
        raise UnsupportedFeatureError(f"while loop at line {stmt.lineno} updates no variable, so it cannot end")
    state = _state(converter, updated)
    step = f"go{stmt.lineno}"
    condition = converter._convert_expression(stmt.test)
    body = _sequential_body(converter, stmt.body, defined, updated)
    loop = f"let {step} {state} = if {condition} then {step} ({body}) else {state} in {step} {state}"
    return f"(case ({loop}) of {state} -> {_sequential_body(converter, rest, defined, out)})"


def _state(converter: "MultiGenPythonToHaskellConverter", names: list[str]) -> str:
    """The tuple pattern and expression for a set of variables; one variable needs no tuple."""
    converted = [converter._to_haskell_var_name(n) for n in names]
    return converted[0] if len(converted) == 1 else f"({', '.join(converted)})"


def _store_target(stmt: ast.stmt) -> Optional[ast.expr]:
    """The single target of an assignment statement, else None."""
    if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1:
        return stmt.targets[0]
    if isinstance(stmt, ast.AnnAssign) and stmt.value is not None:
        return stmt.target
    if isinstance(stmt, ast.AugAssign):
        return stmt.target
    return None


def _appended_list(stmt: ast.stmt) -> Optional[str]:
    """The list name in an `xs.append(v)` statement, else None."""
    if (
        isinstance(stmt, ast.Expr)
        and isinstance(stmt.value, ast.Call)
        and isinstance(stmt.value.func, ast.Attribute)
        and stmt.value.func.attr == "append"
        and isinstance(stmt.value.func.value, ast.Name)
        and len(stmt.value.args) == 1
    ):
        return stmt.value.func.value.id
    return None


def _assigned_names(stmts: list[ast.stmt]) -> set[str]:
    """Names rebound anywhere in the statements: assignments, item stores and appends."""
    names: set[str] = set()
    for stmt in (n for s in stmts for n in ast.walk(s) if isinstance(n, ast.stmt)):
        target = _store_target(stmt)
        if isinstance(target, ast.Name):
            names.add(target.id)
        elif isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
            names.add(target.value.id)
        appended = _appended_list(stmt)
        if appended is not None:
            names.add(appended)
    return names


def _ends_function(stmts: list[ast.stmt]) -> bool:
    """Whether the statements always end the function with return or raise."""
    return bool(stmts) and isinstance(stmts[-1], (ast.Return, ast.Raise))


def _has_exit(stmts: list[ast.stmt]) -> bool:
    """Whether a return, raise, break or continue appears anywhere in the statements."""
    exits = (ast.Return, ast.Raise, ast.Break, ast.Continue)
    return any(isinstance(n, exits) for s in stmts for n in ast.walk(s))
