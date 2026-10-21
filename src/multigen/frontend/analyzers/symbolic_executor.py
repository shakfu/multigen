"""Basic Symbolic Execution Engine.

This module provides symbolic execution capabilities for analyzing Python code paths,
tracking symbolic values, and detecting potential runtime errors without actual execution.
"""

import ast
import operator
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional, Union

from ..base import AnalysisContext, AnalysisLevel, AnalysisReport, BaseAnalyzer


class SymbolicValueType(Enum):
    """Types of symbolic values."""

    UNKNOWN = "unknown"
    CONSTANT = "constant"
    VARIABLE = "variable"
    BINARY_OP = "binary_op"
    UNARY_OP = "unary_op"
    FUNCTION_CALL = "function_call"
    CONDITIONAL = "conditional"
    LIST = "list"
    DICT = "dict"


@dataclass
class SymbolicValue:
    """Represents a symbolic value during execution."""

    value_type: SymbolicValueType
    concrete_value: Optional[Any] = None
    symbolic_expr: str = ""
    constraints: list[str] = field(default_factory=list)
    possible_values: set[Any] = field(default_factory=set)
    line_number: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)

    def is_concrete(self) -> bool:
        """Check if this symbolic value has a concrete value."""
        return self.concrete_value is not None

    def is_constant(self) -> bool:
        """Check if this is a constant value."""
        return self.value_type == SymbolicValueType.CONSTANT and self.is_concrete()

    def add_constraint(self, constraint: str) -> None:
        """Add a constraint to this symbolic value."""
        if constraint not in self.constraints:
            self.constraints.append(constraint)

    def merge_with(self, other: "SymbolicValue") -> "SymbolicValue":
        """Merge this symbolic value with another (for control flow joins)."""
        if self.is_concrete() and other.is_concrete() and self.concrete_value == other.concrete_value:
            return self

        # Create a new symbolic value representing the union
        merged = SymbolicValue(
            value_type=SymbolicValueType.CONDITIONAL,
            symbolic_expr=f"({self.symbolic_expr} | {other.symbolic_expr})",
            constraints=self.constraints + other.constraints,
        )

        if self.is_concrete():
            merged.possible_values.add(self.concrete_value)
        if other.is_concrete():
            merged.possible_values.add(other.concrete_value)

        merged.possible_values.update(self.possible_values)
        merged.possible_values.update(other.possible_values)

        return merged


@dataclass
class SymbolicState:
    """Represents the symbolic state at a program point."""

    variables: dict[str, SymbolicValue] = field(default_factory=dict)
    path_condition: list[str] = field(default_factory=list)
    program_counter: int = 0
    call_stack: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def copy(self) -> "SymbolicState":
        """Create a deep copy of this symbolic state."""
        new_state = SymbolicState(
            variables=self.variables.copy(),
            path_condition=self.path_condition.copy(),
            program_counter=self.program_counter,
            call_stack=self.call_stack.copy(),
            metadata=self.metadata.copy(),
        )
        return new_state

    def set_variable(self, name: str, value: SymbolicValue) -> None:
        """Set a variable to a symbolic value."""
        self.variables[name] = value

    def get_variable(self, name: str) -> Optional[SymbolicValue]:
        """Get the symbolic value of a variable."""
        return self.variables.get(name)

    def add_path_condition(self, condition: str) -> None:
        """Add a condition to the current path."""
        self.path_condition.append(condition)

    def merge_with(self, other: "SymbolicState") -> "SymbolicState":
        """Merge this state with another (for control flow joins)."""
        merged = SymbolicState()

        # Merge variables
        all_vars = set(self.variables.keys()) | set(other.variables.keys())
        for var_name in all_vars:
            self_val = self.variables.get(var_name)
            other_val = other.variables.get(var_name)

            if self_val is None and other_val is not None:
                merged.variables[var_name] = other_val
            elif self_val is not None and other_val is None:
                merged.variables[var_name] = self_val
            elif self_val is not None and other_val is not None:
                merged.variables[var_name] = self_val.merge_with(other_val)

        # Merge path conditions (intersection for feasible paths)
        merged.path_condition = list(set(self.path_condition) & set(other.path_condition))

        return merged


@dataclass
class ExecutionPath:
    """Represents a single execution path through the program."""

    path_id: int
    initial_state: SymbolicState
    final_state: SymbolicState
    visited_lines: list[int] = field(default_factory=list)
    path_conditions: list[str] = field(default_factory=list)
    assertions: list[str] = field(default_factory=list)
    potential_errors: list[str] = field(default_factory=list)
    is_feasible: bool = True
    is_complete: bool = False

    def add_visited_line(self, line_no: int) -> None:
        """Add a line number to the visited lines."""
        self.visited_lines.append(line_no)

    def add_assertion(self, assertion: str) -> None:
        """Add an assertion that must hold on this path."""
        self.assertions.append(assertion)

    def add_potential_error(self, error: str) -> None:
        """Add a potential error detected on this path."""
        self.potential_errors.append(error)


@dataclass
class SymbolicExecutionReport(AnalysisReport):
    """Extended analysis report for symbolic execution."""

    execution_paths: list[ExecutionPath] = field(default_factory=list)
    total_paths: int = 0
    completed_paths: int = 0
    feasible_paths: int = 0
    potential_errors: list[str] = field(default_factory=list)
    coverage_info: dict[str, Any] = field(default_factory=dict)
    symbolic_constraints: list[str] = field(default_factory=list)
    # Reasons the paths under-approximate the program; empty means exhaustive.
    approximations: list[str] = field(default_factory=list)


class SymbolicExecutor(BaseAnalyzer):
    """Basic symbolic execution engine for Python code analysis."""

    def __init__(self, analysis_level: AnalysisLevel = AnalysisLevel.BASIC):
        super().__init__("SymbolicExecutor", analysis_level)
        self._path_counter = 0
        self._max_paths = 100  # Limit to prevent explosion
        self._max_depth = 50  # Maximum recursion depth
        self._max_while_iterations = 3
        self._approximations: list[str] = []

    def analyze(self, context: AnalysisContext) -> SymbolicExecutionReport:
        """Perform symbolic execution analysis on the given context."""
        start_time = time.time()

        try:
            # Initialize symbolic execution
            self._path_counter = 0
            self._approximations = []
            execution_paths = []

            # Create initial symbolic state
            initial_state = SymbolicState()
            self._next_nodes: dict[ast.AST, Optional[ast.AST]] = {}
            # Loop -> statement after it (skipping orelse); break/continue -> enclosing loop.
            self._loop_after: dict[ast.AST, Optional[ast.AST]] = {}
            self._enclosing_loop: dict[ast.AST, ast.AST] = {}
            self._register_continuations(context.ast_node)

            # Set up parameters if analyzing a function
            if isinstance(context.ast_node, ast.FunctionDef):
                self._setup_function_parameters(context.ast_node, initial_state)

            # Start symbolic execution
            paths = self._execute_symbolically(context.ast_node, initial_state)
            execution_paths.extend(paths)

            # Analyze results
            potential_errors = []
            for path in execution_paths:
                potential_errors.extend(path.potential_errors)

            execution_time = (time.time() - start_time) * 1000

            # Calculate coverage information
            coverage_info = self._calculate_coverage(context.ast_node, execution_paths)

            return SymbolicExecutionReport(
                analyzer_name=self.name,
                success=True,
                confidence=0.8,  # High confidence for symbolic execution
                findings=self._generate_findings(execution_paths),
                warnings=potential_errors,
                errors=[],
                metadata={
                    "max_paths_limit": self._max_paths,
                    "max_depth_limit": self._max_depth,
                    "analysis_level": self.analysis_level.value,
                },
                execution_time_ms=execution_time,
                execution_paths=execution_paths,
                total_paths=len(execution_paths),
                completed_paths=sum(1 for p in execution_paths if p.is_complete),
                feasible_paths=sum(1 for p in execution_paths if p.is_feasible),
                potential_errors=potential_errors,
                coverage_info=coverage_info,
                symbolic_constraints=self._collect_all_constraints(execution_paths),
                approximations=list(self._approximations),
            )

        except Exception as e:
            execution_time = (time.time() - start_time) * 1000
            return SymbolicExecutionReport(
                analyzer_name=self.name,
                success=False,
                confidence=0.0,
                findings=[],
                warnings=[],
                errors=[f"Symbolic execution failed: {str(e)}"],
                metadata={},
                execution_time_ms=execution_time,
            )

    def _setup_function_parameters(self, func_node: ast.FunctionDef, state: SymbolicState) -> None:
        """Set up symbolic values for function parameters."""
        for _i, arg in enumerate(func_node.args.args):
            param_value = SymbolicValue(
                value_type=SymbolicValueType.VARIABLE, symbolic_expr=f"param_{arg.arg}", line_number=func_node.lineno
            )
            state.set_variable(arg.arg, param_value)

    def _execute_symbolically(self, node: ast.AST, initial_state: SymbolicState) -> list[ExecutionPath]:
        """Execute the AST node symbolically and return all execution paths."""
        worklist: list[tuple[ast.AST, SymbolicState, list[int]]] = [
            (node, initial_state, [])
        ]  # (node, state, path_history)
        completed_paths = []

        while worklist and self._path_counter < self._max_paths:
            current_node, current_state, path_history = worklist.pop(0)

            if len(path_history) > self._max_depth:
                # Create truncated path
                path = ExecutionPath(
                    path_id=self._path_counter,
                    initial_state=initial_state,
                    final_state=current_state,
                    visited_lines=path_history,
                    path_conditions=current_state.path_condition.copy(),
                    is_complete=False,
                )
                path.add_potential_error("Path truncated due to depth limit")
                self._approximations.append(f"path {self._path_counter} truncated at depth {self._max_depth}")
                completed_paths.append(path)
                self._path_counter += 1
                continue

            try:
                new_paths = self._execute_node(current_node, current_state, path_history)

                for new_node, new_state, new_history in new_paths:
                    if new_node is None:  # Path completed
                        path = ExecutionPath(
                            path_id=self._path_counter,
                            initial_state=initial_state,
                            final_state=new_state,
                            visited_lines=new_history,
                            path_conditions=new_state.path_condition.copy(),
                            is_complete=True,
                        )
                        completed_paths.append(path)
                        self._path_counter += 1
                    else:
                        worklist.append((new_node, new_state, new_history))

            except Exception as e:
                # Create error path
                path = ExecutionPath(
                    path_id=self._path_counter,
                    initial_state=initial_state,
                    final_state=current_state,
                    visited_lines=path_history,
                    path_conditions=current_state.path_condition.copy(),
                    is_complete=False,
                )
                path.add_potential_error(f"Execution error: {str(e)}")
                completed_paths.append(path)
                self._path_counter += 1

        if worklist:
            self._approximations.append(f"path budget of {self._max_paths} exhausted")

        return completed_paths

    def _execute_node(
        self, node: ast.AST, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute a single AST node symbolically."""
        new_history = path_history + [getattr(node, "lineno", 0)]

        if isinstance(node, ast.FunctionDef):
            return self._execute_function(node, state, new_history)
        elif isinstance(node, ast.If):
            return self._execute_if(node, state, new_history)
        elif isinstance(node, ast.While):
            return self._execute_while(node, state, new_history)
        elif isinstance(node, ast.For):
            return self._execute_for(node, state, new_history)
        elif isinstance(node, ast.Assign):
            return self._execute_assign(node, state, new_history)
        elif isinstance(node, ast.AugAssign):
            return self._execute_aug_assign(node, state, new_history)
        elif isinstance(node, ast.Return):
            return self._execute_return(node, state, new_history)
        elif isinstance(node, ast.Expr):
            return self._execute_expression_stmt(node, state, new_history)
        elif isinstance(node, (ast.Break, ast.Continue)):
            return self._execute_jump(node, state, new_history)
        else:
            # Generic statements continue with the next statement in their block.
            return [(self._next_nodes.get(node), state, new_history)]

    def _register_continuations(self, node: ast.AST) -> None:
        """Register the next statement for each sequential AST block."""

        def register_block(
            block: list[ast.stmt], continuation: Optional[ast.stmt], loop: Optional[ast.stmt] = None
        ) -> None:
            for index, statement in enumerate(block):
                next_statement = block[index + 1] if index + 1 < len(block) else continuation
                self._next_nodes[statement] = next_statement
                if isinstance(statement, ast.If):
                    register_block(statement.body, next_statement, loop)
                    register_block(statement.orelse, next_statement, loop)
                elif isinstance(statement, (ast.For, ast.While)):
                    # The body falls through to the loop header, which decides the next iteration.
                    self._loop_after[statement] = next_statement
                    register_block(statement.body, statement, statement)
                    register_block(statement.orelse, next_statement, loop)
                elif isinstance(statement, ast.Try):
                    register_block(statement.body, next_statement, loop)
                    register_block(statement.orelse, next_statement, loop)
                    register_block(statement.finalbody, next_statement, loop)
                    for handler in statement.handlers:
                        register_block(handler.body, next_statement, loop)
                elif isinstance(statement, (ast.Break, ast.Continue)) and loop is not None:
                    self._enclosing_loop[statement] = loop

        if isinstance(node, (ast.Module, ast.FunctionDef)):
            register_block(node.body, None)

    def _execute_function(
        self, func_node: ast.FunctionDef, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute a function definition."""
        # For a function, we execute its body
        if func_node.body:
            return [(func_node.body[0], state, path_history)]
        else:
            return [(None, state, path_history)]

    def _execute_if(
        self, if_node: ast.If, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute an if statement, creating branches for both paths."""
        results: list[tuple[Optional[ast.AST], SymbolicState, list[int]]] = []

        # Evaluate condition symbolically
        condition_value = self._evaluate_expression(if_node.test, state)
        condition_expr = condition_value.symbolic_expr
        # A concrete condition has only one feasible branch.
        known = bool(condition_value.concrete_value) if condition_value.is_concrete() else None

        if known is not False:
            true_state = state.copy()
            true_state.add_path_condition(condition_expr)
            results.append((self._first_or_next(if_node.body, if_node), true_state, path_history))

        if known is not True:
            false_state = state.copy()
            false_state.add_path_condition(f"not ({condition_expr})")
            results.append((self._first_or_next(if_node.orelse, if_node), false_state, path_history))

        return results

    def _loop_exit(self, loop_node: Union[ast.While, ast.For]) -> Optional[ast.AST]:
        """Return the statement a loop continues at when it terminates normally."""
        return loop_node.orelse[0] if loop_node.orelse else self._next_nodes.get(loop_node)

    def _first_or_next(self, block: list[ast.stmt], node: ast.AST) -> Optional[ast.AST]:
        """Return the first statement of a block, or the node's continuation if the block is empty."""
        return block[0] if block else self._next_nodes.get(node)

    def _with_loop_entry(self, state: SymbolicState, loop_node: ast.AST, entry: Optional[tuple[int, Any]]) -> None:
        """Set (or clear, with None) a loop's (iteration, iterable) entry in a copied state."""
        loops = dict(state.metadata.get("loops", {}))
        if entry is None:
            loops.pop(loop_node, None)
        else:
            loops[loop_node] = entry
        state.metadata["loops"] = loops

    def _note_unroll_limit(self, loop_node: Union[ast.While, ast.For]) -> None:
        """Record that paths needing more iterations than the bound were cut."""
        kind = "while" if isinstance(loop_node, ast.While) else "for"
        note = f"line {loop_node.lineno}: {kind} loop unrolled to {self._max_while_iterations} iterations"
        if note not in self._approximations:
            self._approximations.append(note)

    def _execute_while(
        self, while_node: ast.While, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Visit a while header: branch on the condition, bounded by the unroll limit."""
        results: list[tuple[Optional[ast.AST], SymbolicState, list[int]]] = []
        iteration = state.metadata.get("loops", {}).get(while_node, (0, None))[0]
        condition_value = self._evaluate_expression(while_node.test, state)
        condition_expr = condition_value.symbolic_expr
        known = bool(condition_value.concrete_value) if condition_value.is_concrete() else None

        if known is not True:
            exit_state = state.copy()
            exit_state.add_path_condition(f"not ({condition_expr})")
            self._with_loop_entry(exit_state, while_node, None)
            results.append((self._loop_exit(while_node), exit_state, path_history))

        if known is not False:
            if iteration < self._max_while_iterations:
                body_state = state.copy()
                body_state.add_path_condition(condition_expr)
                self._with_loop_entry(body_state, while_node, (iteration + 1, None))
                results.append((self._first_or_next(while_node.body, while_node), body_state, path_history))
            else:
                self._note_unroll_limit(while_node)

        return results

    def _loop_items(self, iterable: ast.expr, state: SymbolicState) -> Optional[list[SymbolicValue]]:
        """Return the concrete items of a range() or list/tuple literal, or None if unknown."""
        if isinstance(iterable, (ast.List, ast.Tuple)):
            return [self._evaluate_expression(element, state) for element in iterable.elts]
        if isinstance(iterable, ast.Call) and isinstance(iterable.func, ast.Name) and iterable.func.id == "range":
            args = [self._evaluate_expression(arg, state) for arg in iterable.args]
            bounds = [arg.concrete_value for arg in args if isinstance(arg.concrete_value, int)]
            if args and not iterable.keywords and len(bounds) == len(args):
                try:
                    values = range(*bounds)
                except (TypeError, ValueError):
                    return None
                # Items past the unroll bound are never bound; one extra keeps the limit visible.
                return [
                    SymbolicValue(SymbolicValueType.CONSTANT, concrete_value=v, symbolic_expr=str(v))
                    for v in values[: self._max_while_iterations + 1]
                ]
        return None

    def _execute_for(
        self, for_node: ast.For, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Visit a for header: bind the next item or exit, bounded by the unroll limit."""
        results: list[tuple[Optional[ast.AST], SymbolicState, list[int]]] = []
        entry = state.metadata.get("loops", {}).get(for_node)
        # The iterable is evaluated once, on entry.
        iteration, items = entry if entry is not None else (0, self._loop_items(for_node.iter, state))
        iter_expr = ast.unparse(for_node.iter)

        has_next = None if items is None else iteration < len(items)
        if has_next is not True:
            exit_state = state.copy()
            if items is None:
                exit_state.add_path_condition(f"len({iter_expr}) == {iteration}")
            self._with_loop_entry(exit_state, for_node, None)
            results.append((self._loop_exit(for_node), exit_state, path_history))

        if has_next is not False:
            if iteration < self._max_while_iterations:
                body_state = state.copy()
                if items is None:
                    body_state.add_path_condition(f"len({iter_expr}) > {iteration}")
                if isinstance(for_node.target, ast.Name):
                    item = (
                        items[iteration]
                        if items is not None
                        else SymbolicValue(
                            value_type=SymbolicValueType.VARIABLE,
                            symbolic_expr=f"loop_var_{for_node.target.id}_{iteration}",
                            line_number=for_node.lineno,
                        )
                    )
                    body_state.set_variable(for_node.target.id, item)
                self._with_loop_entry(body_state, for_node, (iteration + 1, items))
                results.append((self._first_or_next(for_node.body, for_node), body_state, path_history))
            else:
                self._note_unroll_limit(for_node)

        return results

    def _execute_jump(
        self, node: Union[ast.Break, ast.Continue], state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Route break past the loop (skipping its else) and continue to the loop header."""
        loop = self._enclosing_loop.get(node)
        if loop is None:
            return [(None, state, path_history)]
        if isinstance(node, ast.Continue):
            return [(loop, state, path_history)]
        new_state = state.copy()
        self._with_loop_entry(new_state, loop, None)
        return [(self._loop_after.get(loop), new_state, path_history)]

    def _execute_assign(
        self, assign_node: ast.Assign, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute an assignment statement."""
        new_state = state.copy()

        # Evaluate the right-hand side
        value = self._evaluate_expression(assign_node.value, new_state)

        # Assign to all targets
        for target in assign_node.targets:
            if isinstance(target, ast.Name):
                new_state.set_variable(target.id, value)

        return [(self._next_nodes.get(assign_node), new_state, path_history)]

    def _execute_aug_assign(
        self, aug_assign_node: ast.AugAssign, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute an augmented assignment statement."""
        new_state = state.copy()

        if isinstance(aug_assign_node.target, ast.Name):
            # x op= v evaluates as x op v, so concrete operands stay concrete.
            target = ast.Name(id=aug_assign_node.target.id, ctx=ast.Load())
            binop = ast.copy_location(
                ast.BinOp(left=target, op=aug_assign_node.op, right=aug_assign_node.value), aug_assign_node
            )
            new_state.set_variable(aug_assign_node.target.id, self._evaluate_expression(binop, new_state))

        return [(self._next_nodes.get(aug_assign_node), new_state, path_history)]

    def _execute_return(
        self, return_node: ast.Return, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute a return statement."""
        new_state = state.copy()

        if return_node.value:
            return_value = self._evaluate_expression(return_node.value, new_state)
            new_state.metadata["return_value"] = return_value

        return [(None, new_state, path_history)]

    def _execute_expression_stmt(
        self, expr_stmt: ast.Expr, state: SymbolicState, path_history: list[int]
    ) -> list[tuple[Optional[ast.AST], SymbolicState, list[int]]]:
        """Execute an expression statement."""
        new_state = state.copy()
        self._evaluate_expression(expr_stmt.value, new_state)
        return [(self._next_nodes.get(expr_stmt), new_state, path_history)]

    def _evaluate_expression(self, expr: ast.expr, state: SymbolicState) -> SymbolicValue:
        """Evaluate an expression symbolically."""
        if isinstance(expr, ast.Constant):
            return SymbolicValue(
                value_type=SymbolicValueType.CONSTANT,
                concrete_value=expr.value,
                symbolic_expr=str(expr.value),
                line_number=getattr(expr, "lineno", 0),
            )
        elif isinstance(expr, ast.Name):
            existing_value = state.get_variable(expr.id)
            if existing_value:
                return existing_value
            else:
                # Unknown variable
                return SymbolicValue(
                    value_type=SymbolicValueType.VARIABLE, symbolic_expr=expr.id, line_number=getattr(expr, "lineno", 0)
                )
        elif isinstance(expr, ast.BinOp):
            left = self._evaluate_expression(expr.left, state)
            right = self._evaluate_expression(expr.right, state)
            op_str = self._get_operator_string(expr.op)

            # Try to compute concrete value if both operands are concrete
            concrete_result = None
            if left.is_concrete() and right.is_concrete():
                try:
                    concrete_result = self._apply_binary_op(expr.op, left.concrete_value, right.concrete_value)
                except (ZeroDivisionError, TypeError, ValueError):
                    # Cannot compute concrete result
                    pass

            return SymbolicValue(
                value_type=SymbolicValueType.BINARY_OP,
                concrete_value=concrete_result,
                symbolic_expr=f"({left.symbolic_expr} {op_str} {right.symbolic_expr})",
                line_number=getattr(expr, "lineno", 0),
            )
        elif isinstance(expr, ast.UnaryOp):
            operand = self._evaluate_expression(expr.operand, state)
            op_str = self._get_unary_operator_string(expr.op)

            concrete_result = None
            if operand.is_concrete():
                try:
                    concrete_result = self._apply_unary_op(expr.op, operand.concrete_value)
                except (TypeError, ValueError):
                    pass

            return SymbolicValue(
                value_type=SymbolicValueType.UNARY_OP,
                concrete_value=concrete_result,
                symbolic_expr=f"({op_str}{operand.symbolic_expr})",
                line_number=getattr(expr, "lineno", 0),
            )
        elif isinstance(expr, ast.Compare):
            operands = [self._evaluate_expression(operand, state) for operand in [expr.left, *expr.comparators]]
            concrete_result = None
            if all(operand.is_concrete() for operand in operands):
                try:
                    concrete_result = all(
                        self._apply_compare_op(op, left.concrete_value, right.concrete_value)
                        for op, left, right in zip(expr.ops, operands, operands[1:])
                    )
                except TypeError:
                    pass
            parts = [operands[0].symbolic_expr]
            for op, operand in zip(expr.ops, operands[1:]):
                parts += [self._COMPARE_OPS[type(op)][0], operand.symbolic_expr]
            return SymbolicValue(
                value_type=SymbolicValueType.BINARY_OP,
                concrete_value=concrete_result,
                symbolic_expr=f"({' '.join(parts)})",
                line_number=getattr(expr, "lineno", 0),
            )
        elif isinstance(expr, ast.Call):
            return SymbolicValue(
                value_type=SymbolicValueType.FUNCTION_CALL,
                symbolic_expr=f"call({ast.unparse(expr)})",
                line_number=getattr(expr, "lineno", 0),
            )
        else:
            # Unknown expression type
            return SymbolicValue(
                value_type=SymbolicValueType.UNKNOWN,
                symbolic_expr=f"unknown({type(expr).__name__})",
                line_number=getattr(expr, "lineno", 0),
            )

    def _get_operator_string(self, op: ast.operator) -> str:
        """Get string representation of a binary operator."""
        op_map = {
            ast.Add: "+",
            ast.Sub: "-",
            ast.Mult: "*",
            ast.Div: "/",
            ast.FloorDiv: "//",
            ast.Mod: "%",
            ast.Pow: "**",
            ast.LShift: "<<",
            ast.RShift: ">>",
            ast.BitOr: "|",
            ast.BitXor: "^",
            ast.BitAnd: "&",
        }
        return op_map.get(type(op), "?")

    def _get_unary_operator_string(self, op: ast.unaryop) -> str:
        """Get string representation of a unary operator."""
        op_map = {ast.UAdd: "+", ast.USub: "-", ast.Not: "not ", ast.Invert: "~"}
        return op_map.get(type(op), "?")

    def _apply_binary_op(self, op: ast.operator, left: Any, right: Any) -> Any:
        """Apply a binary operator to concrete values."""
        if isinstance(op, ast.Add):
            return left + right
        elif isinstance(op, ast.Sub):
            return left - right
        elif isinstance(op, ast.Mult):
            return left * right
        elif isinstance(op, ast.Div):
            if right == 0:
                raise ZeroDivisionError("Division by zero")
            return left / right
        elif isinstance(op, ast.FloorDiv):
            if right == 0:
                raise ZeroDivisionError("Division by zero")
            return left // right
        elif isinstance(op, ast.Mod):
            if right == 0:
                raise ZeroDivisionError("Modulo by zero")
            return left % right
        elif isinstance(op, ast.Pow):
            return left**right
        else:
            raise TypeError(f"Unsupported operator: {type(op)}")

    _COMPARE_OPS: dict[type, tuple[str, Any]] = {
        ast.Eq: ("==", operator.eq),
        ast.NotEq: ("!=", operator.ne),
        ast.Lt: ("<", operator.lt),
        ast.LtE: ("<=", operator.le),
        ast.Gt: (">", operator.gt),
        ast.GtE: (">=", operator.ge),
        ast.Is: ("is", operator.is_),
        ast.IsNot: ("is not", operator.is_not),
        ast.In: ("in", lambda a, b: operator.contains(b, a)),
        ast.NotIn: ("not in", lambda a, b: not operator.contains(b, a)),
    }

    def _apply_compare_op(self, op: ast.cmpop, left: Any, right: Any) -> bool:
        """Apply a comparison operator to concrete values."""
        return bool(self._COMPARE_OPS[type(op)][1](left, right))

    def _apply_unary_op(self, op: ast.unaryop, operand: Any) -> Any:
        """Apply a unary operator to a concrete value."""
        if isinstance(op, ast.UAdd):
            return +operand
        elif isinstance(op, ast.USub):
            return -operand
        elif isinstance(op, ast.Not):
            return not operand
        elif isinstance(op, ast.Invert):
            return ~operand
        else:
            raise TypeError(f"Unsupported unary operator: {type(op)}")

    def _calculate_coverage(self, node: ast.AST, paths: list[ExecutionPath]) -> dict[str, Any]:
        """Calculate coverage information from execution paths."""
        all_lines = set()
        covered_lines = set()

        # Collect all line numbers in the AST
        for child in ast.walk(node):
            if hasattr(child, "lineno"):
                all_lines.add(child.lineno)

        # Collect covered lines from all paths
        for path in paths:
            covered_lines.update(path.visited_lines)

        coverage_percentage = (len(covered_lines) / len(all_lines)) * 100 if all_lines else 0

        return {
            "total_lines": len(all_lines),
            "covered_lines": len(covered_lines),
            "coverage_percentage": coverage_percentage,
            "uncovered_lines": list(all_lines - covered_lines),
        }

    def _collect_all_constraints(self, paths: list[ExecutionPath]) -> list[str]:
        """Collect all unique constraints from execution paths."""
        constraints = set()
        for path in paths:
            constraints.update(path.path_conditions)
            constraints.update(path.assertions)
        return list(constraints)

    def _generate_findings(self, paths: list[ExecutionPath]) -> list[str]:
        """Generate a list of analysis findings."""
        findings = []

        findings.append(f"Symbolic execution generated {len(paths)} execution paths")

        completed_paths = sum(1 for p in paths if p.is_complete)
        findings.append(f"{completed_paths} paths completed successfully")

        feasible_paths = sum(1 for p in paths if p.is_feasible)
        findings.append(f"{feasible_paths} paths are feasible")

        total_errors = sum(len(p.potential_errors) for p in paths)
        if total_errors > 0:
            findings.append(f"Detected {total_errors} potential runtime errors")

        return findings
