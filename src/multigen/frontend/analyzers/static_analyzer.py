"""Static Code Analyzer with Control Flow Analysis.

This module provides comprehensive static analysis capabilities including:
- Control flow graph construction
- Data flow analysis
- Dead code detection
- Variable usage analysis
- Complexity metrics
- Safety checks
"""

import ast
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from ..base import AnalysisContext, AnalysisLevel, AnalysisReport, BaseAnalyzer


class NodeType(Enum):
    """Types of nodes in the control flow graph."""

    ENTRY = "entry"
    EXIT = "exit"
    STATEMENT = "statement"
    CONDITION = "condition"
    LOOP_HEADER = "loop_header"
    LOOP_BODY = "loop_body"
    FUNCTION_CALL = "function_call"
    RETURN = "return"
    BREAK = "break"
    CONTINUE = "continue"


@dataclass
class CFGNode:
    """Node in the Control Flow Graph."""

    id: int
    node_type: NodeType
    ast_node: Optional[ast.AST] = None
    line_number: int = 0
    code: str = ""
    predecessors: set[int] = field(default_factory=set)
    successors: set[int] = field(default_factory=set)
    dominators: set[int] = field(default_factory=set)
    post_dominators: set[int] = field(default_factory=set)
    metadata: dict[str, Any] = field(default_factory=dict)

    def add_predecessor(self, node_id: int) -> None:
        """Add a predecessor node."""
        self.predecessors.add(node_id)

    def add_successor(self, node_id: int) -> None:
        """Add a successor node."""
        self.successors.add(node_id)

    def is_branch_node(self) -> bool:
        """Check if this node is a branch point."""
        return len(self.successors) > 1

    def is_merge_node(self) -> bool:
        """Check if this node is a merge point."""
        return len(self.predecessors) > 1


@dataclass
class ControlFlowGraph:
    """Control Flow Graph representation."""

    nodes: dict[int, CFGNode] = field(default_factory=dict)
    entry_node: Optional[int] = None
    exit_nodes: set[int] = field(default_factory=set)
    edges: set[tuple[int, int]] = field(default_factory=set)
    next_id: int = 0

    def create_node(self, node_type: NodeType, ast_node: Optional[ast.AST] = None) -> CFGNode:
        """Create a new CFG node."""
        node = CFGNode(
            id=self.next_id,
            node_type=node_type,
            ast_node=ast_node,
            line_number=getattr(ast_node, "lineno", 0) if ast_node else 0,
            code=ast.unparse(ast_node) if ast_node else "",
        )
        self.nodes[self.next_id] = node
        self.next_id += 1
        return node

    def add_edge(self, from_id: int, to_id: int) -> None:
        """Add an edge between two nodes."""
        if from_id in self.nodes and to_id in self.nodes:
            self.nodes[from_id].add_successor(to_id)
            self.nodes[to_id].add_predecessor(from_id)
            self.edges.add((from_id, to_id))

    def get_reachable_nodes(self, start_id: int) -> set[int]:
        """Get all nodes reachable from the start node."""
        reachable = set()
        stack = [start_id]

        while stack:
            current = stack.pop()
            if current not in reachable:
                reachable.add(current)
                stack.extend(self.nodes[current].successors)

        return reachable

    def find_dead_code(self) -> set[int]:
        """Find unreachable nodes (dead code)."""
        if self.entry_node is None:
            return set()

        reachable = self.get_reachable_nodes(self.entry_node)
        all_nodes = set(self.nodes.keys())
        return all_nodes - reachable

    def calculate_dominators(self) -> None:
        """Calculate dominator sets for all nodes."""
        if self.entry_node is None:
            return

        # Initialize dominators
        all_nodes = set(self.nodes.keys())
        for node_id in self.nodes:
            if node_id == self.entry_node:
                self.nodes[node_id].dominators = {node_id}
            else:
                self.nodes[node_id].dominators = all_nodes.copy()

        # Iterative algorithm
        changed = True
        while changed:
            changed = False
            for node_id in self.nodes:
                if node_id == self.entry_node:
                    continue

                # Intersection of dominators of all predecessors
                if self.nodes[node_id].predecessors:
                    new_dominators = all_nodes.copy()
                    for pred_id in self.nodes[node_id].predecessors:
                        new_dominators &= self.nodes[pred_id].dominators
                    new_dominators.add(node_id)

                    if new_dominators != self.nodes[node_id].dominators:
                        self.nodes[node_id].dominators = new_dominators
                        changed = True


@dataclass
class VariableInfo:
    """Information about a variable in the analysis."""

    name: str
    first_definition: int  # Line number
    last_usage: int  # Line number
    definition_points: set[int] = field(default_factory=set)
    usage_points: set[int] = field(default_factory=set)
    is_parameter: bool = False
    is_global: bool = False
    inferred_type: Optional[str] = None
    is_constant: bool = False
    is_loop_variable: bool = False


@dataclass
class StaticAnalysisReport(AnalysisReport):
    """Extended analysis report for static analysis."""

    cfg: ControlFlowGraph = field(default_factory=ControlFlowGraph)
    variables: dict[str, VariableInfo] = field(default_factory=dict)
    dead_code_nodes: set[int] = field(default_factory=set)
    complexity_metrics: dict[str, int] = field(default_factory=dict)
    potential_issues: list[str] = field(default_factory=list)
    performance_hints: list[str] = field(default_factory=list)


class StaticAnalyzer(BaseAnalyzer):
    """Static code analyzer with control flow analysis."""

    def __init__(self, analysis_level: AnalysisLevel = AnalysisLevel.BASIC):
        super().__init__("StaticAnalyzer", analysis_level)
        self._current_cfg: ControlFlowGraph
        self._variables: dict[str, VariableInfo] = {}
        self._loop_stack: list[tuple[int, int]] = []  # (header_id, exit_id)

    def analyze(self, context: AnalysisContext) -> StaticAnalysisReport:
        """Perform static analysis on the given context."""
        start_time = time.time()

        try:
            # Initialize analysis state
            self._current_cfg = ControlFlowGraph()
            self._variables = {}
            self._loop_stack = []

            # Build control flow graph
            entry_node = self._current_cfg.create_node(NodeType.ENTRY)
            self._current_cfg.entry_node = entry_node.id

            # Analyze the AST
            if isinstance(context.ast_node, ast.FunctionDef):
                self._analyze_function(context.ast_node)
            elif isinstance(context.ast_node, ast.Module):
                self._analyze_module(context.ast_node)
            elif isinstance(context.ast_node, ast.stmt):
                self._analyze_statements([context.ast_node])
            else:
                # For non-stmt nodes, skip analysis
                pass

            # Perform additional analyses
            self._current_cfg.calculate_dominators()
            dead_code = self._current_cfg.find_dead_code()
            complexity_metrics = self._calculate_complexity_metrics()
            issues, hints = self._analyze_patterns()

            execution_time = (time.time() - start_time) * 1000

            return StaticAnalysisReport(
                analyzer_name=self.name,
                success=True,
                confidence=0.9,  # High confidence for static analysis
                findings=self._generate_findings(),
                warnings=issues,
                errors=[],
                metadata={
                    "node_count": len(self._current_cfg.nodes),
                    "edge_count": len(self._current_cfg.edges),
                    "variable_count": len(self._variables),
                },
                execution_time_ms=execution_time,
                cfg=self._current_cfg,
                variables=self._variables,
                dead_code_nodes=dead_code,
                complexity_metrics=complexity_metrics,
                potential_issues=issues,
                performance_hints=hints,
            )

        except Exception as e:
            execution_time = (time.time() - start_time) * 1000
            return StaticAnalysisReport(
                analyzer_name=self.name,
                success=False,
                confidence=0.0,
                findings=[],
                warnings=[],
                errors=[f"Analysis failed: {str(e)}"],
                metadata={},
                execution_time_ms=execution_time,
                cfg=ControlFlowGraph(),
                variables={},
                dead_code_nodes=set(),
                complexity_metrics={},
                potential_issues=[],
                performance_hints=[],
            )

    def _analyze_function(self, func_node: ast.FunctionDef) -> int:
        """Analyze a function definition."""
        # Record parameters as variables
        for arg in func_node.args.args:
            self._variables[arg.arg] = VariableInfo(
                name=arg.arg, first_definition=func_node.lineno, last_usage=func_node.lineno, is_parameter=True
            )

        # Analyze function body
        return self._analyze_statements(func_node.body)

    def _analyze_module(self, module_node: ast.Module) -> int:
        """Analyze a module."""
        return self._analyze_statements(module_node.body)

    def _analyze_statements(self, statements: list[ast.stmt]) -> int:
        """Analyze a list of statements and return the last node ID."""
        current_node_id = self._current_cfg.entry_node
        if current_node_id is None:
            current_node_id = 0

        for stmt in statements:
            current_node_id = self._analyze_statement(stmt, current_node_id)

        return current_node_id

    def _analyze_statement(self, stmt: ast.stmt, current_node_id: int) -> int:
        """Analyze a single statement and return the next node ID."""
        if isinstance(stmt, ast.If):
            return self._analyze_if(stmt, current_node_id)
        elif isinstance(stmt, ast.While):
            return self._analyze_while(stmt, current_node_id)
        elif isinstance(stmt, ast.For):
            return self._analyze_for(stmt, current_node_id)
        elif isinstance(stmt, ast.Return):
            return self._analyze_return(stmt, current_node_id)
        elif isinstance(stmt, ast.Break):
            return self._analyze_break(stmt, current_node_id)
        elif isinstance(stmt, ast.Continue):
            return self._analyze_continue(stmt, current_node_id)
        elif isinstance(stmt, ast.Assign):
            return self._analyze_assign(stmt, current_node_id)
        elif isinstance(stmt, ast.AugAssign):
            return self._analyze_aug_assign(stmt, current_node_id)
        elif isinstance(stmt, ast.Expr):
            return self._analyze_expression_statement(stmt, current_node_id)
        else:
            # Generic statement
            node = self._current_cfg.create_node(NodeType.STATEMENT, stmt)
            self._current_cfg.add_edge(current_node_id, node.id)
            return node.id

    def _analyze_if(self, if_stmt: ast.If, current_node_id: int) -> int:
        """Analyze an if statement."""
        # Create condition node
        condition_node = self._current_cfg.create_node(NodeType.CONDITION, if_stmt)
        self._current_cfg.add_edge(current_node_id, condition_node.id)

        # Analyze if body
        if_exit = self._analyze_statements(if_stmt.body)
        if if_exit is None:
            if_exit = condition_node.id

        # Analyze else body if present
        else_exit = condition_node.id
        if if_stmt.orelse:
            else_exit = self._analyze_statements(if_stmt.orelse)

        # Create merge node
        merge_node = self._current_cfg.create_node(NodeType.STATEMENT)
        self._current_cfg.add_edge(if_exit, merge_node.id)
        self._current_cfg.add_edge(else_exit, merge_node.id)

        return merge_node.id

    def _analyze_while(self, while_stmt: ast.While, current_node_id: int) -> int:
        """Analyze a while loop."""
        # Create loop header
        header_node = self._current_cfg.create_node(NodeType.LOOP_HEADER, while_stmt)
        self._current_cfg.add_edge(current_node_id, header_node.id)

        # Create exit node
        exit_node = self._current_cfg.create_node(NodeType.STATEMENT)

        # Track loop for break/continue handling
        self._loop_stack.append((header_node.id, exit_node.id))

        # Analyze loop body
        body_exit = self._analyze_statements(while_stmt.body)

        # Connect body back to header
        if body_exit is not None:
            self._current_cfg.add_edge(body_exit, header_node.id)

        # Connect header to exit (loop condition fails)
        self._current_cfg.add_edge(header_node.id, exit_node.id)

        self._loop_stack.pop()
        return exit_node.id

    def _analyze_for(self, for_stmt: ast.For, current_node_id: int) -> int:
        """Analyze a for loop."""
        # Record loop variable
        if isinstance(for_stmt.target, ast.Name):
            var_name = for_stmt.target.id
            if var_name not in self._variables:
                self._variables[var_name] = VariableInfo(
                    name=var_name, first_definition=for_stmt.lineno, last_usage=for_stmt.lineno, is_loop_variable=True
                )
            self._variables[var_name].definition_points.add(for_stmt.lineno)

        # Similar to while loop structure
        header_node = self._current_cfg.create_node(NodeType.LOOP_HEADER, for_stmt)
        self._current_cfg.add_edge(current_node_id, header_node.id)

        exit_node = self._current_cfg.create_node(NodeType.STATEMENT)
        self._loop_stack.append((header_node.id, exit_node.id))

        body_exit = self._analyze_statements(for_stmt.body)
        if body_exit is not None:
            self._current_cfg.add_edge(body_exit, header_node.id)

        self._current_cfg.add_edge(header_node.id, exit_node.id)
        self._loop_stack.pop()
        return exit_node.id

    def _analyze_return(self, return_stmt: ast.Return, current_node_id: int) -> int:
        """Analyze a return statement."""
        return_node = self._current_cfg.create_node(NodeType.RETURN, return_stmt)
        self._current_cfg.add_edge(current_node_id, return_node.id)
        self._current_cfg.exit_nodes.add(return_node.id)
        return return_node.id

    def _analyze_break(self, break_stmt: ast.Break, current_node_id: int) -> int:
        """Analyze a break statement."""
        break_node = self._current_cfg.create_node(NodeType.BREAK, break_stmt)
        self._current_cfg.add_edge(current_node_id, break_node.id)

        if self._loop_stack:
            _, exit_node_id = self._loop_stack[-1]
            self._current_cfg.add_edge(break_node.id, exit_node_id)

        return break_node.id

    def _analyze_continue(self, continue_stmt: ast.Continue, current_node_id: int) -> int:
        """Analyze a continue statement."""
        continue_node = self._current_cfg.create_node(NodeType.CONTINUE, continue_stmt)
        self._current_cfg.add_edge(current_node_id, continue_node.id)

        if self._loop_stack:
            header_node_id, _ = self._loop_stack[-1]
            self._current_cfg.add_edge(continue_node.id, header_node_id)

        return continue_node.id

    def _analyze_assign(self, assign_stmt: ast.Assign, current_node_id: int) -> int:
        """Analyze an assignment statement."""
        # Record variable assignments
        for target in assign_stmt.targets:
            if isinstance(target, ast.Name):
                var_name = target.id
                if var_name not in self._variables:
                    self._variables[var_name] = VariableInfo(
                        name=var_name, first_definition=assign_stmt.lineno, last_usage=assign_stmt.lineno
                    )
                self._variables[var_name].definition_points.add(assign_stmt.lineno)

        # Analyze value expression for variable usage
        self._analyze_expression(assign_stmt.value)

        # Create assignment node
        assign_node = self._current_cfg.create_node(NodeType.STATEMENT, assign_stmt)
        self._current_cfg.add_edge(current_node_id, assign_node.id)
        return assign_node.id

    def _analyze_aug_assign(self, aug_assign_stmt: ast.AugAssign, current_node_id: int) -> int:
        """Analyze an augmented assignment statement."""
        if isinstance(aug_assign_stmt.target, ast.Name):
            var_name = aug_assign_stmt.target.id
            if var_name not in self._variables:
                self._variables[var_name] = VariableInfo(
                    name=var_name, first_definition=aug_assign_stmt.lineno, last_usage=aug_assign_stmt.lineno
                )
            # Both usage and definition
            self._variables[var_name].usage_points.add(aug_assign_stmt.lineno)
            self._variables[var_name].definition_points.add(aug_assign_stmt.lineno)

        # Analyze value expression
        self._analyze_expression(aug_assign_stmt.value)

        # Create assignment node
        assign_node = self._current_cfg.create_node(NodeType.STATEMENT, aug_assign_stmt)
        self._current_cfg.add_edge(current_node_id, assign_node.id)
        return assign_node.id

    def _analyze_expression_statement(self, expr_stmt: ast.Expr, current_node_id: int) -> int:
        """Analyze an expression statement."""
        self._analyze_expression(expr_stmt.value)

        # Check if it's a function call
        if isinstance(expr_stmt.value, ast.Call):
            call_node = self._current_cfg.create_node(NodeType.FUNCTION_CALL, expr_stmt)
        else:
            call_node = self._current_cfg.create_node(NodeType.STATEMENT, expr_stmt)

        self._current_cfg.add_edge(current_node_id, call_node.id)
        return call_node.id

    def _analyze_expression(self, expr: ast.expr) -> None:
        """Analyze an expression for variable usage."""
        if isinstance(expr, ast.Name):
            var_name = expr.id
            if var_name in self._variables:
                line_no = getattr(expr, "lineno", 0)
                self._variables[var_name].usage_points.add(line_no)
                self._variables[var_name].last_usage = max(self._variables[var_name].last_usage, line_no)
        elif isinstance(expr, ast.Call):
            # Analyze function arguments
            for arg in expr.args:
                self._analyze_expression(arg)
            for keyword in expr.keywords:
                self._analyze_expression(keyword.value)
        elif isinstance(expr, ast.BinOp):
            self._analyze_expression(expr.left)
            self._analyze_expression(expr.right)
        elif isinstance(expr, ast.UnaryOp):
            self._analyze_expression(expr.operand)
        elif isinstance(expr, ast.Compare):
            self._analyze_expression(expr.left)
            for comparator in expr.comparators:
                self._analyze_expression(comparator)
        elif isinstance(expr, ast.BoolOp):
            for value in expr.values:
                self._analyze_expression(value)
        # Add more expression types as needed

    def _calculate_complexity_metrics(self) -> dict[str, int]:
        """Calculate various complexity metrics."""
        metrics = {
            "cyclomatic_complexity": 1,  # Base complexity
            "nodes": len(self._current_cfg.nodes),
            "edges": len(self._current_cfg.edges),
            "variables": len(self._variables),
            "loops": 0,
            "conditionals": 0,
            "function_calls": 0,
        }

        for node in self._current_cfg.nodes.values():
            if node.node_type == NodeType.CONDITION:
                metrics["conditionals"] += 1
                metrics["cyclomatic_complexity"] += 1
            elif node.node_type == NodeType.LOOP_HEADER:
                metrics["loops"] += 1
                metrics["cyclomatic_complexity"] += 1
            elif node.node_type == NodeType.FUNCTION_CALL:
                metrics["function_calls"] += 1

        return metrics

    def _analyze_patterns(self) -> tuple[list[str], list[str]]:
        """Analyze code patterns for issues and performance hints."""
        issues = []
        hints = []

        # Check for unused variables
        for var_name, var_info in self._variables.items():
            if not var_info.usage_points and not var_info.is_parameter:
                issues.append(f"Variable '{var_name}' is defined but never used")

        # Check for variables used before definition
        for var_name, var_info in self._variables.items():
            if var_info.usage_points and var_info.definition_points:
                min_usage = min(var_info.usage_points)
                min_definition = min(var_info.definition_points)
                if min_usage < min_definition and not var_info.is_parameter:
                    issues.append(f"Variable '{var_name}' may be used before definition")

        # Performance hints
        if len(self._variables) > 20:
            hints.append("Consider reducing the number of variables for better cache performance")

        complexity = self._calculate_complexity_metrics()
        if complexity["cyclomatic_complexity"] > 10:
            hints.append("High cyclomatic complexity - consider breaking into smaller functions")

        return issues, hints

    def _generate_findings(self) -> list[str]:
        """Generate a list of analysis findings."""
        findings = []

        findings.append(f"Control flow graph contains {len(self._current_cfg.nodes)} nodes")
        findings.append(f"Found {len(self._variables)} variables")

        dead_code = self._current_cfg.find_dead_code()
        if dead_code:
            findings.append(f"Detected {len(dead_code)} unreachable code blocks")

        complexity = self._calculate_complexity_metrics()
        findings.append(f"Cyclomatic complexity: {complexity['cyclomatic_complexity']}")

        return findings
