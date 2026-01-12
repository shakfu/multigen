"""Call Graph Construction and Analysis for the Intelligence Layer.

This module provides comprehensive call graph analysis capabilities for static
Python code analysis, focusing on function relationships and call patterns.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from ..base import AnalysisContext, AnalysisReport, BaseAnalyzer


class CallType(Enum):
    """Types of function calls in the call graph."""

    DIRECT = "direct"  # Direct function call
    INDIRECT = "indirect"  # Function pointer or dynamic call
    RECURSIVE = "recursive"  # Self-recursive call
    MUTUAL = "mutual"  # Mutually recursive call
    EXTERNAL = "external"  # Call to external/library function
    UNKNOWN = "unknown"  # Unable to determine call target


class CallContext(Enum):
    """Context in which a call occurs."""

    UNCONDITIONAL = "unconditional"  # Always executed
    CONDITIONAL = "conditional"  # Inside if/else block
    LOOP = "loop"  # Inside loop body
    EXCEPTION = "exception"  # Inside try/except block
    NESTED = "nested"  # Multiple levels of nesting


@dataclass
class CallSite:
    """Information about a specific function call site."""

    caller: str
    callee: str
    line_number: int
    call_type: CallType = CallType.DIRECT
    call_context: CallContext = CallContext.UNCONDITIONAL
    arguments_count: int = 0
    is_method_call: bool = False
    is_builtin: bool = False
    confidence: float = 1.0  # Confidence in call resolution


@dataclass
class FunctionNode:
    """Node representing a function in the call graph."""

    name: str
    is_defined: bool = True  # False for external functions
    is_recursive: bool = False
    is_leaf: bool = False  # No outgoing calls
    is_root: bool = False  # No incoming calls
    parameters_count: int = 0
    local_calls: set[str] = field(default_factory=set)
    external_calls: set[str] = field(default_factory=set)
    call_sites: list[CallSite] = field(default_factory=list)
    callers: set[str] = field(default_factory=set)
    callees: set[str] = field(default_factory=set)
    depth_from_root: int = 0
    max_call_depth: int = 0


@dataclass
class CallPath:
    """A path through the call graph."""

    functions: list[str] = field(default_factory=list)
    total_depth: int = 0
    has_cycles: bool = False
    cycle_functions: set[str] = field(default_factory=set)
    estimated_complexity: int = 0


@dataclass
class CallGraphMetrics:
    """Metrics derived from call graph analysis."""

    total_functions: int = 0
    total_call_sites: int = 0
    max_call_depth: int = 0
    average_fan_out: float = 0.0  # Average calls per function
    average_fan_in: float = 0.0  # Average callers per function
    cyclomatic_complexity: int = 0
    strongly_connected_components: int = 0
    leaf_functions: int = 0
    root_functions: int = 0
    recursive_functions: int = 0
    external_dependencies: int = 0


@dataclass
class CallGraphReport(AnalysisReport):
    """Report from call graph analysis."""

    call_graph: dict[str, FunctionNode] = field(default_factory=dict)
    call_sites: list[CallSite] = field(default_factory=list)
    call_paths: list[CallPath] = field(default_factory=list)
    metrics: CallGraphMetrics = field(default_factory=CallGraphMetrics)
    cycles: list[list[str]] = field(default_factory=list)
    critical_paths: list[CallPath] = field(default_factory=list)
    optimization_opportunities: list[str] = field(default_factory=list)


class CallGraphAnalyzer(BaseAnalyzer):
    """Analyzer for constructing and analyzing function call graphs."""

    def __init__(self) -> None:
        super().__init__("CallGraphAnalyzer")
        self._defined_functions: set[str] = set()
        self._call_sites: list[CallSite] = []
        self._current_function: Optional[str] = None
        self._current_context: CallContext = CallContext.UNCONDITIONAL
        self._context_stack: list[CallContext] = []
        self._builtin_functions = {
            "print",
            "len",
            "range",
            "enumerate",
            "zip",
            "map",
            "filter",
            "sum",
            "max",
            "min",
            "abs",
            "round",
            "sorted",
            "reversed",
            "all",
            "any",
            "isinstance",
            "hasattr",
            "getattr",
            "setattr",
        }

    def analyze(self, context: AnalysisContext) -> CallGraphReport:
        """Analyze call graph from the given context."""
        try:
            # Initialize analysis state
            self._reset_state()

            # First pass: collect function definitions
            self._collect_definitions(context.ast_node)

            # Second pass: collect call sites
            self._collect_call_sites(context.ast_node)

            # Build the call graph
            call_graph = self._build_call_graph()

            # Analyze call patterns
            call_paths = self._analyze_call_paths(call_graph)
            cycles = self._detect_cycles(call_graph)
            metrics = self._calculate_metrics(call_graph)

            # Find optimization opportunities
            optimizations = self._find_optimization_opportunities(call_graph, cycles)

            # Find critical paths
            critical_paths = self._find_critical_paths(call_paths)

            return CallGraphReport(
                analyzer_name=self.name,
                success=True,
                confidence=self._calculate_confidence(call_graph),
                findings=self._generate_findings(call_graph, cycles, metrics),
                warnings=self._generate_warnings(call_graph, cycles),
                errors=[],
                metadata={
                    "analysis_level": self.analysis_level.value,
                    "functions_analyzed": len(self._defined_functions),
                    "call_sites_found": len(self._call_sites),
                },
                call_graph=call_graph,
                call_sites=self._call_sites,
                call_paths=call_paths,
                metrics=metrics,
                cycles=cycles,
                critical_paths=critical_paths,
                optimization_opportunities=optimizations,
            )

        except Exception as e:
            return CallGraphReport(
                analyzer_name=self.name,
                success=False,
                confidence=0.0,
                findings=[],
                warnings=[],
                errors=[f"Call graph analysis failed: {str(e)}"],
                metadata={"error_type": type(e).__name__},
            )

    def _reset_state(self) -> None:
        """Reset analyzer state for new analysis."""
        self._defined_functions.clear()
        self._call_sites.clear()
        self._current_function = None
        self._current_context = CallContext.UNCONDITIONAL
        self._context_stack.clear()

    def _collect_definitions(self, node: ast.AST) -> None:
        """Collect all function definitions in the AST."""
        for child in ast.walk(node):
            if isinstance(child, ast.FunctionDef):
                self._defined_functions.add(child.name)

    def _collect_call_sites(self, node: ast.AST) -> None:
        """Collect all function call sites in the AST."""
        self._visit_node_for_calls(node)

    def _visit_node_for_calls(self, node: ast.AST) -> None:
        """Visit AST node and collect call sites."""
        if isinstance(node, ast.FunctionDef):
            old_function = self._current_function
            self._current_function = node.name

            for child in node.body:
                self._visit_node_for_calls(child)

            self._current_function = old_function

        elif isinstance(node, ast.If):
            self._push_context(CallContext.CONDITIONAL)
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)
            self._pop_context()

        elif isinstance(node, (ast.While, ast.For)):
            self._push_context(CallContext.LOOP)
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)
            self._pop_context()

        elif isinstance(node, ast.Try):
            self._push_context(CallContext.EXCEPTION)
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)
            self._pop_context()

        elif isinstance(node, ast.ExceptHandler):
            self._push_context(CallContext.EXCEPTION)
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)
            self._pop_context()

        elif isinstance(node, ast.Call):
            self._process_call(node)
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)

        else:
            for child_node in ast.iter_child_nodes(node):
                self._visit_node_for_calls(child_node)

    def _push_context(self, context: CallContext) -> None:
        """Push a new call context onto the stack."""
        self._context_stack.append(self._current_context)
        self._current_context = context

    def _pop_context(self) -> None:
        """Pop the current call context from the stack."""
        if self._context_stack:
            self._current_context = self._context_stack.pop()
        else:
            self._current_context = CallContext.UNCONDITIONAL

    def _process_call(self, node: ast.Call) -> None:
        """Process a function call node."""
        if not self._current_function:
            return

        callee = self._extract_callee_name(node)
        if not callee:
            return

        # Determine call type
        call_type = self._determine_call_type(callee)

        # Check if it's a method call
        is_method = isinstance(node.func, ast.Attribute)

        # Check if it's a builtin
        is_builtin = callee in self._builtin_functions

        call_site = CallSite(
            caller=self._current_function,
            callee=callee,
            line_number=node.lineno,
            call_type=call_type,
            call_context=self._current_context,
            arguments_count=len(node.args),
            is_method_call=is_method,
            is_builtin=is_builtin,
            confidence=self._calculate_call_confidence(node, callee),
        )

        self._call_sites.append(call_site)

    def _extract_callee_name(self, node: ast.Call) -> Optional[str]:
        """Extract the name of the called function."""
        if isinstance(node.func, ast.Name):
            return node.func.id
        elif isinstance(node.func, ast.Attribute):
            return node.func.attr
        return None

    def _determine_call_type(self, callee: str) -> CallType:
        """Determine the type of function call."""
        if callee == self._current_function:
            return CallType.RECURSIVE
        elif callee in self._defined_functions:
            return CallType.DIRECT
        elif callee in self._builtin_functions:
            return CallType.EXTERNAL
        else:
            return CallType.UNKNOWN

    def _calculate_call_confidence(self, node: ast.Call, callee: str) -> float:
        """Calculate confidence in call resolution."""
        if isinstance(node.func, ast.Name):
            if callee in self._defined_functions or callee in self._builtin_functions:
                return 1.0
            else:
                return 0.7  # Might be imported function
        elif isinstance(node.func, ast.Attribute):
            return 0.8  # Method calls have moderate confidence
        else:
            return 0.5  # Complex call expressions

    def _build_call_graph(self) -> dict[str, FunctionNode]:
        """Build the call graph from collected information."""
        call_graph = {}

        # Initialize nodes for all defined functions
        for func_name in self._defined_functions:
            call_graph[func_name] = FunctionNode(name=func_name)

        # Process call sites
        for call_site in self._call_sites:
            caller_node = call_graph.get(call_site.caller)
            if not caller_node:
                continue

            # Add call site to caller
            caller_node.call_sites.append(call_site)

            # Update caller's call sets
            if call_site.callee in self._defined_functions:
                caller_node.local_calls.add(call_site.callee)
                caller_node.callees.add(call_site.callee)
            else:
                caller_node.external_calls.add(call_site.callee)

            # Update callee's caller set (if it's a defined function)
            if call_site.callee in call_graph:
                call_graph[call_site.callee].callers.add(call_site.caller)

        # Set node properties
        for node in call_graph.values():
            node.is_leaf = len(node.callees) == 0
            node.is_root = len(node.callers) == 0
            node.is_recursive = node.name in node.callees

        return call_graph

    def _analyze_call_paths(self, call_graph: dict[str, FunctionNode]) -> list[CallPath]:
        """Analyze possible call paths through the graph."""
        paths = []

        # Find root functions (entry points)
        root_functions = [name for name, node in call_graph.items() if node.is_root]

        # Generate paths from each root
        for root in root_functions:
            root_paths = self._generate_paths_from_function(call_graph, root, set())
            paths.extend(root_paths)

        return paths

    def _generate_paths_from_function(
        self,
        call_graph: dict[str, FunctionNode],
        func_name: str,
        visited: set[str],
        current_path: Optional[list[str]] = None,
    ) -> list[CallPath]:
        """Generate all paths starting from a given function."""
        if current_path is None:
            current_path = []

        paths = []
        current_path = current_path + [func_name]

        # Check for cycles
        has_cycle = func_name in visited
        cycle_functions = set()
        if has_cycle:
            cycle_start = current_path.index(func_name)
            cycle_functions = set(current_path[cycle_start:])

        # Get function node
        node = call_graph.get(func_name)
        if not node:
            return [
                CallPath(
                    functions=current_path,
                    total_depth=len(current_path),
                    has_cycles=has_cycle,
                    cycle_functions=cycle_functions,
                    estimated_complexity=len(current_path),
                )
            ]

        # If this is a leaf or we've detected a cycle, end the path
        if node.is_leaf or has_cycle:
            return [
                CallPath(
                    functions=current_path,
                    total_depth=len(current_path),
                    has_cycles=has_cycle,
                    cycle_functions=cycle_functions,
                    estimated_complexity=len(current_path) + (2 if has_cycle else 0),
                )
            ]

        # Continue exploring callees
        new_visited = visited | {func_name}
        for callee in node.local_calls:
            if callee in call_graph:  # Only follow local calls
                sub_paths = self._generate_paths_from_function(call_graph, callee, new_visited, current_path)
                paths.extend(sub_paths)

        return (
            paths
            if paths
            else [
                CallPath(
                    functions=current_path,
                    total_depth=len(current_path),
                    has_cycles=has_cycle,
                    cycle_functions=cycle_functions,
                    estimated_complexity=len(current_path),
                )
            ]
        )

    def _detect_cycles(self, call_graph: dict[str, FunctionNode]) -> list[list[str]]:
        """Detect cycles in the call graph using DFS."""
        cycles = []
        visited = set()
        rec_stack = set()

        def dfs(node_name: str, path: list[str]) -> None:
            if node_name in rec_stack:
                # Found a cycle
                cycle_start = path.index(node_name)
                cycle = path[cycle_start:] + [node_name]
                cycles.append(cycle)
                return

            if node_name in visited:
                return

            visited.add(node_name)
            rec_stack.add(node_name)

            node = call_graph.get(node_name)
            if node:
                for callee in node.local_calls:
                    if callee in call_graph:
                        dfs(callee, path + [node_name])

            rec_stack.remove(node_name)

        for func_name in call_graph:
            if func_name not in visited:
                dfs(func_name, [])

        return cycles

    def _calculate_metrics(self, call_graph: dict[str, FunctionNode]) -> CallGraphMetrics:
        """Calculate metrics from the call graph."""
        metrics = CallGraphMetrics()

        metrics.total_functions = len(call_graph)
        metrics.total_call_sites = len(self._call_sites)

        # Calculate fan-out and fan-in
        total_fan_out = sum(len(node.callees) for node in call_graph.values())
        total_fan_in = sum(len(node.callers) for node in call_graph.values())

        metrics.average_fan_out = total_fan_out / metrics.total_functions if metrics.total_functions > 0 else 0.0
        metrics.average_fan_in = total_fan_in / metrics.total_functions if metrics.total_functions > 0 else 0.0

        # Count special function types
        metrics.leaf_functions = sum(1 for node in call_graph.values() if node.is_leaf)
        metrics.root_functions = sum(1 for node in call_graph.values() if node.is_root)
        metrics.recursive_functions = sum(1 for node in call_graph.values() if node.is_recursive)

        # Count external dependencies
        all_external_calls = set()
        for node in call_graph.values():
            all_external_calls.update(node.external_calls)
        metrics.external_dependencies = len(all_external_calls)

        # Calculate max call depth (simplified)
        metrics.max_call_depth = self._calculate_max_depth(call_graph)

        return metrics

    def _calculate_max_depth(self, call_graph: dict[str, FunctionNode]) -> int:
        """Calculate maximum call depth in the graph."""
        max_depth = 0

        def dfs_depth(node_name: str, visited: set[str]) -> int:
            if node_name in visited:
                return 0  # Avoid infinite recursion

            node = call_graph.get(node_name)
            if not node or node.is_leaf:
                return 1

            visited.add(node_name)
            depths = []
            for callee in node.local_calls:
                if callee in call_graph:
                    depths.append(dfs_depth(callee, visited.copy()))

            return 1 + max(depths) if depths else 1

        for func_name in call_graph:
            depth = dfs_depth(func_name, set())
            max_depth = max(max_depth, depth)

        return max_depth

    def _find_optimization_opportunities(
        self, call_graph: dict[str, FunctionNode], cycles: list[list[str]]
    ) -> list[str]:
        """Find potential optimization opportunities."""
        opportunities = []

        # Functions that are called frequently but never call others (potential inline candidates)
        for node in call_graph.values():
            if node.is_leaf and len(node.callers) >= 2:
                opportunities.append(f"Inline candidate: {node.name} (called by {len(node.callers)} functions)")

        # Functions with high fan-out (potential for refactoring)
        for node in call_graph.values():
            if len(node.callees) >= 5:
                opportunities.append(f"High fan-out: {node.name} calls {len(node.callees)} functions")

        # Recursive functions (potential for optimization)
        for node in call_graph.values():
            if node.is_recursive:
                opportunities.append(f"Recursive function: {node.name} (consider iterative alternative)")

        # Cycles (potential deadlock or inefficiency)
        for cycle in cycles:
            if len(cycle) > 2:
                opportunities.append(f"Complex cycle detected: {' -> '.join(cycle)}")

        return opportunities

    def _find_critical_paths(self, call_paths: list[CallPath]) -> list[CallPath]:
        """Find critical paths that might affect performance."""
        # Sort by complexity and depth
        sorted_paths = sorted(call_paths, key=lambda p: (p.estimated_complexity, p.total_depth), reverse=True)

        # Return top 5 most complex paths
        return sorted_paths[:5]

    def _calculate_confidence(self, call_graph: dict[str, FunctionNode]) -> float:
        """Calculate overall confidence in the analysis."""
        if not self._call_sites:
            return 1.0

        total_confidence = sum(call_site.confidence for call_site in self._call_sites)
        return total_confidence / len(self._call_sites)

    def _generate_findings(
        self, call_graph: dict[str, FunctionNode], cycles: list[list[str]], metrics: CallGraphMetrics
    ) -> list[str]:
        """Generate analysis findings."""
        findings = [
            f"Analyzed {metrics.total_functions} functions with {metrics.total_call_sites} call sites",
            f"Found {metrics.root_functions} entry points and {metrics.leaf_functions} leaf functions",
            f"Average fan-out: {metrics.average_fan_out:.2f}, fan-in: {metrics.average_fan_in:.2f}",
            f"Maximum call depth: {metrics.max_call_depth}",
            f"External dependencies: {metrics.external_dependencies}",
        ]

        if cycles:
            findings.append(f"Detected {len(cycles)} call cycles")

        if metrics.recursive_functions > 0:
            findings.append(f"Found {metrics.recursive_functions} recursive functions")

        return findings

    def _generate_warnings(self, call_graph: dict[str, FunctionNode], cycles: list[list[str]]) -> list[str]:
        """Generate analysis warnings."""
        warnings = []

        # Warn about complex cycles
        for cycle in cycles:
            if len(cycle) > 3:
                warnings.append(f"Complex call cycle: {' -> '.join(cycle)}")

        # Warn about functions with very high fan-out
        for node in call_graph.values():
            if len(node.callees) > 10:
                warnings.append(f"Function {node.name} has very high fan-out ({len(node.callees)} calls)")

        # Warn about unreachable functions
        for node in call_graph.values():
            if len(node.callers) == 0 and not node.name.startswith("_"):
                warnings.append(f"Function {node.name} appears to be unreachable")

        return warnings
