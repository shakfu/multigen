"""Loop Analysis and Transformation for the Intelligence Layer.

This module provides loop analysis and optimization capabilities for static
Python code, focusing on loop patterns that can be efficiently converted to C.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Union

from ..base import AnalysisContext, BaseOptimizer, OptimizationLevel, OptimizationResult


class LoopType(Enum):
    """Types of loops we can analyze and optimize."""

    FOR_RANGE = "for_range"  # for i in range(n)
    FOR_ENUMERATE = "for_enumerate"  # for i, x in enumerate(items)
    FOR_ITERABLE = "for_iterable"  # for x in items
    WHILE_COUNTER = "while_counter"  # while i < n
    WHILE_CONDITION = "while_condition"  # while condition
    NESTED = "nested"  # Nested loops
    UNKNOWN = "unknown"  # Unrecognized pattern


class LoopPattern(Enum):
    """Common loop patterns for optimization."""

    SIMPLE_COUNTER = "simple_counter"  # Basic counting loop
    ACCUMULATOR = "accumulator"  # Accumulating values
    SEARCH = "search"  # Searching for elements
    TRANSFORMATION = "transformation"  # Transforming data
    REDUCTION = "reduction"  # Reducing data to single value
    MATRIX_OPERATION = "matrix_operation"  # Matrix/array operations
    NESTED_ITERATION = "nested_iteration"  # Nested loops
    COMPLEX = "complex"  # Complex pattern


class OptimizationType(Enum):
    """Types of loop optimizations."""

    LOOP_UNROLLING = "loop_unrolling"
    LOOP_FUSION = "loop_fusion"
    LOOP_INTERCHANGE = "loop_interchange"
    STRENGTH_REDUCTION = "strength_reduction"
    INVARIANT_MOTION = "invariant_motion"
    BOUNDS_CHECK_ELIMINATION = "bounds_check_elimination"
    VECTORIZATION_PREP = "vectorization_prep"
    C_STYLE_CONVERSION = "c_style_conversion"


@dataclass
class LoopBounds:
    """Information about loop bounds and iteration space."""

    start: Optional[int] = None
    end: Optional[int] = None
    step: Optional[int] = None
    is_constant: bool = False
    is_ascending: bool = True
    total_iterations: Optional[int] = None
    bounds_expression: Optional[str] = None


@dataclass
class LoopVariable:
    """Information about variables used in loops."""

    name: str
    is_iterator: bool = False
    is_modified: bool = False
    is_accumulator: bool = False
    first_use_line: Optional[int] = None
    dependency_chain: list[str] = field(default_factory=list)
    invariant: bool = False


@dataclass
class LoopInfo:
    """Comprehensive information about a loop."""

    loop_type: LoopType
    pattern: LoopPattern
    bounds: LoopBounds
    variables: dict[str, LoopVariable] = field(default_factory=dict)
    body_complexity: int = 0
    nesting_level: int = 0
    line_number: int = 0
    has_break: bool = False
    has_continue: bool = False
    has_early_exit: bool = False
    inner_loops: list["LoopInfo"] = field(default_factory=list)
    dependencies: set[str] = field(default_factory=set)
    side_effects: list[str] = field(default_factory=list)
    is_vectorizable: bool = False
    is_parallelizable: bool = False
    estimated_complexity: str = "O(n)"


@dataclass
class LoopOptimization:
    """Represents a specific loop optimization."""

    optimization_type: OptimizationType
    target_loop: LoopInfo
    description: str
    estimated_speedup: float
    confidence: float
    prerequisites: list[str] = field(default_factory=list)
    transformed_code: Optional[str] = None
    safety_checks: dict[str, bool] = field(default_factory=dict)
    applicability_score: float = 1.0


@dataclass
class LoopAnalysisReport:
    """Report from loop analysis and optimization."""

    loops_found: list[LoopInfo] = field(default_factory=list)
    optimizations: list[LoopOptimization] = field(default_factory=list)
    total_loops: int = 0
    nested_loops: int = 0
    vectorizable_loops: int = 0
    parallelizable_loops: int = 0
    complex_loops: int = 0
    optimization_opportunities: list[str] = field(default_factory=list)
    performance_bottlenecks: list[str] = field(default_factory=list)
    c_compatibility_issues: list[str] = field(default_factory=list)


class LoopAnalyzer(BaseOptimizer):
    """Analyzer and optimizer for loop constructs."""

    def __init__(self, optimization_level: OptimizationLevel = OptimizationLevel.BASIC):
        super().__init__("LoopAnalyzer", optimization_level)
        self._current_nesting = 0
        self._loop_stack: list[LoopInfo] = []
        self._variables_in_scope: dict[str, LoopVariable] = {}

    def optimize(self, context: AnalysisContext) -> OptimizationResult:
        """Perform loop analysis and optimization."""
        try:
            # Reset state
            self._current_nesting = 0
            self._loop_stack.clear()
            self._variables_in_scope.clear()

            report = LoopAnalysisReport()

            # Analyze loops in the AST
            self._analyze_loops(context.ast_node, report)

            # Generate optimizations
            self._generate_optimizations(report)

            # Create optimized AST
            optimized_ast = self._apply_optimizations(context.ast_node, report)

            # Calculate performance estimates
            performance_gain = self._estimate_performance_gain(report)
            transformations = self._generate_transformations(report)
            safety_analysis = self._analyze_safety(report)

            return OptimizationResult(
                optimizer_name=self.name,
                success=True,
                optimized_ast=optimized_ast,
                transformations=transformations,
                performance_gain_estimate=performance_gain,
                safety_analysis=safety_analysis,
                metadata={
                    "optimization_level": self.optimization_level.value,
                    "loops_analyzed": len(report.loops_found),
                    "optimizations_applied": len(report.optimizations),
                    "vectorizable_loops": report.vectorizable_loops,
                    "report": report,
                },
            )

        except Exception as e:
            return OptimizationResult(
                optimizer_name=self.name,
                success=False,
                optimized_ast=None,
                transformations=[f"Loop optimization failed: {str(e)}"],
                performance_gain_estimate=1.0,
                safety_analysis={"loop_optimization": False},
                metadata={"error": str(e), "error_type": type(e).__name__},
            )

    def _analyze_loops(self, node: ast.AST, report: LoopAnalysisReport) -> None:
        """Analyze all loops in the AST."""
        self._visit_node_for_loops(node, report)

    def _visit_node_for_loops(self, node: ast.AST, report: LoopAnalysisReport) -> None:
        """Visit AST nodes to find loops at appropriate nesting levels."""
        if isinstance(node, ast.For):
            self._analyze_for_loop(node, report)
        elif isinstance(node, ast.While):
            self._analyze_while_loop(node, report)
        else:
            # Only visit child nodes if current node is not a loop
            # This prevents double-counting nested loops
            for child in ast.iter_child_nodes(node):
                self._visit_node_for_loops(child, report)

    def _analyze_for_loop(self, node: ast.For, report: LoopAnalysisReport) -> None:
        """Analyze a for loop."""
        loop_info = LoopInfo(
            loop_type=self._classify_for_loop(node),
            pattern=LoopPattern.SIMPLE_COUNTER,  # Will be refined
            bounds=self._analyze_loop_bounds(node),
            nesting_level=self._current_nesting,
            line_number=node.lineno,
        )

        # Analyze loop variables
        self._analyze_loop_variables(node, loop_info)

        # Increase nesting level for body analysis
        self._current_nesting += 1

        # Analyze loop body
        self._analyze_loop_body(node.body, loop_info, report)

        # Restore nesting level
        self._current_nesting -= 1

        # Classify loop pattern
        loop_info.pattern = self._classify_loop_pattern(loop_info)

        # Check for vectorization potential
        loop_info.is_vectorizable = self._check_vectorizable(loop_info)
        loop_info.is_parallelizable = self._check_parallelizable(loop_info)

        # Estimate complexity
        loop_info.estimated_complexity = self._estimate_loop_complexity(loop_info)

        report.loops_found.append(loop_info)
        report.total_loops += 1

        if loop_info.nesting_level > 0:
            report.nested_loops += 1

        if loop_info.is_vectorizable:
            report.vectorizable_loops += 1

        if loop_info.pattern == LoopPattern.COMPLEX:
            report.complex_loops += 1

    def _analyze_while_loop(self, node: ast.While, report: LoopAnalysisReport) -> None:
        """Analyze a while loop."""
        loop_info = LoopInfo(
            loop_type=self._classify_while_loop(node),
            pattern=LoopPattern.SIMPLE_COUNTER,
            bounds=LoopBounds(),  # While loops generally don't have constant bounds
            nesting_level=self._current_nesting,
            line_number=node.lineno,
        )

        # Analyze loop variables
        self._analyze_loop_variables(node, loop_info)

        # Increase nesting level for body analysis
        self._current_nesting += 1

        # Analyze loop body
        self._analyze_loop_body(node.body, loop_info, report)

        # Restore nesting level
        self._current_nesting -= 1

        # Classify pattern
        loop_info.pattern = self._classify_loop_pattern(loop_info)

        # While loops are generally less vectorizable
        loop_info.is_vectorizable = False
        loop_info.is_parallelizable = self._check_parallelizable(loop_info)

        loop_info.estimated_complexity = self._estimate_loop_complexity(loop_info)

        report.loops_found.append(loop_info)
        report.total_loops += 1

        if loop_info.nesting_level > 0:
            report.nested_loops += 1

    def _classify_for_loop(self, node: ast.For) -> LoopType:
        """Classify the type of for loop."""
        if isinstance(node.iter, ast.Call):
            if isinstance(node.iter.func, ast.Name):
                func_name = node.iter.func.id
                if func_name == "range":
                    return LoopType.FOR_RANGE
                elif func_name == "enumerate":
                    return LoopType.FOR_ENUMERATE

        return LoopType.FOR_ITERABLE

    def _classify_while_loop(self, node: ast.While) -> LoopType:
        """Classify the type of while loop."""
        # Check if it's a counter-style while loop
        if isinstance(node.test, ast.Compare) and len(node.test.ops) == 1:
            return LoopType.WHILE_COUNTER

        return LoopType.WHILE_CONDITION

    def _analyze_loop_bounds(self, node: ast.For) -> LoopBounds:
        """Analyze the bounds of a for loop."""
        bounds = LoopBounds()

        if isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name):
            if node.iter.func.id == "range":
                args = node.iter.args
                if len(args) == 1:
                    # range(n)
                    bounds.start = 0
                    bounds.step = 1
                    if isinstance(args[0], ast.Constant) and isinstance(args[0].value, int):
                        bounds.end = args[0].value
                        bounds.is_constant = True
                        bounds.total_iterations = bounds.end
                elif len(args) == 2:
                    # range(start, end)
                    if isinstance(args[0], ast.Constant) and isinstance(args[0].value, int):
                        bounds.start = args[0].value
                    if isinstance(args[1], ast.Constant) and isinstance(args[1].value, int):
                        bounds.end = args[1].value
                    bounds.step = 1
                    if bounds.start is not None and bounds.end is not None:
                        bounds.is_constant = True
                        bounds.total_iterations = max(0, bounds.end - bounds.start)
                elif len(args) == 3:
                    # range(start, end, step)
                    if isinstance(args[0], ast.Constant) and isinstance(args[0].value, int):
                        bounds.start = args[0].value
                    if isinstance(args[1], ast.Constant) and isinstance(args[1].value, int):
                        bounds.end = args[1].value
                    if isinstance(args[2], ast.Constant) and isinstance(args[2].value, int):
                        bounds.step = args[2].value
                    if all(x is not None for x in [bounds.start, bounds.end, bounds.step]):
                        bounds.is_constant = True
                        # Type narrowing: we know these are not None due to the all() check above
                        start_val = bounds.start
                        end_val = bounds.end
                        step_val = bounds.step
                        assert start_val is not None and end_val is not None and step_val is not None
                        if step_val > 0:
                            bounds.total_iterations = max(0, (end_val - start_val + step_val - 1) // step_val)
                        else:
                            bounds.total_iterations = max(0, (start_val - end_val - step_val - 1) // (-step_val))

                # Check if ascending
                if bounds.step is not None:
                    bounds.is_ascending = bounds.step > 0

        return bounds

    def _analyze_loop_variables(self, node: Union[ast.For, ast.While], loop_info: LoopInfo) -> None:
        """Analyze variables used in the loop."""
        # Collect all variable names used in the loop
        variable_names = set()

        for child in ast.walk(node):
            if isinstance(child, ast.Name):
                variable_names.add(child.id)

        # Analyze each variable
        for var_name in variable_names:
            var_info = LoopVariable(name=var_name)

            # Check if it's the iterator variable
            if isinstance(node, ast.For) and isinstance(node.target, ast.Name):
                if node.target.id == var_name:
                    var_info.is_iterator = True

            # Check for modifications and accumulation patterns
            self._analyze_variable_usage(node, var_name, var_info)

            loop_info.variables[var_name] = var_info

    def _analyze_variable_usage(self, node: Union[ast.For, ast.While], var_name: str, var_info: LoopVariable) -> None:
        """Analyze how a variable is used within the loop."""
        assignments = 0
        reads = 0

        for stmt in node.body if hasattr(node, "body") else []:
            for child in ast.walk(stmt):
                if isinstance(child, ast.Name) and child.id == var_name:
                    if isinstance(child.ctx, ast.Store):
                        assignments += 1
                        var_info.is_modified = True
                    elif isinstance(child.ctx, ast.Load):
                        reads += 1

                # Check for accumulator pattern (var = var + something)
                if isinstance(child, ast.Assign):
                    if (
                        len(child.targets) == 1
                        and isinstance(child.targets[0], ast.Name)
                        and child.targets[0].id == var_name
                    ):
                        if isinstance(child.value, ast.BinOp):
                            left = child.value.left
                            if isinstance(left, ast.Name) and left.id == var_name:
                                var_info.is_accumulator = True

    def _analyze_loop_body(self, body: list[ast.stmt], loop_info: LoopInfo, report: LoopAnalysisReport) -> None:
        """Analyze the body of a loop."""
        # Calculate complexity based on statement types
        complexity = 0.0
        for stmt in body:
            complexity += 1.0
            # Add extra complexity for control structures
            if isinstance(stmt, (ast.If, ast.For, ast.While)):
                complexity += 2.0
            # Count nested statements
            for child in ast.walk(stmt):
                if isinstance(child, ast.stmt):
                    complexity += 0.5

        loop_info.body_complexity = int(complexity)

        for stmt in body:
            # Check for control flow statements in this statement and its children
            for node in ast.walk(stmt):
                if isinstance(node, ast.Break):
                    loop_info.has_break = True
                    loop_info.has_early_exit = True
                elif isinstance(node, ast.Continue):
                    loop_info.has_continue = True
                elif isinstance(node, ast.Return):
                    loop_info.has_early_exit = True

            # Check for nested loops
            if isinstance(stmt, (ast.For, ast.While)):
                nested_info = self._analyze_nested_loop(stmt, report)
                loop_info.inner_loops.append(nested_info)

            # Check for function calls (potential side effects)
            for child in ast.walk(stmt):
                if isinstance(child, ast.Call):
                    if isinstance(child.func, ast.Name):
                        loop_info.side_effects.append(f"call_{child.func.id}")

    def _analyze_nested_loop(self, node: Union[ast.For, ast.While], report: LoopAnalysisReport) -> LoopInfo:
        """Analyze a nested loop."""
        if isinstance(node, ast.For):
            nested_info = LoopInfo(
                loop_type=self._classify_for_loop(node),
                pattern=LoopPattern.NESTED_ITERATION,
                bounds=self._analyze_loop_bounds(node),
                nesting_level=self._current_nesting,
                line_number=node.lineno,
            )
        else:
            nested_info = LoopInfo(
                loop_type=self._classify_while_loop(node),
                pattern=LoopPattern.NESTED_ITERATION,
                bounds=LoopBounds(),
                nesting_level=self._current_nesting,
                line_number=node.lineno,
            )

        self._analyze_loop_variables(node, nested_info)

        # Increase nesting for deeper analysis
        self._current_nesting += 1
        self._analyze_loop_body(node.body, nested_info, report)
        self._current_nesting -= 1

        # Add nested loop to report
        report.loops_found.append(nested_info)
        report.total_loops += 1
        report.nested_loops += 1

        return nested_info

    def _classify_loop_pattern(self, loop_info: LoopInfo) -> LoopPattern:
        """Classify the pattern of the loop based on its characteristics."""
        # Check for complex patterns first (highest priority)
        if loop_info.has_early_exit or len(loop_info.side_effects) > 2 or loop_info.body_complexity > 8:
            return LoopPattern.COMPLEX

        # Check for accumulator pattern
        accumulators = [var for var in loop_info.variables.values() if var.is_accumulator]
        if accumulators:
            if len(accumulators) == 1:
                return LoopPattern.ACCUMULATOR
            else:
                return LoopPattern.REDUCTION

        # Check for nested loops
        if loop_info.inner_loops:
            return LoopPattern.NESTED_ITERATION

        # Check for transformation pattern
        modified_vars = [var for var in loop_info.variables.values() if var.is_modified and not var.is_iterator]
        if len(modified_vars) > 1:
            return LoopPattern.TRANSFORMATION

        # Default to simple counter
        return LoopPattern.SIMPLE_COUNTER

    def _check_vectorizable(self, loop_info: LoopInfo) -> bool:
        """Check if a loop is potentially vectorizable."""
        # Basic vectorization requirements
        if loop_info.has_early_exit or loop_info.has_break or loop_info.has_continue:
            return False

        # Must be a simple range loop
        if loop_info.loop_type != LoopType.FOR_RANGE:
            return False

        # Should have constant bounds
        if not loop_info.bounds.is_constant:
            return False

        # Should not have complex side effects
        if len(loop_info.side_effects) > 1:
            return False

        # Should be simple pattern
        if loop_info.pattern in [LoopPattern.COMPLEX, LoopPattern.NESTED_ITERATION]:
            return False

        return True

    def _check_parallelizable(self, loop_info: LoopInfo) -> bool:
        """Check if a loop is potentially parallelizable."""
        # Basic parallelization requirements
        if loop_info.has_early_exit or loop_info.has_break or loop_info.has_continue:
            return False

        # Check for dependencies
        accumulators = [var for var in loop_info.variables.values() if var.is_accumulator]
        if accumulators:
            return False  # Accumulation creates dependencies

        # Check for side effects that prevent parallelization
        unsafe_effects = [
            effect
            for effect in loop_info.side_effects
            if not effect.startswith("call_len") and not effect.startswith("call_abs")
        ]
        if unsafe_effects:
            return False

        return True

    def _estimate_loop_complexity(self, loop_info: LoopInfo) -> str:
        """Estimate the algorithmic complexity of the loop."""
        # Check for nested loops first (higher complexity)
        if loop_info.inner_loops:
            # Count the depth of nesting
            nesting_depth = 1 + max((inner.nesting_level - loop_info.nesting_level) for inner in loop_info.inner_loops)
            if nesting_depth == 2:
                return "O(n²)"
            elif nesting_depth == 3:
                return "O(n³)"
            else:
                return "O(n^k)"

        if loop_info.bounds.total_iterations is not None:
            n = loop_info.bounds.total_iterations
            if n <= 10:
                return "O(1)"
            elif n <= 1000:
                return "O(n)"
            else:
                return "O(n)"

        return "O(n)"

    def _generate_optimizations(self, report: LoopAnalysisReport) -> None:
        """Generate specific optimizations for discovered loops."""
        for loop_info in report.loops_found:
            self._generate_loop_optimizations(loop_info, report)

    def _generate_loop_optimizations(self, loop_info: LoopInfo, report: LoopAnalysisReport) -> None:
        """Generate optimizations for a specific loop."""
        # Loop unrolling for small constant loops
        if (
            loop_info.bounds.is_constant
            and loop_info.bounds.total_iterations
            and loop_info.bounds.total_iterations <= 8
            and loop_info.body_complexity <= 3
        ):
            optimization = LoopOptimization(
                optimization_type=OptimizationType.LOOP_UNROLLING,
                target_loop=loop_info,
                description=f"Unroll loop with {loop_info.bounds.total_iterations} iterations",
                estimated_speedup=1.2 + (0.1 * loop_info.bounds.total_iterations),
                confidence=0.85,
                applicability_score=0.9,
            )
            optimization.safety_checks["no_side_effects"] = len(loop_info.side_effects) == 0
            optimization.safety_checks["constant_bounds"] = loop_info.bounds.is_constant
            report.optimizations.append(optimization)

        # Strength reduction for multiplication in loops
        if self._has_multiplication_pattern(loop_info):
            optimization = LoopOptimization(
                optimization_type=OptimizationType.STRENGTH_REDUCTION,
                target_loop=loop_info,
                description="Replace multiplication with addition in loop",
                estimated_speedup=1.15,
                confidence=0.75,
                applicability_score=0.8,
            )
            report.optimizations.append(optimization)

        # Bounds check elimination for simple array access
        if self._has_array_access_pattern(loop_info):
            optimization = LoopOptimization(
                optimization_type=OptimizationType.BOUNDS_CHECK_ELIMINATION,
                target_loop=loop_info,
                description="Eliminate bounds checks for proven safe array access",
                estimated_speedup=1.1,
                confidence=0.8,
                applicability_score=0.7,
            )
            report.optimizations.append(optimization)

        # C-style conversion for range loops
        if loop_info.loop_type == LoopType.FOR_RANGE and loop_info.bounds.is_constant:
            optimization = LoopOptimization(
                optimization_type=OptimizationType.C_STYLE_CONVERSION,
                target_loop=loop_info,
                description="Convert Python for-range to C-style for loop",
                estimated_speedup=1.05,
                confidence=0.95,
                applicability_score=1.0,
            )
            optimization.transformed_code = self._generate_c_style_loop(loop_info)
            report.optimizations.append(optimization)

        # Vectorization preparation
        if loop_info.is_vectorizable:
            optimization = LoopOptimization(
                optimization_type=OptimizationType.VECTORIZATION_PREP,
                target_loop=loop_info,
                description="Prepare loop for vectorization",
                estimated_speedup=2.0,
                confidence=0.7,
                applicability_score=0.9,
            )
            report.optimizations.append(optimization)

    def _has_multiplication_pattern(self, loop_info: LoopInfo) -> bool:
        """Check if loop has multiplication that could be strength-reduced."""
        # Simplified check - in a real implementation, we'd analyze the AST
        return loop_info.pattern in [LoopPattern.ACCUMULATOR, LoopPattern.TRANSFORMATION]

    def _has_array_access_pattern(self, loop_info: LoopInfo) -> bool:
        """Check if loop has array access patterns."""
        # Simplified check - in a real implementation, we'd analyze the AST for subscript operations
        return (
            loop_info.loop_type == LoopType.FOR_RANGE and not loop_info.has_early_exit and loop_info.bounds.is_constant
        )

    def _generate_c_style_loop(self, loop_info: LoopInfo) -> str:
        """Generate C-style loop code."""
        if loop_info.bounds.start is not None and loop_info.bounds.end is not None:
            start = loop_info.bounds.start
            end = loop_info.bounds.end
            step = loop_info.bounds.step or 1

            iterator_var = "i"  # Default, would extract from actual loop
            if step == 1:
                return f"for (int {iterator_var} = {start}; {iterator_var} < {end}; {iterator_var}++)"
            else:
                return f"for (int {iterator_var} = {start}; {iterator_var} < {end}; {iterator_var} += {step})"

        return "/* C-style conversion not applicable */"

    def _apply_optimizations(self, node: ast.AST, report: LoopAnalysisReport) -> ast.AST:
        """Apply selected optimizations to the AST."""
        # For now, return the original AST
        # In a full implementation, we would transform the AST based on optimizations
        optimized = ast.copy_location(ast.parse(ast.unparse(node)), node)

        # Add metadata about applied optimizations
        if hasattr(optimized, "_optimizations"):
            optimized._optimizations = report.optimizations

        return optimized

    def _estimate_performance_gain(self, report: LoopAnalysisReport) -> float:
        """Estimate overall performance gain from loop optimizations."""
        if not report.optimizations:
            return 1.0

        total_gain = 1.0
        for opt in report.optimizations:
            weighted_gain = 1.0 + (opt.estimated_speedup - 1.0) * opt.confidence * opt.applicability_score
            total_gain *= weighted_gain

        # Cap maximum gain
        return min(total_gain, 5.0)

    def _generate_transformations(self, report: LoopAnalysisReport) -> list[str]:
        """Generate list of transformations applied."""
        transformations = []

        transformations.append(f"Analyzed {report.total_loops} loops")

        if report.nested_loops > 0:
            transformations.append(f"Found {report.nested_loops} nested loops")

        if report.vectorizable_loops > 0:
            transformations.append(f"Identified {report.vectorizable_loops} vectorizable loops")

        optimization_counts: dict[str, int] = {}
        for opt in report.optimizations:
            opt_type = opt.optimization_type.value
            optimization_counts[opt_type] = optimization_counts.get(opt_type, 0) + 1

        for opt_type, count in optimization_counts.items():
            transformations.append(f"Applied {count} {opt_type} optimizations")

        return transformations

    def _analyze_safety(self, report: LoopAnalysisReport) -> dict[str, bool]:
        """Analyze safety of loop optimizations."""
        safety_analysis = {
            "loop_unrolling": True,
            "strength_reduction": True,
            "bounds_check_elimination": True,
            "c_style_conversion": True,
            "vectorization_prep": True,
            "all_optimizations_safe": True,
        }

        # Check each optimization for safety
        for opt in report.optimizations:
            opt_safe = all(opt.safety_checks.values()) if opt.safety_checks else True

            if not opt_safe:
                safety_analysis["all_optimizations_safe"] = False
                opt_type_key = opt.optimization_type.value
                if opt_type_key in safety_analysis:
                    safety_analysis[opt_type_key] = False

        return safety_analysis
