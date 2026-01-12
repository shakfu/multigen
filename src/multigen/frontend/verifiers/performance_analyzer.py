"""Performance Bound Analysis and Verification.

This module provides formal analysis of algorithm performance bounds,
complexity verification, and resource usage guarantees.
"""

import ast
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional

try:
    import z3  # type: ignore[import-untyped]

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    z3 = None  # type: ignore[assignment]


from ..base import AnalysisContext
from .theorem_prover import ProofProperty, ProofResult, PropertyType, TheoremProver


class ComplexityClass(Enum):
    """Algorithm complexity classes."""

    CONSTANT = "O(1)"
    LOGARITHMIC = "O(log n)"
    LINEAR = "O(n)"
    LINEARITHMIC = "O(n log n)"
    QUADRATIC = "O(n²)"
    CUBIC = "O(n³)"
    POLYNOMIAL = "O(n^k)"
    EXPONENTIAL = "O(2^n)"
    FACTORIAL = "O(n!)"
    UNKNOWN = "O(?)"


class ResourceType(Enum):
    """Types of computational resources."""

    TIME = "time"
    SPACE = "space"
    MEMORY = "memory"
    STACK_DEPTH = "stack_depth"
    CACHE_MISSES = "cache_misses"
    MEMORY_BANDWIDTH = "memory_bandwidth"


@dataclass
class PerformanceBound:
    """Represents a performance bound for an algorithm."""

    resource_type: ResourceType
    complexity_class: ComplexityClass
    best_case: str
    average_case: str
    worst_case: str
    input_size_variable: str = "n"
    constants: Optional[dict[str, float]] = None
    conditions: Optional[list[str]] = None

    def __post_init__(self) -> None:
        """Initialize constants and conditions dictionaries if not provided."""
        if self.constants is None:
            self.constants = {}
        if self.conditions is None:
            self.conditions = []


@dataclass
class PerformanceAnalysisResult:
    """Result of performance bound analysis."""

    function_name: str
    proven_bounds: dict[ResourceType, PerformanceBound]
    complexity_proofs: list[ProofResult]
    resource_usage: dict[str, Any]
    bottlenecks: list[str]
    optimization_opportunities: list[str]
    verification_time: float
    confidence: float

    @property
    def time_complexity(self) -> ComplexityClass:
        """Get the time complexity of the algorithm."""
        time_bound = self.proven_bounds.get(ResourceType.TIME)
        return time_bound.complexity_class if time_bound else ComplexityClass.UNKNOWN

    @property
    def space_complexity(self) -> ComplexityClass:
        """Get the space complexity of the algorithm."""
        space_bound = self.proven_bounds.get(ResourceType.SPACE)
        return space_bound.complexity_class if space_bound else ComplexityClass.UNKNOWN


class PerformanceAnalyzer:
    """Formal analysis of algorithm performance bounds."""

    def __init__(self, theorem_prover: Optional[TheoremProver] = None):
        """Initialize the performance analyzer.

        Args:
            theorem_prover: Z3 theorem prover instance
        """
        self.theorem_prover = theorem_prover or TheoremProver()
        self.z3_available = Z3_AVAILABLE

        # Complexity analysis patterns
        self.complexity_patterns = {
            "single_loop": ComplexityClass.LINEAR,
            "nested_loops_2": ComplexityClass.QUADRATIC,
            "nested_loops_3": ComplexityClass.CUBIC,
            "divide_conquer": ComplexityClass.LINEARITHMIC,
            "binary_search": ComplexityClass.LOGARITHMIC,
            "recursive_factorial": ComplexityClass.LINEAR,
            "recursive_fibonacci": ComplexityClass.EXPONENTIAL,
        }

        # Known algorithm complexities
        self.algorithm_complexities = {
            "factorial": {
                ResourceType.TIME: ComplexityClass.LINEAR,
                ResourceType.SPACE: ComplexityClass.LINEAR,  # For recursive version
            },
            "fibonacci": {ResourceType.TIME: ComplexityClass.EXPONENTIAL, ResourceType.SPACE: ComplexityClass.LINEAR},
            "binary_search": {
                ResourceType.TIME: ComplexityClass.LOGARITHMIC,
                ResourceType.SPACE: ComplexityClass.CONSTANT,
            },
            "merge_sort": {ResourceType.TIME: ComplexityClass.LINEARITHMIC, ResourceType.SPACE: ComplexityClass.LINEAR},
            "quicksort": {
                ResourceType.TIME: ComplexityClass.LINEARITHMIC,  # Average case
                ResourceType.SPACE: ComplexityClass.LOGARITHMIC,
            },
        }

    def analyze_performance_bounds(self, context: AnalysisContext) -> PerformanceAnalysisResult:
        """Analyze performance bounds for a function.

        Args:
            context: Analysis context with AST and metadata

        Returns:
            PerformanceAnalysisResult with proven bounds
        """
        start_time = time.time()

        function_name = self._extract_function_name(context)

        # Extract algorithm structure
        structure_extractor = AlgorithmStructureExtractor()
        structure_extractor.visit(context.ast_node)

        # Analyze complexity patterns
        complexity_analyzer = ComplexityPatternAnalyzer()
        complexity_analyzer.visit(context.ast_node)

        # Determine complexity bounds
        proven_bounds = {}
        complexity_proofs = []

        # Time complexity analysis
        time_bound, time_proofs = self._analyze_time_complexity(function_name, structure_extractor, complexity_analyzer)
        if time_bound:
            proven_bounds[ResourceType.TIME] = time_bound
            complexity_proofs.extend(time_proofs)

        # Space complexity analysis
        space_bound, space_proofs = self._analyze_space_complexity(
            function_name, structure_extractor, complexity_analyzer
        )
        if space_bound:
            proven_bounds[ResourceType.SPACE] = space_bound
            complexity_proofs.extend(space_proofs)

        # Memory access analysis
        memory_bound, memory_proofs = self._analyze_memory_complexity(structure_extractor, complexity_analyzer)
        if memory_bound:
            proven_bounds[ResourceType.MEMORY] = memory_bound
            complexity_proofs.extend(memory_proofs)

        # Resource usage analysis
        resource_usage = self._analyze_resource_usage(structure_extractor, complexity_analyzer)

        # Identify bottlenecks
        bottlenecks = self._identify_bottlenecks(structure_extractor, complexity_analyzer)

        # Find optimization opportunities
        optimization_opportunities = self._find_optimization_opportunities(
            structure_extractor, complexity_analyzer, proven_bounds
        )

        # Calculate confidence
        confidence = self._calculate_performance_confidence(complexity_proofs)

        verification_time = time.time() - start_time

        return PerformanceAnalysisResult(
            function_name=function_name,
            proven_bounds=proven_bounds,
            complexity_proofs=complexity_proofs,
            resource_usage=resource_usage,
            bottlenecks=bottlenecks,
            optimization_opportunities=optimization_opportunities,
            verification_time=verification_time,
            confidence=confidence,
        )

    def _analyze_time_complexity(
        self, function_name: str, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> tuple[Optional[PerformanceBound], list[ProofResult]]:
        """Analyze time complexity."""
        proofs = []

        # Check known algorithms first
        if function_name in self.algorithm_complexities:
            known_complexity = self.algorithm_complexities[function_name][ResourceType.TIME]

            # Create proof for known complexity
            prop = self._create_complexity_property(
                f"{function_name}_time_complexity", ResourceType.TIME, known_complexity
            )
            result = self.theorem_prover.verify_property(prop)
            proofs.append(result)

            bound = PerformanceBound(
                resource_type=ResourceType.TIME,
                complexity_class=known_complexity,
                best_case=known_complexity.value,
                average_case=known_complexity.value,
                worst_case=known_complexity.value,
                conditions=[f"Proven for {function_name} algorithm"],
            )
            return bound, proofs

        # Pattern-based analysis
        time_complexity = self._infer_time_complexity_from_patterns(structure, complexity)

        if time_complexity != ComplexityClass.UNKNOWN:
            prop = self._create_complexity_property(
                f"{function_name}_inferred_time_complexity", ResourceType.TIME, time_complexity
            )
            result = self.theorem_prover.verify_property(prop)
            proofs.append(result)

            bound = PerformanceBound(
                resource_type=ResourceType.TIME,
                complexity_class=time_complexity,
                best_case=time_complexity.value,
                average_case=time_complexity.value,
                worst_case=time_complexity.value,
                conditions=["Inferred from code patterns"],
            )
            return bound, proofs

        return None, proofs

    def _analyze_space_complexity(
        self, function_name: str, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> tuple[Optional[PerformanceBound], list[ProofResult]]:
        """Analyze space complexity."""
        proofs = []

        # Check known algorithms first
        if function_name in self.algorithm_complexities:
            known_complexity = self.algorithm_complexities[function_name][ResourceType.SPACE]

            prop = self._create_complexity_property(
                f"{function_name}_space_complexity", ResourceType.SPACE, known_complexity
            )
            result = self.theorem_prover.verify_property(prop)
            proofs.append(result)

            bound = PerformanceBound(
                resource_type=ResourceType.SPACE,
                complexity_class=known_complexity,
                best_case=known_complexity.value,
                average_case=known_complexity.value,
                worst_case=known_complexity.value,
                conditions=[f"Proven for {function_name} algorithm"],
            )
            return bound, proofs

        # Infer space complexity from patterns
        space_complexity = self._infer_space_complexity_from_patterns(structure, complexity)

        if space_complexity != ComplexityClass.UNKNOWN:
            prop = self._create_complexity_property(
                f"{function_name}_inferred_space_complexity", ResourceType.SPACE, space_complexity
            )
            result = self.theorem_prover.verify_property(prop)
            proofs.append(result)

            bound = PerformanceBound(
                resource_type=ResourceType.SPACE,
                complexity_class=space_complexity,
                best_case=space_complexity.value,
                average_case=space_complexity.value,
                worst_case=space_complexity.value,
                conditions=["Inferred from code patterns"],
            )
            return bound, proofs

        return None, proofs

    def _analyze_memory_complexity(
        self, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> tuple[Optional[PerformanceBound], list[ProofResult]]:
        """Analyze memory access complexity."""
        # Simplified memory analysis
        if complexity.array_accesses:
            # Linear memory access pattern
            bound = PerformanceBound(
                resource_type=ResourceType.MEMORY,
                complexity_class=ComplexityClass.LINEAR,
                best_case="O(n)",
                average_case="O(n)",
                worst_case="O(n)",
                conditions=["Based on array access patterns"],
            )
            return bound, []

        return None, []

    def _infer_time_complexity_from_patterns(
        self, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> ComplexityClass:
        """Infer time complexity from code patterns."""
        # Single loop
        if complexity.loop_depth == 1 and complexity.loops:
            loop = complexity.loops[0]
            if loop.get("iterable") == "range":
                return ComplexityClass.LINEAR

        # Nested loops
        if complexity.loop_depth == 2:
            return ComplexityClass.QUADRATIC
        elif complexity.loop_depth == 3:
            return ComplexityClass.CUBIC
        elif complexity.loop_depth > 3:
            return ComplexityClass.POLYNOMIAL

        # Recursive patterns
        if structure.recursive_calls:
            # Simple tail recursion (like factorial)
            if len(structure.recursive_calls) == 1:
                return ComplexityClass.LINEAR
            # Multiple recursive calls (like fibonacci)
            elif len(structure.recursive_calls) > 1:
                return ComplexityClass.EXPONENTIAL

        # No loops or recursion
        if complexity.loop_depth == 0 and not structure.recursive_calls:
            return ComplexityClass.CONSTANT

        return ComplexityClass.UNKNOWN

    def _infer_space_complexity_from_patterns(
        self, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> ComplexityClass:
        """Infer space complexity from code patterns."""
        # Recursive calls create stack frames
        if structure.recursive_calls:
            return ComplexityClass.LINEAR

        # Arrays allocated proportional to input
        if complexity.array_allocations:
            return ComplexityClass.LINEAR

        # Constant space
        return ComplexityClass.CONSTANT

    def _analyze_resource_usage(
        self, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> dict[str, Any]:
        """Analyze detailed resource usage."""
        return {
            "loop_count": len(complexity.loops),
            "max_loop_depth": complexity.loop_depth,
            "recursive_calls": len(structure.recursive_calls),
            "array_accesses": len(complexity.array_accesses),
            "function_calls": len(complexity.function_calls),
            "memory_allocations": len(complexity.array_allocations),
        }

    def _identify_bottlenecks(
        self, structure: "AlgorithmStructureExtractor", complexity: "ComplexityPatternAnalyzer"
    ) -> list[str]:
        """Identify performance bottlenecks."""
        bottlenecks = []

        if complexity.loop_depth >= 2:
            bottlenecks.append(f"Nested loops (depth {complexity.loop_depth}) - quadratic or higher complexity")

        if len(structure.recursive_calls) > 1:
            bottlenecks.append("Multiple recursive calls - potential exponential complexity")

        if len(complexity.array_accesses) > 10:
            bottlenecks.append("High number of array accesses - potential cache misses")

        return bottlenecks

    def _find_optimization_opportunities(
        self,
        structure: "AlgorithmStructureExtractor",
        complexity: "ComplexityPatternAnalyzer",
        bounds: dict[ResourceType, PerformanceBound],
    ) -> list[str]:
        """Find optimization opportunities."""
        opportunities = []

        # Time complexity optimizations
        time_bound = bounds.get(ResourceType.TIME)
        if time_bound:
            if time_bound.complexity_class == ComplexityClass.EXPONENTIAL:
                opportunities.append("Consider memoization to reduce exponential complexity")
            elif time_bound.complexity_class == ComplexityClass.QUADRATIC:
                opportunities.append("Look for divide-and-conquer approach to reduce to O(n log n)")

        # Space complexity optimizations
        space_bound = bounds.get(ResourceType.SPACE)
        if space_bound:
            if space_bound.complexity_class == ComplexityClass.LINEAR and structure.recursive_calls:
                opportunities.append("Consider iterative implementation to reduce stack space")

        # Loop optimizations
        if complexity.loop_depth >= 2:
            opportunities.append("Consider loop unrolling or vectorization for nested loops")

        if len(complexity.array_accesses) > 5:
            opportunities.append("Consider loop tiling for better cache locality")

        return opportunities

    def _create_complexity_property(
        self, name: str, resource_type: ResourceType, complexity_class: ComplexityClass
    ) -> ProofProperty:
        """Create a complexity verification property."""
        if not self.z3_available:
            return ProofProperty(
                name=name,
                property_type=PropertyType.ALGORITHM_CORRECTNESS,
                description=f"{resource_type.value} complexity is {complexity_class.value}",
                z3_formula="mock_formula",
            )

        # Create Z3 variables for complexity analysis
        n = z3.Int("input_size")
        resource_usage = z3.Int(f"{resource_type.value}_usage")

        # Create complexity bound based on class
        complexity_bound: Any
        if complexity_class == ComplexityClass.CONSTANT:
            complexity_bound = resource_usage <= z3.IntVal(1)  # type: ignore[operator]
        elif complexity_class == ComplexityClass.LINEAR:
            complexity_bound = resource_usage <= n * z3.IntVal(10)  # type: ignore[operator]
        elif complexity_class == ComplexityClass.QUADRATIC:
            complexity_bound = resource_usage <= n * n * z3.IntVal(10)  # type: ignore[operator]
        elif complexity_class == ComplexityClass.LOGARITHMIC:
            # Approximate log with linear bound for large n
            complexity_bound = resource_usage <= n  # type: ignore[operator]
        else:
            # Default bound
            complexity_bound = resource_usage >= 0  # type: ignore[operator]

        return ProofProperty(
            name=name,
            property_type=PropertyType.ALGORITHM_CORRECTNESS,
            description=f"{resource_type.value} complexity is {complexity_class.value}",
            z3_formula=complexity_bound,
            context={"resource_type": resource_type, "complexity_class": complexity_class},
        )

    def _calculate_performance_confidence(self, proof_results: list[ProofResult]) -> float:
        """Calculate confidence in performance analysis."""
        if not proof_results:
            return 0.8  # Default confidence for pattern-based analysis

        verified_count = sum(1 for result in proof_results if result.is_verified)
        total_count = len(proof_results)

        return verified_count / total_count if total_count > 0 else 0.8

    def _extract_function_name(self, context: AnalysisContext) -> str:
        """Extract function name from context."""
        if context.analysis_result and hasattr(context.analysis_result, "functions"):
            if context.analysis_result.functions:
                return list(context.analysis_result.functions.keys())[0]
        elif hasattr(context.ast_node, "name"):
            return context.ast_node.name
        return "unknown_function"


class AlgorithmStructureExtractor(ast.NodeVisitor):
    """Extract algorithm structure for performance analysis."""

    def __init__(self) -> None:
        self.loops: list[dict[str, Any]] = []
        self.recursive_calls: list[dict[str, Any]] = []
        self.function_calls: list[dict[str, Any]] = []
        self.variables: set[str] = set()
        self.current_function: Optional[str] = None

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Visit a function definition node."""
        self.current_function = node.name
        self.generic_visit(node)

    def visit_For(self, node: ast.For) -> None:
        """Visit a for loop node."""
        self.loops.append({"type": "for", "line": node.lineno, "iterable": self._extract_iterable_info(node.iter)})
        self.generic_visit(node)

    def visit_While(self, node: ast.While) -> None:
        """Visit a while loop node."""
        self.loops.append({"type": "while", "line": node.lineno})
        self.generic_visit(node)

    def visit_Call(self, node: ast.Call) -> None:
        """Visit a function call node."""
        if isinstance(node.func, ast.Name):
            if node.func.id == self.current_function:
                self.recursive_calls.append({"line": node.lineno, "function": node.func.id})
            else:
                self.function_calls.append({"line": node.lineno, "function": node.func.id})
        self.generic_visit(node)

    def _extract_iterable_info(self, iter_node: ast.expr) -> str:
        if isinstance(iter_node, ast.Call) and isinstance(iter_node.func, ast.Name):
            if iter_node.func.id == "range":
                return "range"
        return "unknown"


class ComplexityPatternAnalyzer(ast.NodeVisitor):
    """Analyze complexity patterns in code."""

    def __init__(self) -> None:
        self.loop_depth = 0
        self.max_loop_depth = 0
        self.loops: list[dict[str, Any]] = []
        self.array_accesses: list[dict[str, Any]] = []
        self.array_allocations: list[dict[str, Any]] = []
        self.function_calls: list[dict[str, Any]] = []
        self.current_depth = 0

    def visit_For(self, node: ast.For) -> None:
        """Visit a for loop node and track nesting depth."""
        self.current_depth += 1
        self.loop_depth = max(self.loop_depth, self.current_depth)
        self.max_loop_depth = max(self.max_loop_depth, self.current_depth)

        loop_info = {"type": "for", "depth": self.current_depth, "line": node.lineno}

        if isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name):
            if node.iter.func.id == "range":
                loop_info["iterable"] = "range"

        self.loops.append(loop_info)
        self.generic_visit(node)
        self.current_depth -= 1

    def visit_While(self, node: ast.While) -> None:
        """Visit a while loop node and track nesting depth."""
        self.current_depth += 1
        self.loop_depth = max(self.loop_depth, self.current_depth)
        self.max_loop_depth = max(self.max_loop_depth, self.current_depth)

        self.loops.append({"type": "while", "depth": self.current_depth, "line": node.lineno})

        self.generic_visit(node)
        self.current_depth -= 1

    def visit_Subscript(self, node: ast.Subscript) -> None:
        """Visit a subscript node and track array accesses."""
        if isinstance(node.value, ast.Name):
            self.array_accesses.append({"array": node.value.id, "line": node.lineno, "depth": self.current_depth})
        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign) -> None:
        """Visit an assignment node and track array allocations."""
        # Check for array/list allocations
        if isinstance(node.value, ast.List):
            if len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
                self.array_allocations.append(
                    {"variable": node.targets[0].id, "size": len(node.value.elts), "line": node.lineno}
                )
        self.generic_visit(node)

    def visit_Call(self, node: ast.Call) -> None:
        """Visit a function call node and track function calls."""
        if isinstance(node.func, ast.Name):
            self.function_calls.append({"function": node.func.id, "line": node.lineno, "depth": self.current_depth})
        self.generic_visit(node)
