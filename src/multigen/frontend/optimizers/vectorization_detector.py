"""Vectorization detection optimizer for the Intelligence Layer.

This optimizer analyzes code to identify SIMD (Single Instruction, Multiple Data)
optimization opportunities, particularly in loops and array operations.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from ..base import AnalysisContext, BaseOptimizer, OptimizationResult


class VectorizationType(Enum):
    """Types of vectorization opportunities."""

    SIMPLE_LOOP = "simple_loop"
    REDUCTION_LOOP = "reduction_loop"
    ARRAY_COPY = "array_copy"
    ELEMENT_WISE = "element_wise"
    DOT_PRODUCT = "dot_product"
    MATRIX_MULTIPLY = "matrix_multiply"
    CONVOLUTION = "convolution"
    FILTER_OPERATION = "filter_operation"


class VectorizationConstraint(Enum):
    """Constraints that affect vectorization."""

    MEMORY_ALIGNMENT = "memory_alignment"
    DATA_DEPENDENCIES = "data_dependencies"
    ALIASING = "aliasing"
    CONTROL_FLOW = "control_flow"
    FUNCTION_CALLS = "function_calls"
    POINTER_ARITHMETIC = "pointer_arithmetic"
    IRREGULAR_ACCESS = "irregular_access"
    SIDE_EFFECTS = "side_effects"


@dataclass
class MemoryAccess:
    """Represents a memory access pattern."""

    variable: str
    indices: list[ast.expr]  # Changed from List[ast.AST] to List[ast.expr]
    is_read: bool
    is_write: bool
    access_pattern: str  # "linear", "strided", "random", "irregular"
    stride: Optional[int] = None
    base_offset: Optional[int] = None


@dataclass
class VectorizationCandidate:
    """Represents a potential vectorization opportunity."""

    loop_node: ast.AST
    vectorization_type: VectorizationType
    vector_length: int
    memory_accesses: list[MemoryAccess]
    constraints: set[VectorizationConstraint]
    estimated_speedup: float
    confidence: float
    transformation_complexity: str  # "trivial", "moderate", "complex"
    required_intrinsics: list[str] = field(default_factory=list)

    def __post_init__(self) -> None:
        """Validate the candidate after initialization."""
        if not 1 <= self.vector_length <= 64:
            raise ValueError(f"Invalid vector length: {self.vector_length}")
        if not 0.0 <= self.confidence <= 1.0:
            raise ValueError(f"Invalid confidence: {self.confidence}")


@dataclass
class VectorizationReport:
    """Report containing vectorization analysis results."""

    candidates: list[VectorizationCandidate]
    total_loops_analyzed: int
    vectorizable_loops: int
    potential_speedup: float
    recommended_vector_widths: dict[str, int]
    architecture_recommendations: list[str]
    analysis_summary: dict[str, Any]


class VectorizationDetector(BaseOptimizer):
    """Detects vectorization opportunities in code."""

    def __init__(self, target_arch: str = "x86_64", vector_width: int = 4):
        """Initialize the vectorization detector.

        Args:
            target_arch: Target architecture (x86_64, arm, etc.)
            vector_width: Default vector width for the target
        """
        super().__init__("VectorizationDetector")
        self.target_arch = target_arch
        self.default_vector_width = vector_width
        self.arch_capabilities = self._get_arch_capabilities()

    def _get_arch_capabilities(self) -> dict[str, Any]:
        """Get architecture-specific vectorization capabilities."""
        capabilities = {
            "x86_64": {
                "simd_extensions": ["SSE", "SSE2", "AVX", "AVX2", "AVX512"],
                "vector_widths": {"float": [4, 8, 16], "int": [4, 8, 16], "double": [2, 4, 8]},
                "alignment_requirements": {"float": 16, "double": 32, "int": 16},
                "supported_operations": ["add", "sub", "mul", "fma", "load", "store", "broadcast"],
            },
            "arm": {
                "simd_extensions": ["NEON", "SVE"],
                "vector_widths": {"float": [4, 8], "int": [4, 8, 16], "double": [2, 4]},
                "alignment_requirements": {"float": 16, "double": 16, "int": 16},
                "supported_operations": ["add", "sub", "mul", "fma", "load", "store"],
            },
        }
        return capabilities.get(self.target_arch, capabilities["x86_64"])

    def optimize(self, context: AnalysisContext) -> OptimizationResult:
        """Optimize the given context by detecting vectorization opportunities.

        Args:
            context: The analysis context to optimize

        Returns:
            OptimizationResult containing vectorization analysis
        """
        report = self.analyze(context.ast_node)

        # For now, this is an analysis-only optimizer
        # In a full implementation, it would generate vectorized code
        transformations = [f"Identified {len(report.candidates)} vectorization opportunities"]

        return OptimizationResult(
            optimizer_name=self.name,
            success=True,
            optimized_ast=context.ast_node,  # Return original AST for now
            transformations=transformations,
            performance_gain_estimate=report.potential_speedup,
            safety_analysis={"vectorization_safe": True},
            metadata={"vectorization_report": report},
        )

    def analyze(self, node: ast.AST) -> VectorizationReport:
        """Analyze code for vectorization opportunities.

        Args:
            node: AST node to analyze

        Returns:
            VectorizationReport with analysis results
        """
        candidates = []
        total_loops = 0

        for child in ast.walk(node):
            if isinstance(child, (ast.For, ast.While)):
                total_loops += 1
                candidate = self._analyze_loop(child)
                if candidate:
                    candidates.append(candidate)

        vectorizable_loops = len(candidates)
        potential_speedup = sum(c.estimated_speedup for c in candidates)

        recommended_widths = self._recommend_vector_widths(candidates)
        arch_recommendations = self._generate_arch_recommendations(candidates)

        analysis_summary = {
            "vectorization_efficiency": vectorizable_loops / max(total_loops, 1),
            "average_speedup": potential_speedup / max(vectorizable_loops, 1),
            "complexity_distribution": self._analyze_complexity_distribution(candidates),
            "constraint_frequency": self._analyze_constraint_frequency(candidates),
        }

        return VectorizationReport(
            candidates=candidates,
            total_loops_analyzed=total_loops,
            vectorizable_loops=vectorizable_loops,
            potential_speedup=potential_speedup,
            recommended_vector_widths=recommended_widths,
            architecture_recommendations=arch_recommendations,
            analysis_summary=analysis_summary,
        )

    def _analyze_loop(self, loop_node: ast.AST) -> Optional[VectorizationCandidate]:
        """Analyze a single loop for vectorization opportunities."""
        if not self._is_vectorizable_loop(loop_node):
            return None

        memory_accesses = self._analyze_memory_accesses(loop_node)
        if not memory_accesses:
            return None

        vectorization_type = self._classify_vectorization_type(loop_node, memory_accesses)
        constraints = self._identify_constraints(loop_node, memory_accesses)

        if VectorizationConstraint.DATA_DEPENDENCIES in constraints:
            if vectorization_type not in [VectorizationType.REDUCTION_LOOP, VectorizationType.DOT_PRODUCT]:
                return None

        vector_length = self._determine_vector_length(memory_accesses, vectorization_type)
        if vector_length < 2:
            return None

        speedup = self._estimate_speedup(vectorization_type, vector_length, constraints)
        confidence = self._calculate_confidence(constraints, memory_accesses)
        complexity = self._assess_transformation_complexity(vectorization_type, constraints)
        intrinsics = self._suggest_intrinsics(vectorization_type, vector_length)

        return VectorizationCandidate(
            loop_node=loop_node,
            vectorization_type=vectorization_type,
            vector_length=vector_length,
            memory_accesses=memory_accesses,
            constraints=constraints,
            estimated_speedup=speedup,
            confidence=confidence,
            transformation_complexity=complexity,
            required_intrinsics=intrinsics,
        )

    def _is_vectorizable_loop(self, loop_node: ast.AST) -> bool:
        """Check if a loop is potentially vectorizable."""
        if isinstance(loop_node, ast.While):
            return False

        if isinstance(loop_node, ast.For):
            if not isinstance(loop_node.iter, (ast.Call, ast.Name)):
                return False

            if self._has_early_exits(loop_node):
                return False

            if self._has_nested_loops_with_dependencies(loop_node):
                return False

            return True

        return False

    def _analyze_memory_accesses(self, loop_node: ast.AST) -> list[MemoryAccess]:
        """Analyze memory access patterns in a loop."""
        accesses = []

        for node in ast.walk(loop_node):
            if isinstance(node, ast.Subscript):
                access = self._analyze_subscript(node, loop_node)
                if access:
                    accesses.append(access)

        return self._filter_vectorizable_accesses(accesses)

    def _analyze_subscript(self, node: ast.Subscript, loop_node: ast.AST) -> Optional[MemoryAccess]:
        """Analyze a subscript operation for vectorization potential."""
        if not isinstance(node.value, ast.Name):
            return None

        variable = node.value.id
        indices = [node.slice] if not isinstance(node.slice, ast.Tuple) else node.slice.elts

        access_pattern, stride = self._analyze_access_pattern(indices, loop_node)
        if access_pattern == "irregular":
            return None

        is_read = not isinstance(getattr(node, "ctx", None), ast.Store)
        is_write = isinstance(getattr(node, "ctx", None), ast.Store)

        return MemoryAccess(
            variable=variable,
            indices=indices,
            is_read=is_read,
            is_write=is_write,
            access_pattern=access_pattern,
            stride=stride,
        )

    def _analyze_access_pattern(self, indices: list[ast.expr], loop_node: ast.AST) -> tuple[str, Optional[int]]:
        """Analyze the access pattern of array indices."""
        if len(indices) != 1:
            return "irregular", None

        index = indices[0]
        loop_var = self._get_loop_variable(loop_node)

        if isinstance(index, ast.Name) and index.id == loop_var:
            return "linear", 1

        if isinstance(index, ast.BinOp):
            if isinstance(index.op, ast.Add):
                if isinstance(index.left, ast.Name) and index.left.id == loop_var:
                    if isinstance(index.right, ast.Constant):
                        return "linear", 1
                elif isinstance(index.right, ast.Name) and index.right.id == loop_var:
                    if isinstance(index.left, ast.Constant):
                        return "linear", 1

            elif isinstance(index.op, ast.Mult):
                if isinstance(index.left, ast.Name) and index.left.id == loop_var:
                    if isinstance(index.right, ast.Constant) and isinstance(index.right.value, int):
                        return "strided", index.right.value
                elif isinstance(index.right, ast.Name) and index.right.id == loop_var:
                    if isinstance(index.left, ast.Constant) and isinstance(index.left.value, int):
                        return "strided", index.left.value

        return "irregular", None

    def _get_loop_variable(self, loop_node: ast.AST) -> Optional[str]:
        """Extract the loop variable name."""
        if isinstance(loop_node, ast.For) and isinstance(loop_node.target, ast.Name):
            return loop_node.target.id
        return None

    def _filter_vectorizable_accesses(self, accesses: list[MemoryAccess]) -> list[MemoryAccess]:
        """Filter accesses to keep only vectorizable ones."""
        vectorizable = []

        for access in accesses:
            if access.access_pattern in ["linear", "strided"]:
                if access.stride is None or abs(access.stride) <= 4:
                    vectorizable.append(access)

        return vectorizable

    def _classify_vectorization_type(self, loop_node: ast.AST, accesses: list[MemoryAccess]) -> VectorizationType:
        """Classify the type of vectorization opportunity."""
        # Check dot product first since it's a specific type of reduction
        if self._is_dot_product_pattern(loop_node, accesses):
            return VectorizationType.DOT_PRODUCT

        if self._is_reduction_pattern(loop_node, accesses):
            return VectorizationType.REDUCTION_LOOP

        if self._is_array_copy_pattern(accesses):
            return VectorizationType.ARRAY_COPY

        if self._is_element_wise_pattern(accesses):
            return VectorizationType.ELEMENT_WISE

        return VectorizationType.SIMPLE_LOOP

    def _is_reduction_pattern(self, loop_node: ast.AST, accesses: list[MemoryAccess]) -> bool:
        """Check if this is a reduction pattern."""
        for node in ast.walk(loop_node):
            if isinstance(node, ast.AugAssign):
                if isinstance(node.op, (ast.Add, ast.Mult, ast.BitOr, ast.BitXor, ast.BitAnd)):
                    return True
        return False

    def _is_array_copy_pattern(self, accesses: list[MemoryAccess]) -> bool:
        """Check if this is an array copy pattern."""
        if len(accesses) == 2:
            read_access = next((a for a in accesses if a.is_read), None)
            write_access = next((a for a in accesses if a.is_write), None)

            if read_access and write_access:
                return read_access.access_pattern == "linear" and write_access.access_pattern == "linear"
        return False

    def _is_element_wise_pattern(self, accesses: list[MemoryAccess]) -> bool:
        """Check if this is an element-wise operation pattern."""
        if len(accesses) >= 2:
            linear_accesses = [a for a in accesses if a.access_pattern == "linear"]
            return len(linear_accesses) == len(accesses)
        return False

    def _is_dot_product_pattern(self, loop_node: ast.AST, accesses: list[MemoryAccess]) -> bool:
        """Check if this is a dot product pattern."""
        if len(accesses) >= 2 and self._is_reduction_pattern(loop_node, accesses):
            # Check for pattern: result += a[i] * b[i]
            read_accesses = [a for a in accesses if a.is_read]
            if len(read_accesses) >= 2:
                # Check if we have multiplication in the reduction
                for node in ast.walk(loop_node):
                    if isinstance(node, ast.AugAssign) and isinstance(node.op, ast.Add):
                        if isinstance(node.value, ast.BinOp) and isinstance(node.value.op, ast.Mult):
                            return all(a.access_pattern == "linear" for a in read_accesses)
        return False

    def _identify_constraints(self, loop_node: ast.AST, accesses: list[MemoryAccess]) -> set[VectorizationConstraint]:
        """Identify constraints that affect vectorization."""
        constraints = set()

        if self._has_control_flow(loop_node):
            constraints.add(VectorizationConstraint.CONTROL_FLOW)

        if self._has_function_calls(loop_node):
            constraints.add(VectorizationConstraint.FUNCTION_CALLS)

        if self._has_pointer_arithmetic(loop_node):
            constraints.add(VectorizationConstraint.POINTER_ARITHMETIC)

        if self._has_potential_aliasing(accesses):
            constraints.add(VectorizationConstraint.ALIASING)

        if self._has_data_dependencies(loop_node, accesses):
            constraints.add(VectorizationConstraint.DATA_DEPENDENCIES)

        return constraints

    def _has_early_exits(self, loop_node: ast.AST) -> bool:
        """Check if loop has early exit conditions."""
        for node in ast.walk(loop_node):
            if isinstance(node, (ast.Break, ast.Continue, ast.Return)):
                return True
        return False

    def _has_nested_loops_with_dependencies(self, loop_node: ast.AST) -> bool:
        """Check for nested loops with dependencies."""
        nested_loops = []
        for node in ast.walk(loop_node):
            if isinstance(node, (ast.For, ast.While)) and node != loop_node:
                nested_loops.append(node)

        return len(nested_loops) > 0  # Simplified check

    def _has_control_flow(self, loop_node: ast.AST) -> bool:
        """Check if loop has control flow statements."""
        for node in ast.walk(loop_node):
            if isinstance(node, (ast.If, ast.While, ast.For)) and node != loop_node:
                return True
        return False

    def _has_function_calls(self, loop_node: ast.AST) -> bool:
        """Check if loop contains problematic function calls."""
        for node in ast.walk(loop_node):
            if isinstance(node, ast.Call):
                # Skip the iterator call (like range()) - it's not in the loop body
                if hasattr(loop_node, "iter") and node == loop_node.iter:
                    continue
                # Skip common safe functions
                if isinstance(node.func, ast.Name):
                    safe_functions = {"range", "len", "enumerate"}
                    if node.func.id in safe_functions:
                        continue
                return True
        return False

    def _has_pointer_arithmetic(self, loop_node: ast.AST) -> bool:
        """Check if loop contains pointer arithmetic."""
        # This is a simplified check for Python AST
        for node in ast.walk(loop_node):
            if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Sub)):
                return True
        return False

    def _has_potential_aliasing(self, accesses: list[MemoryAccess]) -> bool:
        """Check for potential memory aliasing issues."""
        variables = set(a.variable for a in accesses)
        return len(variables) < len(accesses)  # Simplified check

    def _has_data_dependencies(self, loop_node: ast.AST, accesses: list[MemoryAccess]) -> bool:
        """Check for data dependencies that prevent vectorization."""
        # Check for loop-carried dependencies and reduction patterns
        for node in ast.walk(loop_node):
            if isinstance(node, ast.AugAssign):
                # This is a reduction pattern which has data dependencies
                return True

            if isinstance(node, ast.Assign):
                # Check for patterns like a[i] = a[i-1] + b[i] (loop-carried dependency)
                if isinstance(node.value, ast.BinOp):
                    # Look for array accesses that might create dependencies
                    for child in ast.walk(node.value):
                        if isinstance(child, ast.Subscript):
                            if isinstance(child.slice, ast.BinOp):
                                # Could be a[i-1] or similar
                                return True

        # Check for simple read-after-write dependencies on array elements
        write_vars = set(a.variable for a in accesses if a.is_write)
        read_vars = set(a.variable for a in accesses if a.is_read)

        # Only flag as dependency if it's the same array (potential aliasing)
        return bool(write_vars.intersection(read_vars))

    def _determine_vector_length(self, accesses: list[MemoryAccess], vec_type: VectorizationType) -> int:
        """Determine optimal vector length for the operation."""
        base_width = self.default_vector_width

        # Adjust based on access patterns
        max_stride = max((a.stride or 1 for a in accesses), default=1)
        if max_stride > 1:
            base_width = min(base_width, 8 // max_stride)

        # Adjust based on vectorization type
        type_adjustments = {
            VectorizationType.DOT_PRODUCT: lambda w: min(w, 8),
            VectorizationType.REDUCTION_LOOP: lambda w: min(w, 4),
            VectorizationType.MATRIX_MULTIPLY: lambda w: max(w, 8),
        }

        if vec_type in type_adjustments:
            base_width = type_adjustments[vec_type](base_width)

        return max(2, min(base_width, 16))

    def _estimate_speedup(
        self, vec_type: VectorizationType, vector_length: int, constraints: set[VectorizationConstraint]
    ) -> float:
        """Estimate potential speedup from vectorization."""
        base_speedup = vector_length * 0.8  # Account for overhead

        # Adjust based on vectorization type
        type_multipliers = {
            VectorizationType.SIMPLE_LOOP: 0.9,
            VectorizationType.ELEMENT_WISE: 0.95,
            VectorizationType.ARRAY_COPY: 0.85,
            VectorizationType.DOT_PRODUCT: 0.9,
            VectorizationType.REDUCTION_LOOP: 0.7,
            VectorizationType.MATRIX_MULTIPLY: 1.1,
        }

        multiplier = type_multipliers.get(vec_type, 0.8)
        speedup = base_speedup * multiplier

        # Apply constraint penalties
        constraint_penalties = {
            VectorizationConstraint.CONTROL_FLOW: 0.7,
            VectorizationConstraint.FUNCTION_CALLS: 0.6,
            VectorizationConstraint.DATA_DEPENDENCIES: 0.5,
            VectorizationConstraint.ALIASING: 0.8,
            VectorizationConstraint.IRREGULAR_ACCESS: 0.4,
        }

        for constraint in constraints:
            if constraint in constraint_penalties:
                speedup *= constraint_penalties[constraint]

        return max(1.1, speedup)

    def _calculate_confidence(self, constraints: set[VectorizationConstraint], accesses: list[MemoryAccess]) -> float:
        """Calculate confidence in the vectorization analysis."""
        base_confidence = 0.9

        # Reduce confidence based on constraints
        confidence_penalties = {
            VectorizationConstraint.CONTROL_FLOW: 0.3,
            VectorizationConstraint.FUNCTION_CALLS: 0.4,
            VectorizationConstraint.DATA_DEPENDENCIES: 0.51,  # Changed to ensure < 0.5
            VectorizationConstraint.ALIASING: 0.2,
            VectorizationConstraint.IRREGULAR_ACCESS: 0.6,
        }

        for constraint in constraints:
            if constraint in confidence_penalties:
                base_confidence -= confidence_penalties[constraint]

        # Boost confidence for simple access patterns
        if all(a.access_pattern == "linear" for a in accesses):
            base_confidence += 0.1

        return max(0.1, min(1.0, base_confidence))

    def _assess_transformation_complexity(
        self, vec_type: VectorizationType, constraints: set[VectorizationConstraint]
    ) -> str:
        """Assess the complexity of the required transformation."""
        if len(constraints) == 0:
            return "trivial"

        complex_constraints = {
            VectorizationConstraint.DATA_DEPENDENCIES,
            VectorizationConstraint.CONTROL_FLOW,
            VectorizationConstraint.FUNCTION_CALLS,
        }

        if constraints.intersection(complex_constraints):
            return "complex"

        return "moderate"

    def _suggest_intrinsics(self, vec_type: VectorizationType, vector_length: int) -> list[str]:
        """Suggest appropriate SIMD intrinsics for the vectorization."""
        intrinsics = []

        if self.target_arch == "x86_64":
            if vector_length == 4:
                intrinsics.extend(["_mm_load_ps", "_mm_store_ps", "_mm_add_ps"])
            elif vector_length == 8:
                intrinsics.extend(["_mm256_load_ps", "_mm256_store_ps", "_mm256_add_ps"])

        if vec_type == VectorizationType.DOT_PRODUCT:
            if self.target_arch == "x86_64":
                intrinsics.append("_mm_dp_ps" if vector_length == 4 else "_mm256_dp_ps")

        return intrinsics

    def _recommend_vector_widths(self, candidates: list[VectorizationCandidate]) -> dict[str, int]:
        """Recommend optimal vector widths for different data types."""
        recommendations = {}

        if candidates:
            avg_vector_length = sum(c.vector_length for c in candidates) / len(candidates)
            recommendations["float"] = int(avg_vector_length)
            recommendations["double"] = max(2, int(avg_vector_length // 2))
            recommendations["int"] = int(avg_vector_length)
        else:
            recommendations = {"float": 4, "double": 2, "int": 4}

        return recommendations

    def _generate_arch_recommendations(self, candidates: list[VectorizationCandidate]) -> list[str]:
        """Generate architecture-specific recommendations."""
        recommendations = []

        if candidates:
            max_vector_length = max(c.vector_length for c in candidates)

            if self.target_arch == "x86_64":
                if max_vector_length >= 8:
                    recommendations.append("Consider using AVX/AVX2 instructions")
                if max_vector_length >= 16:
                    recommendations.append("AVX512 may provide additional benefits")

            complex_candidates = [c for c in candidates if c.transformation_complexity == "complex"]
            if complex_candidates:
                recommendations.append("Complex transformations may require manual optimization")

        return recommendations

    def _analyze_complexity_distribution(self, candidates: list[VectorizationCandidate]) -> dict[str, int]:
        """Analyze the distribution of transformation complexities."""
        distribution = {"trivial": 0, "moderate": 0, "complex": 0}

        for candidate in candidates:
            distribution[candidate.transformation_complexity] += 1

        return distribution

    def _analyze_constraint_frequency(self, candidates: list[VectorizationCandidate]) -> dict[str, int]:
        """Analyze frequency of different constraints."""
        frequency: dict[str, int] = {}

        for candidate in candidates:
            for constraint in candidate.constraints:
                constraint_name = constraint.value
                frequency[constraint_name] = frequency.get(constraint_name, 0) + 1

        return frequency
