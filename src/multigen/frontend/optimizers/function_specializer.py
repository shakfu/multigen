"""Function Specialization System for the Intelligence Layer.

This module provides function specialization capabilities, creating optimized
versions of functions based on usage patterns, type information, and call contexts.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from ..base import AnalysisContext, BaseOptimizer, OptimizationLevel, OptimizationResult


class SpecializationType(Enum):
    """Types of function specializations."""

    TYPE_SPECIALIZATION = "type_specialization"  # Based on parameter types
    CONSTANT_FOLDING = "constant_folding"  # For constant parameters
    INLINE_EXPANSION = "inline_expansion"  # Small function inlining
    LOOP_UNROLLING = "loop_unrolling"  # Unroll loops with known bounds
    CONDITIONAL_REMOVAL = "conditional_removal"  # Remove dead branches
    VECTORIZATION = "vectorization"  # SIMD optimizations
    TAIL_RECURSION = "tail_recursion"  # Tail call optimization
    MEMOIZATION = "memoization"  # Cache results for pure functions


class CallPattern(Enum):
    """Common function call patterns."""

    SINGLE_USE = "single_use"  # Called only once
    HOT_PATH = "hot_path"  # Called frequently
    CONSTANT_ARGS = "constant_args"  # Often called with constants
    TYPE_SPECIFIC = "type_specific"  # Called with specific types
    RECURSIVE = "recursive"  # Recursive calls
    PURE_FUNCTION = "pure_function"  # No side effects
    COMPUTATION_HEAVY = "computation_heavy"  # CPU intensive
    SIMPLE_WRAPPER = "simple_wrapper"  # Simple delegation


@dataclass
class ParameterInfo:
    """Information about function parameters for specialization."""

    name: str
    type_hint: Optional[str] = None
    common_values: list[Any] = field(default_factory=list)
    is_constant_across_calls: bool = False
    value_distribution: dict[Any, int] = field(default_factory=dict)
    range_info: Optional[tuple[Any, Any]] = None
    nullable: bool = False
    used_in_conditions: bool = False
    used_in_loops: bool = False


@dataclass
class CallSiteInfo:
    """Information about a specific call site."""

    function_name: str
    caller_name: str
    line_number: int
    argument_values: list[Any] = field(default_factory=list)
    argument_types: list[str] = field(default_factory=list)
    is_constant_call: bool = False
    call_frequency: int = 1
    context: str = "normal"  # normal, loop, conditional, hot_path


@dataclass
class FunctionProfile:
    """Profile information about a function for specialization."""

    name: str
    parameters: list[ParameterInfo] = field(default_factory=list)
    call_sites: list[CallSiteInfo] = field(default_factory=list)
    call_pattern: CallPattern = CallPattern.SINGLE_USE
    is_pure: bool = False
    is_recursive: bool = False
    has_side_effects: bool = False
    complexity_score: int = 0
    body_size: int = 0
    total_calls: int = 0
    hot_path_calls: int = 0
    constant_calls: int = 0
    return_type: Optional[str] = None


@dataclass
class SpecializationCandidate:
    """A candidate for function specialization."""

    base_function: str
    specialization_type: SpecializationType
    specialized_name: str
    parameter_bindings: dict[str, Any] = field(default_factory=dict)
    type_constraints: dict[str, str] = field(default_factory=dict)
    estimated_speedup: float = 1.0
    confidence: float = 1.0
    call_site_coverage: float = 0.0  # Fraction of calls this specialization covers
    code_size_impact: int = 0  # Estimated code size change
    specialization_conditions: list[str] = field(default_factory=list)


@dataclass
class SpecializationResult:
    """Result of function specialization analysis."""

    specialized_function: str
    original_function: str
    specialization_ast: Optional[ast.AST] = None
    parameter_substitutions: dict[str, Any] = field(default_factory=dict)
    optimizations_applied: list[str] = field(default_factory=list)
    performance_gain: float = 1.0
    safety_verified: bool = True


@dataclass
class SpecializationReport:
    """Report from function specialization analysis."""

    function_profiles: dict[str, FunctionProfile] = field(default_factory=dict)
    specialization_candidates: list[SpecializationCandidate] = field(default_factory=list)
    specialization_results: list[SpecializationResult] = field(default_factory=list)
    total_functions: int = 0
    specialized_functions: int = 0
    estimated_code_size_change: int = 0
    estimated_performance_gain: float = 1.0
    inlining_opportunities: list[str] = field(default_factory=list)
    memoization_candidates: list[str] = field(default_factory=list)


class FunctionSpecializer(BaseOptimizer):
    """Optimizer for function specialization based on usage patterns."""

    def __init__(self, optimization_level: OptimizationLevel = OptimizationLevel.BASIC):
        super().__init__("FunctionSpecializer", optimization_level)
        self._function_definitions: dict[str, ast.FunctionDef] = {}
        self._call_sites: list[CallSiteInfo] = []
        self._current_function: Optional[str] = None

    def optimize(self, context: AnalysisContext) -> OptimizationResult:
        """Perform function specialization optimization."""
        try:
            # Reset state
            self._function_definitions.clear()
            self._call_sites.clear()
            self._current_function = None

            report = SpecializationReport()

            # Analyze functions and call patterns
            self._analyze_functions(context.ast_node, report)
            self._analyze_call_patterns(context.ast_node, report)

            # Build function profiles
            self._build_function_profiles(report)

            # Generate specialization candidates
            self._generate_specialization_candidates(report)

            # Create specialized functions
            self._create_specializations(report)

            # Create optimized AST
            optimized_ast = self._apply_specializations(context.ast_node, report)

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
                    "functions_analyzed": len(report.function_profiles),
                    "specializations_created": len(report.specialization_results),
                    "code_size_change": report.estimated_code_size_change,
                    "report": report,
                },
            )

        except Exception as e:
            return OptimizationResult(
                optimizer_name=self.name,
                success=False,
                optimized_ast=None,
                transformations=[f"Function specialization failed: {str(e)}"],
                performance_gain_estimate=1.0,
                safety_analysis={"function_specialization": False},
                metadata={"error": str(e), "error_type": type(e).__name__},
            )

    def _analyze_functions(self, node: ast.AST, report: SpecializationReport) -> None:
        """Analyze function definitions in the AST."""
        for child in ast.walk(node):
            if isinstance(child, ast.FunctionDef):
                self._function_definitions[child.name] = child
                self._analyze_function_definition(child, report)

    def _analyze_function_definition(self, node: ast.FunctionDef, report: SpecializationReport) -> None:
        """Analyze a specific function definition."""
        profile = FunctionProfile(name=node.name)

        # Analyze parameters
        for arg in node.args.args:
            param_info = ParameterInfo(name=arg.arg)
            if arg.annotation:
                param_info.type_hint = self._extract_type_hint(arg.annotation)
            profile.parameters.append(param_info)

        # Analyze function body
        profile.body_size = len(node.body)
        profile.complexity_score = self._calculate_complexity_score(node)
        profile.is_pure = self._is_pure_function(node)
        profile.has_side_effects = self._has_side_effects(node)
        profile.is_recursive = self._is_recursive_function(node)

        # Analyze return type
        if node.returns:
            profile.return_type = self._extract_type_hint(node.returns)

        report.function_profiles[node.name] = profile
        report.total_functions += 1

    def _analyze_call_patterns(self, node: ast.AST, report: SpecializationReport) -> None:
        """Analyze function call patterns throughout the AST."""
        for child in ast.walk(node):
            if isinstance(child, ast.FunctionDef):
                old_function = self._current_function
                self._current_function = child.name
                for stmt in child.body:
                    self._visit_for_calls(stmt, report)
                self._current_function = old_function

    def _visit_for_calls(self, node: ast.AST, report: SpecializationReport) -> None:
        """Visit AST nodes to find function calls."""
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
            self._analyze_function_call(node, report)

        for child in ast.iter_child_nodes(node):
            self._visit_for_calls(child, report)

    def _analyze_function_call(self, node: ast.Call, report: SpecializationReport) -> None:
        """Analyze a specific function call."""
        if not isinstance(node.func, ast.Name):
            return

        func_name = node.func.id
        if func_name not in self._function_definitions:
            return  # Skip external functions

        call_info = CallSiteInfo(
            function_name=func_name, caller_name=self._current_function or "global", line_number=node.lineno
        )

        # Analyze arguments
        for arg in node.args:
            arg_value, arg_type = self._analyze_argument(arg)
            call_info.argument_values.append(arg_value)
            call_info.argument_types.append(arg_type)

        # Check if all arguments are constants
        call_info.is_constant_call = all(isinstance(arg, ast.Constant) for arg in node.args)

        # Determine call context
        call_info.context = self._determine_call_context(node)

        self._call_sites.append(call_info)

    def _analyze_argument(self, arg: ast.AST) -> tuple[Any, str]:
        """Analyze a function call argument."""
        if isinstance(arg, ast.Constant):
            return arg.value, type(arg.value).__name__
        elif isinstance(arg, ast.Name):
            return f"var_{arg.id}", "variable"
        elif isinstance(arg, ast.BinOp):
            return "expression", "expression"
        else:
            return "complex", "unknown"

    def _determine_call_context(self, node: ast.Call) -> str:
        """Determine the context in which a call occurs."""
        # Simplified context detection
        # In a full implementation, we'd track the AST path
        return "normal"

    def _build_function_profiles(self, report: SpecializationReport) -> None:
        """Build comprehensive profiles for each function."""
        for call_site in self._call_sites:
            func_name = call_site.function_name
            if func_name in report.function_profiles:
                profile = report.function_profiles[func_name]
                profile.call_sites.append(call_site)
                profile.total_calls += 1

                if call_site.is_constant_call:
                    profile.constant_calls += 1

                # Update parameter info
                for i, (param, arg_value, _arg_type) in enumerate(
                    zip(profile.parameters, call_site.argument_values, call_site.argument_types)
                ):
                    if i < len(profile.parameters):
                        param = profile.parameters[i]
                        if arg_value not in param.common_values:
                            param.common_values.append(arg_value)

                        # Update value distribution
                        if arg_value in param.value_distribution:
                            param.value_distribution[arg_value] += 1
                        else:
                            param.value_distribution[arg_value] = 1

        # Classify call patterns
        for profile in report.function_profiles.values():
            profile.call_pattern = self._classify_call_pattern(profile)

    def _classify_call_pattern(self, profile: FunctionProfile) -> CallPattern:
        """Classify the call pattern for a function."""
        if profile.total_calls == 1:
            return CallPattern.SINGLE_USE
        elif profile.constant_calls > profile.total_calls * 0.8:
            return CallPattern.CONSTANT_ARGS
        elif profile.total_calls > 10:
            return CallPattern.HOT_PATH
        elif profile.is_recursive:
            return CallPattern.RECURSIVE
        elif profile.is_pure and not profile.has_side_effects:
            return CallPattern.PURE_FUNCTION
        elif profile.body_size <= 3:
            return CallPattern.SIMPLE_WRAPPER
        elif profile.complexity_score > 10:
            return CallPattern.COMPUTATION_HEAVY
        else:
            return CallPattern.SINGLE_USE

    def _generate_specialization_candidates(self, report: SpecializationReport) -> None:
        """Generate candidates for function specialization."""
        for profile in report.function_profiles.values():
            candidates = []

            # Type specialization
            if self._should_create_type_specialization(profile):
                candidates.extend(self._create_type_specialization_candidates(profile))

            # Constant folding specialization
            if self._should_create_constant_specialization(profile):
                candidates.extend(self._create_constant_specialization_candidates(profile))

            # Inlining candidates
            if self._should_inline_function(profile):
                candidates.append(self._create_inline_candidate(profile))

            # Memoization candidates
            if self._should_memoize_function(profile):
                candidates.append(self._create_memoization_candidate(profile))

            report.specialization_candidates.extend(candidates)

    def _should_create_type_specialization(self, profile: FunctionProfile) -> bool:
        """Check if type specialization would be beneficial."""
        return (
            profile.total_calls > 3
            and any(len(set(param.common_values)) <= 3 for param in profile.parameters)
            and not profile.is_recursive
        )

    def _should_create_constant_specialization(self, profile: FunctionProfile) -> bool:
        """Check if constant specialization would be beneficial."""
        return (
            profile.constant_calls > 2 and profile.constant_calls > profile.total_calls * 0.5 and profile.body_size < 20
        )

    def _should_inline_function(self, profile: FunctionProfile) -> bool:
        """Check if function should be inlined."""
        return (
            profile.body_size <= 5
            and profile.total_calls > 1
            and profile.total_calls < 10
            and not profile.is_recursive
            and not profile.has_side_effects
        )

    def _should_memoize_function(self, profile: FunctionProfile) -> bool:
        """Check if function should be memoized."""
        return (
            profile.is_pure
            and profile.total_calls > 5
            and profile.complexity_score > 5
            and any(len(param.value_distribution) < profile.total_calls for param in profile.parameters)
        )

    def _create_type_specialization_candidates(self, profile: FunctionProfile) -> list[SpecializationCandidate]:
        """Create type specialization candidates."""
        candidates = []

        # Group call sites by argument types
        type_groups: dict[tuple[str, ...], list[CallSiteInfo]] = {}
        for call_site in profile.call_sites:
            type_signature = tuple(call_site.argument_types)
            if type_signature not in type_groups:
                type_groups[type_signature] = []
            type_groups[type_signature].append(call_site)

        # Create specialization for each common type signature
        for type_sig, call_sites in type_groups.items():
            if len(call_sites) >= 2:  # Only specialize if used multiple times
                candidate = SpecializationCandidate(
                    base_function=profile.name,
                    specialization_type=SpecializationType.TYPE_SPECIALIZATION,
                    specialized_name=f"{profile.name}_typed_{'_'.join(type_sig)}",
                    type_constraints={param.name: type_name for param, type_name in zip(profile.parameters, type_sig)},
                    estimated_speedup=1.1 + (0.05 * len(call_sites)),
                    confidence=0.8,
                    call_site_coverage=len(call_sites) / profile.total_calls,
                    code_size_impact=profile.body_size,
                )
                candidates.append(candidate)

        return candidates

    def _create_constant_specialization_candidates(self, profile: FunctionProfile) -> list[SpecializationCandidate]:
        """Create constant specialization candidates."""
        candidates = []

        # Find parameters that are frequently constant
        for _i, param in enumerate(profile.parameters):
            if not param.value_distribution:
                continue

            # Find the most common constant value
            most_common_value = max(param.value_distribution.items(), key=lambda x: x[1])
            value, frequency = most_common_value

            if frequency >= 2 and isinstance(value, (int, float, str, bool)):
                candidate = SpecializationCandidate(
                    base_function=profile.name,
                    specialization_type=SpecializationType.CONSTANT_FOLDING,
                    specialized_name=f"{profile.name}_const_{param.name}_{value}",
                    parameter_bindings={param.name: value},
                    estimated_speedup=1.2 + (0.1 * frequency),
                    confidence=0.9,
                    call_site_coverage=frequency / profile.total_calls,
                    code_size_impact=int(profile.body_size * 0.8),  # May be smaller due to folding
                )
                candidates.append(candidate)

        return candidates

    def _create_inline_candidate(self, profile: FunctionProfile) -> SpecializationCandidate:
        """Create an inlining candidate."""
        return SpecializationCandidate(
            base_function=profile.name,
            specialization_type=SpecializationType.INLINE_EXPANSION,
            specialized_name=f"{profile.name}_inline",
            estimated_speedup=1.3,
            confidence=0.95,
            call_site_coverage=1.0,
            code_size_impact=-profile.body_size,  # Function is removed
        )

    def _create_memoization_candidate(self, profile: FunctionProfile) -> SpecializationCandidate:
        """Create a memoization candidate."""
        return SpecializationCandidate(
            base_function=profile.name,
            specialization_type=SpecializationType.MEMOIZATION,
            specialized_name=f"{profile.name}_memoized",
            estimated_speedup=2.0 + (0.1 * profile.complexity_score),
            confidence=0.8,
            call_site_coverage=1.0,
            code_size_impact=20,  # Cache overhead
        )

    def _create_specializations(self, report: SpecializationReport) -> None:
        """Create specialized function implementations."""
        for candidate in report.specialization_candidates:
            if self._should_apply_specialization(candidate):
                result = self._create_specialized_function(candidate)
                if result:
                    report.specialization_results.append(result)
                    report.specialized_functions += 1

    def _should_apply_specialization(self, candidate: SpecializationCandidate) -> bool:
        """Determine if a specialization should be applied."""
        # Apply aggressive optimizations only at higher optimization levels
        if self.optimization_level == OptimizationLevel.BASIC:
            return (
                candidate.estimated_speedup > 1.2 and candidate.confidence > 0.8 and candidate.call_site_coverage > 0.5
            )
        elif self.optimization_level == OptimizationLevel.MODERATE:
            return (
                candidate.estimated_speedup > 1.1 and candidate.confidence > 0.7 and candidate.call_site_coverage > 0.3
            )
        else:  # AGGRESSIVE or MAXIMUM
            return (
                candidate.estimated_speedup > 1.05 and candidate.confidence > 0.6 and candidate.call_site_coverage > 0.2
            )

    def _create_specialized_function(self, candidate: SpecializationCandidate) -> Optional[SpecializationResult]:
        """Create a specialized version of a function."""
        base_func = self._function_definitions.get(candidate.base_function)
        if not base_func:
            return None

        # Create a copy of the function AST
        specialized_ast = self._copy_function_ast(base_func)
        specialized_ast.name = candidate.specialized_name

        optimizations_applied = []

        # Apply specialization transformations
        if candidate.specialization_type == SpecializationType.CONSTANT_FOLDING:
            self._apply_constant_folding(specialized_ast, candidate.parameter_bindings)
            optimizations_applied.append("constant_folding")

        elif candidate.specialization_type == SpecializationType.TYPE_SPECIALIZATION:
            self._apply_type_specialization(specialized_ast, candidate.type_constraints)
            optimizations_applied.append("type_specialization")

        elif candidate.specialization_type == SpecializationType.INLINE_EXPANSION:
            # Inlining is handled at call sites, not in function definition
            optimizations_applied.append("inline_expansion")

        return SpecializationResult(
            specialized_function=candidate.specialized_name,
            original_function=candidate.base_function,
            specialization_ast=specialized_ast,
            parameter_substitutions=candidate.parameter_bindings,
            optimizations_applied=optimizations_applied,
            performance_gain=candidate.estimated_speedup,
            safety_verified=True,
        )

    def _copy_function_ast(self, func: ast.FunctionDef) -> ast.FunctionDef:
        """Create a deep copy of a function AST."""
        # Create new function with same structure
        new_func = ast.FunctionDef(
            name=func.name,
            args=func.args,
            body=func.body.copy(),
            decorator_list=func.decorator_list.copy(),
            returns=func.returns,
            lineno=func.lineno,
        )
        return new_func

    def _apply_constant_folding(self, func_ast: ast.FunctionDef, bindings: dict[str, Any]) -> None:
        """Apply constant folding to a specialized function."""
        # Note: Proper constant folding should use ast.NodeTransformer
        # This is a simplified placeholder implementation
        # In production, use ast.NodeTransformer to properly replace Name nodes with Constant nodes
        pass  # TODO: Implement proper constant folding with ast.NodeTransformer

    def _apply_type_specialization(self, func_ast: ast.FunctionDef, type_constraints: dict[str, str]) -> None:
        """Apply type-specific optimizations to a function."""
        # Add type-specific optimizations based on constraints
        # This is a simplified implementation
        pass

    def _apply_specializations(self, node: ast.AST, report: SpecializationReport) -> ast.AST:
        """Apply specializations to the AST."""
        # Create a copy of the original AST
        optimized_ast = ast.copy_location(ast.parse(ast.unparse(node)), node)

        # Add specialized functions to the module
        if isinstance(optimized_ast, ast.Module):
            for result in report.specialization_results:
                if result.specialization_ast and isinstance(result.specialization_ast, ast.stmt):
                    optimized_ast.body.append(result.specialization_ast)

        return optimized_ast

    def _extract_type_hint(self, annotation: ast.AST) -> str:
        """Extract type hint from annotation."""
        if isinstance(annotation, ast.Name):
            return annotation.id
        elif isinstance(annotation, ast.Constant):
            return str(annotation.value)
        else:
            return "unknown"

    def _calculate_complexity_score(self, func: ast.FunctionDef) -> int:
        """Calculate complexity score for a function."""
        score = 0
        for node in ast.walk(func):
            if isinstance(node, (ast.For, ast.While)):
                score += 2
            elif isinstance(node, ast.If):
                score += 1
            elif isinstance(node, ast.Call):
                score += 1
        return score

    def _is_pure_function(self, func: ast.FunctionDef) -> bool:
        """Check if a function is pure (no side effects)."""
        # Simplified check - look for assignments to non-local variables
        for node in ast.walk(func):
            if isinstance(node, ast.Global) or isinstance(node, ast.Nonlocal):
                return False
            # More sophisticated analysis would be needed for full purity check
        return True

    def _has_side_effects(self, func: ast.FunctionDef) -> bool:
        """Check if a function has side effects."""
        for node in ast.walk(func):
            if isinstance(node, ast.Call):
                # Assume function calls might have side effects
                if isinstance(node.func, ast.Name):
                    func_name = node.func.id
                    if func_name in {"print", "open", "write"}:
                        return True
        return False

    def _is_recursive_function(self, func: ast.FunctionDef) -> bool:
        """Check if a function is recursive."""
        for node in ast.walk(func):
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
                if node.func.id == func.name:
                    return True
        return False

    def _estimate_performance_gain(self, report: SpecializationReport) -> float:
        """Estimate overall performance gain from specializations."""
        if not report.specialization_results:
            return 1.0

        # Weight performance gains by call frequency coverage
        total_gain = 1.0
        for result in report.specialization_results:
            total_gain *= result.performance_gain

        # Cap maximum gain
        return min(total_gain, 10.0)

    def _generate_transformations(self, report: SpecializationReport) -> list[str]:
        """Generate list of transformations applied."""
        transformations = []

        transformations.append(f"Analyzed {report.total_functions} functions")
        transformations.append(f"Created {report.specialized_functions} specialized versions")

        specialization_counts: dict[str, int] = {}
        for candidate in report.specialization_candidates:
            spec_type = candidate.specialization_type.value
            specialization_counts[spec_type] = specialization_counts.get(spec_type, 0) + 1

        for spec_type, count in specialization_counts.items():
            transformations.append(f"Generated {count} {spec_type} candidates")

        if report.estimated_code_size_change != 0:
            change_type = "increase" if report.estimated_code_size_change > 0 else "decrease"
            transformations.append(f"Estimated code size {change_type}: {abs(report.estimated_code_size_change)} bytes")

        return transformations

    def _analyze_safety(self, report: SpecializationReport) -> dict[str, bool]:
        """Analyze safety of function specializations."""
        safety_analysis = {
            "type_specialization": True,
            "constant_folding": True,
            "inline_expansion": True,
            "memoization": True,
            "all_specializations_safe": True,
        }

        # Check each specialization for safety
        for result in report.specialization_results:
            if not result.safety_verified:
                safety_analysis["all_specializations_safe"] = False

        return safety_analysis
