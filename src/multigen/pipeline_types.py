"""Typed dataclasses for pipeline phase results.

This module provides strongly-typed dataclasses for all 7 pipeline phase results,
replacing the generic `dict[PipelinePhase, Any]` with structured types that
enable better IDE support, type checking, and documentation.

Each phase in the MultiGen pipeline produces a specific result type:
1. ValidationPhaseResult - Static-python validation and translatability
2. AnalysisPhaseResult - AST parsing and semantic analysis
3. PythonOptimizationPhaseResult - Python-level optimizations
4. MappingPhaseResult - Python to target language semantic mapping (uses SemanticMapping)
5. TargetOptimizationPhaseResult - Target language optimizations
6. GenerationPhaseResult - Code generation output
7. BuildPhaseResult - Compilation/build output

Usage:
    >>> from multigen.pipeline_types import ValidationPhaseResult
    >>> result = ValidationPhaseResult(is_valid=True, parse_success=True)
    >>> if result.is_valid and not result.unsupported_features:
    ...     print("Code is fully translatable")
"""

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Optional

if TYPE_CHECKING:
    from .backends.optimizer import OptimizationInfo
    from .frontend.base import OptimizationResult


@dataclass
class ValidationPhaseResult:
    """Result from Phase 1: Validation.

    Contains information about code validity, supported features,
    and any constraint violations found during validation.

    Attributes:
        is_valid: Whether the code passed all validation checks
        parse_success: Whether the code successfully parsed as Python
        tier: Optional tier classification (e.g., "tier1", "tier2")
        violations: List of validation violations (blocking errors)
        warnings: List of validation warnings (non-blocking)
        supported_features: List of Python features detected and supported
        unsupported_features: List of Python features that cannot be translated
        memory_safety_checked: Whether memory safety analysis was performed
        memory_safety_errors: Critical memory safety issues found
        memory_safety_warnings: Non-critical memory safety concerns
    """

    is_valid: bool
    parse_success: bool
    tier: Optional[str] = None
    violations: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    supported_features: list[str] = field(default_factory=list)
    unsupported_features: list[str] = field(default_factory=list)
    memory_safety_checked: bool = False
    memory_safety_errors: list[str] = field(default_factory=list)
    memory_safety_warnings: list[str] = field(default_factory=list)


@dataclass
class AnalysisPhaseResult:
    """Result from Phase 2: Analysis.

    Contains information about the analyzed code structure,
    including function counts, imports, and complexity metrics.

    Attributes:
        success: Whether analysis completed successfully
        convertible: Whether the code can be converted to target language
        function_count: Number of functions detected
        global_variable_count: Number of global variables detected
        imports: List of import statements found
        complexity: Complexity classification ("simple", "moderate", "complex")
        errors: List of analysis errors (blocking)
        warnings: List of analysis warnings (non-blocking)
        advanced_analysis: Optional dict with advanced analysis results
            (type inference, call graph, bounds checking, etc.)
    """

    success: bool
    convertible: bool
    function_count: int = 0
    global_variable_count: int = 0
    imports: list[str] = field(default_factory=list)
    complexity: str = "simple"
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    advanced_analysis: Optional[dict[str, Any]] = None


@dataclass
class PythonOptimizationPhaseResult:
    """Result from Phase 3: Python Optimization.

    Contains information about Python-level optimizations applied
    before mapping to the target language.

    Attributes:
        enabled: Whether optimization was enabled for this run
        optimizations_applied: List of optimization passes that were applied
        compile_time: Results from compile-time evaluation pass
        loops: Results from loop analysis and optimization
        specialization: Results from function specialization
        vectorization: Results from vectorization detection
    """

    enabled: bool
    optimizations_applied: list[str] = field(default_factory=list)
    compile_time: Optional["OptimizationResult"] = None
    loops: Optional["OptimizationResult"] = None
    specialization: Optional["OptimizationResult"] = None
    vectorization: Optional["OptimizationResult"] = None


# Note: MappingPhaseResult uses SemanticMapping from pipeline.py
# (already exists and is well-structured)


@dataclass
class TargetOptimizationPhaseResult:
    """Result from Phase 5: Target Optimization.

    Contains information about target language-specific optimizations
    applied to the generated code or IR.

    Attributes:
        target_language: The target language being optimized for
        optimization_level: Numeric optimization level (0-3)
        optimizer_available: Whether the backend provides an optimizer
        optimizations_applied: List of optimizations that were applied
        optimizer_info: Detailed optimizer configuration if available
    """

    target_language: str
    optimization_level: int
    optimizer_available: bool
    optimizations_applied: list[str] = field(default_factory=list)
    optimizer_info: Optional["OptimizationInfo"] = None


@dataclass
class GenerationPhaseResult:
    """Result from Phase 6: Generation.

    Contains information about the generated source code file.

    Attributes:
        success: Whether code generation succeeded
        source_file: Path to the generated source file
        backend_name: Name of the backend used (e.g., "llvm", "rust")
        file_extension: File extension of generated file (e.g., ".rs", ".ll")
        generated_lines: Number of lines in generated code
    """

    success: bool
    source_file: str
    backend_name: str
    file_extension: str
    generated_lines: int


@dataclass
class BuildPhaseResult:
    """Result from Phase 7: Build.

    Contains information about the build/compilation process.

    Attributes:
        success: Whether the build succeeded
        mode: Build mode used ("none", "makefile", "direct")
        outputs: Dict mapping output type to file path
            (e.g., {"executable": "/path/to/binary"})
        compile_time_ms: Compilation time in milliseconds (if measured)
        executable_size_bytes: Size of generated executable (if applicable)
    """

    success: bool
    mode: str
    outputs: dict[str, str] = field(default_factory=dict)
    compile_time_ms: Optional[float] = None
    executable_size_bytes: Optional[int] = None
