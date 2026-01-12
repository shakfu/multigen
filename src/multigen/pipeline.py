"""Pipeline module for MultiGen - Python-to-Many code translation.

This module implements the core MultiGen pipeline that transforms Python modules
into optimized target language code and optionally builds executables. The pipeline
consists of seven clear phases:

1. Validation Phase: Static-python style validation and translatability assessment
2. Analysis Phase: AST parsing and semantic element breakdown
3. Python Optimization Phase: Python-level optimizations
4. Mapping Phase: Python semantics to target language semantics mapping
5. Target Optimization Phase: Target language-level optimizations
6. Generation Phase: Target language code generation
7. Build Phase: Direct compilation or build file generation

Usage:
    from multigen.pipeline import MultiGenPipeline

    # Basic usage
    pipeline = MultiGenPipeline(target_language="rust")
    result = pipeline.convert("my_module.py")

    # With build
    result = pipeline.convert("my_module.py", build_mode=BuildMode.DIRECT)
"""

import ast
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Optional, Union

from .backends.preferences import BackendPreferences
from .backends.registry import registry
from .common import log

# Import frontend analysis components
try:
    from .frontend import (
        AnalysisContext,
        ASTAnalyzer,
        BoundsChecker,
        BoundsProver,
        CallGraphAnalyzer,
        CompileTimeEvaluator,
        CorrectnessProver,
        FunctionSpecializer,
        ImmutabilityAnalyzer,
        LoopAnalyzer,
        MutabilityClass,
        StaticAnalyzer,
        StaticPythonSubsetValidator,
        SymbolicExecutor,
        TheoremProver,
        TypeInferenceEngine,
        VectorizationDetector,
        analyze_python_code,
    )
    from .frontend.base import AnalysisLevel
    from .frontend.base import OptimizationLevel as FrontendOptimizationLevel
    from .frontend.python_constraints import PythonConstraintChecker

    FRONTEND_AVAILABLE = True
except ImportError:
    # Fallback if frontend components not available
    FRONTEND_AVAILABLE = False

# Import Z3 for formal verification (optional)
try:
    import z3  # type: ignore[import-untyped,import-not-found]  # noqa: F401

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False

# Import C/C++ memory safety checker
try:
    from .backends.c.memory_safety import MemorySafetyChecker

    MEMORY_SAFETY_AVAILABLE = True
except ImportError:
    MEMORY_SAFETY_AVAILABLE = False


class OptimizationLevel(Enum):
    """Optimization levels for code generation."""

    NONE = "none"
    BASIC = "basic"
    MODERATE = "moderate"
    AGGRESSIVE = "aggressive"


class BuildMode(Enum):
    """Build modes for the pipeline."""

    NONE = "none"  # Generate code only
    MAKEFILE = "makefile"  # Generate build file (Makefile, Cargo.toml, etc.)
    DIRECT = "direct"  # Compile directly to executable


class PipelinePhase(Enum):
    """Pipeline phase identifiers."""

    VALIDATION = "validation"
    ANALYSIS = "analysis"
    PYTHON_OPTIMIZATION = "python_optimization"
    MAPPING = "mapping"
    TARGET_OPTIMIZATION = "target_optimization"
    GENERATION = "generation"
    BUILD = "build"


@dataclass
class PipelineConfig:
    """Configuration for the MultiGen pipeline."""

    optimization_level: OptimizationLevel = OptimizationLevel.MODERATE
    target_language: str = "c"
    output_dir: Optional[str] = None
    build_mode: BuildMode = BuildMode.NONE
    compiler: Optional[str] = None
    compiler_flags: Optional[list[str]] = None
    include_dirs: Optional[list[str]] = None
    libraries: Optional[list[str]] = None
    enable_advanced_analysis: bool = True
    enable_optimizations: bool = True
    enable_formal_verification: bool = False  # Z3-based formal verification (optional)
    strict_verification: bool = (
        False  # Halt code generation on verification failures (requires enable_formal_verification)
    )
    backend_preferences: Optional[BackendPreferences] = None
    progress_callback: Optional[Callable[[PipelinePhase, str], None]] = None

    def __post_init__(self) -> None:
        """Initialize default values."""
        if self.compiler_flags is None:
            self.compiler_flags = []
        if self.include_dirs is None:
            self.include_dirs = []
        if self.libraries is None:
            self.libraries = []
        if self.compiler is None:
            # Set default compiler based on target language
            compiler_defaults = {"c": "gcc", "rust": "rustc", "go": "go", "cpp": "g++"}
            self.compiler = compiler_defaults.get(self.target_language, "gcc")


@dataclass
class ContainerMapping:
    """Mapping for a Python container type to target language type."""

    python_type: str  # e.g., "list[int]", "dict[str, int]"
    target_type: str  # e.g., "vec_int", "HashMap<String, i64>"
    element_types: list[str] = field(default_factory=list)  # Resolved element types
    requires_imports: list[str] = field(default_factory=list)  # Imports needed


@dataclass
class SemanticMapping:
    """Semantic mapping from Python to target language.

    This dataclass captures the mapping decisions made during Phase 4,
    providing structured information for the code generation phase.
    """

    target_language: str
    # Type mappings: Python type name -> target language type
    type_mappings: dict[str, str] = field(default_factory=dict)
    # Container mappings: variable name -> ContainerMapping
    container_mappings: dict[str, ContainerMapping] = field(default_factory=dict)
    # Function return type mappings: function name -> target return type
    function_return_types: dict[str, str] = field(default_factory=dict)
    # Variable type mappings: variable name -> target type
    variable_types: dict[str, str] = field(default_factory=dict)
    # Required imports for the target language
    required_imports: list[str] = field(default_factory=list)
    # Semantic notes for code generation hints
    semantic_notes: dict[str, str] = field(default_factory=dict)


@dataclass
class PipelineResult:
    """Result from pipeline execution."""

    success: bool
    input_file: str
    output_files: dict[str, str]  # file_type -> file_path
    target_language: str = "c"
    generated_code: Optional[str] = None
    build_file_content: Optional[str] = None
    executable_path: Optional[str] = None
    phase_results: dict[PipelinePhase, Any] = field(default_factory=dict)
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    generated_files: list[str] = field(default_factory=list)
    # Semantic mapping from Phase 4 (optional, for advanced usage)
    semantic_mapping: Optional[SemanticMapping] = None


def _map_optimization_level(pipeline_level: OptimizationLevel) -> "FrontendOptimizationLevel":
    """Map pipeline OptimizationLevel to frontend OptimizationLevel."""
    if not FRONTEND_AVAILABLE:
        return None  # type: ignore

    mapping = {
        OptimizationLevel.NONE: FrontendOptimizationLevel.NONE,
        OptimizationLevel.BASIC: FrontendOptimizationLevel.BASIC,
        OptimizationLevel.MODERATE: FrontendOptimizationLevel.MODERATE,
        OptimizationLevel.AGGRESSIVE: FrontendOptimizationLevel.AGGRESSIVE,
    }
    return mapping.get(pipeline_level, FrontendOptimizationLevel.MODERATE)


class MultiGenPipeline:
    """Multi-language Python code generation pipeline."""

    def __init__(self, config: Optional[PipelineConfig] = None, target_language: str = "c"):
        """Initialize the pipeline with configuration and target language."""
        if config is None:
            config = PipelineConfig(target_language=target_language)
        elif target_language != "c":
            config.target_language = target_language

        self.config = config
        self.log = log.config(self.__class__.__name__)
        self._init_components()

    def _report_progress(self, phase: PipelinePhase, message: str) -> None:
        """Report progress to callback if configured.

        Args:
            phase: Current pipeline phase
            message: Progress message
        """
        if self.config.progress_callback:
            self.config.progress_callback(phase, message)

    def _init_components(self) -> None:
        """Initialize pipeline components."""
        # Get backend for target language
        try:
            self.backend = registry.get_backend(self.config.target_language, self.config.backend_preferences)
            self.emitter = self.backend.get_emitter()
            self.builder = self.backend.get_builder()
            self.factory = self.backend.get_factory()
            self.container_system = self.backend.get_container_system()
        except ValueError as e:
            raise ValueError(f"Unsupported target language '{self.config.target_language}': {e}") from e

        # Initialize frontend analysis components if available
        if FRONTEND_AVAILABLE and self.config.enable_advanced_analysis:
            self.subset_validator = StaticPythonSubsetValidator()
            # Note: constraint_checker deprecated in v0.1.64 (replaced by PythonConstraintChecker + backend-specific checkers)
            self.ast_analyzer = ASTAnalyzer()

            # Advanced static analysis components
            self.static_analyzer = StaticAnalyzer()
            self.symbolic_executor = SymbolicExecutor()
            self.bounds_checker = BoundsChecker()
            self.call_graph_analyzer = CallGraphAnalyzer()

            # Flow-sensitive type inference
            self.type_inference_engine = TypeInferenceEngine(enable_flow_sensitive=True)

            if self.config.enable_optimizations:
                self.compile_time_evaluator = CompileTimeEvaluator()
                self.loop_analyzer = LoopAnalyzer()
                self.function_specializer = FunctionSpecializer()
                self.vectorization_detector = VectorizationDetector()

            # Formal verification components (optional, requires Z3)
            if self.config.enable_formal_verification:
                if not Z3_AVAILABLE:
                    self.log.warning(
                        "Formal verification requested but Z3 not available. Install with: pip install multigen[z3]"
                    )
                    if self.config.strict_verification:
                        self.log.error(
                            "Strict verification mode requires Z3. "
                            "Code generation will fail without formal verification."
                        )
                    self.bounds_prover = None
                    self.correctness_prover = None
                    self.theorem_prover = None
                else:
                    mode = "strict mode" if self.config.strict_verification else "warning mode"
                    self.log.info(f"Formal verification enabled in {mode} (Z3 available)")
                    self.bounds_prover = BoundsProver()
                    self.correctness_prover = CorrectnessProver()
                    self.theorem_prover = TheoremProver()
            else:
                if self.config.strict_verification:
                    self.log.warning(
                        "Strict verification mode enabled but formal verification is disabled. "
                        "Set enable_formal_verification=True to use strict mode."
                    )
                self.bounds_prover = None
                self.correctness_prover = None
                self.theorem_prover = None
        else:
            self.log.debug("Using simplified analysis pipeline")

    def convert(self, input_path: Union[str, Path], output_path: Optional[Union[str, Path]] = None) -> PipelineResult:
        """Convert Python module through complete pipeline.

        Args:
            input_path: Path to Python file or module
            output_path: Output directory or file path

        Returns:
            PipelineResult with all outputs and metadata
        """
        input_path = Path(input_path)
        if not input_path.exists():
            self.log.error(f"Input file not found: {input_path}")
            return PipelineResult(
                success=False,
                input_file=str(input_path),
                output_files={},
                target_language=self.config.target_language,
                errors=[f"Input file not found: {input_path}"],
            )

        # Determine output directory
        if output_path:
            output_dir = Path(output_path)
        elif self.config.output_dir:
            output_dir = Path(self.config.output_dir)
        else:
            # Default to build/src structure
            output_dir = Path("build") / "src"

        output_dir.mkdir(parents=True, exist_ok=True)

        result = PipelineResult(
            success=True, input_file=str(input_path), output_files={}, target_language=self.config.target_language
        )

        try:
            self.log.info(f"Starting pipeline conversion for: {input_path} -> {self.config.target_language}")

            # Read input file
            source_code = input_path.read_text()

            # Phase 1: Validation
            self.log.debug("Starting validation phase")
            self._report_progress(PipelinePhase.VALIDATION, "Validating Python code")
            if not self._validation_phase(source_code, input_path, result):
                self.log.error("Validation phase failed")
                return result

            # Phase 2: Analysis
            self.log.debug("Starting analysis phase")
            self._report_progress(PipelinePhase.ANALYSIS, "Analyzing AST and types")
            analysis_result = self._analysis_phase(source_code, result)
            if analysis_result is None:
                self.log.error("Analysis phase failed")
                return result

            # Phase 3: Python Optimization
            self.log.debug("Starting Python optimization phase")
            self._report_progress(PipelinePhase.PYTHON_OPTIMIZATION, "Optimizing Python IR")
            optimized_analysis = self._python_optimization_phase(source_code, analysis_result, result)

            # Phase 4: Mapping (language-agnostic to target-specific)
            self.log.debug("Starting mapping phase")
            self._report_progress(PipelinePhase.MAPPING, f"Mapping to {self.config.target_language.upper()}")
            mapped_result = self._mapping_phase(optimized_analysis, result)

            # Phase 5: Target Optimization
            self.log.debug("Starting target optimization phase")
            self._report_progress(
                PipelinePhase.TARGET_OPTIMIZATION, f"Optimizing {self.config.target_language.upper()} code"
            )
            target_optimized = self._target_optimization_phase(mapped_result, result)

            # Phase 6: Generation
            self.log.debug("Starting generation phase")
            self._report_progress(PipelinePhase.GENERATION, f"Generating {self.config.target_language.upper()} source")
            if not self._generation_phase(source_code, target_optimized, output_dir, result):
                self.log.error("Generation phase failed")
                return result

            # Phase 7: Build
            if self.config.build_mode != BuildMode.NONE:
                self.log.debug(f"Starting build phase with mode: {self.config.build_mode}")
                build_msg = "Compiling" if self.config.build_mode == BuildMode.DIRECT else "Generating build file"
                self._report_progress(PipelinePhase.BUILD, build_msg)
                if not self._build_phase(output_dir, result):
                    self.log.error("Build phase failed")
                    return result

            self.log.info(f"Pipeline conversion completed successfully for: {input_path}")
            return result

        except Exception as e:
            self.log.error(f"Pipeline error: {str(e)}")
            result.success = False
            result.errors.append(f"Pipeline error: {str(e)}")
            return result

    def _validation_phase(self, source_code: str, input_path: Path, result: PipelineResult) -> bool:
        """Phase 1: Validate static-python style and translatability."""
        try:
            # Parse AST for validation
            ast.parse(source_code)

            if FRONTEND_AVAILABLE and self.config.enable_advanced_analysis:
                # Validate Python subset compatibility
                validation_result = self.subset_validator.validate_code(source_code)
                result.phase_results[PipelinePhase.VALIDATION] = validation_result

                if not validation_result.is_valid:
                    result.success = False
                    result.errors.extend([str(violation) for violation in validation_result.violations])
                    return False

                # Check memory safety constraints for C/C++ targets
                if MEMORY_SAFETY_AVAILABLE and self.config.target_language in ["c", "cpp"]:
                    memory_safety_checker = MemorySafetyChecker(language=self.config.target_language)
                    memory_warnings = memory_safety_checker.check_code(source_code)

                    # Add memory safety warnings/errors
                    for warning in memory_warnings:
                        if warning.severity == "error":
                            result.errors.append(
                                f"[{warning.violation_type.value}] {warning.message} (line {warning.line})"
                            )
                        else:
                            result.warnings.append(
                                f"[{warning.violation_type.value}] {warning.message} (line {warning.line})"
                            )

                    # Fail if there are critical memory safety errors
                    critical_errors = [w for w in memory_warnings if w.severity == "error"]
                    if critical_errors:
                        result.success = False
                        return False

                # Run formal verification if enabled
                if self.config.enable_formal_verification and self.bounds_prover is not None:
                    self.log.info("Running formal verification with Z3...")

                    # Parse AST for verification
                    tree = ast.parse(source_code)

                    # Verify each function
                    for node in ast.walk(tree):
                        if isinstance(node, ast.FunctionDef):
                            # Create analysis context for verifier
                            from .frontend.base import AnalysisLevel

                            context = AnalysisContext(
                                ast_node=node,
                                source_code=source_code,
                                analysis_level=AnalysisLevel.BASIC,
                                analysis_result=None,
                            )

                            # Run bounds verification
                            proof = self.bounds_prover.verify_memory_safety(context)

                            # Report verification results
                            if not proof.is_safe:
                                for unsafe_access in proof.unsafe_accesses:
                                    error_msg = (
                                        f"[FORMAL_VERIFICATION] Potential unsafe memory access "
                                        f"in '{proof.function_name}' at line {unsafe_access.line_number}"
                                    )
                                    if self.config.strict_verification:
                                        result.errors.append(error_msg)
                                    else:
                                        result.warnings.append(error_msg)

                                # In strict mode, halt on first unsafe function
                                if self.config.strict_verification:
                                    result.success = False
                                    result.errors.append(
                                        f"[FORMAL_VERIFICATION] Code generation halted due to verification failures in '{proof.function_name}'. "
                                        f"Fix unsafe memory accesses or disable strict_verification mode."
                                    )
                                    self.log.error(f"Verification failed in strict mode: {proof.summary}")
                                    return False

                            # Add verification recommendations
                            for recommendation in proof.recommendations:
                                if self.config.strict_verification and not proof.is_safe:
                                    result.errors.append(f"[FORMAL_VERIFICATION] {recommendation}")
                                else:
                                    result.warnings.append(f"[FORMAL_VERIFICATION] {recommendation}")

                            self.log.debug(f"Verification complete: {proof.summary}")

            else:
                # Basic validation - just check if it parses
                result.phase_results[PipelinePhase.VALIDATION] = {"basic_parse": True}

            return True

        except SyntaxError as e:
            result.success = False
            result.errors.append(f"Syntax error: {str(e)}")
            return False
        except Exception as e:
            result.success = False
            result.errors.append(f"Validation phase error: {str(e)}")
            return False

    def _analysis_phase(self, source_code: str, result: PipelineResult) -> Optional[Any]:
        """Phase 2: AST parsing and semantic element breakdown with advanced static analysis."""
        try:
            if FRONTEND_AVAILABLE and self.config.enable_advanced_analysis:
                # Use comprehensive AST analysis
                analysis_result = self.ast_analyzer.analyze(source_code)
                result.phase_results[PipelinePhase.ANALYSIS] = {"ast_analysis": analysis_result}

                if not analysis_result.convertible:
                    result.success = False
                    result.errors.extend(analysis_result.errors)
                    result.warnings.extend(analysis_result.warnings)
                    return None

                # Run advanced static analysis
                advanced_analysis: dict[str, Any] = {}

                # Parse AST for advanced analysis
                ast_root = ast.parse(source_code)

                # Create AnalysisContext for advanced analyzers
                analysis_context = AnalysisContext(
                    source_code=source_code,
                    ast_node=ast_root,
                    analysis_result=analysis_result,
                    analysis_level=AnalysisLevel.INTERMEDIATE,
                    optimization_level=FrontendOptimizationLevel.MODERATE,
                )

                # Static analysis (control flow, data flow)
                static_report = self.static_analyzer.analyze(analysis_context)
                advanced_analysis["static_analysis"] = static_report

                # Symbolic execution
                symbolic_report = self.symbolic_executor.analyze(analysis_context)
                advanced_analysis["symbolic_execution"] = symbolic_report

                # Bounds checking
                bounds_report = self.bounds_checker.analyze(analysis_context)
                advanced_analysis["bounds_checking"] = bounds_report

                # Call graph analysis
                call_graph_report = self.call_graph_analyzer.analyze(analysis_context)
                advanced_analysis["call_graph"] = call_graph_report

                # Flow-sensitive type inference
                type_inference_results: dict[str, dict[str, Any]] = {}
                for node in ast.walk(ast_root):
                    if isinstance(node, ast.FunctionDef):
                        func_inference = self.type_inference_engine.analyze_function_signature_enhanced(node)
                        type_inference_results[node.name] = func_inference
                        self.log.debug(f"Type inference completed for function: {node.name}")

                advanced_analysis["type_inference"] = type_inference_results

                # Immutability analysis
                immutability_analyzer = ImmutabilityAnalyzer()
                immutability_results = immutability_analyzer.analyze_module(ast_root)
                advanced_analysis["immutability"] = immutability_results
                self.log.debug(f"Immutability analysis completed for {len(immutability_results)} functions")

                # Python constraint checking (uses immutability results)
                python_constraint_checker = PythonConstraintChecker(immutability_results=immutability_results)
                constraint_violations = python_constraint_checker.check_code(source_code)
                advanced_analysis["python_constraints"] = constraint_violations

                # Add warnings/errors from constraint violations
                for violation in constraint_violations:
                    if violation.severity == "error":
                        result.errors.append(f"[{violation.rule_id}] {violation.message} (line {violation.line})")
                    else:
                        result.warnings.append(f"[{violation.rule_id}] {violation.message} (line {violation.line})")

                # Fail if there are critical constraint violations
                critical_errors = [v for v in constraint_violations if v.severity == "error"]
                if critical_errors:
                    result.success = False
                    self.log.error(f"Found {len(critical_errors)} critical constraint violations")
                    return None

                # Store advanced analysis results
                result.phase_results[PipelinePhase.ANALYSIS]["advanced"] = advanced_analysis

                # Store additional data for later phases
                # Note: Additional data (source_code, ast_root) stored in phase_results
                return analysis_result
            else:
                # Simple analysis using built-in frontend function
                try:
                    simple_analysis = analyze_python_code(source_code)
                    result.phase_results[PipelinePhase.ANALYSIS] = simple_analysis

                    # Create a simple analysis result object
                    class SimpleAnalysisResult:
                        def __init__(self, code: str, analysis: Any) -> None:
                            self.source_code = code
                            self.ast_root = ast.parse(code)
                            self.analysis = analysis
                            self.convertible = analysis.convertible
                            self.errors = analysis.errors if hasattr(analysis, "errors") else []
                            self.warnings = analysis.warnings if hasattr(analysis, "warnings") else []

                    return SimpleAnalysisResult(source_code, simple_analysis)
                except Exception:
                    # Fallback to basic analysis if simplified result creation fails
                    basic_result = type(
                        "BasicAnalysis",
                        (),
                        {
                            "source_code": source_code,
                            "ast_root": ast.parse(source_code),
                            "convertible": True,
                            "errors": [],
                            "warnings": [],
                        },
                    )()
                    result.phase_results[PipelinePhase.ANALYSIS] = {"basic": True}
                    return basic_result

        except Exception as e:
            result.success = False
            result.errors.append(f"Analysis phase error: {str(e)}")
            return None

    def _python_optimization_phase(self, source_code: str, analysis_result: Any, result: PipelineResult) -> Any:
        """Phase 3: Python-level optimizations."""
        try:
            if FRONTEND_AVAILABLE and self.config.enable_optimizations:
                # Create analysis context
                context = AnalysisContext(
                    source_code=source_code,
                    ast_node=ast.parse(source_code),
                    analysis_result=analysis_result,
                    optimization_level=_map_optimization_level(self.config.optimization_level),
                )

                optimizations = {}

                # Compile-time evaluation
                compile_time_result = self.compile_time_evaluator.optimize(context)
                optimizations["compile_time"] = compile_time_result

                # Loop analysis
                loop_result = self.loop_analyzer.optimize(context)
                optimizations["loops"] = loop_result

                # Function specialization
                specialization_result = self.function_specializer.optimize(context)
                optimizations["specialization"] = specialization_result

                # Vectorization detection
                vectorization_result = self.vectorization_detector.optimize(context)
                optimizations["vectorization"] = vectorization_result

                result.phase_results[PipelinePhase.PYTHON_OPTIMIZATION] = optimizations
            else:
                result.phase_results[PipelinePhase.PYTHON_OPTIMIZATION] = {"enabled": False}

            return analysis_result  # Return analysis for next phase

        except Exception as e:
            result.warnings.append(f"Python optimization phase warning: {str(e)}")
            return analysis_result  # Continue with unoptimized version

    def _mapping_phase(self, analysis_result: Any, result: PipelineResult) -> Any:
        """Phase 4: Map Python semantics to target language semantics.

        This phase transforms Python type information into target language types,
        computing container mappings, function return types, and variable types
        using the backend's type system and container system.
        """
        try:
            # Create semantic mapping structure
            semantic_mapping = SemanticMapping(target_language=self.config.target_language)

            # Standard Python type mappings using backend's emitter
            python_types = ["int", "float", "bool", "str", "None", "Any"]
            for py_type in python_types:
                try:
                    target_type = self.emitter.map_python_type(py_type)
                    semantic_mapping.type_mappings[py_type] = target_type
                except Exception:
                    # Fallback for types the backend doesn't explicitly handle
                    semantic_mapping.type_mappings[py_type] = py_type

            # Extract type information from analysis result if available
            if analysis_result is not None:
                self._extract_function_types(analysis_result, semantic_mapping)
                self._extract_variable_types(analysis_result, semantic_mapping, result)
                self._extract_container_types(analysis_result, semantic_mapping, result)

            # Get required imports from container system
            try:
                semantic_mapping.required_imports = self.container_system.get_required_imports()
            except Exception:
                pass  # Container system may not have imports

            # Add backend-specific semantic notes
            semantic_mapping.semantic_notes["backend"] = self.backend.get_name()
            semantic_mapping.semantic_notes["extension"] = self.backend.get_file_extension()

            # Store mapping in result
            result.semantic_mapping = semantic_mapping
            result.phase_results[PipelinePhase.MAPPING] = {
                "target_language": self.config.target_language,
                "type_mappings": semantic_mapping.type_mappings,
                "container_mappings": {
                    k: {"python_type": v.python_type, "target_type": v.target_type}
                    for k, v in semantic_mapping.container_mappings.items()
                },
                "function_return_types": semantic_mapping.function_return_types,
                "variable_count": len(semantic_mapping.variable_types),
                "required_imports": semantic_mapping.required_imports,
            }

            self.log.debug(
                f"Phase 4 mapping complete: {len(semantic_mapping.type_mappings)} types, "
                f"{len(semantic_mapping.container_mappings)} containers, "
                f"{len(semantic_mapping.function_return_types)} functions"
            )

            return analysis_result

        except Exception as e:
            result.warnings.append(f"Mapping phase warning: {str(e)}")
            return analysis_result

    def _extract_function_types(self, analysis_result: Any, semantic_mapping: SemanticMapping) -> None:
        """Extract function return types from analysis result."""
        # Try to get function information from analysis result
        if hasattr(analysis_result, "functions"):
            for func_name, func_info in analysis_result.functions.items():
                if hasattr(func_info, "return_type") and func_info.return_type:
                    py_return_type = str(func_info.return_type)
                    try:
                        target_return_type = self.emitter.map_python_type(py_return_type)
                        semantic_mapping.function_return_types[func_name] = target_return_type
                    except Exception:
                        semantic_mapping.function_return_types[func_name] = py_return_type

    def _extract_variable_types(
        self, analysis_result: Any, semantic_mapping: SemanticMapping, result: PipelineResult
    ) -> None:
        """Extract variable types from analysis result."""
        # Try type inference results from advanced analysis
        analysis_phase = result.phase_results.get(PipelinePhase.ANALYSIS, {})
        advanced = analysis_phase.get("advanced", {})
        type_inference = advanced.get("type_inference", {})

        for func_name, func_inference in type_inference.items():
            if isinstance(func_inference, dict):
                # Extract parameter types
                param_types = func_inference.get("parameter_types", {})
                for param_name, param_type in param_types.items():
                    var_key = f"{func_name}.{param_name}"
                    try:
                        target_type = self.emitter.map_python_type(str(param_type))
                        semantic_mapping.variable_types[var_key] = target_type
                    except Exception:
                        semantic_mapping.variable_types[var_key] = str(param_type)

    def _extract_container_types(
        self, analysis_result: Any, semantic_mapping: SemanticMapping, result: PipelineResult
    ) -> None:
        """Extract container type mappings from analysis result."""
        # Common container patterns to look for
        container_patterns = {
            "list[int]": ("int",),
            "list[str]": ("str",),
            "list[float]": ("float",),
            "dict[str, int]": ("str", "int"),
            "dict[str, str]": ("str", "str"),
            "dict[int, int]": ("int", "int"),
            "set[int]": ("int",),
            "set[str]": ("str",),
        }

        # Map containers using the backend's container system
        for py_type, element_types in container_patterns.items():
            try:
                if py_type.startswith("list["):
                    elem_type = self.emitter.map_python_type(element_types[0])
                    target_type = self.container_system.get_list_type(elem_type)
                elif py_type.startswith("dict["):
                    key_type = self.emitter.map_python_type(element_types[0])
                    val_type = self.emitter.map_python_type(element_types[1])
                    target_type = self.container_system.get_dict_type(key_type, val_type)
                elif py_type.startswith("set["):
                    elem_type = self.emitter.map_python_type(element_types[0])
                    target_type = self.container_system.get_set_type(elem_type)
                else:
                    continue

                semantic_mapping.container_mappings[py_type] = ContainerMapping(
                    python_type=py_type,
                    target_type=target_type,
                    element_types=[self.emitter.map_python_type(t) for t in element_types],
                )
            except Exception:
                # Backend may not support all container types
                pass

    def _target_optimization_phase(self, analysis_result: Any, result: PipelineResult) -> Any:
        """Phase 5: Target language-specific optimizations."""
        try:
            # This phase would handle target language-specific optimizations
            # For example:
            # - C: STC container optimizations, pointer optimizations
            # - Rust: Borrow checker optimizations, zero-copy optimizations
            # - Go: Goroutine usage, channel optimizations

            target_opts = {
                "target_language": self.config.target_language,
                "optimizations_applied": [],
            }

            # Backend-specific optimizations could be added here
            if hasattr(self.backend, "optimize"):
                target_opts["backend_optimizations"] = self.backend.optimize(analysis_result)

            result.phase_results[PipelinePhase.TARGET_OPTIMIZATION] = target_opts
            return analysis_result

        except Exception as e:
            result.warnings.append(f"Target optimization phase warning: {str(e)}")
            return analysis_result

    def _generation_phase(
        self, source_code: str, analysis_result: Any, output_dir: Path, result: PipelineResult
    ) -> bool:
        """Phase 6: Target language code generation."""
        try:
            # Generate code using selected backend
            generated_code = self.emitter.emit_module(source_code, analysis_result)

            # Write source file with correct extension
            file_extension = self.backend.get_file_extension()
            source_file_path = output_dir / (Path(result.input_file).stem + file_extension)
            source_file_path.write_text(generated_code)

            result.generated_code = generated_code
            result.output_files[f"{self.config.target_language}_source"] = str(source_file_path)
            result.generated_files.append(str(source_file_path))
            result.phase_results[PipelinePhase.GENERATION] = {
                "source_file": str(source_file_path),
                "backend": self.backend.get_name(),
                "generated_lines": len(generated_code.splitlines()),
            }
            return True

        except Exception as e:
            result.success = False
            result.errors.append(f"Generation phase error: {str(e)}")
            return False

    def _build_phase(self, output_dir: Path, result: PipelineResult) -> bool:
        """Phase 7: Build executable or generate build file."""
        try:
            source_key = f"{self.config.target_language}_source"
            source_file = result.output_files.get(source_key)
            if not source_file:
                result.errors.append("No source file available for build phase")
                return False

            source_file_path = Path(source_file)

            if self.config.build_mode == BuildMode.MAKEFILE:
                # Generate build file using backend
                build_content = self.builder.generate_build_file([str(source_file_path)], source_file_path.stem)
                build_file_path = output_dir / self.builder.get_build_filename()
                build_file_path.write_text(build_content)

                result.build_file_content = build_content
                result.output_files["build_file"] = str(build_file_path)

            elif self.config.build_mode == BuildMode.DIRECT:
                # Direct compilation using backend
                # Pass optimization level to builder (LLVM backend uses this)
                opt_level_map = {
                    OptimizationLevel.NONE: 0,
                    OptimizationLevel.BASIC: 1,
                    OptimizationLevel.MODERATE: 2,
                    OptimizationLevel.AGGRESSIVE: 3,
                }
                opt_level = opt_level_map.get(self.config.optimization_level, 2)

                # Check if builder supports opt_level parameter (LLVM does)
                import inspect

                sig = inspect.signature(self.builder.compile_direct)
                if "opt_level" in sig.parameters:
                    success = self.builder.compile_direct(
                        str(source_file_path), str(output_dir), opt_level=opt_level
                    )
                else:
                    success = self.builder.compile_direct(str(source_file_path), str(output_dir))

                if success:
                    executable_path = output_dir / source_file_path.stem
                    result.executable_path = str(executable_path)
                    result.output_files["executable"] = str(executable_path)
                else:
                    result.errors.append("Direct compilation failed")
                    return False

            result.phase_results[PipelinePhase.BUILD] = {
                "mode": self.config.build_mode.value,
                "outputs": result.output_files,
            }

            return True

        except Exception as e:
            result.success = False
            result.errors.append(f"Build phase error: {str(e)}")
            return False


# Compatibility aliases and functions
CGenPipeline = MultiGenPipeline  # For backward compatibility


def convert_python_to_language(
    input_path: Union[str, Path],
    target_language: str = "c",
    output_path: Optional[Union[str, Path]] = None,
    optimization_level: OptimizationLevel = OptimizationLevel.MODERATE,
    build_mode: BuildMode = BuildMode.NONE,
) -> PipelineResult:
    """Convert Python file to target language using MultiGen pipeline."""
    config = PipelineConfig(
        optimization_level=optimization_level,
        build_mode=build_mode,
        target_language=target_language,
        output_dir=str(output_path) if output_path else "build/src",
    )
    pipeline = MultiGenPipeline(config)
    return pipeline.convert(input_path, output_path)
