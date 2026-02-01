"""Tests for pipeline phase result types."""

import pytest

from multigen.pipeline_types import (
    AnalysisPhaseResult,
    BuildPhaseResult,
    GenerationPhaseResult,
    PythonOptimizationPhaseResult,
    TargetOptimizationPhaseResult,
    ValidationPhaseResult,
)


class TestValidationPhaseResult:
    """Tests for ValidationPhaseResult dataclass."""

    def test_basic_construction(self) -> None:
        """Test basic construction with required fields."""
        result = ValidationPhaseResult(is_valid=True, parse_success=True)
        assert result.is_valid is True
        assert result.parse_success is True
        assert result.tier is None
        assert result.violations == []
        assert result.warnings == []

    def test_construction_with_all_fields(self) -> None:
        """Test construction with all fields."""
        result = ValidationPhaseResult(
            is_valid=False,
            parse_success=True,
            tier="tier2",
            violations=["async not supported"],
            warnings=["consider using type hints"],
            supported_features=["functions", "classes"],
            unsupported_features=["async"],
            memory_safety_checked=True,
            memory_safety_errors=["buffer overflow"],
            memory_safety_warnings=["potential null"],
        )
        assert result.is_valid is False
        assert result.tier == "tier2"
        assert len(result.violations) == 1
        assert len(result.supported_features) == 2
        assert result.memory_safety_checked is True

    def test_default_list_independence(self) -> None:
        """Test that default lists are independent between instances."""
        r1 = ValidationPhaseResult(is_valid=True, parse_success=True)
        r2 = ValidationPhaseResult(is_valid=True, parse_success=True)
        r1.violations.append("test")
        assert "test" not in r2.violations


class TestAnalysisPhaseResult:
    """Tests for AnalysisPhaseResult dataclass."""

    def test_basic_construction(self) -> None:
        """Test basic construction with required fields."""
        result = AnalysisPhaseResult(success=True, convertible=True)
        assert result.success is True
        assert result.convertible is True
        assert result.function_count == 0
        assert result.complexity == "simple"
        assert result.errors == []

    def test_construction_with_analysis_data(self) -> None:
        """Test construction with analysis data."""
        result = AnalysisPhaseResult(
            success=True,
            convertible=True,
            function_count=5,
            global_variable_count=2,
            imports=["math", "os"],
            complexity="moderate",
            advanced_analysis={"type_inference": {"main": {"return": "int"}}},
        )
        assert result.function_count == 5
        assert result.global_variable_count == 2
        assert len(result.imports) == 2
        assert result.complexity == "moderate"
        assert result.advanced_analysis is not None


class TestPythonOptimizationPhaseResult:
    """Tests for PythonOptimizationPhaseResult dataclass."""

    def test_disabled_optimization(self) -> None:
        """Test result when optimization is disabled."""
        result = PythonOptimizationPhaseResult(enabled=False)
        assert result.enabled is False
        assert result.optimizations_applied == []
        assert result.compile_time is None

    def test_enabled_with_optimizations(self) -> None:
        """Test result when optimization is enabled."""
        result = PythonOptimizationPhaseResult(
            enabled=True,
            optimizations_applied=["constant_folding", "loop_unrolling"],
            compile_time={"constants_evaluated": 5},
            loops={"unrolled_loops": 2},
            specialization={"specialized_functions": 1},
            vectorization={"vectorized_loops": 0},
        )
        assert result.enabled is True
        assert len(result.optimizations_applied) == 2
        assert result.compile_time is not None
        assert result.loops is not None


class TestTargetOptimizationPhaseResult:
    """Tests for TargetOptimizationPhaseResult dataclass."""

    def test_without_optimizer(self) -> None:
        """Test result when backend has no optimizer."""
        result = TargetOptimizationPhaseResult(
            target_language="rust",
            optimization_level=2,
            optimizer_available=False,
        )
        assert result.target_language == "rust"
        assert result.optimization_level == 2
        assert result.optimizer_available is False
        assert result.optimizations_applied == []
        assert result.optimizer_info is None

    def test_with_optimizer(self) -> None:
        """Test result when backend has optimizer."""
        from multigen.backends.optimizer import OptimizationInfo

        opt_info = OptimizationInfo(
            level=3,
            level_name="O3",
            passes_applied=["dce", "inline"],
        )
        result = TargetOptimizationPhaseResult(
            target_language="llvm",
            optimization_level=3,
            optimizer_available=True,
            optimizations_applied=["dce", "inline"],
            optimizer_info=opt_info,
        )
        assert result.optimizer_available is True
        assert result.optimizer_info is not None
        assert result.optimizer_info.level == 3


class TestGenerationPhaseResult:
    """Tests for GenerationPhaseResult dataclass."""

    def test_successful_generation(self) -> None:
        """Test result for successful generation."""
        result = GenerationPhaseResult(
            success=True,
            source_file="/build/src/main.rs",
            backend_name="rust",
            file_extension=".rs",
            generated_lines=150,
        )
        assert result.success is True
        assert result.source_file == "/build/src/main.rs"
        assert result.backend_name == "rust"
        assert result.file_extension == ".rs"
        assert result.generated_lines == 150

    def test_failed_generation(self) -> None:
        """Test result for failed generation."""
        result = GenerationPhaseResult(
            success=False,
            source_file="",
            backend_name="rust",
            file_extension=".rs",
            generated_lines=0,
        )
        assert result.success is False


class TestBuildPhaseResult:
    """Tests for BuildPhaseResult dataclass."""

    def test_no_build(self) -> None:
        """Test result when no build was performed."""
        result = BuildPhaseResult(
            success=True,
            mode="none",
        )
        assert result.success is True
        assert result.mode == "none"
        assert result.outputs == {}
        assert result.compile_time_ms is None

    def test_direct_compilation(self) -> None:
        """Test result for direct compilation."""
        result = BuildPhaseResult(
            success=True,
            mode="direct",
            outputs={"executable": "/build/bin/main"},
            compile_time_ms=1234.5,
            executable_size_bytes=32768,
        )
        assert result.mode == "direct"
        assert "executable" in result.outputs
        assert result.compile_time_ms == 1234.5
        assert result.executable_size_bytes == 32768

    def test_makefile_generation(self) -> None:
        """Test result for makefile generation."""
        result = BuildPhaseResult(
            success=True,
            mode="makefile",
            outputs={"build_file": "/build/src/Makefile"},
        )
        assert result.mode == "makefile"
        assert "build_file" in result.outputs


class TestTypeNarrowing:
    """Tests for type narrowing with isinstance()."""

    def test_validation_isinstance(self) -> None:
        """Test isinstance works with ValidationPhaseResult."""
        result = ValidationPhaseResult(is_valid=True, parse_success=True)
        assert isinstance(result, ValidationPhaseResult)

    def test_target_optimization_isinstance(self) -> None:
        """Test isinstance works with TargetOptimizationPhaseResult."""
        result = TargetOptimizationPhaseResult(
            target_language="cpp",
            optimization_level=2,
            optimizer_available=False,
        )
        assert isinstance(result, TargetOptimizationPhaseResult)

    def test_distinguishing_types(self) -> None:
        """Test that different phase results are distinguishable."""
        validation = ValidationPhaseResult(is_valid=True, parse_success=True)
        analysis = AnalysisPhaseResult(success=True, convertible=True)
        generation = GenerationPhaseResult(
            success=True,
            source_file="test.c",
            backend_name="c",
            file_extension=".c",
            generated_lines=10,
        )

        # Each should only match its own type
        assert isinstance(validation, ValidationPhaseResult)
        assert not isinstance(validation, AnalysisPhaseResult)

        assert isinstance(analysis, AnalysisPhaseResult)
        assert not isinstance(analysis, GenerationPhaseResult)

        assert isinstance(generation, GenerationPhaseResult)
        assert not isinstance(generation, ValidationPhaseResult)
