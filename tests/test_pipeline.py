"""Tests for MultiGen pipeline system."""

import tempfile
from pathlib import Path

import pytest

from multigen.backends.registry import registry
from multigen.pipeline import BuildMode, MultiGenPipeline, OptimizationLevel, PipelineConfig


class TestMultiGenPipeline:
    """Test the multi-language pipeline."""

    def test_pipeline_initialization(self):
        """Test pipeline initialization with different targets."""
        # Test default initialization
        pipeline = MultiGenPipeline()
        assert pipeline.config.target_language == "c"

        # Test with specific target
        if registry.has_backend("rust"):
            pipeline = MultiGenPipeline(target_language="rust")
            assert pipeline.config.target_language == "rust"

    def test_pipeline_with_config(self):
        """Test pipeline with custom configuration."""
        config = PipelineConfig(
            target_language="c", optimization_level=OptimizationLevel.AGGRESSIVE, build_mode=BuildMode.DIRECT
        )
        pipeline = MultiGenPipeline(config)
        assert pipeline.config.target_language == "c"
        assert pipeline.config.optimization_level == OptimizationLevel.AGGRESSIVE

    def test_unsupported_language_raises_error(self):
        """Test that unsupported language raises error."""
        with pytest.raises(ValueError, match="Unsupported target language"):
            MultiGenPipeline(target_language="nonexistent")

    @pytest.mark.parametrize("target", ["c", "rust", "go"])
    def test_pipeline_conversion(self, target):
        """Test pipeline conversion for different targets."""
        if not registry.has_backend(target):
            pytest.skip(f"Backend {target} not available")

        # Create a simple Python file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def add(x: int, y: int) -> int:
    return x + y
""")
            temp_python_file = f.name

        try:
            # Create temporary output directory
            with tempfile.TemporaryDirectory() as temp_dir:
                pipeline = MultiGenPipeline(target_language=target)
                result = pipeline.convert(temp_python_file, temp_dir)

                # Check result
                assert result.success, f"Conversion failed: {result.errors}"
                assert result.target_language == target
                assert len(result.generated_files) > 0

                # Check generated file exists
                generated_file = Path(result.generated_files[0])
                assert generated_file.exists()
                assert generated_file.suffix == pipeline.backend.get_file_extension()

                # Check file has content
                content = generated_file.read_text()
                assert len(content) > 0
                assert "add" in content  # Function name should be present

        finally:
            # Clean up
            Path(temp_python_file).unlink()

    @pytest.mark.parametrize("target", ["c", "rust", "go"])
    def test_pipeline_with_build_file_generation(self, target):
        """Test pipeline with build file generation."""
        if not registry.has_backend(target):
            pytest.skip(f"Backend {target} not available")

        # Create a simple Python file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def main() -> int:
    print("Hello, world!")
    return 0
""")
            temp_python_file = f.name

        try:
            # Create temporary output directory
            with tempfile.TemporaryDirectory() as temp_dir:
                config = PipelineConfig(target_language=target, build_mode=BuildMode.MAKEFILE)
                pipeline = MultiGenPipeline(config)
                result = pipeline.convert(temp_python_file, temp_dir)

                # Check result
                assert result.success, f"Conversion failed: {result.errors}"

                # Check build file was generated
                build_file_key = "build_file"
                assert build_file_key in result.output_files

                build_file = Path(result.output_files[build_file_key])
                assert build_file.exists()

                # Check build file has content
                build_content = build_file.read_text()
                assert len(build_content) > 0

        finally:
            # Clean up
            Path(temp_python_file).unlink()

    def test_pipeline_error_handling(self):
        """Test pipeline error handling."""
        pipeline = MultiGenPipeline(target_language="c")

        # Test with non-existent file
        result = pipeline.convert("nonexistent.py")
        assert not result.success
        assert len(result.errors) > 0


class TestPipelineIntegration:
    """Integration tests for pipeline functionality."""

    def test_convert_python_to_language_function(self):
        """Test the convenience function for conversion."""
        from multigen.pipeline import convert_python_to_language

        # Create a simple Python file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def multiply(a: int, b: int) -> int:
    return a * b
""")
            temp_python_file = f.name

        try:
            # Create temporary output directory
            with tempfile.TemporaryDirectory() as temp_dir:
                result = convert_python_to_language(
                    temp_python_file, target_language="c", output_path=temp_dir, build_mode=BuildMode.NONE
                )

                # Check result
                assert result.success
                assert result.target_language == "c"
                assert len(result.generated_files) > 0

        finally:
            # Clean up
            Path(temp_python_file).unlink()

    @pytest.mark.parametrize("target", ["c", "rust", "go"])
    def test_different_function_patterns(self, target):
        """Test conversion of different function patterns."""
        if not registry.has_backend(target):
            pytest.skip(f"Backend {target} not available")

        test_cases = [
            # Simple arithmetic
            """def add(x: int, y: int) -> int:
    return x + y""",
            # Simple return
            """def get_answer() -> int:
    return 42""",
            # Single parameter
            """def twice(x: int) -> int:
    return x * 2""",
        ]

        for i, python_code in enumerate(test_cases):
            with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
                f.write(python_code)
                temp_python_file = f.name

            try:
                with tempfile.TemporaryDirectory() as temp_dir:
                    pipeline = MultiGenPipeline(target_language=target)
                    result = pipeline.convert(temp_python_file, temp_dir)

                    assert result.success, f"Case {i} failed for {target}: {result.errors}"

                    # Check generated file
                    generated_file = Path(result.generated_files[0])
                    content = generated_file.read_text()
                    assert len(content) > 0

            finally:
                Path(temp_python_file).unlink()


class TestPipelinePhase4Mapping:
    """Tests for Phase 4 (Mapping) semantic transformation."""

    def test_semantic_mapping_created(self):
        """Test that SemanticMapping is created during conversion."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def add(x: int, y: int) -> int:
    return x + y
""")
            temp_python_file = f.name

        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                pipeline = MultiGenPipeline(target_language="c")
                result = pipeline.convert(temp_python_file, temp_dir)

                assert result.success, f"Conversion failed: {result.errors}"
                # Check semantic_mapping was created
                assert result.semantic_mapping is not None
                assert result.semantic_mapping.target_language == "c"

        finally:
            Path(temp_python_file).unlink()

    def test_type_mappings_populated(self):
        """Test that basic type mappings are populated."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def test(x: int, y: float, flag: bool) -> str:
    return "hello"
""")
            temp_python_file = f.name

        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                pipeline = MultiGenPipeline(target_language="c")
                result = pipeline.convert(temp_python_file, temp_dir)

                assert result.success
                mapping = result.semantic_mapping
                assert mapping is not None

                # Check basic types are mapped
                assert "int" in mapping.type_mappings
                assert "float" in mapping.type_mappings
                assert "bool" in mapping.type_mappings
                assert "str" in mapping.type_mappings

        finally:
            Path(temp_python_file).unlink()

    def test_phase_results_contain_mapping_info(self):
        """Test that phase_results contains mapping phase information."""
        from multigen.pipeline import PipelinePhase

        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def compute(x: int) -> int:
    return x * 2
""")
            temp_python_file = f.name

        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                pipeline = MultiGenPipeline(target_language="c")
                result = pipeline.convert(temp_python_file, temp_dir)

                assert result.success
                # Check mapping phase is in results
                assert PipelinePhase.MAPPING in result.phase_results
                mapping_result = result.phase_results[PipelinePhase.MAPPING]

                assert "target_language" in mapping_result
                assert mapping_result["target_language"] == "c"
                assert "type_mappings" in mapping_result

        finally:
            Path(temp_python_file).unlink()

    @pytest.mark.parametrize("target", ["c", "cpp", "rust", "go"])
    def test_semantic_mapping_per_backend(self, target):
        """Test semantic mapping is created for each backend."""
        if not registry.has_backend(target):
            pytest.skip(f"Backend {target} not available")

        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("""def square(n: int) -> int:
    return n * n
""")
            temp_python_file = f.name

        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                pipeline = MultiGenPipeline(target_language=target)
                result = pipeline.convert(temp_python_file, temp_dir)

                assert result.success, f"Conversion failed for {target}: {result.errors}"
                assert result.semantic_mapping is not None
                assert result.semantic_mapping.target_language == target
                # Each backend should have semantic notes
                assert "backend" in result.semantic_mapping.semantic_notes
                assert "extension" in result.semantic_mapping.semantic_notes

        finally:
            Path(temp_python_file).unlink()
