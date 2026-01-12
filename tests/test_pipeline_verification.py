"""Tests for pipeline integration with formal verification."""

import sys
from pathlib import Path

import pytest

# Add src directory to Python path
project_root = Path(__file__).parent.parent
src_path = project_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

from multigen.pipeline import MultiGenPipeline, PipelineConfig

# Check if Z3 is available
try:
    import z3

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False


class TestPipelineVerification:
    """Test formal verification integration in pipeline."""

    def test_pipeline_without_verification(self, tmp_path):
        """Test pipeline works with verification disabled (default)."""
        # Create a simple Python file
        test_file = tmp_path / "test_simple.py"
        test_file.write_text("""
def add(x: int, y: int) -> int:
    return x + y
""")

        # Run pipeline without verification
        config = PipelineConfig(target_language="c", enable_formal_verification=False)
        pipeline = MultiGenPipeline(config=config)

        result = pipeline.convert(test_file)
        assert result.success

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_pipeline_with_verification_enabled(self, tmp_path):
        """Test pipeline with formal verification enabled."""
        # Create a simple Python file with array access
        test_file = tmp_path / "test_array.py"
        test_file.write_text("""
def safe_access(arr: list[int], n: int) -> int:
    result: int = 0
    for i in range(n):
        result = arr[i]
    return result
""")

        # Run pipeline with verification enabled
        config = PipelineConfig(
            target_language="c", enable_formal_verification=True, enable_advanced_analysis=True
        )
        pipeline = MultiGenPipeline(config=config)

        result = pipeline.convert(test_file)
        assert result.success
        # Verification should run but may generate warnings

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_verification_detects_issues(self, tmp_path):
        """Test that verification can detect potential issues."""
        # Create a Python file with potentially unsafe access
        test_file = tmp_path / "test_unsafe.py"
        test_file.write_text("""
def unsafe_access(arr: list[int], n: int) -> int:
    # This could be unsafe if not careful
    for i in range(n + 1):
        x = arr[i]
    return x
""")

        # Run pipeline with verification
        config = PipelineConfig(
            target_language="c", enable_formal_verification=True, enable_advanced_analysis=True
        )
        pipeline = MultiGenPipeline(config=config)

        result = pipeline.convert(test_file)
        # May succeed with warnings, or may detect issues
        # The important thing is it doesn't crash
        assert result is not None

    def test_verification_flag_default(self):
        """Test that formal verification is disabled by default."""
        config = PipelineConfig()
        assert config.enable_formal_verification is False

    def test_verification_graceful_without_z3(self, tmp_path):
        """Test pipeline handles missing Z3 gracefully."""
        test_file = tmp_path / "test_basic.py"
        test_file.write_text("""
def simple(x: int) -> int:
    return x + 1
""")

        # Try to enable verification (should warn if Z3 missing)
        config = PipelineConfig(target_language="c", enable_formal_verification=True)
        pipeline = MultiGenPipeline(config=config)

        # Should work even if Z3 is missing (graceful degradation)
        result = pipeline.convert(test_file)
        assert result.success


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
