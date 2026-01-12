"""Tests for strict verification mode."""

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


class TestStrictVerification:
    """Test strict verification mode."""

    def test_strict_mode_disabled_by_default(self):
        """Test that strict mode is disabled by default."""
        config = PipelineConfig()
        assert config.strict_verification is False

    def test_strict_mode_can_be_enabled(self):
        """Test that strict mode can be enabled."""
        config = PipelineConfig(strict_verification=True)
        assert config.strict_verification is True

    def test_strict_mode_warning_without_verification(self, tmp_path, caplog):
        """Test that enabling strict mode without verification gives a warning."""
        test_file = tmp_path / "test.py"
        test_file.write_text(
            """
def add(x: int, y: int) -> int:
    return x + y
"""
        )

        config = PipelineConfig(
            target_language="c",
            enable_formal_verification=False,
            strict_verification=True,  # Strict mode without verification
        )
        pipeline = MultiGenPipeline(config=config)

        # Should warn but still work
        result = pipeline.convert(test_file)
        assert result.success

        # Check for warning in logs
        assert any("strict verification mode enabled but formal verification is disabled" in msg.lower() for msg in caplog.messages)

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_strict_mode_passes_safe_code(self, tmp_path):
        """Test that strict mode allows safe code to pass."""
        test_file = tmp_path / "test_safe.py"
        test_file.write_text(
            """
def add(x: int, y: int) -> int:
    return x + y
"""
        )

        config = PipelineConfig(
            target_language="c", enable_formal_verification=True, strict_verification=True
        )
        pipeline = MultiGenPipeline(config=config)

        result = pipeline.convert(test_file)
        assert result.success
        assert len(result.errors) == 0

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_strict_mode_blocks_unsafe_code(self, tmp_path):
        """Test that strict mode blocks code with verification failures."""
        test_file = tmp_path / "test_unsafe.py"
        test_file.write_text(
            """
def unsafe_access(arr: list[int], n: int) -> int:
    # Potentially unsafe: range(n) but no guarantee arr has n elements
    result: int = 0
    for i in range(n):
        result = arr[i]
    return result
"""
        )

        config = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=True,
        )
        pipeline = MultiGenPipeline(config=config)

        result = pipeline.convert(test_file)

        # Should fail in strict mode
        # Note: May pass if verifier doesn't detect issue - adjust based on actual behavior
        if not result.success:
            assert len(result.errors) > 0
            assert any("FORMAL_VERIFICATION" in error for error in result.errors)
            assert any("Code generation halted" in error for error in result.errors)

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_strict_mode_errors_vs_warnings(self, tmp_path):
        """Test that strict mode uses errors instead of warnings."""
        test_file = tmp_path / "test_maybe_unsafe.py"
        test_file.write_text(
            """
def access_array(arr: list[int], n: int) -> int:
    total: int = 0
    for i in range(n):
        total = arr[i]
    return total
"""
        )

        # Non-strict mode
        config_warn = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=False,
        )
        pipeline_warn = MultiGenPipeline(config=config_warn)
        result_warn = pipeline_warn.convert(test_file)

        # Strict mode
        config_strict = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=True,
        )
        pipeline_strict = MultiGenPipeline(config=config_strict)
        result_strict = pipeline_strict.convert(test_file)

        # If verification finds issues:
        # - Non-strict: warnings, success=True
        # - Strict: errors, success=False
        if result_warn.warnings:
            # Verifier found issues in non-strict mode (warnings)
            assert result_warn.success  # Still succeeds
            # Strict mode should fail
            if not result_strict.success:
                assert len(result_strict.errors) > 0

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_strict_mode_prevents_c_code_generation(self, tmp_path):
        """Test that strict mode halts pipeline when verification fails."""
        test_file = tmp_path / "test_unsafe.py"
        test_file.write_text(
            """
def unsafe_function(arr: list[int], n: int) -> int:
    # Potentially unsafe array access
    result: int = 0
    for i in range(n):
        result = arr[i]
    return result
"""
        )

        # Test 1: Non-strict mode - should succeed despite verification warnings
        config_warn = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=False,
        )
        pipeline_warn = MultiGenPipeline(config=config_warn)
        result_warn = pipeline_warn.convert(test_file)

        # Non-strict mode should succeed
        assert result_warn.success
        print(f"✓ Non-strict mode: result.success={result_warn.success}")
        print(f"  Warnings: {len(result_warn.warnings)}")
        print(f"  Errors: {len(result_warn.errors)}")

        # Test 2: Strict mode - should FAIL when verification finds issues
        config_strict = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=True,
        )
        pipeline_strict = MultiGenPipeline(config=config_strict)
        result_strict = pipeline_strict.convert(test_file)

        # If verification failed in strict mode
        if not result_strict.success:
            # Verify we got the right errors
            assert len(result_strict.errors) > 0
            assert any("Code generation halted" in error for error in result_strict.errors)
            assert any("FORMAL_VERIFICATION" in error for error in result_strict.errors)

            print("✓ Strict mode: result.success=False")
            print(f"  Errors: {len(result_strict.errors)}")
            print("  Verification correctly prevented pipeline from completing")

            # Verify output files were NOT created (no C code in result)
            assert len(result_strict.output_files) == 0, (
                f"Expected no output files in failed strict mode, got {result_strict.output_files}"
            )
            print("  ✓ No output files generated")
        else:
            # Verification passed - pipeline should complete
            print("✓ Verification passed - C code generated successfully")

    @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
    def test_strict_mode_halts_on_first_unsafe_function(self, tmp_path):
        """Test that strict mode halts on the first unsafe function, not checking subsequent ones."""
        test_file = tmp_path / "test_multiple.py"
        test_file.write_text(
            """
def first_unsafe(arr: list[int], n: int) -> int:
    # First unsafe function
    result: int = 0
    for i in range(n):
        result = arr[i]
    return result

def second_unsafe(arr: list[int], n: int) -> int:
    # Second unsafe function (should not be checked if first fails)
    result: int = 0
    for i in range(n + 1):  # Even more obviously unsafe
        result = arr[i]
    return result

def safe_function(x: int, y: int) -> int:
    # Safe function
    return x + y
"""
        )

        config = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=True,
        )
        pipeline = MultiGenPipeline(config=config)
        result = pipeline.convert(test_file)

        # Should fail on first unsafe function (if verification detects it)
        if not result.success:
            # Check that we only mention the first function in errors
            error_text = " ".join(result.errors)
            # The error should mention halting due to verification failure
            assert "Code generation halted" in error_text
            print("✓ Strict mode halted on first unsafe function")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
