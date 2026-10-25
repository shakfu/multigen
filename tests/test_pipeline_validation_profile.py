"""Tests for profile-driven validation in pipeline phase 1."""

import pytest

from multigen.pipeline import MultiGenPipeline, PipelineConfig, PipelinePhase

# Union is the remaining experimental construct, and the parameter's type
# cannot be pinned down, so this exercises both policy switches. Do not
# substitute a construct whose status may change: that is what broke these
# tests when generics were reclassified. The target is C++, which emits Union;
# the C backend refuses it at generation.
SOURCE = "from typing import Union\n\n\ndef pick(x: Union[int, float]) -> int:\n    return 1\n"


@pytest.fixture
def program(tmp_path):
    path = tmp_path / "p.py"
    path.write_text(SOURCE)
    return path


def _convert(tmp_path, program, profile):
    config = PipelineConfig(
        target_language="cpp",
        validation_profile=profile,
        output_dir=str(tmp_path / profile),
    )
    result = MultiGenPipeline(config=config).convert(program)
    return result, result.phase_results[PipelinePhase.VALIDATION]


class TestValidationProfileConfig:
    """The profile is configuration, and an unknown one is caught early."""

    def test_default_is_portable(self):
        assert PipelineConfig().validation_profile == "portable"

    def test_unknown_profile_fails_at_construction(self):
        """A typo must not surface halfway through a conversion."""
        with pytest.raises(ValueError, match="Unknown validation profile"):
            MultiGenPipeline(config=PipelineConfig(validation_profile="nonexistent"))


class TestProfileDrivenPhaseOne:
    """The profile decides whether generation happens at all."""

    def test_portable_generates(self, tmp_path, program):
        result, phase = _convert(tmp_path, program, "portable")

        assert result.success
        assert phase.is_valid
        assert "cpp_source" in result.output_files

    def test_strict_static_blocks_generation(self, tmp_path, program):
        """Blocking diagnostics stop the run before anything is generated."""
        result, phase = _convert(tmp_path, program, "strict-static")

        assert not result.success
        assert not phase.is_valid
        assert "cpp_source" not in result.output_files
        assert result.errors

    def test_phase_result_records_the_profile(self, tmp_path, program):
        for profile in ("portable", "strict-static"):
            _, phase = _convert(tmp_path, program, profile)
            assert phase.profile == profile

    def test_phase_result_carries_structured_diagnostics(self, tmp_path, program):
        """Phase 1 exposes rule ids and positions, not just strings."""
        _, phase = _convert(tmp_path, program, "portable")

        assert phase.diagnostics
        diagnostic = phase.diagnostics[0]
        assert diagnostic.rule_id.startswith(("STATIC.", "CONSTRAINT."))
        assert diagnostic.span is not None
        assert phase.warnings == [d.message for d in phase.diagnostics if not d.is_error]

    def test_invalid_source_reports_before_generation(self, tmp_path):
        """An unrecognised construct is caught in phase 1, not by a backend."""
        source = tmp_path / "async.py"
        source.write_text("async def f(x: int) -> int:\n    return x\n")

        config = PipelineConfig(target_language="c", output_dir=str(tmp_path / "out"))
        result = MultiGenPipeline(config=config).convert(source)
        phase = result.phase_results[PipelinePhase.VALIDATION]

        assert not result.success
        assert not phase.is_valid
        assert "cpp_source" not in result.output_files
        assert any("AsyncFunctionDef" in v for v in phase.violations)
