"""LLVM backend float semantics beyond the differential `floats.py` fixture."""

import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

pytestmark = pytest.mark.skipif(shutil.which("clang") is None, reason="clang not available")

SOURCE = """\
def half(n: int) -> float:
    return n / 2


def main() -> int:
    z: float = 0.0
    n: int = 9
    q: float = n / 4
    print(q)
    print(half(n))
    print(-z)
    print(n // 2.0)
    print(n == 9.0)
    print(True)
    print(1 > 2.5)
    print(100.0)
    print(1e16)
    print(1.5e16)
    print(123456.789)
    print(0.0001)
    print(1e300 * 1e10)
    return 0
"""


def _run(tmp_path: Path, source: str) -> subprocess.CompletedProcess[str]:
    src = tmp_path / "floats.py"
    src.write_text(source)
    result = MultiGenPipeline(
        config=PipelineConfig(target_language="llvm", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors
    return subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=10)


def test_float_semantics_match_cpython(tmp_path: Path) -> None:
    run = _run(tmp_path, SOURCE)
    assert run.returncode == 0, run.stderr
    expected = ["2.25", "4.5", "-0.0", "4.0", "True", "True", "False"]
    expected += ["100.0", "1e+16", "1.5e+16", "123456.789", "0.0001", "inf"]
    assert run.stdout.split() == expected


def test_int_true_division_assign_is_rejected(tmp_path: Path) -> None:
    src = tmp_path / "div.py"
    src.write_text("def main() -> int:\n    n: int = 7\n    n /= 2\n    return n\n")
    result = MultiGenPipeline(config=PipelineConfig(target_language="llvm", build_mode=BuildMode.NONE)).convert(src)
    assert not result.success
    assert any("/=" in str(e) for e in result.errors)
