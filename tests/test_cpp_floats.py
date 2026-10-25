"""C++ backend: float repr edge cases and true-division errors match CPython."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

REPR_SOURCE = """\
def main() -> None:
    print(100.0)
    print(1e15)
    print(1e16)
    print(1.5e-7)
    print(0.0001)
    print(123456789.125)
    print(-0.0)
    print(0.1 + 0.2)
    print(2.0 ** 0.5)
    print(6 / 3)
    print(True, False, 1.0, 1)
"""

ZERO_DIV_SOURCE = """\
def main() -> None:
    n: int = 0
    print(1 / n)
"""


def _build(tmp_path: Path, name: str, source: str) -> tuple[Path, str]:
    if shutil.which("g++") is None:
        pytest.skip("g++ not available")
    src = tmp_path / f"{name}.py"
    src.write_text(source + '\n\nif __name__ == "__main__":\n    main()\n')
    result = MultiGenPipeline(
        config=PipelineConfig(target_language="cpp", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors
    assert result.executable_path
    return src, result.executable_path


def test_float_repr_matches_python(tmp_path: Path) -> None:
    src, exe = _build(tmp_path, "float_repr", REPR_SOURCE)
    expected = subprocess.run([sys.executable, str(src)], capture_output=True, text=True, timeout=10, check=True)
    run = subprocess.run([exe], capture_output=True, text=True, timeout=10)
    assert run.returncode == 0, run.stderr
    assert run.stdout == expected.stdout


def test_true_division_by_zero_raises(tmp_path: Path) -> None:
    _, exe = _build(tmp_path, "zero_div", ZERO_DIV_SOURCE)
    run = subprocess.run([exe], capture_output=True, text=True, timeout=10)
    assert run.returncode == 1
    assert run.stderr.strip().endswith("ZeroDivisionError: division by zero")
