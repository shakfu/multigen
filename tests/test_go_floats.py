"""Go backend: int-to-float promotion and Python float repr beyond `tests/fixtures/differential/floats.py`."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

SOURCE = """
def half(n: int) -> float:
    return n / 2


def widen(n: int) -> float:
    return n


def main() -> None:
    n: int = 3
    y: float = 1.5
    y *= n
    y -= n // 2
    print(y)
    print(half(n))
    print(n <= y, n == 3.0)
    # Python keeps an int in a float-annotated slot; Go promotes it. Adding 0.5 hides the repr difference.
    z: float = n
    print(z + 0.5, widen(n) + 0.5)
    print(100.0, 1e16, 1e15, 1e-4, 1.5e-7, 1e22, 123456789.125)
    print(str(2.0) + "!")
    print(True, n > 2)


if __name__ == "__main__":
    main()
"""


def test_go_float_output_matches_python(tmp_path: Path) -> None:
    if shutil.which("go") is None:
        pytest.skip("go not available")
    src = tmp_path / "go_floats.py"
    src.write_text(SOURCE)
    expected = subprocess.run([sys.executable, str(src)], capture_output=True, text=True, check=True).stdout

    result = MultiGenPipeline(
        config=PipelineConfig(target_language="go", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors

    run = subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=30)
    assert run.returncode == 0, run.stderr
    assert run.stdout == expected
