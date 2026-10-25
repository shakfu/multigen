"""Haskell float semantics beyond tests/fixtures/differential/floats.py."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

SOURCE = """
def avg(n: int) -> float:
    total: float = 0.0
    for i in range(n):
        total += i
    return total / n


def half(a: int, b: int) -> float:
    return a / b


def main() -> None:
    n: int = 3
    z: float = 1.25
    print(avg(4))
    print(half(7, 2))
    print(z >= n)
    print(z * 2 > n + 1)
    print(f"z={z} n={n}")
    print(1e16)
    print(1e15)
    print(123456.789)
    print(0.0001)
    print(-0.0)
    print(1.5e-7)
    print(2.5e100)


if __name__ == "__main__":
    main()
"""


def test_int_operands_promote_with_from_integral() -> None:
    code = MultiGenPythonToHaskellConverter().convert_code(SOURCE)
    assert "acc + ((fromIntegral i))" in code
    assert "half a b = ((fromIntegral a) / (fromIntegral b))" in code
    assert "(z >= (fromIntegral n))" in code
    assert "(toString z)" in code


@pytest.mark.skipif(shutil.which("ghc") is None, reason="ghc not installed")
def test_output_matches_cpython(tmp_path: Path) -> None:
    source = tmp_path / "haskell_floats.py"
    source.write_text(SOURCE)
    config = PipelineConfig(target_language="haskell", build_mode=BuildMode.DIRECT)
    result = MultiGenPipeline(target_language="haskell", config=config).convert(source, output_path=tmp_path / "out")
    assert result.success, result.errors
    assert result.executable_path
    actual = subprocess.run([result.executable_path], capture_output=True, text=True, timeout=30, check=True)
    expected = subprocess.run([sys.executable, str(source)], capture_output=True, text=True, check=True)
    assert actual.stdout == expected.stdout
