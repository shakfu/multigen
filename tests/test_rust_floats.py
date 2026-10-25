"""Rust float promotion and Python-style float/bool formatting, checked against CPython."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

PROGRAM = """
import math


def half(v: float) -> float:
    return v / 2


def is_big(v: float) -> bool:
    return v > 100


def main() -> None:
    n: int = 3
    print(half(n))
    print(is_big(1e3))
    print(str(2.0) + "!")
    z: float = 0.5
    print(f"z={z}")
    ok: bool = n == 3
    print(ok)
    print(not ok)
    print(1e16)
    print(1.5e16)
    print(1e22)
    print(0.0001)
    print(123456789.125)
    print(-0.0)
    print(5e-324)
    print(1.7976931348623157e308)
    print(2.0 ** 10)
    print(z ** n)
    print(n / 2 > 1)
    w: float = n
    w /= 4
    print(w)
    print(math.sqrt(16) + n)
    vals: list[float] = [1.5, 2.5]
    print(vals[0] + n)
    # A map read takes the map's value type; it was read as i64 for every map
    prices: dict[str, float] = {"a": 1.5}
    p = prices["a"]
    print(p + n)


if __name__ == "__main__":
    main()
"""


@pytest.mark.skipif(shutil.which("rustc") is None, reason="rustc not installed")
def test_float_promotion_and_formatting_match_cpython(tmp_path: Path) -> None:
    source = tmp_path / "rust_floats.py"
    source.write_text(PROGRAM)
    expected = subprocess.run([sys.executable, str(source)], capture_output=True, text=True, check=True).stdout

    out_dir = tmp_path / "out"
    config = PipelineConfig(target_language="rust", build_mode=BuildMode.DIRECT)
    result = MultiGenPipeline(config=config).convert(source, output_path=out_dir)
    assert result.success, result.errors

    proc = subprocess.run([str(result.executable_path)], capture_output=True, text=True, timeout=30)
    assert (proc.stdout, proc.returncode) == (expected, 0), proc.stderr
