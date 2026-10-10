"""C and C++ backends: // and % must floor like Python, not truncate toward zero."""

import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

PAIRS = [(7, 2), (-7, 2), (7, -2), (-7, -2), (6, 3), (-6, 3), (0, -5), (1, -1)]

SOURCE = """\
def show(a: int, b: int) -> int:
    q: int = a // b
    r: int = a % b
    print(q)
    print(r)
    q2: int = a
    q2 //= b
    r2: int = a
    r2 %= b
    print(q2)
    print(r2)
    return 0


def divide(a: int, b: int) -> int:
    try:
        return a // b
    except ZeroDivisionError:
        return -999


def main() -> int:
{calls}
    print(divide(1, 0))
    return 0


if __name__ == "__main__":
    main()
"""


def _convert(tmp_path: Path, source: str, build_mode: BuildMode, target: str = "c") -> tuple[str, str]:
    src = tmp_path / "floordiv.py"
    src.write_text(source)
    result = MultiGenPipeline(
        config=PipelineConfig(target_language=target, build_mode=build_mode, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors
    return result.generated_code or "", result.executable_path or ""


def test_int_floor_ops_use_runtime_helpers(tmp_path: Path) -> None:
    code, _ = _convert(tmp_path, SOURCE.format(calls="    show(-7, 2)"), BuildMode.NONE)

    assert "multigen_floordiv_int(a, b)" in code
    assert "multigen_mod_int(a, b)" in code
    assert "q2 = multigen_floordiv_int(q2, b);" in code
    assert "r2 = multigen_mod_int(r2, b);" in code


def test_float_floor_div_keeps_double_division(tmp_path: Path) -> None:
    code, _ = _convert(tmp_path, "def f(x: float, y: float) -> float:\n    return x // y\n", BuildMode.NONE)

    assert "multigen_floordiv_int" not in code


@pytest.mark.parametrize("target,compiler", [("c", "gcc"), ("cpp", "g++")])
def test_int_floor_ops_match_python(tmp_path: Path, target: str, compiler: str) -> None:
    if shutil.which(compiler) is None:
        pytest.skip(f"{compiler} not available")
    calls = "\n".join(f"    show({a}, {b})" for a, b in PAIRS)
    _, exe = _convert(tmp_path, SOURCE.format(calls=calls), BuildMode.DIRECT, target)

    run = subprocess.run([exe], capture_output=True, text=True, timeout=10)
    assert run.returncode == 0, run.stderr

    expected = [str(v) for a, b in PAIRS for v in (a // b, a % b, a // b, a % b)] + ["-999"]
    assert run.stdout.split() == expected


CPP_EXTRA = """\
def main() -> int:
    x: float = -7.5
    print(x // 2.0)
    print(x % 2.0)
    a: list[int] = [-7, 7]
    a[0] //= 2
    a[1] %= -2
    print(a[0])
    print(a[1])
    return 0


if __name__ == "__main__":
    main()
"""


@pytest.mark.skipif(shutil.which("g++") is None, reason="g++ not available")
def test_cpp_float_and_subscript_floor_ops(tmp_path: Path) -> None:
    code, exe = _convert(tmp_path, CPP_EXTRA, BuildMode.DIRECT, "cpp")
    assert "multigen::floordiv" in code

    run = subprocess.run([exe], capture_output=True, text=True, timeout=10)
    assert run.returncode == 0, run.stderr
    # CPython: -7.5 // 2.0 == -4.0, -7.5 % 2.0 == 0.5, -7 // 2 == -4, 7 % -2 == -1
    assert [float(v) for v in run.stdout.split()] == [-4.0, 0.5, -4.0, -1.0]


ALL_BACKENDS_SOURCE = """\
def main() -> int:
    a: int = -7
    print(a // 2)
    print(a % 2)
    print(7 // -2)
    print(7 % -2)
    x: int = -7
    x //= 2
    print(x)
    y: int = -7
    y %= 3
    print(y)
    return 0
"""

EXTRA_TOOLS = {"llvm": "clang", "typescript": "deno"}


@pytest.mark.parametrize("target", ["c", "cpp", "rust", "go", "haskell", "ocaml", "llvm", "typescript"])
def test_floor_ops_match_python_on_every_backend(tmp_path: Path, target: str) -> None:
    """Native / and % truncate toward zero in C, C++, Rust, Go, OCaml and LLVM; Python floors."""
    from test_compilation import toolchain_available

    if not toolchain_available(target) or (target in EXTRA_TOOLS and not shutil.which(EXTRA_TOOLS[target])):
        pytest.skip(f"{target} toolchain not available")

    _, exe = _convert(tmp_path, ALL_BACKENDS_SOURCE, BuildMode.DIRECT, target)
    run = subprocess.run([exe], capture_output=True, text=True, timeout=30)
    assert run.returncode == 0, run.stderr
    # CPython: -7 // 2, -7 % 2, 7 // -2, 7 % -2, -7 // 2, -7 % 3
    assert run.stdout.split() == ["-4", "1", "-4", "-1", "-4", "2"]


@pytest.mark.skipif(shutil.which("rustc") is None, reason="rustc not available")
def test_rust_floor_ops_accept_closure_references(tmp_path: Path) -> None:
    """Filter closures receive &i64; native % auto-derefs, so the helpers must too."""
    source = (
        "def count() -> int:\n"
        "    s: set = {x - 6 for x in range(13) if (x - 6) % 3 == 0}\n"
        "    return len(s)\n"
        "\n\n"
        "def main() -> int:\n"
        "    print(count())\n"
        "    return 0\n"
    )
    _, exe = _convert(tmp_path, source, BuildMode.DIRECT, "rust")
    run = subprocess.run([exe], capture_output=True, text=True, timeout=30)
    assert run.stdout.split() == ["5"]  # -6, -3, 0, 3, 6
