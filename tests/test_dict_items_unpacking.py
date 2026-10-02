"""`for k, v in d.items()`: accepted by the validator and translated by every backend."""

import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.frontend.static_validation import StaticValidator
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

SOURCE = """\
def loop_sum(d: dict[int, int]) -> int:
    total: int = 0
    for k, v in d.items():
        total += k * v
    return total


def filtered(d: dict[int, int]) -> int:
    f: dict[int, int] = {k: v for k, v in d.items() if v > 20}
    return len(f)


def pair_sum(d: dict[int, int]) -> int:
    xs: list[int] = [k + v for k, v in d.items()]
    s: int = 0
    for x in xs:
        s += x
    return s


def main() -> int:
    d: dict[int, int] = {x: x * 2 for x in range(50)}
    print(loop_sum(d))
    print(filtered(d))
    print(pair_sum(d))
    return 0
"""

EXTRA_TOOLS = {"llvm": "clang", "typescript": "deno"}


@pytest.mark.parametrize(
    "code",
    [
        "def f(d: dict[int, int]) -> int:\n    for k, v in d.items():\n        pass\n    return 0\n",
        "def f(d: dict[int, int]) -> int:\n    return len({k: v for k, v in d.items()})\n",
        "def f(d: dict[int, int]) -> int:\n    return len([k for k, v in d.items()])\n",
    ],
)
def test_items_unpacking_is_accepted(code: str) -> None:
    assert StaticValidator().validate_code(code).is_valid


@pytest.mark.parametrize(
    "code",
    [
        "def f() -> int:\n    p = (1, 2)\n    return 0\n",
        "def f(xs: list[int]) -> int:\n    for a, b in xs:\n        pass\n    return 0\n",
        "def f(d: dict[int, int]) -> int:\n    for k, v, w in d.items():\n        pass\n    return 0\n",
        "def f(d: dict[int, int]) -> int:\n    for (k, v) in d.items(1):\n        pass\n    return 0\n",
    ],
    ids=["tuple-value", "unpack-list", "three-names", "items-with-args"],
)
def test_other_tuples_stay_rejected(code: str) -> None:
    report = StaticValidator().validate_code(code)

    assert not report.is_valid
    assert any("Tuples" in d.message for d in report.errors())


@pytest.mark.parametrize("target", ["c", "cpp", "rust", "go", "haskell", "ocaml", "llvm", "typescript"])
def test_items_unpacking_matches_python(tmp_path: Path, target: str) -> None:
    from test_compilation import toolchain_available

    if not toolchain_available(target) or (target in EXTRA_TOOLS and not shutil.which(EXTRA_TOOLS[target])):
        pytest.skip(f"{target} toolchain not available")

    src = tmp_path / "items.py"
    src.write_text(SOURCE)
    result = MultiGenPipeline(
        config=PipelineConfig(target_language=target, build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors

    run = subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=60)
    assert run.returncode == 0, run.stderr
    # CPython: sum(2k*k), count of 2k > 20, sum(3k) for k in range(50)
    assert run.stdout.split() == ["80850", "39", "3675"]
