"""Comprehension filters and boolean operators: translated correctly or refused, never dropped."""

import ast
import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.backends.converter_utils import normalize_ast
from multigen.frontend.static_validation import StaticValidator
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

ALL_BACKENDS = ["c", "cpp", "rust", "go", "haskell", "ocaml", "llvm", "typescript"]
EXTRA_TOOLS = {"llvm": "clang", "typescript": "deno"}

TWO_IFS = """\
def two_ifs() -> int:
    ys: list[int] = [x for x in range(20) if x % 2 == 0 if x % 3 == 0]
    return len(ys)


def main() -> int:
    print(two_ifs())
    return 0
"""

AND_OR = """\
def main() -> int:
    n: int = 0
    for i in range(10):
        if i > 1 and i < 5:
            n += 1
        if i < 1 or i > 8:
            n += 10
    print(n)
    return 0
"""


def _build_and_run(tmp_path: Path, source: str, target: str) -> tuple[bool, str]:
    from test_compilation import toolchain_available

    if not toolchain_available(target) or (target in EXTRA_TOOLS and not shutil.which(EXTRA_TOOLS[target])):
        pytest.skip(f"{target} toolchain not available")
    src = tmp_path / "prog.py"
    src.write_text(source)
    result = MultiGenPipeline(
        config=PipelineConfig(target_language=target, build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    if not result.success:
        return False, ""
    run = subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=60)
    return True, run.stdout.strip()


def test_normalize_merges_comprehension_ifs() -> None:
    tree = normalize_ast(ast.parse("[x for x in xs if a if b if c]"))
    gen = tree.body[0].value.generators[0]  # type: ignore[attr-defined]

    assert len(gen.ifs) == 1
    assert ast.unparse(gen.ifs[0]) == "a and b and c"


def test_second_for_clause_is_rejected() -> None:
    code = "def f() -> int:\n    return len([a + b for a in range(3) for b in range(2)])\n"

    assert not StaticValidator().validate_code(code).is_valid


@pytest.mark.parametrize("target", ALL_BACKENDS)
def test_two_if_clauses_never_drop_a_filter(tmp_path: Path, target: str) -> None:
    """C++ and Go used to keep only the first `if` and print 10."""
    built, out = _build_and_run(tmp_path, TWO_IFS, target)
    if target in ("c", "cpp", "go", "haskell", "typescript"):
        assert built
    if built:
        assert out == "4"  # CPython: 0, 6, 12, 18


@pytest.mark.parametrize("target", ["go", "ocaml"])
def test_bool_operators(tmp_path: Path, target: str) -> None:
    """Go and OCaml refused `and`/`or` everywhere."""
    built, out = _build_and_run(tmp_path, AND_OR, target)
    assert built
    assert out == "23"  # CPython: 3 for 2..4, plus 10 each for 0 and 9
