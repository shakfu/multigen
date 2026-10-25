"""Haskell pure functions translated statement by statement, checked against CPython.

These bodies match no fold pattern. The where-clause form cannot order rebindings, so
they were refused; the sequential translation runs them in order with `case` bindings.
"""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.errors import UnsupportedFeatureError
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

PROGRAMS = {
    "nested_loops_with_if": (
        "def f(matrix: list[list[int]]) -> int:\n"
        "    total = 0\n"
        "    for row in matrix:\n"
        "        for cell in row:\n"
        "            total += cell\n"
        "            if cell > 5:\n"
        "                total *= 2\n"
        "    return total\n",
        "f([[1, 6], [3, 7]])",
    ),
    "list_item_store": (
        "def f(n: int) -> int:\n    xs: list[int] = [1, 2]\n    xs[0] = n\n    return n + xs[0] + xs[1]\n",
        "f(5)",
    ),
    "dict_item_store": (
        "def f(n: int) -> int:\n    d: dict[int, int] = {}\n    d[1] = n\n    return n + d[1]\n",
        "f(5)",
    ),
    "accumulator_reads_itself": (
        "def f(n: int) -> int:\n    t: int = 1\n    for i in range(n):\n        t += t\n    return t\n",
        "f(5)",
    ),
    "list_store_in_loop": (
        "def f(n: int) -> int:\n"
        "    xs: list[int] = [0, 0]\n"
        "    for i in range(2):\n"
        "        xs[i] = n + i\n"
        "    return xs[0] + xs[1]\n",
        "f(4)",
    ),
    "while_then_for": (
        "def f(n: int) -> int:\n"
        "    x: int = 0\n"
        "    while x < 3:\n"
        "        x += 1\n"
        "    for i in range(n):\n"
        "        x = x + i\n"
        "    return x\n",
        "f(4)",
    ),
    "if_rebinds_before_return": (
        "def f(n: int) -> int:\n    x: int = 1\n    if n > 2:\n        x = 5\n    return x + n\n",
        "f(3) + 10 * f(1)",
    ),
    "dict_word_count": (
        "def f(text: str) -> int:\n"
        "    counts: dict[str, int] = {}\n"
        "    for w in text.split(' '):\n"
        "        if w in counts:\n"
        "            counts[w] = counts[w] + 1\n"
        "        else:\n"
        "            counts[w] = 1\n"
        "    return counts['a']\n",
        "f('a b a c a')",
    ),
}


@pytest.mark.skipif(shutil.which("ghc") is None, reason="ghc not installed")
@pytest.mark.parametrize("name", sorted(PROGRAMS))
def test_sequential_translation_matches_cpython(name: str, tmp_path: Path) -> None:
    body, call = PROGRAMS[name]
    source = tmp_path / f"{name}.py"
    source.write_text(f"{body}\n\ndef main() -> None:\n    print({call})\n\n\nif __name__ == '__main__':\n    main()\n")
    expected = subprocess.run([sys.executable, str(source)], capture_output=True, text=True, check=True).stdout

    config = PipelineConfig(target_language="haskell", build_mode=BuildMode.DIRECT)
    result = MultiGenPipeline(config=config).convert(source, output_path=tmp_path / "out")
    assert result.success, result.errors

    proc = subprocess.run([str(result.executable_path)], capture_output=True, text=True, timeout=30)
    assert (proc.stdout, proc.returncode) == (expected, 0), proc.stderr


def test_loop_with_break_is_still_refused() -> None:
    """break cannot leave a foldl; refusing beats dropping it."""
    source = "def f(n: int) -> int:\n    t: int = 0\n    for i in range(n):\n        if i > 2:\n            break\n        t = t * 2 + i\n    return t\n"
    with pytest.raises(UnsupportedFeatureError):
        MultiGenPythonToHaskellConverter().convert_code(source)
