"""Rust backend: dicts must iterate in Python insertion order."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

pytestmark = pytest.mark.skipif(shutil.which("rustc") is None, reason="rustc not installed")

STR_KEYS = """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3, "beta": 4, "kappa": 5}
    for k in d:
        print(k)
    return 0
"""

INT_KEYS = """\
def main() -> int:
    d: dict[int, int] = {5: 50, 1: 10, 3: 30, 100: 1, 2: 20}
    for k in d:
        print(k)
    return 0
"""

INSERTED = """\
def main() -> int:
    d: dict[int, int] = {}
    d[9] = 1
    d[4] = 1
    d[7] = 1
    d[0] = 1
    for k in d:
        print(k)
    return 0
"""

OVERWRITE = """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3}
    d["zeta"] = 9
    d["alpha"] = 8
    for k, v in d.items():
        print(k)
        print(v)
    return 0
"""

POP_REINSERT = """\
def main() -> int:
    d: dict[int, int] = {5: 50, 1: 10, 3: 30, 2: 20}
    d.pop(1)
    d[1] = 11
    d.pop(3)
    for k, v in d.items():
        print(k)
        print(v)
    print(len(d))
    return 0
"""

ITEMS_COMPREHENSION = """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3, "beta": 4}
    ks: list[str] = [k for k, v in d.items()]
    for k in ks:
        print(k)
    big: list[int] = [v for k, v in d.items() if v > 1]
    for v in big:
        print(v)
    doubled: dict[str, int] = {k: v * 2 for k, v in d.items()}
    for k, v in doubled.items():
        print(k)
        print(v)
    return 0
"""

KEYS_VALUES = """\
def main() -> int:
    d: dict[int, int] = {5: 50, 1: 10, 3: 30, 100: 1}
    for k in d.keys():
        print(k)
    for v in d.values():
        print(v)
    return 0
"""

RANGE_COMPREHENSION = """\
def build(n: int) -> dict[int, int]:
    r: dict[int, int] = {}
    for i in range(n):
        r[n - i] = i
    return r


def main() -> int:
    b: dict[int, int] = build(6)
    for k in b:
        print(k)
    sq: dict[int, int] = {x * 7 % 10: x for x in range(10)}
    for k in sq:
        print(k)
    if build(3) == {1: 2, 2: 1, 3: 0}:
        print(1)
    return 0
"""


@pytest.mark.parametrize(
    "body",
    [STR_KEYS, INT_KEYS, INSERTED, OVERWRITE, POP_REINSERT, ITEMS_COMPREHENSION, KEYS_VALUES, RANGE_COMPREHENSION],
    ids=["str_keys", "int_keys", "inserted", "overwrite", "pop_reinsert", "items_comp", "keys_values", "range_comp"],
)
def test_rust_dict_iterates_in_insertion_order(tmp_path: Path, body: str) -> None:
    src = tmp_path / "dict_order.py"
    src.write_text(body + '\n\nif __name__ == "__main__":\n    main()\n')
    result = MultiGenPipeline(
        config=PipelineConfig(target_language="rust", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors
    assert result.executable_path

    expected = subprocess.run([sys.executable, str(src)], capture_output=True, text=True, check=True).stdout
    actual = subprocess.run([result.executable_path], capture_output=True, text=True, timeout=10, check=True).stdout
    assert actual == expected
