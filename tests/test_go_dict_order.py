"""Go backend: dicts iterate in insertion order, like Python, not in Go's randomized map order.

`del d[k]` is refused by the validator, so delete-then-reinsert is not tested.
"""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

# 16 keys, neither sorted nor hash-ordered: a randomized map matches this order with negligible odds.
INT_KEYS = [5, 1, 3, 14, 9, 0, 12, 7, 2, 15, 10, 4, 13, 8, 11, 6]
STR_KEYS = ["q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "a", "s", "d", "f", "g", "h"]
INT_DICT = "{" + ", ".join(f"{k}: {k * 10}" for k in INT_KEYS) + "}"
STR_DICT = "{" + ", ".join(f'"{k}": {i}' for i, k in enumerate(STR_KEYS)) + "}"

BODIES = {
    "str_literal": f"""
    d: dict[str, int] = {STR_DICT}
    for k in d:
        print(k)
""",
    "int_literal": f"""
    d: dict[int, int] = {INT_DICT}
    for k in d:
        print(k)
""",
    "built_by_assignment": f"""
    d: dict[int, int] = {{}}
    xs: list[int] = {INT_KEYS}
    for x in xs:
        d[x] = x * 2
    for k, v in d.items():
        print(k, v)
""",
    "overwrite_keeps_position": f"""
    d: dict[str, int] = {STR_DICT}
    d["r"] = 100
    d["a"] = 200
    d["z"] = 300
    d["q"] = 400
    for k, v in d.items():
        print(k, v)
""",
    "items_comprehensions": f"""
    d: dict[int, int] = {INT_DICT}
    ks: list[int] = [k + v for k, v in d.items()]
    for k in ks:
        print(k)
    big: dict[int, int] = {{k: v for k, v in d.items() if v > 30}}
    for k in big:
        print(k, big[k])
    sq: dict[int, int] = {{k: k * k for k in d}}
    for k, v in sq.items():
        print(k, v)
""",
    "keys_and_values": f"""
    d: dict[int, int] = {INT_DICT}
    for k in d.keys():
        print(k)
    for v in d.values():
        print(v)
    ks: list[int] = [k for k in d.keys()]
    for k in ks:
        print(k)
    vs: list[int] = [v + 1 for v in d.values()]
    for v in vs:
        print(v)
""",
}

RUNS = 3


@pytest.mark.parametrize("name", sorted(BODIES))
def test_dict_iteration_matches_python(tmp_path: Path, name: str) -> None:
    if shutil.which("go") is None:
        pytest.skip("go not available")
    src = tmp_path / f"{name}.py"
    src.write_text(f"def main() -> int:{BODIES[name]}    return 0\n\n\nif __name__ == '__main__':\n    main()\n")
    expected = subprocess.run([sys.executable, str(src)], capture_output=True, text=True, check=True).stdout

    result = MultiGenPipeline(
        config=PipelineConfig(target_language="go", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors

    # Go reseeds map iteration on every range, so repeated runs catch a lucky match.
    for _ in range(RUNS):
        run = subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=30)
        assert run.returncode == 0, run.stderr
        assert run.stdout == expected
