"""C++ backend: dicts iterate in Python insertion order, not hash order."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

# `del d[k]` is refused by the validator, so delete+reinsert uses `pop`.
SOURCES = {
    "literal_str": """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3, "beta": 4, "omega": 5}
    for k in d:
        print(k)
    return 0
""",
    "literal_int": """\
def main() -> int:
    d: dict[int, int] = {5: 50, 1: 10, 3: 30, 100: 1, 17: 2}
    for k, v in d.items():
        print(k)
        print(v)
    return 0
""",
    "overwrite_keeps_position": """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3}
    d["alpha"] = 20
    d["new"] = 4
    for k, v in d.items():
        print(k)
        print(v)
    return 0
""",
    "pop_reinsert_moves_to_end": """\
def main() -> int:
    d: dict[int, int] = {5: 50, 1: 10, 3: 30}
    print(d.pop(5))
    d[5] = 55
    for k in d.keys():
        print(k)
    for v in d.values():
        print(v)
    return 0
""",
    "comprehensions": """\
def main() -> int:
    d: dict[str, int] = {"zeta": 1, "alpha": 2, "mid": 3, "beta": 4}
    ks: list[str] = [k for k, v in d.items()]
    for k in ks:
        print(k)
    vs: list[int] = [v for v in d.values() if v > 1]
    for v in vs:
        print(v)
    names: list[str] = [k for k in d]
    for k in names:
        print(k)
    doubled: dict[str, int] = {k: v * 2 for k, v in d.items()}
    for k, v in doubled.items():
        print(k)
        print(v)
    return 0
""",
    "insertion_from_loop": """\
def main() -> int:
    d: dict[int, int] = {}
    for i in range(40):
        d[(i * 37) % 101] = i
    for k in d:
        print(k)
    return 0
""",
    "mutated_parameter": """\
def fill(d: dict[str, int]) -> None:
    d["q"] = 7
    d["b"] = 0


def main() -> int:
    d: dict[str, int] = {"x": 1, "b": 2}
    fill(d)
    for k, v in d.items():
        print(k)
        print(v)
    return 0
""",
}


@pytest.mark.parametrize("name", sorted(SOURCES))
def test_dict_iteration_matches_python(tmp_path: Path, name: str) -> None:
    if shutil.which("g++") is None:
        pytest.skip("g++ not available")
    src = tmp_path / f"{name}.py"
    src.write_text(SOURCES[name] + '\n\nif __name__ == "__main__":\n    main()\n')
    result = MultiGenPipeline(
        config=PipelineConfig(target_language="cpp", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
    ).convert(src)
    assert result.success, result.errors
    assert result.executable_path

    expected = subprocess.run([sys.executable, str(src)], capture_output=True, text=True, timeout=10, check=True)
    run = subprocess.run([result.executable_path], capture_output=True, text=True, timeout=10)
    assert run.returncode == 0, run.stderr
    assert run.stdout == expected.stdout
