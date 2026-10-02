"""In-place mutation of container arguments must be visible to the caller."""

import ast
import shutil
import subprocess
from pathlib import Path

import pytest

from multigen.backends.converter_utils import find_in_place_mutated_params
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

SOURCE = """\
def set_first(a: list[int], v: int) -> int:
    a[0] = v
    return 0


def via_helper(a: list[int]) -> int:
    set_first(a, 7)
    return 0


def grow(a: list[int]) -> int:
    a.append(9)
    return 0


def rebind(a: list[int]) -> int:
    a = [0, 0, 0]
    a[0] = 5
    return 0


def total(a: list[int]) -> int:
    s: int = 0
    for x in a:
        s += x
    return s


def main() -> int:
    xs: list[int] = [1, 2, 3]
    set_first(xs, 4)
    print(xs[0])
    via_helper(xs)
    print(xs[0])
    grow(xs)
    print(len(xs))
    rebind(xs)
    print(xs[0])
    print(total(xs))
    return 0


if __name__ == "__main__":
    main()
"""


def test_find_in_place_mutated_params() -> None:
    mutated = find_in_place_mutated_params(ast.parse(SOURCE))

    assert mutated["set_first"] == {"a"}
    assert mutated["via_helper"] == {"a"}  # through the call to set_first
    assert mutated["grow"] == {"a"}
    assert mutated["rebind"] == set()  # the caller never sees the new list
    assert mutated["total"] == set()


def test_cpp_takes_mutated_containers_by_reference(tmp_path: Path) -> None:
    src = tmp_path / "mut.py"
    src.write_text(SOURCE)
    result = MultiGenPipeline(config=PipelineConfig(target_language="cpp", output_dir=str(tmp_path / "o"))).convert(src)
    assert result.success, result.errors
    code = result.generated_code or ""

    assert "int set_first(std::vector<int>& a, int v)" in code
    assert "int via_helper(std::vector<int>& a)" in code
    assert "int rebind(std::vector<int> a)" in code
    assert "int total(std::vector<int> a)" in code


# Reassigning a container parameter does not yet compile in C or Rust (TODO.md).
REBIND_UNSUPPORTED = {"c", "rust"}


@pytest.mark.parametrize(
    "target,tool", [("c", "gcc"), ("cpp", "g++"), ("rust", "rustc"), ("go", "go"), ("typescript", "deno")]
)
def test_mutation_matches_python(tmp_path: Path, target: str, tool: str) -> None:
    """Callee mutations, including growth and through a helper, reach the caller; rebinding does not."""
    if shutil.which(tool) is None:
        pytest.skip(f"{tool} not available")
    source = SOURCE
    expected = ["4", "7", "4", "7", "21"]  # CPython
    if target in REBIND_UNSUPPORTED:
        source = SOURCE.replace("    rebind(xs)\n    print(xs[0])\n", "")
        source = source[: source.index("def rebind")] + source[source.index("def total") :]
        expected = ["4", "7", "4", "21"]
    src = tmp_path / "mut.py"
    src.write_text(source)
    result = MultiGenPipeline(
        config=PipelineConfig(target_language=target, build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "o"))
    ).convert(src)
    assert result.success, result.errors

    run = subprocess.run([result.executable_path or ""], capture_output=True, text=True, timeout=30)
    assert run.stdout.split() == expected
