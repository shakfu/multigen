"""Generated build files: layout, naming, injection safety, and build options."""

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from multigen.backends.base import CompilationResult
from multigen.backends.c.builder import CBuilder
from multigen.common.makefilegen import MakefileGenerator, check_make_safe
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

HELLO = 'def main() -> int:\n    print(42)\n    return 0\n\n\nif __name__ == "__main__":\n    main()\n'


def _cli_build_makefile(tmp_path: Path, target: str, filename: str = "hello.py") -> subprocess.CompletedProcess[str]:
    src = tmp_path / filename
    src.write_text(HELLO)
    return subprocess.run(
        [
            sys.executable,
            "-m",
            "multigen.cli.main",
            "--build-dir",
            str(tmp_path / "build"),
            "build",
            "-t",
            target,
            "-m",
            str(src),
        ],
        capture_output=True,
        text=True,
        cwd=tmp_path,
    )


@pytest.mark.parametrize("target,compiler", [("c", "gcc"), ("cpp", "g++")])
def test_cli_makefile_builds_and_runs(tmp_path: Path, target: str, compiler: str) -> None:
    """The CLI moves the Makefile above the generated sources; make must still find them."""
    if not (shutil.which(compiler) and shutil.which("make")):
        pytest.skip(f"{compiler} or make not available")

    cli = _cli_build_makefile(tmp_path, target)
    assert cli.returncode == 0, cli.stderr
    build = tmp_path / "build"
    assert (build / "Makefile").exists()

    make = subprocess.run(["make"], cwd=build, capture_output=True, text=True)
    assert make.returncode == 0, make.stderr

    run = subprocess.run([str(build / "hello")], capture_output=True, text=True, timeout=10)
    assert run.stdout.strip() == "42"


@pytest.mark.parametrize(
    "target,build_file",
    [("typescript", "deno.json"), ("ocaml", "dune-project"), ("haskell", "multigen-project.cabal")],
)
def test_cli_keeps_backend_build_file_name(tmp_path: Path, target: str, build_file: str) -> None:
    cli = _cli_build_makefile(tmp_path, target)
    assert cli.returncode == 0, cli.stderr

    assert (tmp_path / "build" / build_file).exists()
    assert not (tmp_path / "build" / "Makefile").exists()


@pytest.mark.parametrize("target", ["c", "cpp", "llvm"])
def test_makefile_rejects_injected_target_name(tmp_path: Path, target: str) -> None:
    """A source filename becomes the Make target; Make syntax in it must not reach the Makefile."""
    src = tmp_path / "x$(shell touch pwned).py"
    src.write_text("def main() -> int:\n    print(1)\n    return 0\n")

    result = MultiGenPipeline(
        config=PipelineConfig(target_language=target, build_mode=BuildMode.MAKEFILE, output_dir=str(tmp_path / "out"))
    ).convert(src)

    assert not result.success
    assert any("unsafe in a Makefile" in error for error in result.errors)
    assert not list((tmp_path / "out").glob("Makefile"))


@pytest.mark.parametrize("value", ["$(shell id)", "a b", "a;b", "a#b", "a:b", "a\nb", "`id`", ""])
def test_check_make_safe_rejects(value: str) -> None:
    with pytest.raises(ValueError):
        check_make_safe(value, "value")


@pytest.mark.parametrize("value", ["hello_world", "/usr/local/include", "-D_POSIX_C_SOURCE=200809L", "-O2", "c++17"])
def test_check_make_safe_accepts(value: str) -> None:
    assert check_make_safe(value, "value") == value


def test_makefile_generator_rejects_unsafe_include_dir() -> None:
    with pytest.raises(ValueError, match="include directory"):
        MakefileGenerator(name="ok", include_dirs=["/opt/$(shell id)"], use_stc=False).generate_makefile()


def test_stc_does_not_override_requested_standard() -> None:
    makefile = CBuilder().generate_build_file(["main.c"], "main")

    assert "STD = -std=c11" in makefile
    assert "-std=c99" not in makefile


def test_c_build_options_reach_compiler_and_makefile(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    src = tmp_path / "prog.py"
    src.write_text("def main() -> int:\n    print(1)\n    return 0\n")
    options = {"compiler": "clang", "compiler_flags": ["-DFOO=1"], "libraries": ["m"]}

    commands: list[list[str]] = []

    def fake_run(self: CBuilder, cmd: list[str], **_: object) -> CompilationResult:
        commands.append(cmd)
        return CompilationResult(success=True, command=cmd)

    monkeypatch.setattr(CBuilder, "_run_command", fake_run)
    direct = MultiGenPipeline(
        config=PipelineConfig(
            target_language="c", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "d"), **options
        )
    ).convert(src)
    assert direct.success, direct.errors
    (cmd,) = commands
    assert cmd[0] == "clang"
    assert "-DFOO=1" in cmd
    assert "-lm" in cmd

    make = MultiGenPipeline(
        config=PipelineConfig(
            target_language="c", build_mode=BuildMode.MAKEFILE, output_dir=str(tmp_path / "m"), **options
        )
    ).convert(src)
    assert make.success, make.errors
    makefile = make.build_file_content or ""
    assert "CC = clang" in makefile
    assert "-DFOO=1" in makefile
    assert "LIBS = -lm" in makefile


def test_unsupported_build_option_is_refused(tmp_path: Path) -> None:
    """Rust ignores --compiler; the pipeline must say so instead of dropping it."""
    src = tmp_path / "prog.py"
    src.write_text("def main() -> int:\n    print(1)\n    return 0\n")

    result = MultiGenPipeline(
        config=PipelineConfig(
            target_language="rust", build_mode=BuildMode.MAKEFILE, output_dir=str(tmp_path / "o"), compiler="clang"
        )
    ).convert(src)

    assert not result.success
    assert any("does not support: compiler" in error for error in result.errors)
