"""OCaml builder for compiling generated OCaml code."""

import shutil
from pathlib import Path
from typing import Any, Optional

from ..base import AbstractBuilder
from ..preferences import BackendPreferences, OCamlPreferences


def _has_opam_initialized() -> bool:
    """Check if opam is available and initialized."""
    import subprocess

    if not shutil.which("opam"):
        return False
    result = subprocess.run(["opam", "var", "prefix"], capture_output=True, text=True)
    return result.returncode == 0


class OCamlBuilder(AbstractBuilder):
    """Builder for OCaml code compilation and execution."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the OCaml builder with preferences."""
        self.preferences = preferences or OCamlPreferences()

    def _copy_runtime_files(self, target_dir: Path) -> None:
        """Copy OCaml runtime files to the target directory."""
        # Copy all .ml files from runtime directory using base class helper
        for runtime_file in self._get_runtime_files("*.ml"):
            target_file = target_dir / runtime_file.name
            if not target_file.exists():
                shutil.copy2(runtime_file, target_file)

    def get_build_command(self, output_file: str) -> list[str]:
        """Get the command to build the OCaml file."""
        base_name = Path(output_file).stem
        return ["opam", "exec", "--", "ocamlc", "-o", base_name, "multigen_runtime.ml", output_file]

    def get_run_command(self, output_file: str) -> list[str]:
        """Get the command to run the compiled OCaml executable."""
        executable = Path(output_file).stem
        return [f"./{executable}"]

    def clean(self, output_file: str) -> bool:
        """Clean build artifacts."""
        base_path = Path(output_file).parent
        base_name = Path(output_file).stem

        # Remove common OCaml build artifacts
        artifacts = [
            base_name,  # executable
            f"{base_name}.cmi",  # compiled interface
            f"{base_name}.cmo",  # compiled object
            "multigen_runtime.cmi",
            "multigen_runtime.cmo",
            "_build",  # dune build directory
            "dune-project",
            "dune",
        ]

        removed_count = 0
        for artifact in artifacts:
            artifact_path = base_path / artifact
            try:
                if artifact_path.is_file():
                    artifact_path.unlink()
                    removed_count += 1
                elif artifact_path.is_dir():
                    import shutil

                    shutil.rmtree(artifact_path)
                    removed_count += 1
            except Exception:
                continue

        return True

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate dune-project build configuration."""
        return "(lang dune 3.0)\n"

    def stage_build_tree(self, source_file: str) -> None:
        """Write the dune stanza and runtime beside the source; dune finds them from dune-project."""
        source_dir = Path(source_file).parent
        self._copy_runtime_files(source_dir)
        name = Path(source_file).stem
        (source_dir / "dune").write_text(f"(executable\n (name {name})\n (modules multigen_runtime {name}))\n")

    def get_build_filename(self) -> str:
        """Get build file name for OCaml."""
        return "dune-project"

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile OCaml source directly using ocamlc."""
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)
        source_dir = paths.source_path.parent

        # Copy runtime file to source directory (OCaml looks for modules there)
        runtime_path = source_dir / "multigen_runtime.ml"
        if not runtime_path.exists():
            self._copy_runtime_files(source_dir)

        # Build command (use opam if initialized, otherwise direct ocamlc)
        if _has_opam_initialized():
            cmd = [
                "opam",
                "exec",
                "--",
                "ocamlc",
                "-I",
                str(source_dir),
                "-o",
                str(paths.executable_path),
                str(runtime_path),
                str(paths.source_path),
            ]
        else:
            cmd = [
                "ocamlc",
                "-I",
                str(source_dir),
                "-o",
                str(paths.executable_path),
                str(runtime_path),
                str(paths.source_path),
            ]

        # Run compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_compile_flags(self) -> list[str]:
        """Get compilation flags for OCaml."""
        return ["-o"]
