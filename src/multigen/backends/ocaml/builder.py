"""OCaml builder for compiling generated OCaml code."""

import subprocess
from pathlib import Path
from typing import Any, Optional

from ..base import AbstractBuilder
from ..preferences import BackendPreferences, OCamlPreferences


class OCamlBuilder(AbstractBuilder):
    """Builder for OCaml code compilation and execution."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the OCaml builder with preferences."""
        self.preferences = preferences or OCamlPreferences()

    def build(self, output_file: str, makefile: bool = False) -> bool:
        """Build the OCaml code.

        Args:
            output_file: The OCaml source file to compile
            makefile: Whether to generate a dune-project file instead of direct compilation

        Returns:
            True if build succeeded, False otherwise
        """
        try:
            if makefile:
                return self._generate_dune_project(output_file)
            else:
                return self._compile_direct(output_file)
        except Exception:
            return False

    def _compile_direct(self, output_file: str) -> bool:
        """Compile OCaml code directly using ocamlc via opam."""
        base_path = Path(output_file).parent
        runtime_path = base_path / "multigen_runtime.ml"

        # Copy runtime file if it doesn't exist
        if not runtime_path.exists():
            self._copy_runtime_files(base_path)

        # Compile with OCaml compiler via opam
        executable = output_file.replace(".ml", "")
        cmd = ["opam", "exec", "--", "ocamlc", "-o", executable, str(runtime_path), output_file]

        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode == 0:
            return True
        else:
            return False

    def _generate_dune_project(self, output_file: str) -> bool:
        """Generate a dune-project file for the OCaml project."""
        base_path = Path(output_file).parent
        project_name = Path(output_file).stem

        # Copy runtime files
        self._copy_runtime_files(base_path)

        # Generate dune-project
        dune_project_content = f"""(lang dune 3.0)

(package
 (name {project_name})
 (depends ocaml dune))
"""

        dune_project_path = base_path / "dune-project"
        with open(dune_project_path, "w") as f:
            f.write(dune_project_content)

        # Generate dune file
        dune_content = f"""(executable
 (public_name {project_name})
 (name {project_name})
 (modules multigen_runtime {project_name}))
"""

        dune_file_path = base_path / "dune"
        with open(dune_file_path, "w") as f:
            f.write(dune_content)

        return True

    def _copy_runtime_files(self, target_dir: Path) -> None:
        """Copy OCaml runtime files to the target directory."""
        import shutil

        # Get the path to the runtime directory
        current_dir = Path(__file__).parent
        runtime_dir = current_dir / "runtime"

        if runtime_dir.exists():
            # Copy all .ml files from runtime directory
            for runtime_file in runtime_dir.glob("*.ml"):
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
        dune_project_content = f"""(lang dune 3.0)

(package
 (name {target_name})
 (depends ocaml dune))
"""

        dune_content = f"""(executable
 (public_name {target_name})
 (name {target_name})
 (modules multigen_runtime {" ".join(Path(f).stem for f in source_files)}))
"""

        return dune_project_content + "\n" + dune_content

    def get_build_filename(self) -> str:
        """Get build file name for OCaml."""
        return "dune-project"

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile OCaml source directly using ocamlc via opam."""
        source_path = Path(source_file).absolute()
        out_dir = Path(output_dir).absolute()
        executable_name = source_path.stem

        # Determine source directory (where .ml files are)
        source_dir = source_path.parent

        # Copy runtime file to source directory (OCaml looks for modules there)
        runtime_path = source_dir / "multigen_runtime.ml"
        if not runtime_path.exists():
            self._copy_runtime_files(source_dir)

        # Compile with OCaml compiler via opam
        # Use absolute paths for all files and include source directory for module resolution
        executable = out_dir / executable_name
        cmd = [
            "opam", "exec", "--", "ocamlc",
            "-I", str(source_dir),  # Add include path for module resolution
            "-o", str(executable),
            str(runtime_path),
            str(source_path)
        ]

        # Run compilation (don't set cwd to avoid path issues)
        result = subprocess.run(cmd, capture_output=True, text=True)

        if result.returncode != 0:
            # Print error for debugging
            if result.stderr:
                print(f"OCaml compilation error: {result.stderr}")
            return False

        return True

    def get_compile_flags(self) -> list[str]:
        """Get compilation flags for OCaml."""
        return ["-o"]
