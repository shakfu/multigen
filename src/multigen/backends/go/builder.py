"""Go build system for MultiGen."""

import json
import shutil
from pathlib import Path
from typing import Any

from ..base import AbstractBuilder

# Generated code imports the runtime as "multigenproject/multigen".
_GO_MOD = """module multigenproject

go 1.21
"""


class GoBuilder(AbstractBuilder):
    """Go build system implementation."""

    def get_build_filename(self) -> str:
        """Return go.mod as the build file name."""
        return "go.mod"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate go.mod for Go project."""
        # The runtime is a nested module, replaced by absolute path, so go.mod still
        # resolves it after the CLI moves go.mod out of the source directory.
        runtime_dir = json.dumps(str(Path(source_files[0]).resolve().parent / "multigen"))
        return f"""{_GO_MOD}
require multigenproject/multigen v0.0.0

replace multigenproject/multigen => {runtime_dir}
"""

    def stage_build_tree(self, source_file: str) -> None:
        """Copy the runtime package into a nested module beside the source."""
        runtime_dir = self._get_runtime_dir()
        if runtime_dir is None:
            return
        package_dir = Path(source_file).parent / "multigen"
        package_dir.mkdir(exist_ok=True)
        shutil.copy2(runtime_dir / "multigen_go_runtime.go", package_dir / "multigen.go")
        (package_dir / "go.mod").write_text("module multigenproject/multigen\n\ngo 1.21\n")

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile Go source directly using go build."""
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)

        # Create a temporary Go-specific build directory to avoid conflicts with C files
        go_build_dir = paths.output_dir / f"go_build_{paths.executable_name}"
        go_build_dir.mkdir(exist_ok=True)

        try:
            # Copy source file to Go build directory
            # IMPORTANT: If filename ends with _test.go, Go treats it as a test file
            source_name = paths.source_path.name
            if source_name.endswith("_test.go"):
                source_name = source_name.replace("_test.go", "_main.go")

            go_source = go_build_dir / source_name
            shutil.copy2(paths.source_path, go_source)

            # Create go.mod file in Go build directory
            go_mod_path = go_build_dir / "go.mod"
            go_mod_path.write_text(_GO_MOD)

            # Copy runtime package if it exists
            runtime_dir = self._get_runtime_dir()
            if runtime_dir:
                runtime_src = runtime_dir / "multigen_go_runtime.go"
                if runtime_src.exists():
                    multigen_pkg_dir = go_build_dir / "multigen"
                    multigen_pkg_dir.mkdir(exist_ok=True)
                    shutil.copy2(runtime_src, multigen_pkg_dir / "multigen.go")

            # Build go build command
            cmd = ["go", "build", "-o", str(paths.executable_path), "."]

            # Run compilation from Go build directory (where go.mod is)
            result = self._run_command(cmd, cwd=str(go_build_dir))

            if not result.success:
                return False

            return True

        finally:
            # Clean up temporary Go build directory
            shutil.rmtree(go_build_dir, ignore_errors=True)

    def get_compile_flags(self) -> list[str]:
        """Get Go compilation flags."""
        return ["-ldflags", "-s -w"]  # Strip debug info for smaller binaries
