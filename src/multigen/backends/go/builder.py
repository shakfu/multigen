"""Go build system for MultiGen."""

import shutil
from typing import Any

from ..base import AbstractBuilder


class GoBuilder(AbstractBuilder):
    """Go build system implementation."""

    def get_build_filename(self) -> str:
        """Return go.mod as the build file name."""
        return "go.mod"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate go.mod for Go project."""
        # Use a standard module name for all multigen projects
        go_mod_content = """module multigenproject

go 1.21
"""
        return go_mod_content

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
            go_mod_content = self.generate_build_file([str(paths.source_path)], paths.executable_name)
            go_mod_path.write_text(go_mod_content)

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
