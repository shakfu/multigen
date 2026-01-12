"""Go build system for MultiGen."""

import shutil
import subprocess
from pathlib import Path
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
        try:
            source_path = Path(source_file).absolute()
            out_dir = Path(output_dir).absolute()
            executable_name = source_path.stem

            # Create a temporary Go-specific build directory to avoid conflicts with C files
            go_build_dir = out_dir / f"go_build_{executable_name}"
            go_build_dir.mkdir(exist_ok=True)

            # Copy source file to Go build directory
            # IMPORTANT: If filename ends with _test.go, Go treats it as a test file
            # So we rename it to avoid this issue
            source_name = source_path.name
            if source_name.endswith("_test.go"):
                # Rename to avoid Go treating it as a test file
                source_name = source_name.replace("_test.go", "_main.go")

            go_source = go_build_dir / source_name
            shutil.copy2(source_path, go_source)

            # Create go.mod file in Go build directory
            go_mod_path = go_build_dir / "go.mod"
            go_mod_content = self.generate_build_file([str(source_path)], executable_name)
            go_mod_path.write_text(go_mod_content)

            # Copy runtime package if it exists
            runtime_src = Path(__file__).parent / "runtime" / "multigen_go_runtime.go"
            if runtime_src.exists():
                # Create multigen package directory in Go build directory
                multigen_pkg_dir = go_build_dir / "multigen"
                multigen_pkg_dir.mkdir(exist_ok=True)
                runtime_dst = multigen_pkg_dir / "multigen.go"
                shutil.copy2(runtime_src, runtime_dst)

            # Build go build command
            # Build the module (current directory) which includes our renamed source and runtime
            cmd = ["go", "build", "-o", str(out_dir / executable_name), "."]

            # Run compilation from Go build directory (where go.mod is)
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=str(go_build_dir))

            if result.returncode != 0:
                # Print error for debugging
                if result.stderr:
                    print(f"Go compilation error: {result.stderr}")
                return False

            # Clean up temporary Go build directory after successful build
            shutil.rmtree(go_build_dir, ignore_errors=True)

            return True

        except Exception as e:
            print(f"Go compilation exception: {e}")
            return False

    def get_compile_flags(self) -> list[str]:
        """Get Go compilation flags."""
        return ["-ldflags", "-s -w"]  # Strip debug info for smaller binaries
