"""C build system for MultiGen with integrated runtime libraries."""

import subprocess
from pathlib import Path
from typing import Any

from ...common.makefilegen import MakefileGenerator
from ..base import AbstractBuilder


class CBuilder(AbstractBuilder):
    """C build system implementation with integrated runtime libraries."""

    def __init__(self) -> None:
        """Initialize builder with runtime support."""
        self.runtime_dir = Path(__file__).parent / "runtime"
        self.use_runtime = self.runtime_dir.exists()

    def get_build_filename(self) -> str:
        """Return Makefile as the build file name."""
        return "Makefile"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate Makefile for C project with MultiGen runtime support using makefilegen."""
        # Prepare include directories
        include_dirs = []
        additional_sources = []

        if self.use_runtime:
            include_dirs.append(str(self.runtime_dir))
            # Add include path for STC headers
            stc_include_dir = Path(__file__).parent / "ext" / "stc" / "include"
            if stc_include_dir.exists():
                include_dirs.append(str(stc_include_dir))

            # Add runtime sources
            additional_sources = self.get_runtime_sources()

        # Use MakefileGenerator for sophisticated Makefile generation
        generator = MakefileGenerator(
            name=target_name,
            source_dir=".",
            build_dir="build",
            flags=["-Wall", "-Wextra", "-O2"],
            include_dirs=include_dirs,
            compiler="gcc",
            std="c11",
            use_stc=True,
            project_type="MultiGen",
            additional_sources=additional_sources,
        )

        return generator.generate_makefile()

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile C source directly using gcc with MultiGen runtime support."""
        try:
            source_path = Path(source_file)
            out_dir = Path(output_dir)
            executable_name = source_path.stem
            output_path = out_dir / executable_name

            # Build gcc command with base flags
            cmd = [
                "gcc",
                "-Wall",
                "-Wextra",
                "-std=c11",
                "-O2",
            ]

            # Add MultiGen runtime support if available
            if self.use_runtime:
                # Add include path for runtime
                cmd.append(f"-I{self.runtime_dir}")
                # Add include path for STC headers
                stc_include_dir = Path(__file__).parent / "ext" / "stc" / "include"
                if stc_include_dir.exists():
                    cmd.append(f"-I{stc_include_dir}")

                # Add runtime sources
                cmd.extend(self.get_runtime_sources())

            # Add main source file and output (use absolute paths to avoid cwd confusion)
            cmd.extend([str(source_path), "-o", str(output_path)])

            # Run compilation (don't set cwd to avoid path resolution issues)
            result = subprocess.run(cmd, capture_output=True, text=True)

            return result.returncode == 0

        except Exception:
            return False

    def get_compile_flags(self) -> list[str]:
        """Get C compilation flags including MultiGen runtime support."""
        flags = ["-Wall", "-Wextra", "-std=c11", "-O2"]

        if self.use_runtime:
            # Add include path for runtime
            flags.append(f"-I{self.runtime_dir}")
            # Add include path for STC headers
            stc_include_dir = Path(__file__).parent / "ext" / "stc" / "include"
            if stc_include_dir.exists():
                flags.append(f"-I{stc_include_dir}")

        return flags

    def get_runtime_sources(self) -> list[str]:
        """Get MultiGen runtime source files for compilation."""
        if not self.use_runtime:
            return []

        runtime_sources = []
        for source_file in self.runtime_dir.glob("*.c"):
            runtime_sources.append(str(source_file))

        return runtime_sources

    def get_runtime_headers(self) -> list[str]:
        """Get MultiGen runtime header files for inclusion."""
        if not self.use_runtime:
            return []

        runtime_headers = []
        for header_file in self.runtime_dir.glob("*.h"):
            runtime_headers.append(header_file.name)

        return runtime_headers
