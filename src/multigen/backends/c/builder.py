"""C build system for MultiGen with integrated runtime libraries."""

from pathlib import Path
from typing import Any, Optional

from ...common.makefilegen import MakefileGenerator
from ..base import AbstractBuilder


class CBuilder(AbstractBuilder):
    """C build system implementation with integrated runtime libraries."""

    def __init__(self) -> None:
        """Initialize builder with runtime support."""
        self._runtime_dir = self._get_runtime_dir()
        self._stc_include_dir = self._get_stc_include_dir()

    def _get_stc_include_dir(self) -> Optional[Path]:
        """Get the STC headers include directory."""
        stc_dir = Path(__file__).parent / "ext" / "stc" / "include"
        return stc_dir if stc_dir.exists() else None

    @property
    def use_runtime(self) -> bool:
        """Check if runtime is available."""
        return self._runtime_dir is not None

    @property
    def runtime_dir(self) -> Optional[Path]:
        """Get the runtime directory (backward compatibility)."""
        return self._runtime_dir

    def get_build_filename(self) -> str:
        """Return Makefile as the build file name."""
        return "Makefile"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate Makefile for C project with MultiGen runtime support using makefilegen."""
        include_dirs: list[str] = []
        additional_sources: list[str] = []

        if self._runtime_dir:
            include_dirs.append(str(self._runtime_dir))
            if self._stc_include_dir:
                include_dirs.append(str(self._stc_include_dir))
            additional_sources = self.get_runtime_sources()

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
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)

        # Build gcc command with base flags
        cmd = ["gcc", "-Wall", "-Wextra", "-std=c11", "-O2"]

        # Add MultiGen runtime support if available
        if self._runtime_dir:
            cmd.append(f"-I{self._runtime_dir}")
            if self._stc_include_dir:
                cmd.append(f"-I{self._stc_include_dir}")
            cmd.extend(self.get_runtime_sources())

        # Add main source file and output
        cmd.extend([str(paths.source_path), "-o", str(paths.executable_path)])

        # Run compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_compile_flags(self) -> list[str]:
        """Get C compilation flags including MultiGen runtime support."""
        flags = ["-Wall", "-Wextra", "-std=c11", "-O2"]

        if self._runtime_dir:
            flags.append(f"-I{self._runtime_dir}")
            if self._stc_include_dir:
                flags.append(f"-I{self._stc_include_dir}")

        return flags

    def get_runtime_sources(self) -> list[str]:
        """Get MultiGen runtime source files for compilation."""
        return [str(f) for f in self._get_runtime_files("*.c")]

    def get_runtime_headers(self) -> list[str]:
        """Get MultiGen runtime header files for inclusion."""
        return [f.name for f in self._get_runtime_files("*.h")]
