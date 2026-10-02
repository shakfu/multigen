"""C build system for MultiGen with integrated runtime libraries."""

from pathlib import Path
from typing import Any, Optional

from ...common.makefilegen import MakefileGenerator
from ..base import AbstractBuilder

# The runtime uses strdup and other POSIX functions across a dozen headers.
# Bare -std=c11 is strict ISO C, where those are not declared, so any generated
# program touching a string-keyed map failed to build on an implicit
# declaration. Requesting POSIX keeps ISO C11 semantics and declares them.
_POSIX_FEATURE_TEST = "-D_POSIX_C_SOURCE=200809L"
_BASE_FLAGS = ["-Wall", "-Wextra", _POSIX_FEATURE_TEST, "-O2"]


class CBuilder(AbstractBuilder):
    """C build system implementation with integrated runtime libraries."""

    def __init__(self) -> None:
        """Initialize builder with runtime support."""
        self._runtime_dir = self._get_runtime_dir()
        self._stc_include_dir = self._get_stc_include_dir()
        self._compiler = "gcc"
        self._extra_flags: list[str] = []
        self._extra_include_dirs: list[str] = []
        self._libraries: list[str] = []

    def apply_build_options(
        self,
        compiler: Optional[str] = None,
        compiler_flags: Optional[list[str]] = None,
        include_dirs: Optional[list[str]] = None,
        libraries: Optional[list[str]] = None,
    ) -> set[str]:
        """Adopt compiler, flags, include dirs and libraries for both build modes."""
        applied: set[str] = set()
        if compiler:
            self._compiler = compiler
            applied.add("compiler")
        if compiler_flags:
            self._extra_flags = list(compiler_flags)
            applied.add("compiler_flags")
        if include_dirs:
            self._extra_include_dirs = list(include_dirs)
            applied.add("include_dirs")
        if libraries:
            self._libraries = list(libraries)
            applied.add("libraries")
        return applied

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
        """Generate Makefile for C project with MultiGen runtime support using makefilegen.

        Raises:
            ValueError: If a name or path contains characters unsafe in a Makefile
        """
        include_dirs: list[str] = []
        additional_sources: list[str] = []

        if self._runtime_dir:
            include_dirs.append(str(self._runtime_dir))
            if self._stc_include_dir:
                include_dirs.append(str(self._stc_include_dir))
            additional_sources = self.get_runtime_sources()

        # Absolute paths: the CLI moves the Makefile out of the source directory.
        sources = [str(Path(f).resolve()) for f in source_files]
        generator = MakefileGenerator(
            name=target_name,
            source_dir=str(Path(sources[0]).parent) if sources else ".",
            build_dir="build",
            flags=_BASE_FLAGS + self._extra_flags,
            include_dirs=include_dirs + self._extra_include_dirs,
            libraries=self._libraries,
            compiler=self._compiler,
            std="c11",
            use_stc=True,
            project_type="MultiGen",
            additional_sources=additional_sources,
            sources=sources,
        )

        return generator.generate_makefile()

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile C source directly using gcc with MultiGen runtime support."""
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)

        # Build gcc command with base flags
        cmd = [self._compiler, "-std=c11", *_BASE_FLAGS, *self._extra_flags]
        cmd.extend(f"-I{d}" for d in self._extra_include_dirs)

        # Add MultiGen runtime support if available
        if self._runtime_dir:
            cmd.append(f"-I{self._runtime_dir}")
            if self._stc_include_dir:
                cmd.append(f"-I{self._stc_include_dir}")
            cmd.extend(self.get_runtime_sources())

        # Add main source file and output
        cmd.extend([str(paths.source_path), "-o", str(paths.executable_path)])
        cmd.extend(f"-l{lib}" for lib in self._libraries)

        # Run compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_compile_flags(self) -> list[str]:
        """Get C compilation flags including MultiGen runtime support."""
        flags = ["-std=c11", *_BASE_FLAGS, *self._extra_flags]

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
