"""C++ builder for compilation and build file generation."""

import shutil
from pathlib import Path
from typing import Any, Optional

from ...common.makefilegen import MakefileGenerator
from ..base import AbstractBuilder


class CppBuilder(AbstractBuilder):
    """Builder for C++ projects."""

    def __init__(self) -> None:
        """Initialize the C++ builder."""
        self.compiler = "g++"
        self.default_flags = ["-std=c++17", "-Wall", "-O2"]
        self.runtime_sources: list[str] = []
        self.runtime_headers_dir: Optional[str] = None
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
            self.compiler = compiler
            applied.add("compiler")
        if compiler_flags:
            self.default_flags.extend(compiler_flags)
            applied.add("compiler_flags")
        if include_dirs:
            self._extra_include_dirs = list(include_dirs)
            applied.add("include_dirs")
        if libraries:
            self._libraries = list(libraries)
            applied.add("libraries")
        return applied

    def get_build_filename(self) -> str:
        """Get the build file name (Makefile for C++)."""
        return "Makefile"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate a Makefile for the C++ project using makefilegen.

        Raises:
            ValueError: If a name or path contains characters unsafe in a Makefile
        """
        # Generated code includes "runtime/multigen_cpp_runtime.hpp", so search the runtime's parent.
        runtime_dir = self.runtime_headers_dir or self._get_runtime_dir()
        include_dirs = ([str(Path(runtime_dir).parent)] if runtime_dir else []) + self._extra_include_dirs

        # Extract flags and standard from default_flags
        flags = [f for f in self.default_flags if not f.startswith("-std=")]
        std = "c++17"
        for f in self.default_flags:
            if f.startswith("-std=c++"):
                std = f[5:]
                break

        # Absolute paths: the CLI moves the Makefile out of the source directory.
        sources = [str(Path(f).resolve()) for f in source_files]
        generator = MakefileGenerator(
            name=target_name,
            source_dir=str(Path(sources[0]).parent) if sources else ".",
            build_dir="build",
            flags=flags,
            include_dirs=include_dirs,
            libraries=self._libraries,
            compiler=self.compiler,
            std=std,
            use_stc=False,
            project_type="MultiGen",
            sources=sources,
        )

        return generator.generate_makefile()

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile C++ source directly to executable."""
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)

        # Setup runtime environment (copy headers if needed)
        self._setup_runtime_environment(str(paths.output_dir))

        # Build the compilation command
        cmd = [self.compiler] + self.get_compile_flags() + [str(paths.source_path), "-o", str(paths.executable_path)]
        cmd.extend(f"-l{lib}" for lib in self._libraries)

        # Execute compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_executable_name(self, source_file: str) -> str:
        """Get the executable name for a source file."""
        return Path(source_file).stem

    def get_compiler_flags(self) -> list[str]:
        """Get default compiler flags."""
        return self.default_flags.copy()

    def set_compiler(self, compiler: str) -> None:
        """Set the compiler to use."""
        self.compiler = compiler

    def add_flag(self, flag: str) -> None:
        """Add a compiler flag."""
        if flag not in self.default_flags:
            self.default_flags.append(flag)

    def remove_flag(self, flag: str) -> None:
        """Remove a compiler flag."""
        if flag in self.default_flags:
            self.default_flags.remove(flag)

    def set_standard(self, standard: str) -> None:
        """Set the C++ standard (e.g., 'c++11', 'c++14', 'c++17', 'c++20')."""
        # Remove existing standard flags
        self.default_flags = [f for f in self.default_flags if not f.startswith("-std=")]
        # Add new standard
        self.default_flags.append(f"-std={standard}")

    def enable_debug(self) -> None:
        """Enable debug mode."""
        self.add_flag("-g")
        self.add_flag("-DDEBUG")
        # Remove optimization flags
        self.default_flags = [f for f in self.default_flags if not f.startswith("-O")]

    def enable_optimization(self, level: str = "2") -> None:
        """Enable optimization."""
        # Remove existing optimization flags
        self.default_flags = [f for f in self.default_flags if not f.startswith("-O")]
        self.add_flag(f"-O{level}")

    def add_include_directory(self, directory: str) -> None:
        """Add an include directory."""
        self.add_flag(f"-I{directory}")

    def add_library(self, library: str) -> None:
        """Add a library to link."""
        self.add_flag(f"-l{library}")

    def add_library_directory(self, directory: str) -> None:
        """Add a library directory."""
        self.add_flag(f"-L{directory}")

    def generate_cmake_file(self, source_files: list[str], target_name: str) -> str:
        """Generate a CMakeLists.txt file as an alternative to Makefile."""
        sources = "\n    ".join(Path(f).name for f in source_files)

        cmake_content = f"""# Generated CMakeLists.txt for {target_name}

cmake_minimum_required(VERSION 3.10)
project({target_name})

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add compiler flags
set(CMAKE_CXX_FLAGS "${{CMAKE_CXX_FLAGS}} -Wall")

# Add the executable
add_executable({target_name}
    {sources}
)

# Set output directory
set_target_properties({target_name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${{CMAKE_BINARY_DIR}}"
)

# Installation
install(TARGETS {target_name} DESTINATION bin)
"""
        return cmake_content

    def get_compile_flags(self) -> list[str]:
        """Get default compilation flags with runtime includes."""
        flags = self.default_flags.copy()
        if self.runtime_headers_dir:
            flags.append(f"-I{self.runtime_headers_dir}")
        flags.extend(f"-I{d}" for d in self._extra_include_dirs)
        return flags

    def _detect_runtime_sources(self, source_files: list[str]) -> list[str]:
        """Detect if runtime sources are needed based on generated code analysis."""
        runtime_sources: list[str] = []

        # Check if any source files use MultiGen runtime features
        for source_file in source_files:
            try:
                with open(source_file) as f:
                    content = f.read()
                    # Check for MultiGen runtime usage patterns
                    if (
                        "multigen_cpp_runtime.hpp" in content
                        or "multigen::" in content
                        or "StringOps::" in content
                        or "Range(" in content
                        or "list_comprehension" in content
                        or "dict_comprehension" in content
                        or "set_comprehension" in content
                    ):
                        # Runtime is needed, but it's header-only for C++
                        # Just ensure the include path is set
                        source_dir = Path(source_file).parent
                        runtime_dir = source_dir / "runtime"
                        if runtime_dir.exists():
                            self.runtime_headers_dir = str(runtime_dir)
                        break
            except FileNotFoundError:
                continue

        return runtime_sources

    def _setup_runtime_environment(self, output_dir: str) -> None:
        """Setup runtime environment in the output directory."""
        output_path = Path(output_dir)
        runtime_dir = output_path / "runtime"
        runtime_dir.mkdir(exist_ok=True)

        # Copy runtime headers from the backend using base class helper
        for header_file in self._get_runtime_files("*.hpp"):
            target_file = runtime_dir / header_file.name
            if not target_file.exists():
                shutil.copy2(header_file, target_file)

        self.runtime_headers_dir = str(runtime_dir)

    def set_runtime_directory(self, runtime_dir: str) -> None:
        """Set the runtime headers directory."""
        self.runtime_headers_dir = runtime_dir

    def get_runtime_sources(self) -> list[str]:
        """Get list of runtime source files (empty for header-only C++ runtime)."""
        return self.runtime_sources.copy()

    def requires_runtime_library(self, source_files: list[str]) -> bool:
        """Check if the project requires MultiGen C++ runtime library."""
        runtime_sources = self._detect_runtime_sources(source_files)
        return len(runtime_sources) > 0 or self.runtime_headers_dir is not None
