#!/usr/bin/env python3
"""MultiGen Makefile Generator

Makefile generator and direct compilation tool for MultiGen projects,
adapted from makefilegen by shakfu: https://github.com/shakfu/makefilegen

Enhanced with STC (Smart Template Containers) library support for
high-performance C code generation projects.
"""

import argparse
import platform
import subprocess
import sys
from pathlib import Path
from typing import Callable, List, Optional, Union

from ..common import log

# type aliases
PathLike = Union[Path, str]
TestFunc = Callable[[str], bool]

# constants
PLATFORM = platform.system()

try:
    VERSION = float(subprocess.check_output(["make", "-v"]).decode().split("\n")[0].replace("GNU Make ", ""))
except (subprocess.CalledProcessError, FileNotFoundError):
    VERSION = 4.0  # Default fallback version


def unique_list(lst: list) -> list:
    """Remove duplicates while preserving order."""
    seen = set()
    result = []
    for item in lst:
        if item not in seen:
            seen.add(item)
            result.append(item)
    return result


class Builder:
    """Direct compilation builder for C/C++ projects with STC support."""

    def __init__(
        self,
        name: str = "main",
        source_dir: str = ".",
        include_dirs: Optional[List[str]] = None,
        library_dirs: Optional[List[str]] = None,
        libraries: Optional[List[str]] = None,
        flags: Optional[List[str]] = None,
        cppflags: Optional[List[str]] = None,
        cxxflags: Optional[List[str]] = None,
        ldflags: Optional[List[str]] = None,
        compiler: str = "gcc",
        std: str = "c99",
        use_stc: bool = True,
        stc_include_path: Optional[str] = None,
        project_type: str = "MultiGen",
    ):
        self.log = log.config(self.__class__.__name__)
        self.name = name
        self.source_dir = Path(source_dir)
        self.include_dirs = include_dirs or []
        self.library_dirs = library_dirs or []
        self.libraries = libraries or []
        self.flags = flags or []
        self.cppflags = cppflags or []
        self.cxxflags = cxxflags or []
        self.ldflags = ldflags or []
        self.compiler = compiler
        self.std = std
        self.use_stc = use_stc
        self.stc_include_path = stc_include_path
        self.project_type = project_type

        # Auto-detect STC if requested
        if self.use_stc and not self.stc_include_path:
            self.stc_include_path = self._detect_stc_path()

        # Add STC configuration
        if self.use_stc and self.stc_include_path:
            self._configure_stc()

    def _detect_stc_path(self) -> Optional[str]:
        """Auto-detect STC include path."""
        try:
            # Try to import MultiGen's STC module from C backend
            import sys

            # Add current directory to path for imports
            current_dir = Path(__file__).parent
            sys.path.insert(0, str(current_dir))

            # Try to find the backends/c/ext/stc module
            backends_dir = current_dir.parent / "backends" / "c" / "ext" / "stc"
            if backends_dir.exists():
                sys.path.insert(0, str(backends_dir.parent))
                from stc import get_stc_include_path  # type: ignore[import-not-found]

                return get_stc_include_path()
        except ImportError:
            pass

        # Look for STC in common locations
        possible_paths = [
            Path(__file__).parent.parent / "backends" / "c" / "ext" / "stc" / "include",
            Path.cwd() / "src" / "multigen" / "backends" / "c" / "ext" / "stc" / "include",
            Path(__file__).parent / "ext" / "stc" / "include",
            Path.cwd() / "src" / "cgen" / "ext" / "stc" / "include",  # Legacy CGen support
            Path("/usr/local/include/stc"),
            Path("/opt/stc/include"),
        ]

        for path in possible_paths:
            if path.exists() and (path / "stc").exists():
                return str(path)

        return None

    def _configure_stc(self) -> None:
        """Configure build settings for STC library."""
        if not self.stc_include_path:
            return

        # Add STC include path
        if self.stc_include_path not in self.include_dirs:
            self.include_dirs.append(self.stc_include_path)

        # Ensure C99 standard for STC compatibility
        if self.std in ["c89", "c90"]:
            self.std = "c99"

        # Add STC-specific flags if not already present
        stc_flags = ["-std=c99", "-DSTC_ENABLED"]
        for flag in stc_flags:
            if flag not in self.flags:
                self.flags.append(flag)

    def get_source_files(self) -> List[Path]:
        """Get all C source files in the source directory."""
        source_files: List[Path] = []
        for ext in ["*.c", "*.cc", "*.cpp", "*.cxx"]:
            source_files.extend(self.source_dir.glob(ext))
        return source_files

    def build_command(self) -> List[str]:
        """Generate the compilation command."""
        cmd = [self.compiler]

        # Add standard
        cmd.extend([f"-std={self.std}"])

        # Add flags
        cmd.extend(self.flags)
        cmd.extend(self.cppflags)
        if self.compiler in ["g++", "clang++"]:
            cmd.extend(self.cxxflags)

        # Add include directories
        for inc_dir in self.include_dirs:
            cmd.extend(["-I", str(inc_dir)])

        # Add library directories
        for lib_dir in self.library_dirs:
            cmd.extend(["-L", str(lib_dir)])

        # Add source files
        source_files = self.get_source_files()
        cmd.extend([str(f) for f in source_files])

        # Add libraries
        for lib in self.libraries:
            cmd.extend(["-l", lib])

        # Add ldflags
        cmd.extend(self.ldflags)

        # Output executable
        cmd.extend(["-o", self.name])

        return cmd

    def build(self, verbose: bool = False) -> bool:
        """Execute the build command."""
        cmd = self.build_command()

        self.log.debug(f"Building with STC support: {self.use_stc}")
        if self.use_stc and self.stc_include_path:
            self.log.debug(f"STC include path: {self.stc_include_path}")
        self.log.debug(f"Build command: {' '.join(cmd)}")

        if verbose:
            print(f"Building with STC support: {self.use_stc}")
            if self.use_stc and self.stc_include_path:
                print(f"STC include path: {self.stc_include_path}")
            print(f"Build command: {' '.join(cmd)}")

        try:
            result = subprocess.run(cmd, check=True, capture_output=True, text=True)
            self.log.info(f"Build successful: {self.name}")
            if result.stdout:
                self.log.debug(f"stdout: {result.stdout}")
            if verbose:
                print(f" Build successful: {self.name}")
                if result.stdout:
                    print(f"stdout: {result.stdout}")
            return True
        except subprocess.CalledProcessError as e:
            self.log.error(f"Build failed: {e}")
            if e.stdout:
                self.log.debug(f"stdout: {e.stdout}")
            if e.stderr:
                self.log.debug(f"stderr: {e.stderr}")
            print(f" Build failed: {e}")
            if e.stdout:
                print(f"stdout: {e.stdout}")
            if e.stderr:
                print(f"stderr: {e.stderr}")
            return False
        except FileNotFoundError:
            self.log.error(f"Compiler not found: {self.compiler}")
            print(f" Compiler not found: {self.compiler}")
            return False


class MakefileGenerator:
    """Generate sophisticated Makefiles for C/C++ projects with STC support."""

    def __init__(
        self,
        name: str = "main",
        source_dir: str = ".",
        build_dir: str = "build",
        include_dirs: Optional[List[str]] = None,
        library_dirs: Optional[List[str]] = None,
        libraries: Optional[List[str]] = None,
        flags: Optional[List[str]] = None,
        cppflags: Optional[List[str]] = None,
        cxxflags: Optional[List[str]] = None,
        ldflags: Optional[List[str]] = None,
        compiler: str = "gcc",
        std: str = "c99",
        use_stc: bool = True,
        stc_include_path: Optional[str] = None,
        project_type: str = "MultiGen",
        additional_sources: Optional[List[str]] = None,
    ):
        self.log = log.config(self.__class__.__name__)
        self.name = name
        self.source_dir = Path(source_dir)
        self.build_dir = Path(build_dir)
        self.include_dirs = include_dirs or []
        self.library_dirs = library_dirs or []
        self.libraries = libraries or []
        self.flags = flags or []
        self.cppflags = cppflags or []
        self.cxxflags = cxxflags or []
        self.ldflags = ldflags or []
        self.compiler = compiler
        self.std = std
        self.use_stc = use_stc
        self.stc_include_path = stc_include_path
        self.project_type = project_type
        self.additional_sources = additional_sources or []

        # Auto-detect STC if requested
        if self.use_stc and not self.stc_include_path:
            self.stc_include_path = self._detect_stc_path()

        # Add STC configuration
        if self.use_stc and self.stc_include_path:
            self._configure_stc()

        self.content: List[str] = []

    def _detect_stc_path(self) -> Optional[str]:
        """Auto-detect STC include path."""
        try:
            # Try to import MultiGen's STC module from C backend
            import sys

            # Add current directory to path for imports
            current_dir = Path(__file__).parent
            sys.path.insert(0, str(current_dir))

            # Try to find the backends/c/ext/stc module
            backends_dir = current_dir.parent / "backends" / "c" / "ext" / "stc"
            if backends_dir.exists():
                sys.path.insert(0, str(backends_dir.parent))
                from stc import get_stc_include_path  # type: ignore[import-not-found]

                return get_stc_include_path()
        except ImportError:
            pass

        # Look for STC in common locations
        possible_paths = [
            Path(__file__).parent.parent / "backends" / "c" / "ext" / "stc" / "include",
            Path.cwd() / "src" / "multigen" / "backends" / "c" / "ext" / "stc" / "include",
            Path(__file__).parent / "ext" / "stc" / "include",
            Path.cwd() / "src" / "cgen" / "ext" / "stc" / "include",  # Legacy CGen support
            Path("/usr/local/include/stc"),
            Path("/opt/stc/include"),
        ]

        for path in possible_paths:
            if path.exists() and (path / "stc").exists():
                return str(path)

        return None

    def _configure_stc(self) -> None:
        """Configure Makefile settings for STC library."""
        if not self.stc_include_path:
            return

        # Add STC include path
        if self.stc_include_path not in self.include_dirs:
            self.include_dirs.append(self.stc_include_path)

        # Ensure C99 standard for STC compatibility
        if self.std in ["c89", "c90"]:
            self.std = "c99"

        # Add STC-specific flags if not already present
        stc_flags = ["-std=c99"]
        for flag in stc_flags:
            if flag not in self.flags:
                self.flags.append(flag)

    def comment(self, text: str) -> "MakefileGenerator":
        """Add a comment to the Makefile."""
        self.content.append(f"# {text}")
        return self

    def blank_line(self) -> "MakefileGenerator":
        """Add a blank line to the Makefile."""
        self.content.append("")
        return self

    def variable(self, name: str, value: str, conditional: bool = False) -> "MakefileGenerator":
        """Add a variable definition."""
        if conditional:
            self.content.append(f"{name} ?= {value}")
        else:
            self.content.append(f"{name} = {value}")
        return self

    def target(
        self,
        name: str,
        dependencies: Optional[List[str]] = None,
        commands: Optional[List[str]] = None,
        phony: bool = False,
    ) -> "MakefileGenerator":
        """Add a target to the Makefile."""
        deps = " ".join(dependencies) if dependencies else ""
        self.content.append(f"{name}: {deps}")

        if commands:
            for cmd in commands:
                self.content.append(f"\t{cmd}")

        if phony:
            self.content.insert(-len(commands) - 1 if commands else -1, f".PHONY: {name}")

        self.blank_line()
        return self

    def pattern_rule(self, target: str, source: str, commands: List[str]) -> "MakefileGenerator":
        """Add a pattern rule to the Makefile."""
        self.content.append(f"{target}: {source}")
        for cmd in commands:
            self.content.append(f"\t{cmd}")
        self.blank_line()
        return self

    def generate_header(self) -> "MakefileGenerator":
        """Generate Makefile header with project information."""
        self.comment("=" * 60)
        self.comment(f"{self.project_type} Project Makefile: {self.name}")
        if self.use_stc:
            self.comment("Enhanced with STC (Smart Template Containers) support")
        self.comment(f"Generated by {self.project_type} Makefile Generator")
        self.comment("=" * 60)
        self.blank_line()
        return self

    def generate_variables(self) -> "MakefileGenerator":
        """Generate standard Makefile variables."""
        # Basic configuration
        self.variable("CC", self.compiler)
        self.variable("TARGET", self.name)
        self.variable("SRCDIR", str(self.source_dir))
        self.variable("BUILDDIR", str(self.build_dir))

        # Standard
        self.variable("STD", f"-std={self.std}")

        # Flags
        if self.flags:
            self.variable("CFLAGS", " ".join(unique_list(self.flags)))
        if self.cppflags:
            self.variable("CPPFLAGS", " ".join(unique_list(self.cppflags)))
        if self.cxxflags:
            self.variable("CXXFLAGS", " ".join(unique_list(self.cxxflags)))
        if self.ldflags:
            self.variable("LDFLAGS", " ".join(unique_list(self.ldflags)))

        # Include directories
        if self.include_dirs:
            include_flags = " ".join([f"-I{d}" for d in unique_list(self.include_dirs)])
            self.variable("INCLUDES", include_flags)

        # Library directories and libraries
        if self.library_dirs:
            lib_dir_flags = " ".join([f"-L{d}" for d in unique_list(self.library_dirs)])
            self.variable("LIBDIRS", lib_dir_flags)

        if self.libraries:
            lib_flags = " ".join([f"-l{lib}" for lib in unique_list(self.libraries)])
            self.variable("LIBS", lib_flags)

        # Source and object files
        if self.additional_sources:
            # Include additional sources explicitly
            additional_src_str = " ".join(self.additional_sources)
            self.variable("SOURCES", f"$(wildcard $(SRCDIR)/*.c) {additional_src_str}")
            # Objects from both regular and additional sources
            self.variable("OBJECTS", "$(patsubst %.c,$(BUILDDIR)/%.o,$(notdir $(SOURCES)))")
        else:
            self.variable("SOURCES", "$(wildcard $(SRCDIR)/*.c)")
            self.variable("OBJECTS", "$(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)")

        # STC configuration
        if self.use_stc and self.stc_include_path:
            self.blank_line()
            self.comment("STC (Smart Template Containers) Configuration")
            self.variable("STC_INCLUDE", str(self.stc_include_path))
            self.variable("STC_FLAGS", "-DSTC_ENABLED")

        # Add VPATH if we have additional sources from other directories
        if self.additional_sources:
            # Extract unique directories from additional sources
            source_dirs = set()
            for src in self.additional_sources:
                src_path = Path(src)
                if src_path.parent != Path("."):
                    source_dirs.add(str(src_path.parent))
            if source_dirs:
                vpath = ":".join([str(self.source_dir)] + list(source_dirs))
                self.blank_line()
                self.variable("VPATH", vpath)

        self.blank_line()
        return self

    def generate_targets(self) -> "MakefileGenerator":
        """Generate standard Makefile targets."""
        # Default target
        self.target("all", [self.name], phony=True)

        # Build directory creation
        self.target("$(BUILDDIR)", commands=["@mkdir -p $(BUILDDIR)"])

        # Object file compilation
        compile_cmd = ["$(CC) $(STD) $(CFLAGS) $(CPPFLAGS)"]
        if "INCLUDES" in [line.split("=")[0].strip() for line in self.content if "=" in line]:
            compile_cmd.append("$(INCLUDES)")
        if self.use_stc:
            compile_cmd.append("$(STC_FLAGS)")
        compile_cmd.extend(["-c $< -o $@"])

        self.pattern_rule("$(BUILDDIR)/%.o", "$(SRCDIR)/%.c", [" ".join(compile_cmd)])

        # Main target
        link_cmd = ["$(CC)"]
        if "LDFLAGS" in [line.split("=")[0].strip() for line in self.content if "=" in line]:
            link_cmd.append("$(LDFLAGS)")
        link_cmd.extend(["$(OBJECTS)"])
        if "LIBDIRS" in [line.split("=")[0].strip() for line in self.content if "=" in line]:
            link_cmd.append("$(LIBDIRS)")
        if "LIBS" in [line.split("=")[0].strip() for line in self.content if "=" in line]:
            link_cmd.append("$(LIBS)")
        link_cmd.extend(["-o $@"])

        self.target(self.name, ["$(BUILDDIR)", "$(OBJECTS)"], [" ".join(link_cmd)])

        # Clean target
        self.target("clean", commands=["@rm -rf $(BUILDDIR)", f"@rm -f {self.name}"], phony=True)

        # Help target
        help_commands = [
            '@echo "Available targets:"',
            '@echo "  all     - Build the project (default)"',
            '@echo "  clean   - Remove build files"',
            '@echo "  help    - Show this help message"',
        ]
        if self.use_stc:
            help_commands.append('@echo "  Built with STC container support"')

        self.target("help", commands=help_commands, phony=True)

        return self

    def generate_makefile(self) -> str:
        """Generate the complete Makefile content."""
        self.content = []  # Reset content

        self.generate_header()
        self.generate_variables()
        self.generate_targets()

        return "\n".join(self.content)

    def write_makefile(self, filename: Optional[str] = None) -> bool:
        """Write the Makefile to disk.

        Args:
            filename: Output filename. If None, defaults to "build/Makefile"
        """
        if filename is None:
            # Default to build directory to avoid overwriting project Makefile
            build_dir = Path("build")
            build_dir.mkdir(exist_ok=True)
            filename = str(build_dir / "Makefile")

        try:
            makefile_content = self.generate_makefile()
            with open(filename, "w") as f:
                f.write(makefile_content)
            self.log.info(f"Generated Makefile: {filename}")
            if self.use_stc and self.stc_include_path:
                self.log.info(f"STC support enabled: {self.stc_include_path}")
            print(f" Generated Makefile: {filename}")
            if self.use_stc and self.stc_include_path:
                print(f" STC support enabled: {self.stc_include_path}")
            return True
        except Exception as e:
            self.log.error(f"Failed to write Makefile: {e}")
            print(f" Failed to write Makefile: {e}")
            return False


class MultiGenMakefileGenerator:
    """MultiGen-specific Makefile generator with STC integration."""

    def __init__(self, project_name: str = "multigen_project", project_type: str = "MultiGen"):
        self.log = log.config(self.__class__.__name__)
        self.project_name = project_name
        self.project_type = project_type

    def create_for_generated_code(
        self,
        c_file: str,
        output_name: Optional[str] = None,
        use_stc: bool = True,
        additional_flags: Optional[List[str]] = None,
        additional_includes: Optional[List[str]] = None,
        makefile_dir: Optional[str] = None,
    ) -> MakefileGenerator:
        """Create a Makefile for generated C code."""
        c_path = Path(c_file)
        if not c_path.exists():
            raise FileNotFoundError(f"C file not found: {c_file}")

        # Determine output name
        if not output_name:
            output_name = c_path.stem

        # Set up basic configuration - compute source_dir relative to makefile location
        if makefile_dir:
            makefile_path = Path(makefile_dir)
            try:
                # Calculate relative path from makefile dir to source dir
                source_dir = str(c_path.parent.relative_to(makefile_path))
            except ValueError:
                # Fallback to absolute path if relative calculation fails
                source_dir = str(c_path.parent)
        else:
            source_dir = str(c_path.parent)

        flags = ["-Wall", "-O2"] + (additional_flags or [])
        includes = additional_includes or []

        # Create generator
        generator = MakefileGenerator(
            name=output_name,
            source_dir=source_dir,
            flags=flags,
            include_dirs=includes,
            use_stc=use_stc,
            compiler="gcc",
            std="c99",
            project_type=self.project_type,
        )

        return generator

    def create_builder_for_generated_code(
        self,
        c_file: str,
        output_name: Optional[str] = None,
        use_stc: bool = True,
        additional_flags: Optional[List[str]] = None,
        additional_includes: Optional[List[str]] = None,
    ) -> Builder:
        """Create a Builder for direct compilation of generated C code."""
        c_path = Path(c_file)
        if not c_path.exists():
            raise FileNotFoundError(f"C file not found: {c_file}")

        # Determine output name
        if not output_name:
            output_name = c_path.stem

        # Set up basic configuration
        source_dir = str(c_path.parent)
        flags = ["-Wall", "-O2"] + (additional_flags or [])
        includes = additional_includes or []

        # Create builder
        builder = Builder(
            name=output_name,
            source_dir=source_dir,
            flags=flags,
            include_dirs=includes,
            use_stc=use_stc,
            compiler="gcc",
            std="c99",
            project_type=self.project_type,
        )

        return builder


def main() -> int:
    """Command-line interface for MultiGen Makefile generator."""
    parser = argparse.ArgumentParser(
        description="MultiGen Makefile Generator with STC support", formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Build command
    build_parser = subparsers.add_parser("build", help="Direct compilation")
    build_parser.add_argument("source", help="C source file to compile")
    build_parser.add_argument("-o", "--output", help="Output executable name")
    build_parser.add_argument("--no-stc", action="store_true", help="Disable STC support")
    build_parser.add_argument("-I", "--include", action="append", help="Include directories")
    build_parser.add_argument("-L", "--libdir", action="append", help="Library directories")
    build_parser.add_argument("-l", "--lib", action="append", help="Libraries to link")
    build_parser.add_argument("--flags", help="Additional compiler flags")
    build_parser.add_argument("--compiler", default="gcc", help="Compiler to use")
    build_parser.add_argument("-v", "--verbose", action="store_true", help="Verbose output")

    # Makefile command
    makefile_parser = subparsers.add_parser("makefile", help="Generate Makefile")
    makefile_parser.add_argument("source", help="C source file or directory")
    makefile_parser.add_argument("-o", "--output", help="Output Makefile name (defaults to build/Makefile)")
    makefile_parser.add_argument("--name", help="Project name")
    makefile_parser.add_argument("--no-stc", action="store_true", help="Disable STC support")
    makefile_parser.add_argument("-I", "--include", action="append", help="Include directories")
    makefile_parser.add_argument("-L", "--libdir", action="append", help="Library directories")
    makefile_parser.add_argument("-l", "--lib", action="append", help="Libraries to link")
    makefile_parser.add_argument("--flags", help="Additional compiler flags")
    makefile_parser.add_argument("--compiler", default="gcc", help="Compiler to use")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    # Parse additional flags
    additional_flags = []
    if args.flags:
        additional_flags = args.flags.split()

    if args.command == "build":
        # Direct compilation
        builder = Builder(
            name=args.output or Path(args.source).stem,
            source_dir=str(Path(args.source).parent),
            include_dirs=args.include or [],
            library_dirs=args.libdir or [],
            libraries=args.lib or [],
            flags=additional_flags,
            compiler=args.compiler,
            use_stc=not args.no_stc,
        )

        success = builder.build(verbose=args.verbose)
        return 0 if success else 1

    elif args.command == "makefile":
        # Makefile generation
        source_path = Path(args.source)

        if source_path.is_file():
            source_dir = str(source_path.parent)
            project_name = args.name or source_path.stem
        else:
            source_dir = str(source_path)
            project_name = args.name or source_path.name

        generator = MakefileGenerator(
            name=project_name,
            source_dir=source_dir,
            include_dirs=args.include or [],
            library_dirs=args.libdir or [],
            libraries=args.lib or [],
            flags=additional_flags,
            compiler=args.compiler,
            use_stc=not args.no_stc,
        )

        success = generator.write_makefile(args.output)
        return 0 if success else 1

    return 1  # Should not reach here


if __name__ == "__main__":
    sys.exit(main())
