"""Abstract base classes for MultiGen language backends."""

import ast
import shutil
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Any, Optional

from .preferences import BackendPreferences

if TYPE_CHECKING:
    from .optimizer import AbstractOptimizer


@dataclass
class CompilationResult:
    """Result from a compilation operation.

    Provides structured information about compilation success/failure
    with detailed error messages and output paths.
    """

    success: bool
    executable_path: Optional[str] = None
    stdout: str = ""
    stderr: str = ""
    return_code: int = 0
    command: list[str] = field(default_factory=list)

    @property
    def error_message(self) -> str:
        """Get a formatted error message if compilation failed."""
        if self.success:
            return ""
        if self.stderr:
            return self.stderr.strip()
        return f"Compilation failed with return code {self.return_code}"


@dataclass
class BuildPaths:
    """Resolved paths for a build operation.

    Centralizes path resolution logic used across all builders.
    """

    source_path: Path
    output_dir: Path
    executable_name: str
    executable_path: Path

    @classmethod
    def from_strings(cls, source_file: str, output_dir: str) -> "BuildPaths":
        """Create BuildPaths from string arguments.

        Args:
            source_file: Path to source file (relative or absolute)
            output_dir: Path to output directory (relative or absolute)

        Returns:
            BuildPaths with resolved absolute paths
        """
        source_path = Path(source_file).absolute()
        out_dir = Path(output_dir).absolute()
        executable_name = source_path.stem
        executable_path = out_dir / executable_name
        return cls(
            source_path=source_path,
            output_dir=out_dir,
            executable_name=executable_name,
            executable_path=executable_path,
        )


class LanguageBackend(ABC):
    """Abstract base for all language backends in MultiGen."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize backend with optional preferences."""
        self.preferences = preferences

    @abstractmethod
    def get_name(self) -> str:
        """Language name (e.g., 'rust', 'go', 'cpp', 'c')."""

    @abstractmethod
    def get_file_extension(self) -> str:
        """Source file extension (e.g., '.rs', '.go', '.cpp', '.c')."""

    @abstractmethod
    def get_factory(self) -> "AbstractFactory":
        """Get language-specific code element factory."""

    @abstractmethod
    def get_emitter(self) -> "AbstractEmitter":
        """Get language-specific code emitter."""

    @abstractmethod
    def get_builder(self) -> "AbstractBuilder":
        """Get language-specific build system."""

    @abstractmethod
    def get_container_system(self) -> "AbstractContainerSystem":
        """Get language-specific container library integration."""

    def get_optimizer(self) -> Optional["AbstractOptimizer"]:
        """Get language-specific optimizer (optional).

        Backends with optimization support should override this method
        to return their optimizer instance. The optimizer implements
        the AbstractOptimizer interface for standardized optimization.

        Returns:
            AbstractOptimizer instance if the backend supports optimization,
            None otherwise (default).
        """
        return None


class AbstractFactory(ABC):
    """Abstract factory for creating language-specific code elements."""

    @abstractmethod
    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create variable declaration."""

    @abstractmethod
    def create_function_signature(self, name: str, params: list[tuple[str, str]], return_type: str) -> str:
        """Create function signature."""

    @abstractmethod
    def create_comment(self, text: str) -> str:
        """Create language-appropriate comment."""

    @abstractmethod
    def create_include(self, library: str) -> str:
        """Create import/include statement."""


class AbstractEmitter(ABC):
    """Abstract emitter for generating language-specific code."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize emitter with optional preferences."""
        self.preferences = preferences

    @abstractmethod
    def emit_function(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> str:
        """Generate complete function in target language."""

    @abstractmethod
    def emit_module(self, source_code: str, analysis_result: Any, semantic_mapping: Any = None) -> str:
        """Generate complete module/file in target language.

        Args:
            source_code: Original Python source code
            analysis_result: Analysis phase result with AST info
            semantic_mapping: Optional SemanticMapping from Phase 4 with pre-computed
                             type mappings, container mappings, and function return types.
                             If provided, emitters can use these instead of re-computing.
        """

    @abstractmethod
    def map_python_type(self, python_type: str) -> str:
        """Map Python type to target language type."""

    @abstractmethod
    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Determine if function can use simple emission strategy."""


class AbstractBuilder(ABC):
    """Abstract builder for language-specific build systems.

    Provides common utilities for compilation:
    - `_resolve_paths()`: Standardized path resolution
    - `_run_command()`: Subprocess execution with error handling
    - `_copy_runtime_file()`: Runtime file copying
    - `_get_runtime_dir()`: Get backend's runtime directory
    """

    @abstractmethod
    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate build configuration (Makefile, Cargo.toml, etc.)."""

    @abstractmethod
    def get_build_filename(self) -> str:
        """Get build file name (Makefile, Cargo.toml, etc.)."""

    @abstractmethod
    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile source directly using language tools.

        Args:
            source_file: Path to source file
            output_dir: Path to output directory
            **kwargs: Additional backend-specific arguments (e.g., opt_level for LLVM)

        Returns:
            True if compilation succeeded
        """

    @abstractmethod
    def get_compile_flags(self) -> list[str]:
        """Get compilation flags for the language."""

    # -------------------------------------------------------------------------
    # Common helper methods for subclasses
    # -------------------------------------------------------------------------

    def _resolve_paths(self, source_file: str, output_dir: str) -> BuildPaths:
        """Resolve and validate build paths.

        Standard path resolution used by all builders. Converts relative
        paths to absolute and computes executable name from source stem.

        Args:
            source_file: Path to source file
            output_dir: Path to output directory

        Returns:
            BuildPaths with resolved paths
        """
        return BuildPaths.from_strings(source_file, output_dir)

    def _run_command(
        self,
        cmd: list[str],
        cwd: Optional[str] = None,
        timeout: Optional[int] = None,
    ) -> CompilationResult:
        """Run a compilation command with standardized error handling.

        Args:
            cmd: Command and arguments to execute
            cwd: Working directory (None for current directory)
            timeout: Timeout in seconds (None for no timeout)

        Returns:
            CompilationResult with success status and output
        """
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=cwd,
                timeout=timeout,
            )
            return CompilationResult(
                success=result.returncode == 0,
                stdout=result.stdout,
                stderr=result.stderr,
                return_code=result.returncode,
                command=cmd,
            )
        except subprocess.TimeoutExpired:
            return CompilationResult(
                success=False,
                stderr="Compilation timed out",
                return_code=-1,
                command=cmd,
            )
        except FileNotFoundError as e:
            return CompilationResult(
                success=False,
                stderr=f"Compiler not found: {e}",
                return_code=-1,
                command=cmd,
            )
        except Exception as e:
            return CompilationResult(
                success=False,
                stderr=f"Compilation error: {e}",
                return_code=-1,
                command=cmd,
            )

    def _copy_runtime_file(
        self,
        runtime_filename: str,
        target_dir: Path,
        runtime_subdir: str = "runtime",
    ) -> Optional[Path]:
        """Copy a runtime file from backend's runtime directory to target.

        Args:
            runtime_filename: Name of the runtime file (e.g., "multigen_runtime.rs")
            target_dir: Directory to copy the file to
            runtime_subdir: Subdirectory name under backend dir (default: "runtime")

        Returns:
            Path to copied file if successful, None if source doesn't exist
        """
        runtime_dir = self._get_runtime_dir(runtime_subdir)
        if runtime_dir is None:
            return None

        source_file = runtime_dir / runtime_filename
        if not source_file.exists():
            return None

        target_file = target_dir / runtime_filename
        shutil.copy2(source_file, target_file)
        return target_file

    def _get_runtime_dir(self, subdir: str = "runtime") -> Optional[Path]:
        """Get the runtime directory for this backend.

        Looks for a subdirectory relative to the builder's module file.

        Args:
            subdir: Name of runtime subdirectory (default: "runtime")

        Returns:
            Path to runtime directory if it exists, None otherwise
        """
        # Get the directory containing this builder's module
        import inspect

        builder_file = inspect.getfile(self.__class__)
        builder_dir = Path(builder_file).parent
        runtime_dir = builder_dir / subdir

        if runtime_dir.exists():
            return runtime_dir
        return None

    def _get_runtime_files(self, pattern: str = "*", subdir: str = "runtime") -> list[Path]:
        """Get all runtime files matching a pattern.

        Args:
            pattern: Glob pattern for files (e.g., "*.c", "*.h")
            subdir: Runtime subdirectory name

        Returns:
            List of matching file paths
        """
        runtime_dir = self._get_runtime_dir(subdir)
        if runtime_dir is None:
            return []
        return list(runtime_dir.glob(pattern))


class AbstractContainerSystem(ABC):
    """Abstract container system for language-specific collections."""

    @abstractmethod
    def get_list_type(self, element_type: str) -> str:
        """Get list/array type for element type."""

    @abstractmethod
    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get dictionary/map type for key-value pair."""

    @abstractmethod
    def get_set_type(self, element_type: str) -> str:
        """Get set type for element type."""

    @abstractmethod
    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate container-specific operations code."""

    @abstractmethod
    def get_required_imports(self) -> list[str]:
        """Get imports required for container operations."""
