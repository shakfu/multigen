"""Abstract base classes for MultiGen language backends."""

import ast
from abc import ABC, abstractmethod
from typing import Any, Optional

from .preferences import BackendPreferences


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


class AbstractFactory(ABC):
    """Abstract factory for creating language-specific code elements."""

    @abstractmethod
    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create variable declaration."""

    @abstractmethod
    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
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
    def emit_module(self, source_code: str, analysis_result: Any) -> str:
        """Generate complete module/file in target language."""

    @abstractmethod
    def map_python_type(self, python_type: str) -> str:
        """Map Python type to target language type."""

    @abstractmethod
    def can_use_simple_emission(self, func_node: ast.FunctionDef, type_context: dict[str, str]) -> bool:
        """Determine if function can use simple emission strategy."""


class AbstractBuilder(ABC):
    """Abstract builder for language-specific build systems."""

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
