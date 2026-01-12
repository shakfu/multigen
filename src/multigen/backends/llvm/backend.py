"""LLVM backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
from ..preferences import BackendPreferences
from .builder import LLVMBuilder
from .containers import LLVMContainerSystem
from .emitter import LLVMEmitter
from .factory import LLVMFactory


class LLVMBackend(LanguageBackend):
    """LLVM IR backend for MultiGen.

    This backend converts MultiGen's Static IR to LLVM IR, enabling:
    - Native binary compilation via LLVM
    - Multiple target architectures (x86, ARM, RISC-V, etc.)
    - WebAssembly output
    - JIT compilation (future)
    - Industrial-strength optimization
    """

    def __init__(self, preferences: Optional[BackendPreferences] = None) -> None:
        """Initialize the LLVM backend."""
        super().__init__(preferences)
        self._emitter: Optional[AbstractEmitter] = None
        self._factory: Optional[AbstractFactory] = None
        self._builder: Optional[AbstractBuilder] = None
        self._container_system: Optional[AbstractContainerSystem] = None

    def get_name(self) -> str:
        """Get backend name."""
        return "llvm"

    def get_file_extension(self) -> str:
        """Get file extension for generated files."""
        return ".ll"  # LLVM IR text format

    def get_emitter(self) -> AbstractEmitter:
        """Get the LLVM emitter."""
        if self._emitter is None:
            self._emitter = LLVMEmitter()
        return self._emitter

    def get_factory(self) -> AbstractFactory:
        """Get the LLVM factory."""
        if self._factory is None:
            self._factory = LLVMFactory()
        return self._factory

    def get_builder(self) -> AbstractBuilder:
        """Get the LLVM builder."""
        if self._builder is None:
            self._builder = LLVMBuilder()
        return self._builder

    def get_container_system(self) -> AbstractContainerSystem:
        """Get the LLVM container system."""
        if self._container_system is None:
            self._container_system = LLVMContainerSystem()
        return self._container_system

    def supports_feature(self, feature: str) -> bool:
        """Check if a feature is supported.

        Args:
            feature: Feature name to check

        Returns:
            True if feature is supported
        """
        supported_features = {
            "functions",
            "variables",
            "arithmetic",
            "control_flow",
            "loops",
            "recursion",
            # Future features:
            # "strings",
            # "containers",
            # "file_io",
            # "jit",
            # "wasm",
        }
        return feature in supported_features

    def get_runtime_dependencies(self) -> list[str]:
        """Get list of runtime dependencies.

        Returns:
            List of runtime library files needed
        """
        # LLVM backend generates self-contained IR for now
        # Future: may need runtime library for strings/containers
        return []

    def validate_compatibility(self, source_code: str) -> tuple[bool, list[str]]:
        """Validate if source code is compatible with LLVM backend.

        Args:
            source_code: Python source code to validate

        Returns:
            Tuple of (is_compatible, list_of_issues)
        """
        issues: list[str] = []

        # For now, assume basic compatibility
        # Future: check for unsupported features (async, generators, etc.)

        return len(issues) == 0, issues
