"""Rust backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
from ..optimizer import AbstractOptimizer, NoOpOptimizer
from ..preferences import BackendPreferences, RustPreferences
from .builder import RustBuilder
from .containers import RustContainerSystem
from .emitter import RustEmitter
from .factory import RustFactory


class RustBackend(LanguageBackend):
    """Rust backend implementation for MultiGen."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize Rust backend with preferences."""
        if preferences is None:
            preferences = RustPreferences()
        super().__init__(preferences)
        self._optimizer: Optional[NoOpOptimizer] = None

    def get_name(self) -> str:
        """Return backend name."""
        return "rust"

    def get_file_extension(self) -> str:
        """Return Rust source file extension."""
        return ".rs"

    def get_factory(self) -> AbstractFactory:
        """Get Rust code element factory."""
        return RustFactory()

    def get_emitter(self) -> AbstractEmitter:
        """Get Rust code emitter."""
        return RustEmitter(self.preferences)

    def get_builder(self) -> AbstractBuilder:
        """Get Rust build system."""
        return RustBuilder()

    def get_container_system(self) -> AbstractContainerSystem:
        """Get Rust container system."""
        return RustContainerSystem()

    def get_optimizer(self) -> AbstractOptimizer:
        """Get Rust optimizer (delegates to compiler).

        Rust optimization is handled by rustc/LLVM (rustc -O, --release, etc.)
        rather than at the code generation level.

        Returns:
            NoOpOptimizer instance
        """
        if self._optimizer is None:
            self._optimizer = NoOpOptimizer()
        return self._optimizer
