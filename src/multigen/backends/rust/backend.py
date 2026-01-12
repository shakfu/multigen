"""Rust backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
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
