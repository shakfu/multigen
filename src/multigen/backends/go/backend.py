"""Go backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
from ..preferences import BackendPreferences, GoPreferences
from .builder import GoBuilder
from .containers import GoContainerSystem
from .emitter import GoEmitter
from .factory import GoFactory


class GoBackend(LanguageBackend):
    """Go backend implementation for MultiGen."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize Go backend with preferences."""
        if preferences is None:
            preferences = GoPreferences()
        super().__init__(preferences)

    def get_name(self) -> str:
        """Return backend name."""
        return "go"

    def get_file_extension(self) -> str:
        """Return Go source file extension."""
        return ".go"

    def get_factory(self) -> AbstractFactory:
        """Get Go code element factory."""
        return GoFactory()

    def get_emitter(self) -> AbstractEmitter:
        """Get Go code emitter."""
        return GoEmitter(self.preferences)

    def get_builder(self) -> AbstractBuilder:
        """Get Go build system."""
        return GoBuilder()

    def get_container_system(self) -> AbstractContainerSystem:
        """Get Go container system."""
        return GoContainerSystem()
