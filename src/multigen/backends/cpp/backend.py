"""C++ backend implementation."""

from typing import TYPE_CHECKING, Optional

from ..base import LanguageBackend
from ..preferences import BackendPreferences, CppPreferences

if TYPE_CHECKING:
    from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory


class CppBackend(LanguageBackend):
    """C++ language backend."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize C++ backend with preferences."""
        if preferences is None:
            preferences = CppPreferences()
        super().__init__(preferences)

    def get_name(self) -> str:
        """Get the backend name."""
        return "cpp"

    def get_file_extension(self) -> str:
        """Get the file extension for C++ files."""
        return ".cpp"

    def get_factory(self) -> "AbstractFactory":
        """Get the C++ code element factory."""
        from .factory import CppFactory

        return CppFactory()

    def get_emitter(self) -> "AbstractEmitter":
        """Get the C++ code emitter."""
        from .emitter import CppEmitter

        return CppEmitter(self.preferences)

    def get_builder(self) -> "AbstractBuilder":
        """Get the C++ builder."""
        from .builder import CppBuilder

        return CppBuilder()

    def get_container_system(self) -> "AbstractContainerSystem":
        """Get the C++ container system."""
        from .containers import CppContainerSystem

        return CppContainerSystem()
