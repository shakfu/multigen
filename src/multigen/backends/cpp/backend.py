"""C++ backend implementation."""

from typing import TYPE_CHECKING, Optional

from ..base import LanguageBackend
from ..optimizer import AbstractOptimizer, NoOpOptimizer
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
        self._optimizer: Optional[NoOpOptimizer] = None

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

    def get_optimizer(self) -> AbstractOptimizer:
        """Get C++ optimizer (delegates to compiler).

        C++ optimization is handled by the compiler (g++ -O2, clang++ -O2, etc.)
        rather than at the code generation level.

        Returns:
            NoOpOptimizer instance
        """
        if self._optimizer is None:
            self._optimizer = NoOpOptimizer()
        return self._optimizer
