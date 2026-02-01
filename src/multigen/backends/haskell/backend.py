"""Haskell backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
from ..optimizer import AbstractOptimizer, NoOpOptimizer
from ..preferences import BackendPreferences, HaskellPreferences
from .builder import HaskellBuilder
from .containers import HaskellContainerSystem
from .emitter import HaskellEmitter
from .factory import HaskellFactory


class HaskellBackend(LanguageBackend):
    """Haskell backend implementation for MultiGen."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize Haskell backend with preferences."""
        if preferences is None:
            preferences = HaskellPreferences()
        super().__init__(preferences)
        self._optimizer: Optional[NoOpOptimizer] = None

    def get_name(self) -> str:
        """Return backend name."""
        return "haskell"

    def get_file_extension(self) -> str:
        """Return Haskell source file extension."""
        return ".hs"

    def get_factory(self) -> AbstractFactory:
        """Get Haskell code element factory."""
        return HaskellFactory()

    def get_emitter(self) -> AbstractEmitter:
        """Get Haskell code emitter."""
        return HaskellEmitter(self.preferences)

    def get_builder(self) -> AbstractBuilder:
        """Get Haskell build system."""
        return HaskellBuilder()

    def get_container_system(self) -> AbstractContainerSystem:
        """Get Haskell container system."""
        return HaskellContainerSystem()

    def get_optimizer(self) -> AbstractOptimizer:
        """Get Haskell optimizer (delegates to compiler).

        Haskell optimization is handled by GHC (ghc -O, -O2, etc.)
        rather than at the code generation level.

        Returns:
            NoOpOptimizer instance
        """
        if self._optimizer is None:
            self._optimizer = NoOpOptimizer()
        return self._optimizer
