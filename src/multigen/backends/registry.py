"""Backend registry system for MultiGen language backends."""

from typing import Optional

from .base import LanguageBackend
from .preferences import BackendPreferences, PreferencesRegistry


class BackendRegistry:
    """Central registry for all language backends."""

    def __init__(self) -> None:
        """Initialize registry with empty backend collection."""
        self._backends: dict[str, type[LanguageBackend]] = {}
        self._register_built_in_backends()

    def register_backend(self, name: str, backend_class: type[LanguageBackend]) -> None:
        """Register a new language backend."""
        self._backends[name] = backend_class

    def get_backend(self, name: str, preferences: Optional[BackendPreferences] = None) -> LanguageBackend:
        """Get backend instance by name with optional preferences."""
        if name not in self._backends:
            available = ", ".join(self.list_backends())
            raise ValueError(f"Unknown backend: {name}. Available: {available}")

        # Create preferences if not provided
        if preferences is None:
            preferences = PreferencesRegistry.create_preferences(name)

        return self._backends[name](preferences)

    def list_backends(self) -> list[str]:
        """List all available backend names."""
        return list(self._backends.keys())

    def has_backend(self, name: str) -> bool:
        """Check if backend is registered."""
        return name in self._backends

    def _register_built_in_backends(self) -> None:
        """Register built-in backends as they become available."""
        # Try to register C backend
        try:
            from .c.backend import CBackend

            self.register_backend("c", CBackend)
        except ImportError:
            pass  # C backend not yet implemented

        # Try to register Rust backend
        try:
            from .rust.backend import RustBackend

            self.register_backend("rust", RustBackend)
        except ImportError:
            pass  # Rust backend not yet implemented

        # Try to register Go backend
        try:
            from .go.backend import GoBackend

            self.register_backend("go", GoBackend)
        except ImportError:
            pass  # Go backend not yet implemented

        # Try to register C++ backend
        try:
            from .cpp.backend import CppBackend

            self.register_backend("cpp", CppBackend)
        except ImportError:
            pass  # C++ backend not yet implemented

        # Try to register Haskell backend
        try:
            from .haskell.backend import HaskellBackend

            self.register_backend("haskell", HaskellBackend)
        except ImportError:
            pass  # Haskell backend not yet implemented

        # Try to register OCaml backend
        try:
            from .ocaml.backend import OCamlBackend

            self.register_backend("ocaml", OCamlBackend)
        except ImportError:
            pass  # OCaml backend not yet implemented

        # Try to register LLVM backend
        try:
            from .llvm.backend import LLVMBackend

            self.register_backend("llvm", LLVMBackend)
        except ImportError:
            pass  # LLVM backend not yet implemented


# Global registry instance
registry = BackendRegistry()
