"""OCaml backend implementation for MultiGen."""

from typing import Optional

from ..base import AbstractBuilder, AbstractContainerSystem, AbstractEmitter, AbstractFactory, LanguageBackend
from ..preferences import BackendPreferences, OCamlPreferences
from .builder import OCamlBuilder
from .containers import OCamlContainerSystem
from .emitter import OCamlEmitter
from .factory import OCamlFactory


class OCamlBackend(LanguageBackend):
    """OCaml backend for MultiGen code generation."""

    def __init__(self, preferences: Optional[BackendPreferences] = None):
        """Initialize the OCaml backend with preferences."""
        if preferences is None:
            preferences = OCamlPreferences()
        super().__init__(preferences)
        # Ensure preferences is never None after initialization
        assert self.preferences is not None

    def get_name(self) -> str:
        """Return backend name."""
        return "ocaml"

    def get_file_extension(self) -> str:
        """Get the file extension for OCaml files."""
        return ".ml"

    def get_factory(self) -> AbstractFactory:
        """Get OCaml code element factory."""
        return OCamlFactory()

    def get_emitter(self) -> AbstractEmitter:
        """Get OCaml code emitter."""
        return OCamlEmitter(self.preferences)

    def get_builder(self) -> AbstractBuilder:
        """Get OCaml build system."""
        return OCamlBuilder()

    def get_container_system(self) -> AbstractContainerSystem:
        """Get OCaml container system."""
        return OCamlContainerSystem()

    def get_build_system(self) -> str:
        """Get the build system used by OCaml."""
        return "dune / ocamlc"

    def get_description(self) -> str:
        """Get a description of the OCaml backend."""
        return "Functional programming with OCaml standard library, pattern matching, and type safety"

    def is_available(self) -> bool:
        """Check if OCaml compiler is available."""
        import subprocess

        try:
            result = subprocess.run(["ocamlc", "-version"], capture_output=True, text=True, timeout=5)
            return result.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return False

    def get_version_info(self) -> str:
        """Get OCaml compiler version information."""
        import subprocess

        try:
            result = subprocess.run(["ocamlc", "-version"], capture_output=True, text=True, timeout=5)
            if result.returncode == 0:
                return result.stdout.strip()
            else:
                return "Unknown version"
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return "OCaml not available"

    def get_advanced_features(self) -> list:
        """Get list of advanced features supported by the OCaml backend."""
        features = [
            "Object-oriented programming (classes, methods, constructors)",
            "Functional programming with immutable data structures",
            "Pattern matching over conditionals",
            "Tail recursion optimization",
            "String methods (upper, lower, strip, find, replace, split)",
            "List comprehensions with functional patterns",
            "Dictionary and set comprehensions",
            "Type annotations with OCaml type system",
            "Module system with nested organization",
            "OCaml standard library integration",
        ]

        # Add preference-specific features
        if self.preferences is not None and self.preferences.get("use_pattern_matching"):
            features.append("Idiomatic pattern matching syntax")

        if self.preferences is not None and self.preferences.get("curried_functions"):
            features.append("Curried function style")

        if self.preferences is not None and self.preferences.get("tail_recursion_opt"):
            features.append("Tail recursion optimization")

        if self.preferences is not None and self.preferences.get("polymorphic_variants"):
            features.append("Polymorphic variant support")

        return features

    def get_preference_summary(self) -> dict:
        """Get a summary of current preferences."""
        if self.preferences is None:
            return {}
        return {
            "OCaml Version": self.preferences.get("ocaml_version", "4.14"),
            "Modern Syntax": self.preferences.get("use_modern_syntax", True),
            "Pattern Matching": self.preferences.get("use_pattern_matching", True),
            "Immutable Data": self.preferences.get("prefer_immutable", True),
            "Curried Functions": self.preferences.get("curried_functions", True),
            "Type Annotations": self.preferences.get("type_annotations", True),
            "Module Structure": self.preferences.get("module_structure", "nested"),
            "List Operations": self.preferences.get("list_operations", "functional"),
            "String Handling": self.preferences.get("string_handling", "stdlib"),
            "Naming Convention": self.preferences.get("naming_convention", "snake_case"),
        }
