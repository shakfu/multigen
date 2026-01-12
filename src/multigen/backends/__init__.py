"""Backend architecture for MultiGen multi-language code generation."""

from .base import AbstractBuilder, AbstractEmitter, AbstractFactory, LanguageBackend
from .registry import registry

__all__ = [
    "LanguageBackend",
    "AbstractFactory",
    "AbstractEmitter",
    "AbstractBuilder",
    "registry",
]
