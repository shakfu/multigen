"""Loop Conversion Strategy Pattern for Multi-Language Code Generation.

This module implements the Strategy pattern to reduce complexity in for-loop
conversion across multiple backend converters (Haskell, OCaml).

Before refactoring:
- Haskell: ~251 lines, complexity ~40-50
- OCaml: ~84 lines, complexity ~15-20

After refactoring:
- Target: ~10-15 lines per backend, complexity ~8-10
- Shared strategies: ~500 lines total (reusable)
"""

import ast
from abc import ABC, abstractmethod
from typing import Optional


class ForLoopStrategy(ABC):
    """Abstract base class for for-loop conversion strategies."""

    @abstractmethod
    def can_handle(self, node: ast.For, context: "LoopContext") -> bool:
        """Check if this strategy can handle this for-loop pattern.

        Args:
            node: Python AST For node
            context: Loop conversion context with backend-specific information

        Returns:
            True if this strategy can handle this loop pattern
        """
        pass

    @abstractmethod
    def convert(self, node: ast.For, context: "LoopContext") -> str:
        """Convert for-loop to target language code.

        Args:
            node: Python AST For node
            context: Loop conversion context

        Returns:
            Converted loop code in target language
        """
        pass


class LoopContext:
    """Shared context for loop conversion with backend-specific helpers.

    This context allows strategies to access backend-specific functionality
    while remaining language-agnostic in their core pattern detection logic.
    """

    def __init__(
        self,
        converter: object,
        current_function: Optional[str] = None,
    ) -> None:
        """Initialize loop conversion context.

        Args:
            converter: Backend converter instance
            current_function: Name of current function being converted
        """
        self.converter = converter
        self.current_function = current_function


class ForLoopConverter:
    """Coordinates for-loop conversion using strategy pattern.

    This converter dispatches loop conversion to appropriate strategies,
    reducing complexity in backend converters.
    """

    def __init__(self, strategies: list[ForLoopStrategy]) -> None:
        """Initialize converter with ordered list of strategies.

        Args:
            strategies: List of loop conversion strategies (order matters - most specific first)
        """
        self.strategies = strategies

    def convert(self, node: ast.For, context: LoopContext) -> Optional[str]:
        """Convert for-loop using first matching strategy.

        Args:
            node: Python AST For node
            context: Loop conversion context

        Returns:
            Converted loop code, or None if no strategy matches
        """
        for strategy in self.strategies:
            if strategy.can_handle(node, context):
                return strategy.convert(node, context)

        # No strategy matched
        return None


__all__ = [
    "ForLoopStrategy",
    "LoopContext",
    "ForLoopConverter",
]
