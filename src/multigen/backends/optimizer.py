"""Abstract optimizer interface for MultiGen backends.

This module defines the abstract interface that all backend-specific optimizers
should implement. It provides a standardized way to apply optimizations to
generated code or IR across different target languages.

The optimizer interface allows:
- Consistent optimization level handling (O0-O3)
- Structured optimization information reporting
- Backend-agnostic optimization queries

Example:
    >>> from multigen.backends.optimizer import AbstractOptimizer, NoOpOptimizer
    >>> optimizer = NoOpOptimizer()
    >>> optimized_code = optimizer.optimize(generated_code)
    >>> info = optimizer.get_optimization_info()
    >>> print(f"Level: {info.level_name}")
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any


@dataclass
class OptimizationInfo:
    """Structured information about optimization configuration and results.

    This dataclass provides a type-safe alternative to dict-based optimization
    info, making it easier to work with optimization metadata.

    Attributes:
        level: Numeric optimization level (0-3, corresponding to O0-O3)
        level_name: Human-readable level name ("O0", "O1", "O2", "O3")
        passes_applied: List of optimization pass names that were applied
        transformations: List of transformations performed on the code
        metrics: Additional backend-specific metrics (e.g., target triple)
    """

    level: int
    level_name: str
    passes_applied: list[str] = field(default_factory=list)
    transformations: list[str] = field(default_factory=list)
    metrics: dict[str, Any] = field(default_factory=dict)


class AbstractOptimizer(ABC):
    """Abstract interface for target-language code optimization.

    This interface standardizes optimization across different backends,
    allowing the pipeline to query and apply optimizations uniformly.

    Implementing classes should:
    - Apply backend-specific optimization passes
    - Report optimization configuration and results
    - Support the standard O0-O3 optimization levels
    """

    @abstractmethod
    def optimize(self, code: str, opt_level: int = 2) -> str:
        """Apply optimization passes to generated code or IR.

        Args:
            code: The generated code or IR to optimize
            opt_level: Optimization level (0=none, 1=basic, 2=moderate, 3=aggressive)

        Returns:
            Optimized code or IR as a string

        Raises:
            ValueError: If the code is invalid or cannot be processed
            RuntimeError: If optimization passes fail
        """

    @abstractmethod
    def get_optimization_info(self) -> OptimizationInfo:
        """Get information about current optimization configuration.

        Returns:
            OptimizationInfo dataclass with optimization settings and metadata
        """

    @property
    @abstractmethod
    def supports_level(self) -> tuple[int, int]:
        """Return supported optimization level range (min, max).

        Returns:
            Tuple of (min_level, max_level) that this optimizer supports.
            For example, (0, 3) means the optimizer supports O0 through O3.
        """


class NoOpOptimizer(AbstractOptimizer):
    """No-operation optimizer for backends without optimization support.

    This optimizer simply returns code unchanged, providing a consistent
    interface for backends that don't implement optimization passes.
    """

    def optimize(self, code: str, opt_level: int = 2) -> str:
        """Return code unchanged.

        Args:
            code: The code to "optimize"
            opt_level: Ignored for no-op optimizer

        Returns:
            The input code unchanged
        """
        return code

    def get_optimization_info(self) -> OptimizationInfo:
        """Get optimization info indicating no optimization.

        Returns:
            OptimizationInfo with level 0 and "none" transformation
        """
        return OptimizationInfo(
            level=0,
            level_name="O0",
            passes_applied=[],
            transformations=["none"],
        )

    @property
    def supports_level(self) -> tuple[int, int]:
        """No-op optimizer only supports level 0.

        Returns:
            (0, 0) indicating only O0 is supported
        """
        return (0, 0)
