"""Base classes for the Intelligence Layer analyzers, optimizers, and verifiers."""

import ast
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional

from ..common import log
from .ast_analyzer import AnalysisResult


class AnalysisLevel(Enum):
    """Levels of analysis depth."""

    BASIC = "basic"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"
    COMPREHENSIVE = "comprehensive"


class OptimizationLevel(Enum):
    """Levels of optimization aggressiveness."""

    NONE = 0
    BASIC = 1
    MODERATE = 2
    AGGRESSIVE = 3
    MAXIMUM = 4


@dataclass
class AnalysisContext:
    """Context information for analysis operations."""

    source_code: str
    ast_node: ast.AST
    analysis_result: Optional[AnalysisResult] = None
    analysis_level: AnalysisLevel = AnalysisLevel.BASIC
    optimization_level: OptimizationLevel = OptimizationLevel.BASIC
    target_architecture: str = "x86_64"
    metadata: Optional[dict[str, Any]] = None

    def __post_init__(self) -> None:
        """Initialize metadata dictionary if not provided."""
        if self.metadata is None:
            self.metadata = {}


@dataclass
class AnalysisReport:
    """Base class for analysis reports."""

    analyzer_name: str
    success: bool
    confidence: float  # 0.0 to 1.0
    findings: list[str]
    warnings: list[str]
    errors: list[str]
    metadata: dict[str, Any]
    execution_time_ms: float = 0.0

    def has_issues(self) -> bool:
        """Check if the analysis found any issues."""
        return len(self.errors) > 0 or len(self.warnings) > 0

    def is_reliable(self, threshold: float = 0.7) -> bool:
        """Check if the analysis is reliable based on confidence threshold."""
        return self.success and self.confidence >= threshold


class BaseAnalyzer(ABC):
    """Base class for all analyzers in the intelligence layer."""

    def __init__(self, name: str, analysis_level: AnalysisLevel = AnalysisLevel.BASIC):
        self.log = log.config(self.__class__.__name__)
        self.name = name
        self.analysis_level = analysis_level
        self._cache: dict[str, Any] = {}

    @abstractmethod
    def analyze(self, context: AnalysisContext) -> AnalysisReport:
        """Perform analysis on the given context.

        Args:
            context: The analysis context containing code and metadata

        Returns:
            AnalysisReport containing the results of the analysis
        """
        pass

    def can_analyze(self, context: AnalysisContext) -> bool:
        """Check if this analyzer can handle the given context.

        Args:
            context: The analysis context to check

        Returns:
            True if this analyzer can process the context
        """
        return True

    def clear_cache(self) -> None:
        """Clear the analyzer's cache."""
        self._cache.clear()

    def _get_cache_key(self, context: AnalysisContext) -> str:
        """Generate a cache key for the given context."""
        import hashlib

        key_data = f"{context.source_code}:{self.analysis_level.value}"
        return hashlib.sha256(key_data.encode()).hexdigest()


class BaseOptimizer(ABC):
    """Base class for all optimizers in the intelligence layer."""

    def __init__(self, name: str, optimization_level: OptimizationLevel = OptimizationLevel.BASIC):
        self.name = name
        self.optimization_level = optimization_level
        self._enabled = True

    @abstractmethod
    def optimize(self, context: AnalysisContext) -> "OptimizationResult":
        """Perform optimization on the given context.

        Args:
            context: The analysis context to optimize

        Returns:
            OptimizationResult containing the optimized representation
        """
        pass

    def can_optimize(self, context: AnalysisContext) -> bool:
        """Check if this optimizer can handle the given context.

        Args:
            context: The analysis context to check

        Returns:
            True if this optimizer can process the context
        """
        return self._enabled

    def enable(self) -> None:
        """Enable this optimizer."""
        self._enabled = True

    def disable(self) -> None:
        """Disable this optimizer."""
        self._enabled = False


@dataclass
class OptimizationResult:
    """Result of an optimization operation."""

    optimizer_name: str
    success: bool
    optimized_ast: Optional[ast.AST]
    transformations: list[str]
    performance_gain_estimate: float  # Estimated performance improvement factor
    safety_analysis: dict[str, bool]  # Safety checks passed
    metadata: dict[str, Any]
    execution_time_ms: float = 0.0

    def is_valid(self) -> bool:
        """Check if the optimization result is valid."""
        return self.success and self.optimized_ast is not None

    def is_safe(self) -> bool:
        """Check if all safety analyses passed."""
        return all(self.safety_analysis.values())


class BaseVerifier(ABC):
    """Base class for all verifiers in the intelligence layer."""

    def __init__(self, name: str):
        self.name = name

    @abstractmethod
    def verify(self, context: AnalysisContext, optimized_result: OptimizationResult) -> "VerificationResult":
        """Verify the correctness of an optimization.

        Args:
            context: The original analysis context
            optimized_result: The result of optimization to verify

        Returns:
            VerificationResult containing verification status
        """
        pass


@dataclass
class VerificationResult:
    """Result of a verification operation."""

    verifier_name: str
    success: bool
    is_correct: bool
    proof_generated: bool
    confidence: float  # 0.0 to 1.0
    verification_details: dict[str, Any]
    execution_time_ms: float = 0.0

    def is_verified(self) -> bool:
        """Check if verification was successful and correct."""
        return self.success and self.is_correct


class IntelligencePipeline:
    """Main pipeline for coordinating analyzers, optimizers, and verifiers."""

    def __init__(self) -> None:
        self.analyzers: list[BaseAnalyzer] = []
        self.optimizers: list[BaseOptimizer] = []
        self.verifiers: list[BaseVerifier] = []

    def add_analyzer(self, analyzer: BaseAnalyzer) -> None:
        """Add an analyzer to the pipeline."""
        self.analyzers.append(analyzer)

    def add_optimizer(self, optimizer: BaseOptimizer) -> None:
        """Add an optimizer to the pipeline."""
        self.optimizers.append(optimizer)

    def add_verifier(self, verifier: BaseVerifier) -> None:
        """Add a verifier to the pipeline."""
        self.verifiers.append(verifier)

    def process(self, context: AnalysisContext) -> dict[str, Any]:
        """Process the context through the entire intelligence pipeline.

        Args:
            context: The analysis context to process

        Returns:
            Dictionary containing all analysis, optimization, and verification results
        """
        results: dict[str, Any] = {
            "analysis_reports": [],
            "optimization_results": [],
            "verification_results": [],
            "final_success": False,
            "pipeline_metadata": {},
        }

        # Run analyzers
        for analyzer in self.analyzers:
            if analyzer.can_analyze(context):
                report = analyzer.analyze(context)
                results["analysis_reports"].append(report)

        # Run optimizers
        for optimizer in self.optimizers:
            if optimizer.can_optimize(context):
                opt_result = optimizer.optimize(context)
                results["optimization_results"].append(opt_result)

                # Run verifiers on optimization results
                for verifier in self.verifiers:
                    ver_result = verifier.verify(context, opt_result)
                    results["verification_results"].append(ver_result)

        # Determine overall success
        results["final_success"] = self._evaluate_pipeline_success(results)

        return results

    def _evaluate_pipeline_success(self, results: dict[str, Any]) -> bool:
        """Evaluate if the pipeline execution was successful."""
        # Check if any critical errors occurred
        for report in results["analysis_reports"]:
            if not report.success:
                return False

        # Check if optimizations were successful and verified
        for opt_result in results["optimization_results"]:
            if not opt_result.is_valid():
                return False

        for ver_result in results["verification_results"]:
            if not ver_result.is_verified():
                return False

        return True
