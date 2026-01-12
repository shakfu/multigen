"""Optimizers module for the Intelligence Layer.

This module contains various code optimizers including compile-time evaluation,
loop optimization, function specialization, and vectorization.
"""

from .compile_time_evaluator import CompileTimeEvaluator, CompileTimeReport, ConstantValue, OptimizationCandidate
from .function_specializer import FunctionProfile, FunctionSpecializer, SpecializationReport
from .function_specializer import SpecializationCandidate as FunctionSpecializationCandidate
from .loop_analyzer import LoopAnalysisReport, LoopAnalyzer, LoopInfo, LoopOptimization
from .vectorization_detector import (
    MemoryAccess,
    VectorizationCandidate,
    VectorizationConstraint,
    VectorizationDetector,
    VectorizationReport,
    VectorizationType,
)

__all__ = [
    "CompileTimeEvaluator",
    "CompileTimeReport",
    "ConstantValue",
    "OptimizationCandidate",
    "LoopAnalyzer",
    "LoopAnalysisReport",
    "LoopInfo",
    "LoopOptimization",
    "FunctionSpecializer",
    "SpecializationReport",
    "FunctionProfile",
    "FunctionSpecializationCandidate",
    "VectorizationDetector",
    "VectorizationReport",
    "VectorizationCandidate",
    "VectorizationType",
    "VectorizationConstraint",
    "MemoryAccess",
]
