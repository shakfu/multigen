"""CGen Frontend - Unified Analysis and Intelligence Layer.

This module handles the complete analysis pipeline from static Python parsing
through intelligent optimization and formal verification. Previously split into
frontend and intelligence layers, this unified layer provides:

Core Analysis:
- AST parsing and analysis
- Type inference and validation
- Static constraint checking
- Python subset validation
- Static IR generation

Intelligent Analysis:
- Static analysis and symbolic execution
- Loop analysis and optimization detection
- Function specialization opportunities
- Compile-time evaluation
- Bounds checking and memory safety
- Call graph analysis
- Vectorization detection
- Formal verification capabilities
"""

# AST Analysis Framework
# Static Analysis and Symbolic Execution
from .analyzers import (
    BoundsChecker,
    BoundsCheckingReport,
    BoundsViolation,
    CallGraphAnalyzer,
    CallGraphReport,
    CallPath,
    CallSite,
    CFGNode,
    ControlFlowGraph,
    ExecutionPath,
    FunctionNode,
    MemoryRegion,
    StaticAnalysisReport,
    StaticAnalyzer,
    SymbolicExecutionReport,
    SymbolicExecutor,
    SymbolicState,
    SymbolicValue,
)
from .ast_analyzer import (
    AnalysisResult,
    ASTAnalyzer,
    FunctionInfo,
    NodeType,
    StaticComplexity,
    TypeInfo,
    VariableInfo,
    analyze_python_code,
    analyze_python_file,
)

# Intelligence Base
from .base import (
    AnalysisContext,
    AnalysisLevel,
    AnalysisReport,
    BaseAnalyzer,
    BaseOptimizer,
    BaseVerifier,
    IntelligencePipeline,
    OptimizationLevel,
    OptimizationResult,
    VerificationResult,
)

# Immutability Analysis
from .immutability_analyzer import ImmutabilityAnalyzer, MutabilityClass

# Static Constraint Checking (DEPRECATED - replaced by python_constraints.py and backend-specific checkers)
# Optimization Analysis
from .optimizers import (
    CompileTimeEvaluator,
    CompileTimeReport,
    ConstantValue,
    FunctionProfile,
    FunctionSpecializationCandidate,
    FunctionSpecializer,
    LoopAnalysisReport,
    LoopAnalyzer,
    LoopInfo,
    LoopOptimization,
    MemoryAccess,
    OptimizationCandidate,
    SpecializationReport,
    VectorizationCandidate,
    VectorizationConstraint,
    VectorizationDetector,
    VectorizationReport,
    VectorizationType,
)

# Python Constraint Checking
from .python_constraints import (
    ConstraintCategory as PythonConstraintCategory,
)
from .python_constraints import (
    PythonConstraintChecker,
    PythonConstraintViolation,
)

# Static IR
from .static_ir import (
    IRBuilder,
    IRDataType,
    IRExpression,
    IRFunction,
    IRModule,
    IRStatement,
    IRType,
    IRVariable,
    build_ir_from_code,
)

# Python Subset Validation
from .subset_validator import FeatureRule, FeatureStatus, StaticPythonSubsetValidator, SubsetTier, ValidationResult

# Type Inference System
from .type_inference import InferenceMethod, InferenceResult, TypeConstraint, TypeInferenceEngine

# Formal Verification
from .verifiers import (
    AlgorithmProof,
    BoundsProver,
    CorrectnessProver,
    MemorySafetyProof,
    ProofResult,
    TheoremProver,
)

__all__ = [
    # AST Analysis
    "ASTAnalyzer",
    "AnalysisResult",
    "FunctionInfo",
    "VariableInfo",
    "TypeInfo",
    "StaticComplexity",
    "NodeType",
    "analyze_python_code",
    "analyze_python_file",
    # Type Inference
    "TypeInferenceEngine",
    "InferenceResult",
    "InferenceMethod",
    "TypeConstraint",
    # Immutability Analysis
    "ImmutabilityAnalyzer",
    "MutabilityClass",
    # Python Constraint Checking
    "PythonConstraintChecker",
    "PythonConstraintViolation",
    "PythonConstraintCategory",
    # Subset Validation
    "StaticPythonSubsetValidator",
    "ValidationResult",
    "FeatureRule",
    "SubsetTier",
    "FeatureStatus",
    # Static IR
    "IRModule",
    "IRFunction",
    "IRVariable",
    "IRStatement",
    "IRExpression",
    "IRType",
    "IRDataType",
    "IRBuilder",
    "build_ir_from_code",
    # Intelligence Base
    "AnalysisContext",
    "AnalysisLevel",
    "AnalysisReport",
    "BaseAnalyzer",
    "BaseOptimizer",
    "BaseVerifier",
    "IntelligencePipeline",
    "OptimizationLevel",
    "OptimizationResult",
    "VerificationResult",
    # Static Analysis
    "StaticAnalyzer",
    "StaticAnalysisReport",
    "CFGNode",
    "ControlFlowGraph",
    "SymbolicExecutor",
    "SymbolicExecutionReport",
    "SymbolicValue",
    "SymbolicState",
    "ExecutionPath",
    "BoundsChecker",
    "BoundsCheckingReport",
    "MemoryRegion",
    "BoundsViolation",
    "CallGraphAnalyzer",
    "CallGraphReport",
    "CallPath",
    "CallSite",
    "FunctionNode",
    # Optimization Analysis
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
    # Formal Verification
    "TheoremProver",
    "ProofResult",
    "BoundsProver",
    "MemorySafetyProof",
    "CorrectnessProver",
    "AlgorithmProof",
]
