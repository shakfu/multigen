# MultiGen TODO

Tasks extracted from PROJECT_REVIEW.md (2025-12-27).

---

## High Priority

### ~~Fix Type Annotation Syntax~~ (NOT AN ISSUE)
- [x] **Location**: `src/multigen/backends/c/converter.py:1515`
- [x] **Status**: Not a problem - `from __future__ import annotations` makes `| None` syntax work on Python 3.9+
- [x] **Verification**: All 944 tests pass, mypy only fails on missing llvmlite/z3 stubs (optional deps)

### ~~Strengthen Phase 4 (Mapping)~~ COMPLETE
- [x] **Status**: Phase 4 implemented (`pipeline.py:674-735`) with SemanticMapping dataclass
- [x] **Computed**: type_mappings, container_mappings, function_return_types, variable_types, required_imports
- [x] **Integration**: `emit_module` now accepts optional `semantic_mapping` parameter (all 7 backends updated)
- [x] **Tests**: 7 new tests in `tests/test_pipeline.py::TestPipelinePhase4Mapping`
- [x] **Future**: Backends can now use pre-computed mappings instead of re-computing (opt-in)

### ~~Haskell Quicksort Benchmark~~ DOCUMENTED
- [x] **Issue**: Imperative quicksort uses in-place array mutations, incompatible with Haskell's purity
- [x] **Impact**: 6/7 benchmarks (86%) - this is a fundamental paradigm limitation, not a bug
- [x] **Documentation**: Comprehensive guide at `docs/haskell_backend_limitations.md`
- [x] **Example**: Functional quicksort at `tests/examples/algorithms/functional_quicksort.py`
- [x] **Tests**: 3 tests in `tests/test_backend_haskell_functional_quicksort.py`
- **Note**: Getting 7/7 would require changing the benchmark itself or using IORef/ST monad (future)

---

## Medium Priority

### Expand Test Coverage
- [ ] Add 6+ tests for OCaml (containers, comprehensions)
- [ ] Add 6+ tests for LLVM (optimization levels, targets)
- [ ] Add WebAssembly integration tests

### Refactor Large Converter Files
- [ ] Extract string method handling from C converter (~300 LOC)
- [ ] Consider breaking down converters >2,500 LOC:
  - `backends/c/converter.py`: 3,134 LOC
  - `backends/llvm/ir_to_llvm.py`: 2,294 LOC
  - `backends/rust/converter.py`: 2,172 LOC
  - `backends/go/converter.py`: 2,002 LOC

### Standardize Documentation
- [ ] Apply Haskell-style docstrings (current best practice) across all backends

### Share Type Inference
- [ ] Extract C's EnhancedTypeInferenceEngine to shared module for use by Go/other backends

---

## Low Priority

### Target Optimization Phase
- [ ] Define `AbstractOptimizer` interface
- [ ] Require implementations from all backends (currently only LLVM has optimization)

### Typed Phase Communication
- [ ] Replace string-keyed dictionaries with typed data classes for phase results
- [ ] Examples: `ValidationPhaseOutput`, `AnalysisPhaseOutput`

### Builder Pattern Consistency
- [ ] Extract common build logic to base class
- [ ] Currently varies from subprocess calls to complex abstractions

---

## Long-Term / Future Releases

### WebAssembly
- [ ] Phase 2: JavaScript runtime bridge
- [ ] Phase 3: WASI support
- [ ] Phase 4: Emscripten integration

### JIT Compilation
- [ ] JIT compilation infrastructure (LLVM ORC)

### IDE Integration
- [ ] LSP server implementation

### Language Features
- [ ] Exception handling (try/except)
- [ ] Context managers (with statement)
- [ ] Generator/yield support

---

## Technical Debt Summary

| Category | Severity | Count | Notes |
|----------|----------|-------|-------|
| TODOs | Low | 15 | Non-critical, well-documented |
| Large Files | Medium | 4 | Converters 2,000+ LOC |
| ~~Type Errors~~ | ~~High~~ | ~~0~~ | Resolved - `__future__.annotations` handles it |
| Missing Tests | Medium | 2 | OCaml, LLVM under-tested |
| ~~Hollow Phases~~ | ~~Medium~~ | ~~1~~ | Phase 4 now integrated, Phase 5 still minimal |

**Updated**: 2026-01-25 - High priority items addressed
