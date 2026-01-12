# MultiGen TODO

Tasks extracted from PROJECT_REVIEW.md (2025-12-27).

---

## High Priority

### Fix Type Annotation Syntax
- [ ] **Location**: `src/multigen/backends/c/converter.py:1542`
- [ ] **Issue**: Uses `list[...] | None` (Python 3.10+) but project targets Python 3.9+
- [ ] **Impact**: mypy type-check fails
- [ ] **Solution**: Replace with `Optional[list[...]]` from typing module

### Strengthen Phase 4 (Mapping)
- [ ] **Issue**: Currently a pass-through with empty type/container mappings
- [ ] **Impact**: Semantic transformation logic scattered across backends
- [ ] **Solution**: Implement proper Python-to-target semantic mappings as explicit transformations

### Haskell Quicksort Benchmark
- [ ] **Issue**: Imperative quicksort algorithm incompatible with pure functional paradigm
- [ ] **Impact**: 6/7 benchmarks (86%) vs 7/7 for other backends
- [ ] **Solution**: Document limitation and provide functional sort alternative in examples

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
| Type Errors | High | 1 | Python 3.10 syntax in 3.9 project |
| Missing Tests | Medium | 2 | OCaml, LLVM under-tested |
| Hollow Phases | Medium | 2 | Phase 4, Phase 5 minimal |
