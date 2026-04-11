# Backends API

Backend base classes and implementations.

## Base Classes

### LanguageBackend

::: multigen.backends.base.LanguageBackend

### AbstractEmitter

::: multigen.backends.base.AbstractEmitter

### AbstractFactory

::: multigen.backends.base.AbstractFactory

## Backend Modules

### C Backend

::: multigen.backends.c

### C++ Backend

::: multigen.backends.cpp

### Rust Backend

::: multigen.backends.rust

### Go Backend

::: multigen.backends.go

### Haskell Backend

::: multigen.backends.haskell

### OCaml Backend

::: multigen.backends.ocaml

### LLVM Backend

::: multigen.backends.llvm

## Backend Selection

Backends are selected via the `target_language` parameter:

```python
from multigen.pipeline import PipelineConfig

# C backend
config = PipelineConfig(target_language="c")

# Rust backend
config = PipelineConfig(target_language="rust")
```

Available backends: `c`, `cpp`, `rust`, `go`, `haskell`, `ocaml`, `llvm`

## Extending Backends

To add a new backend:

1. Subclass `LanguageBackend`
2. Implement required abstract methods
3. Create backend-specific emitter, factory, builder
4. Register in `multigen.backends.registry`

See `CLAUDE.md` for detailed architecture notes.
