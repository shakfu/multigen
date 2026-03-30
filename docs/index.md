# MultiGen Documentation

**MultiGen** (Multi-Language Generator) translates Python code to C, C++, Rust, Go, Haskell, OCaml, and LLVM IR with zero external runtime dependencies.

## Features

- **7 Production Backends**: C, C++, Rust, Go, Haskell, OCaml, LLVM
- **Formal Verification**: Optional Z3-based memory safety proofs
- **Type Inference**: Automatic type detection for containers and functions
- **Zero Dependencies**: Self-contained runtime libraries
- **1353 Tests**: Comprehensive test suite with 100% benchmark pass rate (49/49)

## Quick Start

Installation:

```bash
pip install multigen
```

Basic Usage:

```bash
# Convert Python to C
multigen convert -t c example.py

# Validate without converting
multigen check example.py

# Build with Makefile generation
multigen build -t c example.py -m
```
