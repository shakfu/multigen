# Backend Guide

MultiGen supports 7 production-ready backends, each with different characteristics and use cases.

## Backend Overview

| Backend  | Benchmarks | Compile Time | Binary Size | Runtime   |
|----------|------------|-------------|-------------|-----------|
| C        | 7/7 (100%) | 390ms       | 95KB        | 2,500 LOC |
| C++      | 7/7 (100%) | 435ms       | 36KB        | 357 LOC   |
| Rust     | 7/7 (100%) | ~1500ms     | 443KB       | 304 LOC   |
| Go       | 7/7 (100%) | 190ms       | 2365KB      | 413 LOC   |
| Haskell  | 7/7 (100%) | 156ms       | 19734KB     | 214 LOC   |
| OCaml    | 7/7 (100%) | 234ms       | 826KB       | 216 LOC   |
| LLVM     | 7/7 (100%) | 310ms       | 49KB        | 8,300 LOC |

## C Backend

**Best for**: Embedded systems, maximum portability, minimal binary size

Features:

- Template-based container system (STC library integration)
- 2D array support
- File I/O and module imports
- Zero external dependencies

```bash
multigen convert -t c example.py
```

Runtime: ~2,500 lines across 16 files, using STC containers with custom fallback.

## C++ Backend

**Best for**: Performance, STL integration, modern C++ projects

Features:

- Header-only runtime (357 lines)
- Multi-pass type inference
- Nested containers (`vector<vector<int>>`)
- Lambda-based comprehensions
- String-keyed maps

```bash
multigen convert -t cpp example.py
```

Smallest runtime (357 LOC), smallest binary (36KB).

## Rust Backend

**Best for**: Memory safety, systems programming, modern projects

Features:

- Ownership-aware code generation
- HashMap type inference
- Automatic cloning/dereferencing
- Immutability analysis
- Pure std library (304 lines)

```bash
multigen convert -t rust example.py
```

Advanced type inference detects function call reassignments for accurate HashMap usage.

## Go Backend

**Best for**: Concurrent systems, cloud services, simplicity

Features:

- Reflection-based comprehensions
- Idiomatic Go patterns
- Fast compilation (190ms)
- Pure std library (413 lines)

```bash
multigen convert -t go example.py
```

Fast compilation, good for rapid iteration.

## Haskell Backend

**Best for**: Functional programming, type safety, academic use

Features:

- Pure functional paradigm
- Data.Map and Data.Set
- Visitor pattern for statement conversion
- Backend-specific quicksort variant (list comprehensions)
- Pure std library (214 lines)

```bash
multigen convert -t haskell example.py
```

## OCaml Backend

**Best for**: Functional programming, fast compilation, type inference

Features:

- Mutable reference system
- Type-aware code generation
- Smart scoping for mutations
- Association lists for dicts
- Pure std library (216 lines)

```bash
multigen convert -t ocaml example.py
```

## LLVM Backend

**Best for**: Native performance, cross-compilation, optimization

Features:

- Native compilation via llvmlite
- O0-O3 optimization levels with 60+ LLVM passes
- Multi-platform targets (x86-64, ARM, RISC-V)
- Full container support (vec, map, set)
- C runtime library (~8,300 lines)

```bash
multigen convert -t llvm example.py
multigen build -t llvm example.py -O aggressive
```

## Choosing a Backend

**For embedded systems**: Use **C** (smallest binary, maximum portability)

**For performance-critical code**: Use **C++** (STL, fast compile, small binary)

**For memory safety**: Use **Rust** (ownership system, formal verification)

**For microservices**: Use **Go** (fast compile, good concurrency)

**For functional projects**: Use **Haskell** or **OCaml** (pure functional)

**For native performance**: Use **LLVM** (optimization passes, cross-compilation)

## Common Features

All backends support:

- Functions and recursion
- Classes and OOP
- Lists, dicts, sets
- Nested containers (2D arrays)
- List/dict/set comprehensions
- List and string slicing
- File I/O and Path operations
- Module imports
- Type annotations
- Augmented assignment (`+=`, `-=`, etc.)
- String methods
- F-strings with format specs (`.2f`, `x`, `d`)
- Built-in functions (len, min, max, sum, range, print)
- Exception handling (try/except/else/finally)
- Context managers (with statement)
- Generators (yield, yield from)

## Backend-Specific Limitations

**Haskell**:

- Imperative loops converted to folds
- Mutable state requires different patterns
- Backend-specific quicksort uses list comprehensions

**LLVM**:

- List and string slicing not yet supported (requires IR-level changes)

**All backends**:

- No async/await
- No lambda functions
- No `raise ... from ...` (exception chaining)

## Next Steps

- [Backend API Reference](../api/backends.md) -- Backend API reference
- [Architecture](../dev-guide/architecture.md) -- Understanding backend architecture
