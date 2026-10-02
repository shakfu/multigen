# Backend Selection Guide

**Last Updated**: October 3, 2026
**Version**: v0.2.0
**MultiGen Backends**: 8, all passing 7/7 benchmarks

Performance figures are averages over the 7 benchmarks from one run on macOS arm64. Run times vary between runs; treat them as approximate.

---

## Quick Decision Tree

```text
┌─ Need WebAssembly or multi-platform?
│  └─ Use LLVM (compiles to any target via LLVM)
│
├─ Integrating with existing codebase?
│  ├─ C++ project? → Use C++
│  ├─ C project? → Use C
│  ├─ Rust project? → Use Rust
│  ├─ Go project? → Use Go
│  ├─ Haskell project? → Use Haskell
│  ├─ OCaml project? → Use OCaml
│  └─ TypeScript/Deno project? → Use TypeScript
│
├─ Maximum performance critical?
│  └─ Use LLVM (O3 optimization, 36% faster than O0)
│     or C++ (STL optimizations, small binaries)
│
├─ Smallest binary size?
│  └─ Use C++ (36KB) or LLVM (54KB)
│
├─ Fastest compilation?
│  └─ Use Go (179ms) or Rust (237ms)
│
└─ Learning functional programming?
   └─ Use Haskell or OCaml
```

---

## Backends Passing 7/7 Benchmarks

### C++ Backend

**Best for**: Integration with existing C++ codebases, small binaries, STL ecosystem

**Strengths**:

- [x] **Smallest binaries** (36KB for typical programs)
- [x] **Advanced type inference** (multi-pass, handles complex patterns)
- [x] **STL integration** (std::vector, std::unordered_map, std::unordered_set)
- [x] **Lambda comprehensions** (efficient list/dict comprehensions)
- [x] **Nested containers** (2D arrays, complex data structures)
- [x] **Header-only runtime** (357 lines, minimal overhead)

**Performance**:

- Compile time: 465ms
- Execute time: 269ms
- Binary size: 36KB

**Use Cases**:

- Integrating Python algorithms into C++ applications
- Performance-critical embedded systems with size constraints
- Projects already using C++ STL
- Quick prototyping with minimal runtime dependencies

**Example**:

```bash
multigen build -t cpp algorithm.py
./build/algorithm
```

---

### C Backend

**Best for**: Maximum portability, embedded systems, minimal dependencies

**Strengths**:

- [x] **Pure C99** (works on any platform with C compiler)
- [x] **Template-based containers** (9 types from 6 generic templates)
- [x] **STC library integration** (high-performance containers)
- [x] **2D arrays and nested types** (comprehensive support)
- [x] **Zero external dependencies** (self-contained runtime)
- [x] **File I/O and module imports** (complete feature parity)

**Performance**:

- Compile time: 405ms
- Execute time: 286ms
- Binary size: 95KB

**Runtime**: ~2,500 lines (16 files)

**Use Cases**:

- Embedded systems (IoT, microcontrollers)
- Legacy system integration
- Maximum portability across platforms
- Systems without C++ compiler

**Example**:

```bash
multigen build -t c data_processing.py
./build/data_processing
```

---

### Rust Backend

**Best for**: Memory safety guarantees, systems programming, production services

**Strengths**:

- [x] **Ownership-aware generation** (automatic cloning/dereferencing)
- [x] **Advanced HashMap inference** (detects reassignment patterns)
- [x] **Memory safety** (compile-time borrow checking)
- [x] **String handling** (strategic cloning for parameters)
- [x] 304-line runtime, pure std library

**Performance**:

- Compile time: 237ms (rustc)
- Execute time: 305ms
- Binary size: 469KB

**Use Cases**:

- Production web services with safety requirements
- CLI tools distributed as single binaries
- Systems programming with Python ergonomics
- Projects requiring strong safety guarantees

**Example**:

```bash
multigen build -t rust web_service.py
./build/web_service
```

---

### Go Backend

**Best for**: Fast compilation, concurrent services, distributed systems

**Strengths**:

- [x] **Fastest compilation** (179ms)
- [x] **Reflection-based comprehensions** (flexible, idiomatic)
- [x] **Goroutine-friendly** (works with Go concurrency)
- [x] **Pure std library** (413 lines, zero dependencies)
- [x] **Large binaries but self-contained** (includes Go runtime)

**Performance**:

- Compile time: 179ms **FASTEST**
- Execute time: 407ms
- Binary size: 2.3MB (includes Go runtime)

**Use Cases**:

- Microservices and distributed systems
- DevOps tooling (fast iteration cycles)
- Network services with concurrent requests
- Quick prototyping with instant compilation

**Example**:

```bash
multigen build -t go microservice.py
./build/microservice
```

---

### OCaml Backend

**Best for**: Functional programming, type safety, academic projects

**Strengths**:

- [x] **Functional paradigm** (pure functions with mutable refs where needed)
- [x] **Type-aware generation** (Python types → accurate OCaml constructs)
- [x] **Smart scoping** (sophisticated mutation detection)
- [x] **Pure std library** (Printf, List, Hashtbl - 216 lines)
- [x] **Ref-based state** (handles Python's mutability in functional style)

**Performance**:

- Compile time: 306ms
- Execute time: 391ms
- Binary size: 831KB

**Use Cases**:

- Academic research (functional programming studies)
- Type-safe transformations of Python code
- Learning functional programming concepts
- Formal verification projects

**Example**:

```bash
multigen build -t ocaml research.py
./build/research
```

---

### LLVM Backend

**Best for**: Cross-platform, optimization, WebAssembly, maximum performance

**Strengths**:

- [x] **Multi-target** (x86-64, ARM, RISC-V, WebAssembly via LLVM)
- [x] **Industry-standard optimization** (O0-O3, 60+ LLVM passes)
- [x] **36.5% performance gain** (O3 vs O0 on benchmarks)
- [x] **Memory-safe** (ASAN-verified, 0 leaks)
- [x] **Native compilation** (LLVM IR → object → executable via llvmlite)
- [x] **Future-proof** (GPU kernels, custom passes, PGO/LTO possible)

**Performance** (O2, default):

- Compile time: 331ms (IR generation + LLVM optimization)
- Execute time: 277ms
- Binary size: 54KB

**Optimization Levels**:

- `-O0` / `-O none`: No optimization (fastest compile, debugging)
- `-O1` / `-O basic`: Basic optimization (70% IR reduction)
- `-O2` / `-O moderate`: Default (balanced performance/compile time)
- `-O3` / `-O aggressive`: Maximum performance (36.5% faster execution)

**Runtime**: ~8,300 lines (comprehensive C runtime library)

**Use Cases**:

- **WebAssembly compilation** (Python → WASM)
- **Cross-compilation** (compile on x86, run on ARM)
- **Performance-critical** (leverage LLVM optimization passes)
- **Multi-platform tools** (single backend for all targets)
- **Future GPU/embedded targets** (via LLVM infrastructure)

**Example**:

```bash
# Default (O2)
multigen build -t llvm algorithm.py

# Maximum performance
multigen build -t llvm -O3 algorithm.py

# Debug build
multigen build -t llvm -O0 algorithm.py
```

### Haskell Backend

**Best for**: Pure functional programming, type safety, academic use

**Strengths**:

- [x] **Pure functional** (Data.Map, Data.Set, foldl/foldM)
- [x] **Type-safe** (strong Haskell type system)
- [x] **Visitor pattern** (separates main/IO from pure functions)
- [!] **In-place list mutation is refused** (see note below)

**Performance**:

- Compile time: 1036ms
- Execute time: 551ms
- Binary size: 19.8MB (includes GHC runtime)

**Use Cases**:

- Functional programming education
- Type-safe algorithm implementations
- Academic research
- Learning Haskell through Python

**Known Limitation**: In-place list mutation (`arr[i] = arr[j]`) cannot be
translated to pure Haskell and is refused. The benchmark runner uses a functional
quicksort variant (`quicksort_haskell.py`). Use functional patterns instead (list
comprehensions, recursive decomposition). See
[Haskell Backend Limitations](haskell_backend_limitations.md) for details and
working examples.

**Example**:

```bash
multigen build -t haskell algorithm.py
./build/algorithm
```

---

### TypeScript Backend

**Best for**: JavaScript/Deno ecosystems, prototyping

**Strengths**:

- [x] Native `class`, template-literal f-strings, array-method comprehensions
- [x] `Map`/`Set` containers keep key types, insertion order and `.size`
- [x] Python semantics via a 197-line runtime (floor division, modulo sign, truthiness)

**Limitations**:

- [!] `int` is a float64 `number`: exact only below 2**53
- [!] `deno compile --no-check`: TypeScript type errors are not caught

**Performance**:

- Compile time: 691ms (`deno compile`)
- Execute time: 994ms
- Binary size: 64.5MB (Deno runtime embedded)

**Example**:

```bash
multigen build -t typescript app.py
./build/app
```

---

## Backend Comparison Matrix

| Feature | C++ | C | Rust | Go | OCaml | LLVM | Haskell | TypeScript |
|---------|-----|---|------|----|----|------|---------|------------|
| **7/7 Benchmarks** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| **Compile Speed** | Medium | Medium | Fast | Fast | Fast | Medium | Slow | Slow |
| **Binary Size** | [+]Tiny | Small | Medium | Large | Medium | Tiny | Large | Largest |
| **Optimization** | High | High | High | Medium | Medium | [+]Highest | Low | JIT (V8) |
| **Memory Safety** | Manual | Manual | [+]Auto | GC | GC | Manual* | GC | GC |
| **Cross-Platform** | Good | [+]Best | Good | Good | Good | [+]Best | Good | Good |
| **Concurrency** | Manual | Manual | Native | [+]Native | Limited | Manual | Limited | Async (event loop) |
| **WebAssembly** | Via Emscripten | Via Emscripten | [x] Native | [x] TinyGo | Via js_of_ocaml | [x] Native | Via GHCJS | [X] |

\* *LLVM backend runtime is ASAN-verified memory-safe (0 leaks, 0 errors)*

---

## Recommendation by Use Case

### Embedded Systems / IoT

**Recommended**: C backend

- Minimal dependencies, C99 portable, small binaries

### Web Services / APIs

**Recommended**: Rust or Go

- Rust: Safety guarantees
- Go: Fast compilation, built-in concurrency

### Performance-Critical Applications

**Recommended**: LLVM (with `-O3`) or C++

- LLVM: 36% faster with O3 optimization
- C++: STL optimizations, smallest binaries

### CLI Tools / DevOps

**Recommended**: Go or Rust

- Go: Fastest compilation (179ms)
- Rust: Single binary with dependencies

### Cross-Platform / Multi-Target

**Recommended**: LLVM

- Compile once, target any platform (x86-64, ARM, RISC-V, WASM)

### Academic / Research

**Recommended**: Haskell or OCaml

- Pure functional paradigm
- Type safety guarantees

### Integration with Existing Code

**Recommended**: Backend matching your codebase language

- C++ → C++, Rust → Rust, etc.

### WebAssembly

**Recommended**: LLVM backend

- Direct LLVM IR → WebAssembly compilation
- 616-byte WASM objects (80% smaller than native)

---

## Performance Benchmarks (All 8 Backends at 7/7)

### Fibonacci (Recursive)

- **Fastest Execution**: C++ (266ms)
- **Smallest Binary**: C++ (34KB)
- **Fastest Compile**: Rust (152ms)

### Matrix Multiplication (2D Arrays)

- **Best Optimization**: LLVM O3 (36% faster than O0)
- **Most Portable**: C (pure C99)

### Quicksort (Lists)

- **Most Idiomatic**: Haskell (pure functional)
- **Best Performance**: LLVM O3 or C++

### String Operations (Wordcount)

- **Complete**: All 8 backends

---

## Quick Start Examples

### C++ - Small Binary, Fast Execution

```bash
# Install
pip install multigen

# Build
multigen build -t cpp algorithm.py

# Run
./build/algorithm
```

### LLVM - Maximum Performance

```bash
# Default (O2)
multigen build -t llvm algorithm.py

# Maximum optimization
multigen build -t llvm -O3 algorithm.py

# Profile
time ./build/algorithm
```

### Go - Fast Iteration

```bash
# Instant compilation
multigen build -t go service.py

# Run
./build/service
```

### Rust - Memory Safety

```bash
# Build with safety checks
multigen build -t rust app.py

# Run
./build/app
```

---

## Migration Path

If you're unsure which backend to choose:

1. **Start with C++** - Good balance of performance, size, and compatibility
2. **Profile your code** - Identify performance bottlenecks
3. **Switch to LLVM with O3** - If you need maximum performance
4. **Switch to Go** - If compile time is critical
5. **Switch to Rust** - If you need safety guarantees
6. **Use language-specific backend** - When integrating with existing code

---

## Getting Help

- **Documentation**: See `docs/backend_comparison.md` for technical details
- **LLVM Guide**: See `docs/dev/llvm_backend_guide.md` for LLVM-specific features
- **Issues**: Report at <https://github.com/anthropics/multigen/issues>
- **Examples**: Check `tests/benchmarks/` for real-world code

---

## Summary

**For most users**: Start with **C++** (balanced) or **LLVM** (maximum performance/flexibility)

**For production**: Use **Rust** (safety) or **Go** (speed)

**For embedded**: Use **C** (portability)

**For research**: Use **Haskell** or **OCaml** (functional paradigm)

**For JavaScript/Deno**: Use **TypeScript**

All backends are actively maintained.
