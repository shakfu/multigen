# MultiGen Backend Comprehensive Comparison

**Last Updated**: October 3, 2026
**Version**: v0.2.0
**Backends**: 8 (C, C++, Rust, Go, Haskell, OCaml, LLVM, TypeScript)

---

## Executive Summary

MultiGen supports **8 backends**. All eight pass 7/7 benchmarks, each checked against CPython's output: 56/56 runs on macOS arm64 with all eight toolchains installed.

The feature tables below predate the TypeScript backend and have no TypeScript column. For measured per-backend support, including TypeScript, run `make capabilities` (`backend_capabilities.json`).

---

## Container Type Support

| Backend | List Types | Dict Types | Set Types | Nested Containers |
|---------|-----------|------------|-----------|-------------------|
| **C++** | `std::vector<int>`, `std::vector<float>`, `std::vector<double>`, `std::vector<string>`, nested vectors | `std::unordered_map<int,int>`, `std::unordered_map<string,int>`, `std::unordered_map<string,string>` | `std::unordered_set<int>`, `std::unordered_set<string>` | [x] Full - 2D arrays, nested vectors |
| **C** | `vec_int`, `vec_float`, `vec_double`, `vec_cstr`, `vec_vec_int` | `map_int_int`, `map_str_str`, `str_int_map` | `set_int`, `set_str` | [x] Full - 9+ types from 6 templates |
| **Rust** | `Vec<i64>`, `Vec<f64>`, `Vec<String>`, nested vectors | `HashMap<i64,i64>`, `HashMap<String,i64>`, `HashMap<String,String>` | `HashSet<i64>`, `HashSet<String>` | [x] Full with ownership tracking |
| **Go** | `[]int`, `[]float64`, `[]string`, nested slices | `map[int]int`, `map[string]int`, `map[string]string` | `map[T]bool` (sets as maps) | [x] Full via generics |
| **Haskell** | `[Int]`, `[Double]`, `[String]`, nested lists | `Data.Map.Map k v` (ordered) | `Data.Set.Set a` (ordered) | [x] Full with pure semantics |
| **OCaml** | `int list`, `float list`, `string list`, nested | `(k * v) list` (assoc lists) | Lists with deduplication | [!] Basic - uses lists |
| **LLVM** | `vec_int*`, `vec_str*`, `vec_vec_int*` | `map_int_int*`, `map_str_int*` | `set_int*` | [!] Partial - 2D arrays supported |

---

## Container Methods - Lists

| Backend | append | insert | extend | remove | pop | clear | indexing | slicing | len |
|---------|--------|--------|--------|--------|-----|-------|----------|---------|-----|
| **C++** | [x] `push_back()` | [X] | [X] | [X] | [X] | [X] | [x] `[]` | [X] | [x] `size()` |
| **C** | [x] `vec_T_push()` | [X] | [X] | [X] | [x] `vec_T_pop()` | [x] `vec_T_clear()` | [x] `vec_T_at()` | [!] Limited | [x] `vec_T_size()` |
| **Rust** | [x] `push()` | [X] | [X] | [X] | [X] | [X] | [x] `[]` | [!] Limited | [x] `len()` |
| **Go** | [x] `append()` | [X] | [X] | [X] | [X] | [X] | [x] `[]` | [x] `[:]` | [x] `len()` |
| **Haskell** | [x] via `++` | [X] | [X] | [X] | [X] | [X] | [x] `!!` | [x] `take/drop` | [x] `length` |
| **OCaml** | [x] `list_append()` | [X] | [X] | [X] | [X] | [X] | [x] `List.nth` | [X] | [x] `List.length` |
| **LLVM** | [x] `vec_int_push()` | [X] | [X] | [X] | [X] | [X] | [x] `vec_int_at()` | [!] Limited | [x] `vec_int_size()` |

### Missing List Methods (All Backends)

- `insert(index, item)` - Insert at position
- `extend(other)` - Append multiple items
- `remove(item)` - Remove first occurrence
- `reverse()` - Reverse in-place
- `sort()` - Sort in-place

---

## Container Methods - Dicts

| Backend | indexing | insert | get | contains (in) | keys | values | items | clear | erase/remove |
|---------|----------|--------|-----|---------------|------|--------|-------|-------|--------------|
| **C++** | [x] `[]` | [x] `[]` | [x] `[]` | [x] `count()` | [X] | [x] `multigen::values()` | [x] iteration | [X] | [X] |
| **C** | [x] `map_KV_get()` | [x] `map_KV_insert()` | [x] `map_KV_get()` | [x] `map_KV_contains()` | [X] | [X] | [X] | [x] `map_KV_clear()` | [x] `map_KV_erase()` |
| **Rust** | [x] `get()/insert()` | [x] `insert()` | [x] `get()` | [x] `contains_key()` | [X] | [X] | [X] | [X] | [X] |
| **Go** | [x] `m[k]` | [x] `m[k]=v` | [x] `m[k]` | [x] `_, ok := m[k]` | [X] | [x] `MapValues()` | [x] `MapItems()` | [X] | [x] `delete()` |
| **Haskell** | [x] `Map.lookup` | [x] `Map.insert` | [x] `Map.lookup` | [x] `Map.member` | [x] `keys()` | [x] `values()` | [x] `items()` | [X] | [X] |
| **OCaml** | [x] assoc lookup | [x] cons | [X] | [x] `List.mem_assoc` | [X] | [X] | [X] | [X] | [X] |
| **LLVM** | [x] `map_KV_get()` | [x] `map_KV_insert()` | [x] `map_KV_get()` | [x] `map_KV_contains()` | [X] | [X] | [X] | [X] | [X] |

### Missing Dict Methods (Most Backends)

- `keys()` - Returns list of keys (only Haskell)
- `values()` - Returns list of values (C++, Go, Haskell)
- `items()` - Returns key-value pairs (Go, Haskell)
- `get(key, default)` - Safe access with default
- `clear()` - Remove all items (only C)

---

## Container Methods - Sets

| Backend | add/insert | remove | discard | clear | contains (in) | union | intersection | difference |
|---------|------------|--------|---------|-------|---------------|-------|--------------|------------|
| **C++** | [x] `insert()` | [X] | [X] | [X] | [x] `count()` | [X] | [X] | [X] |
| **C** | [x] `set_T_insert()` | [x] `set_T_erase()` | [x] `set_T_erase()` | [x] `set_T_clear()` | [x] `set_T_contains()` | [X] | [X] | [X] |
| **Rust** | [x] `insert()` | [X] | [X] | [X] | [x] `contains()` | [X] | [X] | [X] |
| **Go** | [x] `m[k]=true` | [x] `delete()` | [x] `delete()` | [X] | [x] `m[k]` | [X] | [X] | [X] |
| **Haskell** | [x] `Set.insert` | [X] | [X] | [X] | [x] `Set.member` | [x] `Set.union` | [x] `Set.intersection` | [x] `Set.difference` |
| **OCaml** | [x] via dedup | [X] | [X] | [X] | [x] `List.mem` | [X] | [X] | [X] |
| **LLVM** | [x] `set_int_insert()` | [X] | [X] | [X] | [x] `set_int_contains()` | [X] | [X] | [X] |

### Missing Set Methods (Most Backends)

- `remove(item)` - Remove with error if missing (only C)
- `discard(item)` - Remove without error (C, Go)
- `clear()` - Remove all elements (only C)
- Set operators: `|` (union), `&` (intersection), `-` (difference) - only Haskell

---

## String Operations

| Backend | upper | lower | strip | split | join | replace | find | startswith | endswith |
|---------|-------|-------|-------|-------|------|---------|------|------------|----------|
| **C++** | [x] | [x] | [x] | [x] | [X] | [x] | [x] | [X] | [X] |
| **C** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] |
| **Rust** | [x] | [x] | [x] | [x] | [X] | [x] | [x] | [X] | [X] |
| **Go** | [x] | [x] | [x] | [x] | [X] | [x] | [x] | [X] | [X] |
| **Haskell** | [x] | [x] | [x] | [x] | [X] | [x] | [x] | [X] | [X] |
| **OCaml** | [x] | [x] | [x] | [x] | [X] | [x] | [x] | [X] | [X] |
| **LLVM** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |

### Missing String Methods (Most Backends)

- `startswith(prefix)` - All except LLVM (v0.1.83)
- `endswith(suffix)` - All except LLVM (v0.1.83)
- `join()` - C++, Rust, Go, Haskell, OCaml (C and LLVM have it)

---

## Built-in Functions

| Backend | len | range | print | min | max | sum | abs | bool | enumerate | zip |
|---------|-----|-------|-------|-----|-----|-----|-----|------|-----------|-----|
| **C++** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] |
| **C** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| **Rust** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] |
| **Go** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] | [X] |
| **Haskell** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] |
| **OCaml** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] |
| **LLVM** | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [X] | [X] | [X] |

### Missing Built-in Functions (Most Backends)

- `enumerate(iterable)` - Only C backend
- `zip(*iterables)` - Only C backend
- `sorted(iterable)` - All backends
- `reversed(iterable)` - All backends
- `all(iterable)` - All backends
- `any(iterable)` - All backends

---

## Comprehensions Support

| Backend | List Comp. | Dict Comp. | Set Comp. | Nested Comp. | Filters | Multiple Iterators |
|---------|-----------|-----------|-----------|-------------|---------|-------------------|
| **C++** | [x] Lambda | [x] Lambda | [x] Lambda | [x] | [x] | [X] |
| **C** | [x] Loop | [x] Loop | [x] Loop | [x] | [x] | [X] |
| **Rust** | [x] Iterator | [x] Iterator | [x] Iterator | [x] | [x] | [X] |
| **Go** | [x] Reflection | [x] Reflection | [x] Reflection | [x] | [x] | [X] |
| **Haskell** | [x] Native | [x] `fromList` | [x] `fromList` | [x] | [x] | [X] |
| **OCaml** | [x] `List.map` | [x] Manual | [x] Dedup | [!] Limited | [x] `filter` | [X] |
| **LLVM** | [x] Loop | [x] Loop | [x] Hash set | [x] | [x] | [X] |

---

## Performance Metrics (from benchmark suite)

Averages over the 7 benchmarks from one run on macOS arm64. Run times vary between runs by up to 10x for the fastest backends; treat the ranking as approximate.

### Execution Time (Average)

| Rank | Backend | Avg Runtime |
|------|---------|-------------|
| 1 | **C++** | 268.7ms |
| 2 | **LLVM** | 276.9ms |
| 3 | **C** | 286.1ms |
| 4 | **Rust** | 304.5ms |
| 5 | **OCaml** | 391.1ms |
| 6 | **Go** | 407.0ms |
| 7 | **Haskell** | 550.6ms |
| 8 | **TypeScript** | 993.6ms |

### Compilation Time (Average)

| Rank | Backend | Avg Compile |
|------|---------|-------------|
| 1 | **Go** | 178.5ms |
| 2 | **Rust** | 237.2ms |
| 3 | **OCaml** | 305.6ms |
| 4 | **LLVM** | 330.7ms |
| 5 | **C** | 404.6ms |
| 6 | **C++** | 464.5ms |
| 7 | **TypeScript** | 690.8ms |
| 8 | **Haskell** | 1035.8ms |

### Binary Size (Average)

| Rank | Backend | Avg Size |
|------|---------|----------|
| 1 | **C++** | 36.1KB |
| 2 | **LLVM** | 53.7KB |
| 3 | **C** | 94.9KB |
| 4 | **Rust** | 468.5KB |
| 5 | **OCaml** | 831.2KB |
| 6 | **Go** | 2.3MB |
| 7 | **Haskell** | 19.8MB |
| 8 | **TypeScript** | 64.5MB |

### Generated Code Size (Lines of Code)

| Rank | Backend | Avg LOC |
|------|---------|---------|
| 1 | **OCaml** | 27 |
| 2 | **Rust** | 37 |
| 3 | **Go** | 38 |
| 4 | **TypeScript** | 38 |
| 5 | **C++** | 51 |
| 6 | **Haskell** | 65 |
| 7 | **C** | 76 |
| 8 | **LLVM** | 327 |

---

## Special Features & Characteristics

### C++ Backend

**Strengths:**

- [x] STL integration (best library support)
- [x] Multi-pass type inference (most sophisticated)
- [x] Lambda-based comprehensions (clean code)
- [x] Smallest binaries (36KB average)
- [x] Header-only runtime (357 lines)

**Weaknesses:**

- [X] Slower compilation (465ms)
- [X] Missing dict methods (keys)
- [X] No list operations (insert, remove)

**Best For:** Production deployments requiring small binaries and C++ ecosystem integration

---

### C Backend

**Strengths:**

- [x] Template system (6 templates → 9+ types)
- [x] Most complete runtime (2,500 lines)
- [x] Strategy pattern for operations
- [x] Full STC containers + fallback
- [x] Only backend with enumerate/zip
- [x] Most container methods (pop, clear, erase)

**Weaknesses:**

- [X] Compilation 405ms
- [X] Verbose generated code (76 LOC avg)
- [X] Manual memory management

**Best For:** Systems programming, embedded, maximum control

---

### Rust Backend

**Strengths:**

- [x] Ownership-aware generation
- [x] HashMap type inference (function call detection)
- [x] Auto dereferencing/cloning
- [x] Memory safety guarantees
- [x] Fast compilation (237ms)

**Weaknesses:**

- [X] Larger binaries (469KB)
- [X] Missing container methods
- [X] No dict.keys/values

**Best For:** Safety-critical applications, modern Rust codebases

---

### Go Backend

**Strengths:**

- [x] **Fastest compilation** (179ms)
- [x] Generics (Go 1.18+)
- [x] Reflection-based comprehensions
- [x] Idiomatic Go patterns
- [x] dict.values() and dict.items()

**Weaknesses:**

- [X] Large binaries (2.3MB, Go runtime included)
- [X] No bool conversion
- [X] Sets via maps (not true sets)

**Best For:** Microservices, cloud deployments, performance-critical code

---

### Haskell Backend

**Strengths:**

- [x] Pure functional semantics
- [x] Visitor pattern (main vs pure functions)
- [x] Strongest type system
- [x] Native set operations (union, intersection, difference)
- [x] dict.keys/values/items support

**Weaknesses:**

- [X] Large binaries (19.8MB, GHC runtime)
- [X] Slowest compilation (1036ms)
- [X] In-place list mutation is refused; the quicksort benchmark uses a functional variant (`quicksort_haskell.py`)
- [X] Limited type inference for containers

**Best For:** Functional programming projects, academic research, provably correct code

---

### OCaml Backend

**Strengths:**

- [x] **Most concise code** (27 LOC)
- [x] **Fastest compile time** among functional languages (306ms)
- [x] Mutable references system with smart scoping
- [x] Type-aware generation
- [x] Sophisticated mutation detection
- [x] Functional + imperative hybrid

**Weaknesses:**

- [X] Uses association lists (not hash tables)
- [X] Limited nested container support
- [X] No dict.keys/values
- [X] Basic set support via lists

**Best For:** Functional programming with mutations, OCaml ecosystem integration

---

### LLVM Backend

**Strengths:**

- [x] **2nd fastest execution** (277ms) in the latest run
- [x] **2nd smallest binaries** (54KB)
- [x] Direct IR generation (no intermediate C/C++)
- [x] Dual compilation modes (AOT + JIT)
- [x] JIT: 7.7x faster development cycle
- [x] Index-based set iteration
- [x] **Most complete string operations** (9 methods including join, startswith, endswith) - v0.1.83
- [x] **Better error messages** (descriptive runtime errors) - v0.1.83
- [x] **Memory-safe** (ASAN verified, 0 leaks) - v0.1.82
- [x] **107 comprehensive tests** (723% increase) - v0.1.83

**Weaknesses:**

- [X] Verbose IR (327 LOC average)
- [X] Manual memory management (~8,300 lines C runtime)
- [X] Newest backend (less mature)
- [X] Limited OOP support
- [X] `dict.items()` only over `dict[int, int]`

**Best For:** Research, compiler development, cross-platform targets, fast iteration (JIT), string-heavy applications

---

### TypeScript Backend

**Strengths:**

- [x] Native `class`, template-literal f-strings, array-method comprehensions
- [x] `Map`/`Set` containers keep key types, insertion order and `.size`
- [x] Python semantics via a 197-line runtime (`floorDiv`, `pyMod`, `split`, truthiness)
- [x] Concise code (38 LOC average)

**Weaknesses:**

- [X] **Largest binaries** (64.5MB, Deno runtime embedded)
- [X] Slowest execution in the latest run (994ms)
- [X] `int` is a float64 `number`: exact only below 2**53
- [X] Builds with `deno compile --no-check`, so TypeScript type errors are not caught

**Best For:** JavaScript/Deno ecosystems, prototyping

---

## Benchmark Results Summary

### Overall Success Rates

| Backend | Success Rate | Benchmarks Passing |
|---------|--------------|-------------------|
| **C++** | 100% (7/7) | All |
| **C** | 100% (7/7) | All |
| **Rust** | 100% (7/7) | All |
| **Go** | 100% (7/7) | All |
| **OCaml** | 100% (7/7) | All |
| **LLVM** | 100% (7/7) | All |
| **Haskell** | 100% (7/7) | All |
| **TypeScript** | 100% (7/7) | All |

### Benchmark Breakdown

| Benchmark | C++ | C | Rust | Go | Haskell | OCaml | LLVM | TypeScript |
|-----------|-----|---|------|----|----|-------|------|------------|
| fibonacci | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| matmul | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| quicksort | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| list_ops | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| dict_ops | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| set_ops | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| wordcount | [x] | [x] | [x] | [x] | [x] | [x] | [x] | [x] |

**Haskell quicksort**: in-place mutation is refused, so the benchmark runner selects the functional variant `quicksort_haskell.py`.

---

## Type Inference Capabilities

| Backend | Strategy | Constant | BinOp | Container Elements | Function Returns | Nested Types |
|---------|----------|----------|-------|-------------------|------------------|--------------|
| **C++** | Multi-pass | [x] | [x] | [x] String-keyed dicts | [x] | [x] Vector of vectors |
| **C** | Strategy pattern | [x] | [x] | [x] Template-based | [x] | [x] 2D arrays |
| **Rust** | Strategy pattern | [x] | [x] | [x] HashMap detection | [x] | [x] Full |
| **Go** | Strategy pattern | [x] | [x] | [x] Reflection-based | [x] | [x] Full |
| **Haskell** | Basic | [x] | [!] | [!] Limited | [x] | [!] Limited |
| **OCaml** | Type-aware | [x] | [x] | [!] Limited | [x] | [!] Basic |
| **LLVM** | Comprehensive | [x] | [x] | [x] List/dict elements | [x] | [x] 2D support |

---

## Design Patterns by Backend

| Pattern | C++ | C | Rust | Go | Haskell | OCaml | LLVM |
|---------|-----|---|------|----|----|-------|------|
| **Strategy** | [x] Type inference | [x] Type inference<br>[x] Container ops | [x] Type inference | [x] Type inference | [x] Loop conversion | [x] Loop conversion | [X] |
| **Visitor** | [X] | [X] | [X] | [X] | [x] Statement conv | [X] | [X] |
| **Factory** | [X] | [x] Container creation | [X] | [X] | [X] | [X] | [X] |
| **Template Method** | [X] | [x] Parameterized templates | [X] | [X] | [X] | [X] | [X] |

**Design Pattern Impact:**

- C++/Rust/Go: 53→8 complexity (85% reduction) via Strategy
- C: 66→10 complexity (85% reduction) via Strategy
- Haskell: 69→15 complexity (78% reduction) via Visitor + 40→8 (80%) via Strategy
- OCaml: 40→8 complexity (80% reduction) via Strategy

---

## Memory Management

| Backend | Model | Automatic Cleanup | Manual Management | Reference Counting | Ownership |
|---------|-------|------------------|-------------------|-------------------|-----------|
| **C++** | RAII | [x] Destructors | [X] | [X] | Move semantics |
| **C** | Manual | [X] | [x] `*_drop()` | [X] | Manual |
| **Rust** | Ownership | [x] Automatic | [X] | [x] `Rc<T>` | [x] Compiler-enforced |
| **Go** | GC | [x] Automatic | [X] | [X] | GC-managed |
| **Haskell** | GC | [x] Automatic | [X] | [X] | Pure functional |
| **OCaml** | GC | [x] Automatic | [X] | [X] | Ref cells |
| **LLVM** | Manual | [X] | [x] Explicit `free()` | [X] | Manual |

---

## Advanced Features

| Feature | C++ | C | Rust | Go | Haskell | OCaml | LLVM |
|---------|-----|---|------|----|----|-------|------|
| Recursion | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| Mutual Recursion | [x] | [x] | [x] | [x] | [x] | [x] | [x] |
| Classes/OOP | [x] | [x] Structs | [x] Structs | [x] Structs | [x] Data types | [x] Records | [!] Limited |
| Inheritance | [X] | [X] | [X] | [X] | [X] | [X] | [X] |
| File I/O | [x] | [x] | [x] | [x] | [x] | [x] | [!] Limited |
| Module Imports | [x] | [x] | [x] | [x] | [x] | [x] | [!] Limited |
| Global Variables | [x] | [x] | [x] | [x] | [x] | [x] | [x] |

---

## Common Limitations (All Backends)

### Not Implemented

- [X] Decorators
- [X] Async/await
- [X] Metaclasses
- [X] Multiple inheritance (most backends)
- [X] Operator overloading (user-defined)

### Partially Implemented

- [!] Exception handling, generators (eager), context managers: see `docs/supported_syntax.md`

- [!] Slicing (basic support, not full Python semantics)
- [!] List methods (append only, no insert/remove/pop in most)
- [!] Dict methods (`items()` unpacking on all backends; keys/values missing in several)
- [!] Set operations (basic only, no operators)

---

## Recommendations by Use Case

### For Production Deployments

**Best Choice: C++ or LLVM**

- Smallest binaries (36-54KB)
- Good performance
- No runtime dependencies
- Mature ecosystems

### For Development Speed

**Best Choice: Go**

- Fastest compilation (179ms)
- Simple code generation
- Large binaries acceptable in cloud

### For Safety-Critical Systems

**Best Choice: Rust**

- Memory safety guarantees
- Ownership tracking
- No null pointer errors
- Moderate binary size

### For Functional Programming

**Best Choice: Haskell or OCaml**

- Pure functional semantics (Haskell)
- Hybrid functional/imperative (OCaml)
- Concise code (OCaml: 27 LOC average)
- Strong type systems

### For Embedded/Systems

**Best Choice: C**

- Most complete runtime
- Full control over memory
- Template system
- No external dependencies

### For Research/Experimentation

**Best Choice: LLVM**

- Direct IR access
- JIT compilation (7.7x faster dev)
- Cross-platform targets
- Newest technology

---

## Feature Completion Roadmap

### Near Term (v0.1.x - v0.2.x)

**Priority 1: Missing Container Methods**

- [ ] `list.insert(index, item)` - All backends
- [ ] `list.remove(item)` - All backends
- [ ] `list.extend(other)` - All backends
- [ ] `dict.keys()` - C++, C, Rust, OCaml, LLVM
- [ ] `dict.values()` - C, Rust, OCaml, LLVM
- [ ] `set.remove(item)` - C++, Rust, Haskell, OCaml, LLVM
- [ ] `set.clear()` - C++, Rust, Go, Haskell, OCaml, LLVM

**Priority 2: String Methods**

- [ ] `str.join(iterable)` - C++, Rust, Go, Haskell, OCaml ([x] C, [x] LLVM have it as of v0.1.83)
- [ ] `str.startswith(prefix)` - C++, C, Rust, Go, Haskell, OCaml ([x] LLVM has it as of v0.1.83)
- [ ] `str.endswith(suffix)` - C++, C, Rust, Go, Haskell, OCaml ([x] LLVM has it as of v0.1.83)

**Priority 3: Built-in Functions**

- [ ] `enumerate(iterable)` - All except C
- [ ] `zip(*iterables)` - All except C
- [ ] `sorted(iterable)` - All backends

### Long Term (v0.3.x+)

- [ ] Advanced slicing
- [ ] Set operators (|, &, -)
- [ ] Tuple unpacking improvements

---

## Conclusion

MultiGen offers **8 backends** with different trade-offs:

- **Go**: Best for cloud/microservices (fast compilation)
- **C++/LLVM**: Best for embedded/systems (small binaries)
- **Rust**: Best for safety-critical (memory safety)
- **C**: Best for maximum control (complete runtime)
- **Haskell/OCaml**: Best for functional programming
- **LLVM**: Best for research/experimentation (JIT mode)
- **TypeScript**: Best for JavaScript/Deno ecosystems

**Overall Quality**: 1691 tests passing, strict type checking, design pattern implementations achieving 79% complexity reduction. Benchmark pass rates are measured against CPython's output; see the README.

**Maturity Level**: all eight backends pass 7/7 benchmarks; see the open defects in TODO.md. No backend translates the full supported subset without known gaps.
