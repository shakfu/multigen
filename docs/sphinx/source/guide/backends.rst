Backend Guide
=============

MultiGen supports 6 production-ready backends, each with different characteristics and use cases.

Backend Overview
----------------

+----------+------------+---------------+-------------+------------------+
| Backend  | Benchmarks | Compile Time  | Binary Size | Runtime          |
+==========+============+===============+=============+==================+
| C        | 7/7 (100%) | 658ms         | 82KB        | 2,500 LOC        |
+----------+------------+---------------+-------------+------------------+
| C++      | 7/7 (100%) | 422ms         | 36KB        | 357 LOC          |
+----------+------------+---------------+-------------+------------------+
| Rust     | 7/7 (100%) | ~1500ms       | ~2MB        | 304 LOC          |
+----------+------------+---------------+-------------+------------------+
| Go       | 7/7 (100%) | 63ms          | 2365KB      | 413 LOC          |
+----------+------------+---------------+-------------+------------------+
| Haskell  | 6/7 (86%)  | 513ms         | 19734KB     | 214 LOC          |
+----------+------------+---------------+-------------+------------------+
| OCaml    | 7/7 (100%) | 209ms         | 771KB       | 216 LOC          |
+----------+------------+---------------+-------------+------------------+

C Backend
---------

**Best for**: Embedded systems, maximum portability, minimal binary size

Features:

- Template-based container system (STC library integration)
- 2D array support
- File I/O and module imports
- Zero external dependencies

Example::

   multigen convert -t c example.py

Runtime: ~2,500 lines across 16 files, using STC containers with custom fallback.

C++ Backend
-----------

**Best for**: Performance, STL integration, modern C++ projects

Features:

- Header-only runtime (357 lines)
- Multi-pass type inference
- Nested containers (``vector<vector<int>>``)
- Lambda-based comprehensions
- String-keyed maps

Example::

   multigen convert -t cpp example.py

Smallest runtime (357 LOC), fastest compilation (422ms), smallest binary (36KB).

Rust Backend
------------

**Best for**: Memory safety, systems programming, modern projects

Features:

- Ownership-aware code generation
- HashMap type inference
- Automatic cloning/dereferencing
- Immutability analysis
- Pure std library (304 lines)

Example::

   multigen convert -t rust example.py

Advanced type inference detects function call reassignments for accurate HashMap usage.

Go Backend
----------

**Best for**: Concurrent systems, cloud services, simplicity

Features:

- Reflection-based comprehensions
- Idiomatic Go patterns
- Fast compilation (63ms)
- Pure std library (413 lines)

Example::

   multigen convert -t go example.py

Fastest compilation, good for rapid iteration.

Haskell Backend
---------------

**Best for**: Functional programming, type safety, academic use

Features:

- Pure functional paradigm
- Data.Map and Data.Set
- Visitor pattern for statement conversion
- Pure std library (214 lines)

Example::

   multigen convert -t haskell example.py

Functional paradigm may limit some imperative Python patterns (86% benchmark pass).

OCaml Backend
-------------

**Best for**: Functional programming, fast compilation, type inference

Features:

- Mutable reference system
- Type-aware code generation
- Smart scoping for mutations
- Association lists for dicts
- Pure std library (216 lines)

Example::

   multigen convert -t ocaml example.py

Fast compilation (209ms), sophisticated mutation detection.

Choosing a Backend
------------------

**For embedded systems**: Use **C** (smallest binary, maximum portability)

**For performance-critical code**: Use **C++** (STL, fast compile, small binary)

**For memory safety**: Use **Rust** (ownership system, formal verification)

**For microservices**: Use **Go** (fast compile, good concurrency)

**For functional projects**: Use **Haskell** or **OCaml** (pure functional)

**For research/academic**: Use **Haskell** (strong type system, functional purity)

Common Features
---------------

All backends support:

- Functions and recursion
- Classes and OOP
- Lists, dicts, sets
- Nested containers (2D arrays)
- List/dict/set comprehensions
- File I/O and Path operations
- Module imports
- Type annotations
- Augmented assignment (+=, -=, etc.)
- String methods
- Built-in functions (len, min, max, sum, range, print)

Backend-Specific Limitations
-----------------------------

**Haskell**:
- Imperative loops converted to folds
- Mutable state requires different patterns
- Some quicksort implementations may need adjustment

**All backends**:
- No exception handling (try/except) yet
- No context managers (with statement) yet
- No generators/yield yet
- No async/await yet

Next Steps
----------

- :doc:`/api/backends` - Backend API reference
- :doc:`/dev/architecture` - Understanding backend architecture
