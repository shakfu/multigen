# Supported Python Syntax

> What python syntax is not yet implemented?

⏺ Explore(Explore Python syntax support)
⎿  Done (29 tool uses · 58.5k tokens · 1m 32s)

⏺ Based on my comprehensive exploration of the MultiGen codebase, here's what
Python syntax is NOT yet implemented:

Tier 4: Fundamentally Unsupported (Won't be in v1.0)

These features are incompatible with static compilation:

1. Exception Handling (try/except/finally/raise)
   - Requires runtime stack unwinding
   - Explicitly marked as NOT_SUPPORTED in subset_validator.py:400-406
2. Lambda Functions
   - Requires function pointers and runtime closures
   - Marked as NOT_SUPPORTED in subset_validator.py:392-398
3. Metaclasses
   - Requires runtime introspection
   - Validated against in subset_validator.py:589
4. Duck Typing
   - Requires runtime type checks
   - Fundamentally incompatible with static compilation
5. Dynamic Code Execution (eval, exec, compile, **import**)
   - Explicitly forbidden in subset_validator.py:596-601

Tier 3: Experimental/Planned (Research Required)

1. Generators (yield)
   - Marked EXPERIMENTAL in subset_validator.py:337-344
   - Could be implemented as C state machines
   - Listed in PRODUCTION_ROADMAP.md as post-v1.0
2. Pattern Matching (match statement, Python 3.10+)
   - Marked PLANNED in subset_validator.py:327-335
   - Could map to switch statements with guards
3. Generic Types (full parameterization)
   - Partially supported for type annotations
   - Full monomorphization not complete

Not Implemented (No Support)

1. Async/Await
   - Not in validator at all
   - Incompatible with synchronous C/Rust/Go
   - Listed in PRODUCTION_ROADMAP.md:258 as post-v1.0
2. Context Managers (with statement)
   - Not implemented
   - Listed in PRODUCTION_ROADMAP.md:256 as post-v1.0
3. Decorators (limited support)
   - [x] Supported: @staticmethod, @classmethod, @dataclass
   - [X] Not supported: @property, user-defined decorators
4. Imports (partial support)
   - [x] Supported: import module, from module import name, import ... as alias
   - [X] Not supported: Relative imports, star imports (from x import *)
5. String Features
   - [x] F-strings (basic expressions, no format specs in Phase 1)
   - [X] String formatting (%, .format())
   - [x] String methods: split, lower, upper, strip, replace, join, etc.
6. Built-in Functions (partial)
   - [X] Not supported: isinstance, type(), globals(), locals(), getattr, setattr, hasattr, dir
7. Class Features (partial)
   - [x] Supported: Basic classes, **init**, methods, single inheritance
   - [X] Not supported: Multiple inheritance, @property, descriptors, magic methods (beyond **init**)
8. Generator Expressions
   - (x for x in items) - different from list comprehensions
   - Not mentioned in supported features
9. **List Slicing (partial support)**
   - [x] **Haskell**: Full support via `drop`/`take` (arr[1:], arr[:n], arr[1:n])
   - [X] **C, C++, Rust, Go, OCaml, LLVM**: Not yet implemented
   - **Status**: On roadmap for future implementation
   - **Workaround**: Use explicit loops or list comprehensions
   - **Example**: Instead of `rest = arr[1:]`, use `rest = [arr[i] for i in range(1, len(arr))]`

Current Support Summary

Fully Supported:

- Functions, classes (basic), control flow (if/while/for/break/continue)
- Type annotations (int, float, str, bool, List, Dict, Set)
- Lists, dicts, sets with operations
- List/dict/set comprehensions
- Augmented assignment (+=, -=, etc.)
- String methods (9+ methods)
- File I/O, path operations
- Module imports (basic)
- OOP (constructors, methods, single inheritance)

Key Limitation: MultiGen focuses on a static, type-annotated subset of Python
suitable for compilation to systems languages. Dynamic features requiring
runtime introspection or exception handling are out of scope.

The roadmap (PRODUCTION_ROADMAP.md:253-282) explicitly states that
exception handling, context managers, generators, and async/await are
post-v1.0 features requiring significant research.

⏺ Based on my review of SUPPORTED_SYNTAX.md and the codebase, here are my
thoughts on what syntax to implement next, prioritized by impact vs
complexity:

High-Impact, Medium Complexity (Recommend These)

1. [x] F-strings (Python 3.6+) [x] **IMPLEMENTED in v0.1.86**

Why: Ubiquitous in modern Python, relatively straightforward to implement

- Maps cleanly to string concatenation + type conversion
- C++: std::to_string() + operator+
- Rust: format!() macro
- Go: fmt.Sprintf()
- All backends have string infrastructure already
- **Status**: Working in 6/7 backends (C, C++, Rust, Go, Haskell, OCaml)
- **Phase 1**: Basic expressions (no format specs)
- **Phase 2**: Format specifications (`.2f`, `:03d`) - future work

Example: f"Result: {x}" → "Result: " + std::to_string(x)

2. [ ] Tuple Support

Why: Fundamental data structure, enables multiple returns

- Already have List/Dict/Set infrastructure
- Enables: def foo() -> tuple[int, str]: return (1, "hello")
- Maps to: C++ std::tuple, Rust tuples, Go structs, Haskell tuples
- Unblocks many real-world patterns

3. [ ] Pattern Matching (Python 3.10+, marked PLANNED)

Why: Modern Python feature, maps well to switch/case

- Backends already use switch in some places
- C/C++: switch + guards
- Rust: Native match (perfect mapping!)
- Go: Type switches
- Haskell/OCaml: Native pattern matching (perfect!)

Medium-Impact, Low Complexity (Quick Wins)

4. [ ] **List Slicing (arr[1:], arr[:n], arr[start:end])**

Why: Common Python pattern, clean syntax

- [x] **Already working in Haskell** (maps to `drop`/`take`)
- Backend mappings straightforward:
  - C++: `std::vector(begin + start, begin + end)` or constructor
  - Rust: `&arr[start..end]` (slices) or `.iter().skip(n).take(m)`
  - Go: `arr[start:end]` (native support!)
  - C: Manual copy or pointer arithmetic
  - OCaml: `List.filteri` or recursive approach
  - LLVM: Vector operations
- **Status**: Haskell complete, other backends pending
- **Impact**: Enables functional programming patterns, cleaner algorithm implementations
- **Example**: `rest = arr[1:]` instead of manual indexing loops

5. [ ] String Formatting (% and .format())

Why: Legacy support for older codebases

- Similar to f-strings but easier (no expression parsing)
- "Hello %s" % name → string substitution
- Less important than f-strings (f-strings preferred)

6. [~] More Built-ins (Partial Support [x])

Why: Incremental improvements, well-defined semantics

- [x] **any()**: IMPLEMENTED in v0.1.87 (boolean reduction: returns True if any element is True)
- [x] **all()**: IMPLEMENTED in v0.1.87 (boolean reduction: returns True if all elements are True)
- [ ] enumerate(): Easy, just counter + loop (requires tuple support)
- [ ] zip(): Easy, parallel iteration (requires tuple support)
- [ ] reversed(): Easy, reverse iteration
- Already have infrastructure for built-ins

6. [ ] @property Decorator

Why: Common OOP pattern, syntactic sugar

- Maps to getter/setter methods
- C++: Member functions
- Rust: Impl methods
- Most backends already support methods

Lower Priority (Post-v1.0 per roadmap)

7. Context Managers (with statement)

Why: Useful but complex (RAII, cleanup, exception integration)

- Requires: Resource cleanup semantics
- C++: RAII destructors
- Rust: Drop trait
- Blocked until exception handling decision made

8. Generators (yield, marked EXPERIMENTAL)

Why: Powerful but requires state machines

- Maps to: Iterator protocol, coroutines, state machines
- Significant backend work required
- See roadmap: post-v1.0

My Recommendation

Top 3 priorities for next implementation:

1. ~~F-strings~~ [x] **COMPLETED in v0.1.86**
2. Tuple support - Enables multiple returns, unlocks real-world patterns,
clean backend mapping
3. Pattern matching - Future-proof (Python 3.10+), perfect mapping to
Rust/Haskell/OCaml, sets MultiGen apart

Rationale:

- ~~F-strings~~: [x] **DONE** - Working in 6/7 backends with smart type conversion
- Tuples: Essential for idiomatic Python (multiple returns, unpacking).
Backend support is straightforward.
- Pattern matching: Differentiates MultiGen as a "modern Python" compiler.
Rust/Haskell/OCaml backends would shine here.

Avoid for now: Context managers, generators, async/await (as roadmap
indicates - need more research, exception handling dependencies)

**Updated Priority (Post v0.1.87):**

1. **List slicing** - Already working in Haskell, straightforward backend mappings, high-impact for algorithms
2. **Tuple support** - Enables multiple returns, unlocks real-world patterns
3. **Pattern matching** - Leverage backend strengths (Rust/Haskell/OCaml), future-proof for Python 3.10+
4. **More built-ins** (enumerate, zip, reversed) - Quick wins, requires tuple support for enumerate/zip
