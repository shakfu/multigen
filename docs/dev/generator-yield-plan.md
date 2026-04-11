# Generator / `yield` Support

**Status**: Shipped (eager-collection strategy)
**Last updated**: 2026-04-11

## Summary

MultiGen supports Python generator functions (`def f(): ... yield x ...`) and
`yield from` across the Haskell, OCaml, C++, C, Go, and Rust backends. The
LLVM backend still emits a stub. The implemented strategy is **eager
collection to a list**: a generator function is rewritten to a regular function
that returns the fully materialized sequence. This document records the
shipped design, why it diverges from the original lazy-iteration plan, the
laziness tradeoff it makes, and the options available if true laziness is
needed later.

## What is implemented

### Scope

- Generator functions: `def f(...): ... yield expr ...`
- `yield from expr` where `expr` is a function call, `range(...)`, or a
  variable
- `yield` inside arbitrary loops, nested loops, and conditionals
- Parameters and local mutable state
- Consumption via `for x in f()` or `list(f())`
- Type-annotated generators (element type inferred from the return annotation)
- Generator expressions, normalized to list comprehensions

### Out of scope (unchanged from the original plan)

- `.send()`, `.throw()`, `.close()`
- Async generators (`async def` + `yield`)
- Returning a generator across function boundaries as a first-class lazy
  stream (a generator returns a concrete list, not an iterator)
- `yield from` over arbitrary expressions (e.g. generator expressions)

### Core transform

Each backend detects a generator locally by walking the `FunctionDef` body for
`ast.Yield` / `ast.YieldFrom`, then rewrites the function as follows:

1. Inject an accumulator local at function entry
   (`Vec<T>` / `std::vector<T>` / slice / list / `IORef`-ish list).
2. Rewrite the return type to the collection type, picking the element type
   from the return annotation (`-> int`) with a per-backend fallback.
3. Replace `yield expr` with an append/push onto the accumulator.
4. Replace `yield from iterable` with an extend/concat of the accumulator by
   the iterable, with a fast path for `range(...)`.
5. Append `return <accumulator>` at function exit.

The consumer side needs no changes: `for x in f()` and `list(f())` already
compile correctly because `f()` now returns a real list.

### Implementation locations

| Backend | Converter | Detection | `yield` / `yield from` |
|---|---|---|---|
| Rust | `backends/rust/converter.py` | `:698` | `:812`, `:820` |
| C++ | `backends/cpp/converter.py` | `:610` | `:916`, `:924` |
| C | `backends/c/converter.py` | `:164` (via `function_converter`-style helper) | `:747`, `:756` |
| Go | `backends/go/converter.py` | `:582` | `:1054`, `:1062` |
| OCaml | `backends/ocaml/converter.py` | `:471` | `:626`, `:634` |
| Haskell | `backends/haskell/function_converter.py` | `:67` | `:382`–`:555` (while / for / fallback patterns) |
| LLVM | `backends/llvm/ir_to_llvm.py` | — | `:2282`, `:2299` (stubs) |

### Validator and error wiring

- `frontend/subset_validator.py:352` registers `generators` as
  `PARTIALLY_SUPPORTED` with the description "eager collection to list".
- `frontend/subset_validator.py:367` registers `yield_from` as
  `PARTIALLY_SUPPORTED`, constrained to function calls, `range()`, and
  variables.
- `frontend/subset_validator.py:382` registers `generator_expressions` as
  `FULLY_SUPPORTED` by normalizing to list comprehensions.
- `errors.py:217` still carries a stale blanket "MultiGen does not support
  generators" string. It is no longer reachable for the six shipping backends;
  it should be either removed or re-scoped to fire only on the LLVM path. See
  [Known loose ends](#known-loose-ends).

### Tests

`tests/test_generators.py` is a single consolidated test module that exercises
every backend against the same fixtures:

- `SIMPLE_WHILE_GENERATOR` — `while` loop with mutable counter
- `FOR_LOOP_GENERATOR` — `for i in range(n): yield i*i`
- `CONDITIONAL_YIELD` — `yield` inside `if`
- `MULTIPLE_YIELDS` — straight-line yields
- `yield from` fixtures for `range(...)`, variables, and calls to other
  generators

Validation tests live in `TestGeneratorValidation` and lock in the per-rule
acceptance criteria (return annotation required, `.send()` rejected, etc.).

## Why eager collection instead of lazy iteration

The original plan proposed per-backend lazy strategies: Haskell lazy lists,
OCaml `Seq.t`, C++20 `std::generator`, STC `cco_async` coroutines in C,
`iter.Seq` in Go 1.23+, and a hand-written state-machine `impl Iterator` in
Rust. That plan also called for a shared detection pass producing a
`GeneratorInfo` on `AnalysisPhaseResult` and a shared lowering module at
`frontend/transforms/generator_lowering.py`.

The shipped implementation takes a different path:

- **Uniform codegen across six backends.** Every target has a cheap, idiomatic "push onto a mutable accumulator" pattern. No per-backend strategy tree.
- **No liveness analysis, no state-variable lifting, no resume-label
  dispatch.** The hardest pieces of the plan (especially for Rust and C) were
  avoided entirely because the eager rewrite doesn't need them.
- **No toolchain or standard-library bumps.** The Go 1.21 → 1.23 MSRV bump,
  the `cpp_standard=c++20` gate, and the STC coroutine integration are all
  unnecessary for eager collection.
- **Shared detection pass not built.** Each backend re-scans the function
  body for `Yield`/`YieldFrom`. Cheap enough that the shared
  `GeneratorInfo` / `pipeline_types` plumbing was never wired up, and
  `frontend/transforms/` does not exist.
- **Consumer-side simplicity.** Because `f()` returns a real list, every
  existing `for`, `list(...)`, `sum(...)`, and indexing site works without
  special-casing the generator call.

The cost of this simplicity is laziness, discussed next.

## The laziness tradeoff

In CPython, a generator call produces an iterator; elements are computed
on demand, and the producer's locals stay alive across yields. The eager
rewrite changes this contract:

1. **The full sequence is materialized before the first consumer step.**
   A generator that yields `n` elements allocates an `n`-element collection
   at call time. Memory use is `O(n)` instead of `O(1)`.
2. **Infinite generators don't terminate.** `def naturals(): i=0; while True:
   yield i; i+=1` compiles, but calling it at runtime allocates until the
   process dies. There is no static check that rejects unbounded loops inside a generator body.
3. **Early break doesn't save work.** `for x in f(): if cond: break`
   still runs `f()` to completion before the `for` starts. A consumer that
   only wanted the first element still pays for the whole list.
4. **Side-effect ordering changes.** In Python, a `print` inside a generator
   interleaves with consumer-side work. After the rewrite, all producer-side
   side effects happen before any consumer-side work.
5. **`generator_expressions` inherits the same semantics.** `(x*x for x in
   xs)` is normalized to a list comprehension, so it also materializes.
   `sum(x*x for x in range(10**9))` will OOM instead of streaming.
6. **`yield from inner(n)` calls `inner(n)` as a whole list first**, then
   extends. Nested generators compose by copying, not by chaining iterators.

These are not bugs in the implementation — they are the price of the chosen
strategy. They should be documented in user-facing docs so that users porting streaming Python code don't get surprised by memory blowups.

### What still works fine

- Bounded generators used in `for` / `list()` / `sum()` / `max()` contexts,
  which is the common case in algorithmic Python code.
- Generators whose consumer drains the full sequence anyway (the "write it
  as a generator because it reads cleaner" pattern).
- Composition of bounded generators via `yield from`.

## Known loose ends

- **Stale error in `errors.py:217`.** The blanket "generators not supported"
  message is unreachable for the six shipping backends. Either delete it or
  convert it to a per-backend gate that only fires for LLVM.
- **LLVM stub.** `visit_yield` / `visit_yield_from` in
  `backends/llvm/ir_to_llvm.py` are stubs. The validator accepts generators,
  so the LLVM backend should raise a clear "generators unsupported on LLVM
  target" error at conversion time rather than silently emitting nothing.
- **No shared detection pass.** Each backend walks the AST independently. If
  more passes end up needing generator metadata (e.g. the analyzers in
  `frontend/analyzers/`), factoring detection into a single
  `GeneratorInfo` on `AnalysisPhaseResult` is still worth doing. It was not
  required for codegen and was therefore skipped.
- **User docs.** The laziness tradeoff is not mentioned in the user-facing
  docs or CLI help. Users need to know that generators are eagerly collected
  before they deploy memory-sensitive code.
- **Unbounded generators.** There is no static rejection of obviously
  infinite generator bodies. A lint/warning in the validator would catch the
  worst footguns (`while True: yield ...` with no `break`/`return`).

## Future options if true laziness is required

True laziness should be reintroduced per-backend, not as a uniform design,
because the cost/benefit varies a lot by target. The options below are
roughly ordered from cheapest to most expensive.

### Option A: native lazy sequences in Haskell and OCaml

Haskell and OCaml can get lazy generators at near-zero implementation cost:

- **Haskell**: emit a lazy list (`[T]`) with `yield x` becoming cons and the body expressed as `foldr`-style builders. Haskell is already lazy by default; the main risk is a strict consumer forcing the whole spine.
- **OCaml**: emit `Seq.t` using `fun () -> Seq.Cons (x, rest)`. `Seq.iter` on the consumer side gives on-demand evaluation.

Both are additive: gate behind a preference (`--prefer lazy_generators=true`) so existing eager behavior stays default until the lazy path is validated. Roughly two new emitter branches per backend and no shared infrastructure.

### Option B: C++20 `std::generator<T>`

C++20 added `std::generator<T>` (coroutine-backed) to the standard library.
When the user has already opted into `cpp_standard=c++20`, emit a
`std::generator<T>` whose body contains `co_yield` in place of the eager
`push_back`. Consumers iterate with a range-based `for`. Fallback to the
current eager path under C++17.

This is the most idiomatic lazy option for the C++ backend and does not
require a shared lowering pass, because `co_yield` preserves locals
automatically.

### Option C: Go `iter.Seq[T]` (Go 1.23+)

Bump the Go backend's MSRV from 1.21 to 1.23 and emit `iter.Seq[T]` using
the `yield func(T) bool` pattern. Consumers use range-over-func. This is the
blessed Go idiom as of 1.23 and also avoids a state-machine transform.

The tradeoff is the MSRV bump. The channel-plus-goroutine fallback from the
original plan is not recommended — it leaks producer goroutines on early
`break`.

### Option D: Rust `impl Iterator` via shared lowering

This is the original plan's hardest piece and remains the only viable path on
stable Rust without new crate dependencies. It requires:

1. A shared lowering pass (`frontend/transforms/generator_lowering.py` in the
   original plan) that:
   - Computes locals live across each yield point.
   - Lifts them into fields of a generated state struct.
   - Assigns each yield point a resume label and rewrites the body into a
     `match state { ... }` inside `Iterator::next`.
2. A Rust emitter that renders the state struct, the `Iterator` impl, and
   the initial constructor.
3. Restricting the MVP to generators that don't yield references into
   locals, to sidestep borrow-checker interaction with the generated enum.

Alternative: depend on the `genawaiter` crate, which provides macro-based
generators on stable. Simpler codegen, but adds a runtime dependency the
project currently doesn't have.

### Option E: C via STC `cco_async` / `cco_yield`

STC is already vendored at `backends/c/ext/stc/include/stc/coroutine.h` and
provides a coroutine primitive suitable for generator lowering. This path
needs the same state-variable lifting as Rust, so it makes sense to build
Option D's shared lowering pass first and reuse it here. The payoff is lazy
C generators without a new dependency, but the engineering cost is
comparable to Rust's.

### Option F: LLVM

Unchanged from the original plan — deferred. A hand-written state machine at IR level is a large project on its own. The minimum viable fix is to make the LLVM backend raise a clear unsupported-feature error at conversion time and point users at this document.

## Suggested sequencing if the laziness work is picked up

1. **Close the loose ends first** (stale error string, LLVM stub error, user
   docs). Cheap, unblocks everything else.
2. **Option A (Haskell + OCaml)** behind a preference. Proves the
   preference-gated dual-strategy model works end to end.
3. **Option B (C++20)** — second easy win, same model.
4. **Option C (Go)** after deciding on the MSRV bump.
5. **Build the shared lowering pass** for Option D.
6. **Option D (Rust)** on top of the shared pass.
7. **Option E (C)** reusing the same pass.
8. **Option F (LLVM)** last, or never.

Each step is independently shippable and each earlier step de-risks the
next. The eager path stays as the default (and the fallback for unsupported
lazy cases) throughout.
