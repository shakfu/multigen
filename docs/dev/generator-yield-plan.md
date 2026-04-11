# Generator / `yield` Support Plan

**Status**: Proposed
**Last updated**: 2026-04-11
**Owner**: TBD

## Goal

Add Python generator function support (`def f(): ... yield x ...`) to MultiGen,
lowering to idiomatic iteration constructs in each target language.

## Current state

Generators are entirely unimplemented:

- `src/multigen/errors.py:213,217` — user-facing "not supported" error for `yield`
  and generator expressions.
- `src/multigen/frontend/subset_validator.py:352` — `FeatureRule` marked
  `FeatureStatus.EXPERIMENTAL` / Tier 3 Advanced, but not wired to any backend.
- `src/multigen/frontend/ast_analyzer.py:330` — only detects `ast.GeneratorExp`
  as a comprehension form; no generator-function awareness.
- No backend converter implements `ast.Yield` or `ast.YieldFrom`.
- `src/multigen/backends/c/ext/stc/include/stc/coroutine.h` — STC vendors a
  coroutine primitive (`cco_async` / `cco_yield`) that the C backend can target
  directly.

## MVP scope (Phase 1)

**In scope**

- Generator functions: `def f(...): ... yield expr ...`
- Consumption via `for x in f(): ...`
- `yield` inside arbitrary loops and conditionals
- Parameters and local state
- Generators returning `None` implicitly at end of function
- Type-annotated generators: `def f() -> Iterator[int]` / `Generator[int, None, None]`

**Out of scope for MVP** (candidates for Phase 2+)

- `yield from`
- `send()`, `throw()`, `close()` — the two-way communication channel
- Generator expressions `(x*2 for x in xs)` — can be lowered to synthetic
  generator functions once functions work
- Async generators (`async def` + `yield`)
- Returning a generator as a first-class value across function boundaries
  beyond immediate `for` consumption

## Architecture

### Shared detection pass

Add a single analysis step before backend dispatch:

- **Where**: new helper in `src/multigen/frontend/analyzers/` or extend
  `ast_analyzer.py`.
- **What**: walk every `FunctionDef`, mark as generator if any `ast.Yield` or
  `ast.YieldFrom` appears in its body (excluding nested `FunctionDef` and
  `Lambda`).
- **Output**: `GeneratorInfo` dataclass attached to `AnalysisPhaseResult`
  (`src/multigen/pipeline_types.py`), mapping function qualname → metadata
  (yield points, live-across-yield locals, element type).
- **Benefit**: each backend reads pre-computed info instead of re-scanning.

### Shared lowering pass (for state-machine targets)

State-machine backends (C, Rust, C++ fallback, Go fallback) need the same
transform:

1. Identify locals live across a yield point → these become fields of a state
   struct/enum.
2. Assign each yield point a unique resume label (0..N-1).
3. Rewrite the function body into a `switch (state)` dispatched on resume.
4. Emit a constructor (initial state) and an advance function / `next()`.

**Where**: new module `src/multigen/frontend/transforms/generator_lowering.py`
emitting a `LoweredGenerator` IR that the state-machine backends consume.

Languages with native lazy sequences (Haskell, OCaml) **skip this pass**
entirely and emit directly from the original AST.

### Validator / error wiring

- Flip `subset_validator.py:352` from `EXPERIMENTAL` to `SUPPORTED` once the
  first backend lands. Keep the feature gated per-backend via an allowlist.
- Remove the blanket error in `errors.py:217` and replace with a per-backend
  check that raises only when the selected target hasn't implemented generators
  yet (initially just LLVM).

## Per-backend strategy

| Backend | Target construct | Complexity | Notes |
|---|---|---|---|
| **Haskell** | Lazy list `[a]` | Low | Generators map to list constructors; `yield x` becomes cons; `for` becomes list iteration. No runtime machinery. |
| **OCaml** | `Seq.t` (stdlib) | Low | Emit `fun () -> Seq.Cons (x, rest)`. Idiomatic native fit. |
| **C++** | C++20 `std::generator<T>` | Low–Medium | Gated on `cpp_standard=c++20` preference. Fallback to manual state machine for C++17. |
| **C** | STC `cco_async` / `cco_yield` | Medium | Uses already-vendored `stc/coroutine.h`. Needs state-variable lifting from shared pass. |
| **Go** | `iter.Seq[T]` (Go 1.23+) | Medium | **Requires MSRV bump**: Go backend currently targets 1.21 (`backends/go/builder.py:21`). Fallback: channel + goroutine (leaks if caller breaks early). |
| **Rust** | `impl Iterator` on generated state enum | High | Stable Rust only, no nightly, no extra crates. Hardest state-machine transform but most valuable — benchmarks need this. |
| **LLVM** | **Deferred** | — | Would need hand-written state machine at IR level. Initially errors with a clear "generators unsupported on LLVM target" message. |

## Execution order

The order is chosen so each step either de-risks the next or unlocks
infrastructure:

1. **Shared detection + `GeneratorInfo`** — lights up `analyzer → pipeline_types`
   plumbing; no codegen changes yet.
2. **Validator gate flip** — allow generators through the pipeline for an
   allowlisted backend (initially empty).
3. **Haskell backend** — easiest codegen, validates end-to-end pipeline.
4. **OCaml backend** — second easy win; confirms the "native lazy sequence"
   path is generalizable.
5. **C++ backend (C++20 path)** — minimal lowering work; preference-gated.
6. **Shared `generator_lowering.py`** — build once for the state-machine
   backends below.
7. **C backend** — consumes lowering + STC coroutine primitives.
8. **Go backend** — decide MSRV bump first; then `iter.Seq` path or channel
   fallback.
9. **Rust backend** — hardest transform; shared lowering pass carries most of
   the weight.
10. **LLVM backend** — explicit unsupported-error wiring.

## Test matrix

For each supported backend, add `tests/test_backend_{lang}_generators.py`
covering:

- `yield` in a plain loop: `yield i for i in range(n)`
- `yield` in a nested loop
- `yield` inside a conditional (`if cond: yield x`)
- Generator consumed by `for` loop
- Generator consumed by `list(...)`
- Generator with parameters and mutable local state
- Early termination by caller (`break` in consuming `for`)
- Generator with zero yields (empty sequence)

Integration: add a generator-based benchmark variant to
`tests/benchmarks/algorithms/` — e.g. a sieve-of-Eratosthenes that yields
primes. Reuses the existing `*_haskell.py`-style override mechanism if a
backend can't express a particular variant.

## Open decisions

1. **LLVM generator support**: acceptable to ship as "6 of 7 backends"
   initially? Alternative is a hand-written state machine at IR level, which is
   a large project on its own.
2. **Go MSRV**: bump `go 1.21` → `go 1.23` in `backends/go/builder.py:21` to
   use `iter.Seq`, or stay on 1.21 and accept the channel+goroutine fallback
   (with its early-break leak)?
3. **Rust strategy**: stable-only manual `Iterator` (recommended — no toolchain
   requirements, no new dependencies) vs. adding `genawaiter` as an optional
   crate dep (simpler codegen, new dependency).
4. **Generator expressions**: include in Phase 1 as a syntactic rewrite to
   synthetic generator functions, or defer to Phase 2? They're a natural
   extension once functions work.
5. **Type annotations**: require `Iterator[T]` / `Generator[T, None, None]`
   return annotations for type-inference-dependent backends (C, Rust), or
   attempt to infer the element type from yield expressions?

## Risks

- **State lifting correctness** (C, Rust): incorrectly identifying locals live
  across yields silently corrupts generator state. Mitigation: use an
  established liveness analysis; test aggressively with generators that yield
  inside nested scopes.
- **Rust borrow checker**: the generated state enum plus `Iterator::next`
  signature interact with lifetimes in ways that manual codegen can get wrong.
  Mitigation: restrict MVP to generators that don't yield references into
  locals.
- **Go early-break leak**: if we take the goroutine fallback, consumers that
  `break` out of a `for` will leak the producer goroutine. Mitigation: prefer
  the `iter.Seq` path (MSRV bump) or require callers to drain.
- **Haskell strictness accidents**: list-based lowering relies on laziness; a
  strict fold at the consumer could force the whole generator. Mitigation:
  prefer `foldr`-style consumers in the generated `for` loop.

## Success criteria

- All MVP test cases pass on every supported backend.
- At least one benchmark uses a generator and runs on ≥ 5 backends.
- The "generators not supported" error only fires for LLVM (and surfaces a
  clear message pointing to this plan).
- `subset_validator.py` reflects per-backend support status accurately.
- No regression in existing 944-test suite.
