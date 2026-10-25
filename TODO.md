# TODO

## Medium

- [ ] **Float gaps left after int promotion.** Each backend now passes `floats.py`; these forms do not:
  - `n /= 2` on an int variable: Python rebinds `n` to a float. C and C++ keep int division; LLVM refuses.
  - An int argument to a float parameter is not promoted in Go or LLVM.
  - `str(float)` and f-string floats use the native format in C and C++.
  - Class methods get no promotion in Rust, Go or Haskell.
  - Go emits `math.Pow` for `**` without importing `math`, and folds the literal `-0.0` to `0.0`.
  - The LLVM JIT does not link the C runtime, so a JIT-run program that prints a float fails on `multigen_print_float`.

- [ ] **LLVM negative list indices raise IndexError.** The runtime takes `size_t` indices, so `xs[i]` with `i = -1` is out of range instead of the last element. C, C++, Rust and Go count from the end.

- [ ] **C `try`/`except` matches exact error codes.** `MGEN_END_TRY` drops an exception that no handler matched instead of re-raising it, and `except Exception` catches only `MGEN_ERROR_GENERIC`.

- [ ] **Function names that are target keywords are not escaped.** A Python function named `mod` does not compile in Rust (`expected identifier, found keyword`).

- [ ] **The bounds prover leaves nested and attribute subscripts UNKNOWN.** Element lengths are not tracked, so `m[i][j]` and `self.items[i]` are never proved, and strict mode rejects them. Loop invariants are limited to counters changed only by `+=`/`-=` a constant.

- [ ] **Reassigning a container parameter does not compile in C or Rust.** C emits `a = {0};` for `a = [0, 0, 0]` (any list-literal reassignment); Rust assigns `vec![...]` to a `&mut Vec` parameter.

- [ ] **OCaml container parameters do not compile**: dicts are emitted as `[]` and indexed with `d.(k) <- v`, and `append` rebinds a local. Haskell refuses mutated array parameters outright.

- [ ] **Haskell `main` cannot append to a list or print a dict value.** `xs.append(4)` emits `xs = xs ++ [4]` inside a `do` block (parse error); `print(d["a"])` on a `dict[str, int]` is an ambiguous `printValue` type.

- [ ] **Haskell functions other than `main` cannot print.** `print` outside `main` is now refused; a `None`-returning function would need an `IO ()` type and a `do` block.

- [ ] **Rust set comprehensions over `range(a, b)` do not compile**: the map closure is `|x| x` over `&i32`, producing `HashSet<&i32>`. One-argument `range` emits `|&x| x` and works.

- [ ] **LLVM `print(len(xs))` fails**: "Print for type IRDataType.VOID not implemented".

- [ ] **Dict iteration order is unaudited in C, Haskell and OCaml.** Haskell's `Data.Map` iterates in key order, not insertion order. LLVM, C++, Rust and Go record insertion order.

- [ ] **`.items()` unpacking is narrower on some backends.** C and LLVM accept only `dict[int, int]` in comprehensions (and no string-keyed maps anywhere); Go and C reject a loop name already bound outside the loop; Rust rejects mutating the dict inside its own loop. Each refuses rather than miscompiles.

- [ ] **The validator still admits shapes some converters cannot build**: C++ comprehensions inside class methods emit `[](x)` lambdas; Rust's non-`.items()` tuple-target comprehension path emits unbound names (unreachable through the validator).

- [ ] **`Enum` members are silently discarded by six backends** at the emitter level (C, C++, Go, Haskell, OCaml, Rust, TypeScript emit an empty type). The validator blocks enums upstream, so this is reachable only through direct emitter use.

- [ ] **Haskell refuses `try`/`except` and `with`.** `catch` and `bracket` run in IO, and the forms the converter emitted did not build in pure functions or in `main`. Supporting them needs variables assigned in the body to stay visible after it, and a typed `fromException` per handler.

- [ ] **TypeScript represents Python `int` as `number`**, losing precision above `2**53`, and its builder passes `--no-check`, so generated type errors are never caught.

## Low

- [ ] **Direct compilation discards compiler stderr.** The pipeline reports only `Direct compilation failed`; diagnosing it means rerunning the toolchain by hand.

- [ ] **`scripts/test_llvm_memory.sh` cannot detect the failures it looks for.** AddressSanitizer output goes to `*_asan.log` via `log_path` but the script greps `*_output.txt`. Its `((passed++))` also returns 1 under `set -e`, aborting on the first successful benchmark.

- [ ] **`make test-benchmark` invokes `tests/benchmarks.py`, which does not exist.**

- [ ] **`ASTAnalyzer` reports Enum members as undeclared globals** (`Global variable 'IDLE' used without type annotation declaration`).

- [ ] **`BoundsChecker.analyze` finds nothing when given a Module node** -- it reports "Analyzed 0 memory regions" and misses even `a[5]` on a 3-element list. It works when handed a `FunctionDef`, which is what the pipeline passes.

- [ ] **Reusing one `LLVMEmitter` for a second module** raises `DuplicatedNameError`; the module is never reset between calls.

- [ ] **The validator rejects plain classes that the C backend translates correctly.** `class Point:` with an `__init__` fails on the missing `self` annotation and `__init__` return type, while C emits a working `typedef struct Point` and `Point_new`. Needs a decision on the method contract before it can be fixed.
