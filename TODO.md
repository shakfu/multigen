# TODO

## Critical

- [ ] **OCaml `--makefile` layout is untested.** `dune-project` plus a staged `src/dune` stanza should build with `dune build ./src/<name>.exe`, but `dune` was not installed when it was written. `test_cli_makefile_builds_and_runs[ocaml]` skips without it.

## High

- [ ] **The bounds prover models no program state** (R-7, `verifiers/bounds_prover.py`). Partly addressed: an access whose offset or region size is not concrete is now reported UNKNOWN instead of being handed to Z3 as unconstrained integers, so guarded and annotated code is no longer reported unsafe, and annotation subscripts (`a: list[int]`) no longer invent a region. Still outstanding: path conditions and a `len()` model, without which only accesses with literal indices into literal-sized regions are decided. Strict mode fails on UNKNOWN, so until then it rejects nearly every subscript, including `a = [1, 2, 3]; a[1]`.

- [ ] **The symbolic executor does not model loop bodies** (R-5, remainder). Loops now continue to the following statement, but bodies run inline through `_execute_simple_statement`, which skips `if`, `break`, `continue`, `return` and nested loops; `for` runs at most once. Each case is listed in `SymbolicExecutionReport.approximations`.

## Medium

- [ ] **LLVM integer `//` and `%` by zero are undefined behaviour** (`sdiv`/`srem`), not a ZeroDivisionError. The other seven backends raise.

- [ ] **Reassigning a container parameter does not compile in C or Rust.** C emits `a = {0};` for `a = [0, 0, 0]` (any list-literal reassignment); Rust assigns `vec![...]` to a `&mut Vec` parameter.

- [ ] **OCaml container parameters do not compile**: dicts are emitted as `[]` and indexed with `d.(k) <- v`, `len(xs)` emits `string_of_int len_array xs` without parentheses, and `append` rebinds a local. Haskell refuses mutated array parameters outright.

- [ ] **Haskell `main` cannot append to a list or print a dict value.** `xs.append(4)` emits `xs = xs ++ [4]` inside a `do` block (parse error); `print(d["a"])` on a `dict[str, int]` is an ambiguous `printValue` type.

- [ ] **Haskell functions other than `main` cannot print.** `print` outside `main` is now refused; a `None`-returning function would need an `IO ()` type and a `do` block.

- [ ] **Rust set comprehensions over `range(a, b)` do not compile**: the map closure is `|x| x` over `&i32`, producing `HashSet<&i32>`. One-argument `range` emits `|&x| x` and works.

- [ ] **LLVM `print(len(xs))` fails**: "Print for type IRDataType.VOID not implemented".

- [ ] **Dict iteration order differs from Python in C++, Rust and Go.** `unordered_map`, `HashMap` and Go maps do not keep insertion order, so `[k for k, v in d.items()]` prints keys in a different order. Sums and counts are unaffected. LLVM's `map_int_int` now records insertion order; C, Haskell (`Data.Map`, key order) and OCaml are unaudited.

- [ ] **Filtered list comprehensions over `range` do not compile in Rust or OCaml** when the result reaches `len()`. Rust maps `|x| x` over `&i32`; OCaml calls `len_array` on the `int list` a comprehension returns.

- [ ] **`.items()` unpacking is narrower on some backends.** C and LLVM accept only `dict[int, int]` in comprehensions (and no string-keyed maps anywhere); Go and C reject a loop name already bound outside the loop; Rust rejects mutating the dict inside its own loop. Each refuses rather than miscompiles.

- [ ] **The validator still admits shapes some converters cannot build**: C++ comprehensions inside class methods emit `[](x)` lambdas; Rust's non-`.items()` tuple-target comprehension path emits unbound names (unreachable through the validator). Haskell builtin wrappers other than `print` (`len'`, `sum'`) do not parenthesize call arguments, and pure-function `if` statements emit `if c then x = ... else ()`.

- [ ] **C float `//` is true division**: `7.0 // 2` gives 3.5. A fix needs `floor()`, and the C builder does not link `-lm`.

- [ ] **Negative list indices are reported as errors** (`analyzers/bounds_checker.py`), though `a[-1]` is valid Python.

- [ ] **`Union[...]` emits invalid C**: the annotation is passed through as a type name, producing `unknown type name 'Union'`. It should refuse instead.

- [ ] **`Enum` members are silently discarded by six backends** at the emitter level (C, C++, Go, Haskell, OCaml, Rust, TypeScript emit an empty type). The validator blocks enums upstream, so this is reachable only through direct emitter use.

- [ ] **20 capability-matrix cells emit plausible source that does not build** (`emit: ok`, `run: build_failed`). See `backend_capabilities.json`; each is a backend defect with a reproducer in `capability_probes.py`.

- [ ] **TypeScript represents Python `int` as `number`**, losing precision above `2**53`, and its builder passes `--no-check`, so generated type errors are never caught.

## Low

- [ ] **Direct compilation discards compiler stderr.** The pipeline reports only `Direct compilation failed`; diagnosing it means rerunning the toolchain by hand.

- [ ] **`scripts/test_llvm_memory.sh` cannot detect the failures it looks for.** AddressSanitizer output goes to `*_asan.log` via `log_path` but the script greps `*_output.txt`. Its `((passed++))` also returns 1 under `set -e`, aborting on the first successful benchmark.

- [ ] **`make test-benchmark` invokes `tests/benchmarks.py`, which does not exist.**

- [ ] **`ASTAnalyzer` reports Enum members as undeclared globals** (`Global variable 'IDLE' used without type annotation declaration`).

- [ ] **`BoundsChecker.analyze` finds nothing when given a Module node** -- it reports "Analyzed 0 memory regions" and misses even `a[5]` on a 3-element list. It works when handed a `FunctionDef`, which is what the pipeline passes.

- [ ] **Reusing one `LLVMEmitter` for a second module** raises `DuplicatedNameError`; the module is never reset between calls.

- [ ] **The validator rejects plain classes that the C backend translates correctly.** `class Point:` with an `__init__` fails on the missing `self` annotation and `__init__` return type, while C emits a working `typedef struct Point` and `Point_new`. Needs a decision on the method contract before it can be fixed.
