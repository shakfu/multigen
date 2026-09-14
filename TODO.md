# TODO

## Critical

- [ ] **`--makefile` places the source where the generated Makefile cannot find it** (R-11). C generation writes `build/src/prog.c` but moves the Makefile to `build/`, whose `$(wildcard $(SRCDIR)/*.c)` then matches nothing; `make` fails with `undefined reference to 'main'`. The same mapping renames TypeScript's `deno.json` to `Makefile`.

- [ ] **Generated Makefiles interpolate filenames unescaped** (R-10, `common/makefilegen.py`). A source file named `evil$(shell ...)` reaches `TARGET`, `all:` and the recipes verbatim and executes when `make` runs.

## High

- [ ] **The bounds prover models no program state** (R-7, `verifiers/bounds_prover.py`). Partly addressed: an access whose offset or region size is not concrete is now reported UNKNOWN instead of being handed to Z3 as unconstrained integers, so guarded and annotated code is no longer reported unsafe, and annotation subscripts (`a: list[int]`) no longer invent a region. Still outstanding: path conditions and a `len()` model, without which only accesses with literal indices into literal-sized regions are decided.

- [ ] **The symbolic executor stops at the first loop** (R-5). `_execute_for` and `_execute_while` return a `None` continuation, so a function containing a loop is never analysed past it and no return value is recorded.

- [ ] **Build configuration is exposed but ignored** (R-9). `compiler`, `compiler_flags`, `include_dirs` and `libraries` on `PipelineConfig` are read nowhere outside `__post_init__`. `multigen build --compiler clang` still emits `CC = gcc`.

## Medium

- [ ] **C floor division truncates toward zero** for negative operands: `-7 // 2` emits `((-7) / 2)` = -3, where Python gives -4.

- [ ] **`vec_int_push` writes after a failed reallocation** (`runtime/multigen_vec_int.h`). `vec_int_grow` returns without growing on allocation failure and the caller stores into `vec->data[vec->size++]` anyway.

- [ ] **Negative list indices are reported as errors** (`analyzers/bounds_checker.py`), though `a[-1]` is valid Python.

- [ ] **`Union[...]` emits invalid C**: the annotation is passed through as a type name, producing `unknown type name 'Union'`. It should refuse instead.

- [ ] **`Enum` members are silently discarded by six backends** at the emitter level (C, C++, Go, Haskell, OCaml, Rust, TypeScript emit an empty type). The validator blocks enums upstream, so this is reachable only through direct emitter use.

- [ ] **20 capability-matrix cells emit plausible source that does not build** (`emit: ok`, `run: build_failed`). See `backend_capabilities.json`; each is a backend defect with a reproducer in `capability_probes.py`.

- [ ] **TypeScript represents Python `int` as `number`**, losing precision above `2**53`, and its builder passes `--no-check`, so generated type errors are never caught.

## Low

- [ ] **`scripts/test_llvm_memory.sh` cannot detect the failures it looks for.** AddressSanitizer output goes to `*_asan.log` via `log_path` but the script greps `*_output.txt`. Its `((passed++))` also returns 1 under `set -e`, aborting on the first successful benchmark.

- [ ] **`scripts/benchmark.py` judges success by process exit status alone** and never compares output against a Python reference, so its pass rates say nothing about semantic equivalence.

- [ ] **`make test-benchmark` invokes `tests/benchmarks.py`, which does not exist.**

- [ ] **`ASTAnalyzer` reports Enum members as undeclared globals** (`Global variable 'IDLE' used without type annotation declaration`).

- [ ] **`BoundsChecker.analyze` finds nothing when given a Module node** -- it reports "Analyzed 0 memory regions" and misses even `a[5]` on a 3-element list. It works when handed a `FunctionDef`, which is what the pipeline passes.

- [ ] **Reusing one `LLVMEmitter` for a second module** raises `DuplicatedNameError`; the module is never reset between calls.

- [ ] **The validator rejects plain classes that the C backend translates correctly.** `class Point:` with an `__init__` fails on the missing `self` annotation and `__init__` return type, while C emits a working `typedef struct Point` and `Point_new`. Needs a decision on the method contract before it can be fixed.
