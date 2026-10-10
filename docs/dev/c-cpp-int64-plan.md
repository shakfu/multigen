# C and C++: 64-bit `int`

Status: deferred. Tracked by the `int_width.py` entries for `c` and `cpp` in `KNOWN_FAILURES` (`tests/test_compilation.py`).

## Problem

Python `int` maps to 32-bit `int` in the C and C++ backends. `2**40` wraps to `0`. Rust, Go, Haskell and OCaml use 64 bits.

## Measured scope

| Area | Sites |
|-|-|
| `backends/c/converter.py` | about 130 references to `int`, `vec_int` or `%d` |
| Other C modules (`container_codegen.py`, `enhanced_type_inference.py`, `type_properties.py`, `template_substitution.py`, `emitter.py`) | about 70 |
| C runtime headers (`multigen_vec_int.h`, `multigen_map_int_int.h`, `multigen_container_ops.h`) | about 35 |
| `backends/cpp/converter.py`, `type_inference.py`, `factory.py` | about 50 |
| C++ runtime (`Range`, `StrOps::find`) | 10 |
| C and C++ test assertions | about 100, in 15 files |

## Approach

1. **C++ first.** It is smaller and has no container naming scheme.
   - Map `int` to `long long` in `type_map`, and in each `std::vector<int>`, `std::unordered_map<int, int>` and `std::unordered_set<int>` default.
   - Many sites compare type strings (`left_type == "int"`). They must change in the same commit.
   - Keep `int main`. C++ requires it.
   - Widen `Range` and `StrOps::find` in the runtime.
2. **C second.** STC derives container names from the element type, so `vec_int` would become `vec_long_long`, and code references `vec_int` by name.
   - Option A: keep the names `vec_int`, `map_int_int` and `set_int`, and change only the instantiated element type. That is fewer edits, but the names no longer state the type.
   - Option B: rename the containers to `vec_i64` and so on. That is more edits, but the names stay accurate.
   - Either way, `%d` becomes `%lld` (or `PRId64`) in every emitted `printf`.
3. Update test assertions file by file. The Python sources in those tests also contain `int`, so blind replacement is unsafe.
4. The `int_width.py` xfail entries turn into strict XPASS failures when the change is complete. Delete them then.
