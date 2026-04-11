# Formal Verification

MultiGen includes Z3-based formal verification for memory safety proofs.

## Overview

Formal verification provides **mathematical guarantees** about your code's safety, not heuristics. The verification system uses the Z3 theorem prover to verify properties like array bounds safety.

!!! note
    CLI support for verification is currently under development. Verification is available through the Python API (see Programmatic Usage below).

## What Gets Verified

Currently verified properties:

1. **Array Bounds Safety**
   - Proves that `arr[i]` is safe given preconditions
   - Detects off-by-one errors (`range(n+1)` vs `len(arr) == n`)
   - Provides counterexamples when unsafe

## Example -- Safe Code

This code passes verification:

```python
def sum_array(arr: list[int]) -> int:
    total: int = 0
    for i in range(len(arr)):
        total += arr[i]  # SAFE: i in range(0, len(arr))
    return total
```

Verification output:

```
[x] Array access arr[i] is SAFE
  Proof: 0 <= i < len(arr) for all iterations
```

## Example -- Unsafe Code

This code fails verification:

```python
def unsafe_function(arr: list[int], n: int) -> int:
    result: int = 0
    for i in range(n):
        result = arr[i]  # UNSAFE: n might be > len(arr)
    return result
```

Verification output:

```
Array access arr[i] is UNSAFE
  Counterexample: i=5, arr_len=5
  Error: Index out of bounds
```

In normal mode, this generates code with a warning. In strict mode, code generation halts.

## Example -- Fixed Code

Fix by using verified bounds:

```python
def safe_function(arr: list[int], n: int) -> int:
    result: int = 0
    limit: int = min(n, len(arr))
    for i in range(limit):
        result = arr[i]  # SAFE: i < min(n, len(arr))
    return result
```

## Verification Modes

The verification system supports different modes through the Python API:

**Normal Mode** (default): No verification, fastest code generation.

**Verification Mode**: Runs verification, shows warnings, generates code anyway.

**Strict Verification Mode**: Runs verification, **halts** on failures, no code generated for unsafe functions.

## Programmatic Usage

Use verification in Python code:

```python
from multigen.pipeline import MultiGenPipeline, PipelineConfig

config = PipelineConfig(
    target_language="c",
    enable_formal_verification=True,
    strict_verification=True,
)

pipeline = MultiGenPipeline(config=config)
result = pipeline.convert("example.py")

if result.success:
    print("Verification passed, code generated")
else:
    print("Verification failed:", result.errors)
```

## Performance Considerations

Verification adds overhead:

- Small projects (<1000 LOC): ~100-500ms
- Medium projects (1000-5000 LOC): ~500-2000ms
- Large projects (5000+ LOC): ~2-10s

For production builds, enable verification. For rapid iteration, disable it.

## Troubleshooting

**Z3 not found**:

```bash
pip install z3-solver
```

**Verification timeout**:

```python
# Increase timeout in config
config.verification_timeout_ms = 10000
```

## Next Steps

- [Verification API](../api/verification.md) -- Verification API reference
- [Testing Guide](../dev-guide/testing.md) -- Writing verification tests
