# Verification API

Formal verification components using Z3 theorem prover.

## Bounds Prover

::: multigen.frontend.verifiers.bounds_prover

## Theorem Prover

::: multigen.frontend.verifiers.theorem_prover

## Correctness Prover

::: multigen.frontend.verifiers.correctness_prover

## Performance Analyzer

::: multigen.frontend.verifiers.performance_analyzer

## Z3 Formula Generator

::: multigen.frontend.verifiers.z3_formula_generator

## Example Usage

Verifying array bounds:

```python
from multigen.frontend.verifiers.bounds_prover import BoundsProver
import ast

code = '''
def sum_array(arr: list[int]) -> int:
    total: int = 0
    for i in range(len(arr)):
        total += arr[i]
    return total
'''

tree = ast.parse(code)
prover = BoundsProver()
result = prover.verify_memory_safety(tree.body[0])

if result.success:
    print("Array access is SAFE")
else:
    print("Verification failed:", result.errors)
```

Using with pipeline:

```python
from multigen.pipeline import MultiGenPipeline, PipelineConfig

config = PipelineConfig(
    target_language="c",
    enable_formal_verification=True,
    strict_verification=True,
)

pipeline = MultiGenPipeline(config=config)
result = pipeline.convert("example.py")
```

## Z3 Availability

Check if Z3 is available:

```python
from multigen.frontend.verifiers.bounds_prover import Z3_AVAILABLE

if Z3_AVAILABLE:
    print("Z3 is available")
else:
    print("Install with: pip install multigen[z3]")
```

Verification is automatically skipped if Z3 is not installed.

## See Also

- [Verification Guide](../guide/verification.md) -- User guide for formal verification
- [Frontend API](frontend.md) -- Frontend analysis APIs
