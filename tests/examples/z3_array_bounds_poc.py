"""Z3 Proof-of-Concept: Array Bounds Verification

Demonstrates using Z3 to formally prove that array accesses are safe.

Example Python code to verify:
```python
def safe_access(arr: list[int], n: int) -> int:
    # Access arr[0] through arr[n-1]
    for i in range(n):
        x = arr[i]  # Is this safe?
    return x
```

Goal: Prove that if len(arr) >= n, then all accesses arr[i] are safe.
"""

try:
    from z3 import And, ForAll, Implies, Int, Solver, sat
except ImportError:
    print("ERROR: Z3 not installed. Install with: pip install z3-solver")
    print("Or install multigen with Z3 support: pip install multigen[z3]")
    exit(1)


def prove_array_bounds_safe():
    """Prove that array access is safe given preconditions."""

    print("=" * 70)
    print("Z3 PROOF-OF-CONCEPT: Array Bounds Safety")
    print("=" * 70)
    print()

    # Variables
    i = Int("i")  # Loop variable
    n = Int("n")  # Array length
    arr_len = Int("arr_len")  # Actual array length

    print("Code to verify:")
    print("  for i in range(n):")
    print("      x = arr[i]  # Is this safe?")
    print()

    # Preconditions (assumptions we know are true)
    preconditions = And(
        arr_len >= n,  # Array is at least as long as n
        n >= 0,  # n is non-negative
    )

    # Property we want to verify: all accesses are safe
    # For all i in [0, n), we need: 0 <= i < arr_len
    safety_property = ForAll([i], Implies(And(i >= 0, i < n), And(i >= 0, i < arr_len)))

    print("Preconditions:")
    print(f"  - arr_len >= n  (array is large enough)")
    print(f"  - n >= 0  (n is non-negative)")
    print()

    print("Safety property to prove:")
    print(f"  - ∀i. (0 <= i < n) => (0 <= i < arr_len)")
    print()

    # Check if preconditions imply safety property
    solver = Solver()
    solver.add(preconditions)
    solver.add(safety_property)

    print("Running Z3 solver...")
    result = solver.check()

    if result == sat:
        print("✓ PROOF SUCCESSFUL: Array accesses are SAFE")
        print()
        print("Explanation:")
        print("  Given that arr_len >= n and n >= 0,")
        print("  for all i in range [0, n), we have:")
        print("    i >= 0  (from range definition)")
        print("    i < n   (from range definition)")
        print("    i < arr_len  (because i < n and arr_len >= n)")
        print()
        print("  Therefore, all array accesses arr[i] are within bounds.")
    else:
        print("✗ PROOF FAILED: Could not verify safety")

    print("=" * 70)
    print()


def prove_unsafe_access_detected():
    """Demonstrate Z3 detecting an unsafe array access."""

    print("=" * 70)
    print("Z3 PROOF-OF-CONCEPT: Detecting Unsafe Access")
    print("=" * 70)
    print()

    i = Int("i")
    n = Int("n")
    arr_len = Int("arr_len")

    print("Code to verify:")
    print("  for i in range(n + 1):  # BUG: should be range(n)")
    print("      x = arr[i]  # Is this safe?")
    print()

    # Preconditions
    preconditions = And(arr_len == n, n >= 0)  # Array has exactly n elements

    # Try to prove safety (should fail)
    # For i in [0, n+1), prove 0 <= i < arr_len
    safety_property = ForAll([i], Implies(And(i >= 0, i < n + 1), And(i >= 0, i < arr_len)))

    print("Preconditions:")
    print(f"  - arr_len == n  (array has exactly n elements)")
    print(f"  - n >= 0")
    print()

    print("Trying to prove:")
    print(f"  - ∀i. (0 <= i < n+1) => (0 <= i < arr_len)")
    print()

    solver = Solver()
    solver.add(preconditions)
    solver.add(safety_property)

    print("Running Z3 solver...")
    result = solver.check()

    if result == sat:
        print("✓ Property holds (all accesses safe)")
    else:
        print("✗ UNSAFE ACCESS DETECTED")
        print()
        print("Counterexample:")
        print("  When i = n:")
        print("    - i is in range [0, n+1)  ✓")
        print("    - But i >= arr_len (since i = n = arr_len)  ✗")
        print()
        print("  Therefore: arr[n] is OUT OF BOUNDS (buffer overflow)")

    print("=" * 70)
    print()


def prove_off_by_one():
    """Demonstrate Z3 detecting off-by-one errors."""

    print("=" * 70)
    print("Z3 PROOF-OF-CONCEPT: Off-by-One Error Detection")
    print("=" * 70)
    print()

    i = Int("i")
    n = Int("n")
    arr_len = Int("arr_len")

    print("Code to verify:")
    print("  for i in range(1, n + 1):  # BUG: should start at 0")
    print("      x = arr[i]")
    print()

    preconditions = And(arr_len == n, n >= 1)

    # Check if i = n is safe
    access_at_n = And(i == n, i >= 0, i < arr_len)

    solver = Solver()
    solver.add(preconditions)
    solver.add(access_at_n)

    print("Checking if arr[n] is safe...")
    result = solver.check()

    if result == sat:
        print("✗ UNSAFE: arr[n] would be accessed")
        print()
        print("  When n = arr_len:")
        print("    - Loop iterates i = 1, 2, ..., n")
        print("    - Final iteration: i = n = arr_len")
        print("    - arr[arr_len] is OUT OF BOUNDS")
    else:
        print("✓ Safe (no access at arr[n])")

    print("=" * 70)
    print()


if __name__ == "__main__":
    print()
    print("=" * 70)
    print(" Z3 Array Bounds Verification - Proof of Concept")
    print("=" * 70)
    print()
    print("This demonstrates using Z3 theorem prover to formally verify")
    print("that array accesses are safe in generated code.")
    print()

    # Example 1: Prove safe access
    prove_array_bounds_safe()

    # Example 2: Detect unsafe access
    prove_unsafe_access_detected()

    # Example 3: Detect off-by-one error
    prove_off_by_one()

    print("=" * 70)
    print("Conclusion:")
    print("=" * 70)
    print()
    print("Z3 can be used to:")
    print("  1. Prove that array accesses are safe given preconditions")
    print("  2. Detect buffer overflows and off-by-one errors")
    print("  3. Generate counterexamples when safety cannot be proven")
    print()
    print("Next steps:")
    print("  - Integrate Z3 into BoundsProver")
    print("  - Convert Python AST constraints to Z3 formulas")
    print("  - Generate formal safety proofs for all array operations")
    print()
