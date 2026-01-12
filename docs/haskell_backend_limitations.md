# Haskell Backend Limitations and Workarounds

**Last Updated**: December 27, 2025
**MultiGen Version**: v0.1.85+

---

## Overview

The Haskell backend successfully translates most Python code to idiomatic Haskell.
However, due to fundamental differences between Python's imperative paradigm and
Haskell's pure functional paradigm, some patterns require alternative approaches.

**Current Status**: 6/7 benchmarks passing (86%)

---

## Known Limitation: In-Place Array Mutations

### The Issue

Python's in-place array mutation patterns cannot be directly translated to Haskell
because Haskell enforces immutability. The benchmark quicksort uses in-place swaps:

```python
# This pattern DOES NOT work in Haskell backend
def quicksort(arr: list, low: int, high: int) -> int:
    if low < high:
        pivot: int = arr[high]
        i: int = low - 1

        for j in range(low, high):
            if arr[j] <= pivot:
                i += 1
                # In-place swap - NOT SUPPORTED
                temp: int = arr[i]
                arr[i] = arr[j]
                arr[j] = temp
        # ...
```

When MultiGen encounters array subscript assignment (e.g., `arr[i] = value`), it raises:

```text
UnsupportedFeatureError: Function 'quicksort' mutates array parameter 'arr'.
Haskell does not support in-place array mutations. Consider using a functional
approach with list comprehensions or recursive decomposition.
```

### Why This Happens

Haskell is a purely functional language where all data is immutable. Python's
`arr[i] = value` implies mutation, which has no direct equivalent in pure Haskell.

---

## Solution: Functional Quicksort

Use a functional implementation that creates new lists instead of mutating:

```python
# This pattern WORKS in Haskell backend
def quicksort(arr: list) -> list:
    """Functional quicksort - creates new lists, no mutation."""
    if len(arr) <= 1:
        return arr

    pivot: int = arr[0]
    rest: list = arr[1:]

    # Use list comprehensions instead of swaps
    less: list = [x for x in rest if x < pivot]
    greater: list = [x for x in rest if x >= pivot]

    # Concatenate sorted sublists
    return quicksort(less) + [pivot] + quicksort(greater)


def main() -> int:
    arr: list = [3, 1, 4, 1, 5, 9, 2, 6]
    sorted_arr: list = quicksort(arr)
    print(sorted_arr[0])  # Prints: 1
    return 0
```

### Generated Haskell Code

```haskell
quicksort :: [Int] -> [Int]
quicksort arr
  | length arr <= 1 = arr
  | otherwise =
      let pivot = arr !! 0
          rest = drop 1 arr
          less = [x | x <- rest, x < pivot]
          greater = [x | x <- rest, x >= pivot]
      in quicksort less ++ [pivot] ++ quicksort greater

main :: IO ()
main = do
    let arr = [3, 1, 4, 1, 5, 9, 2, 6]
    let sortedArr = quicksort arr
    print (sortedArr !! 0)
```

---

## Pattern Comparison

| Pattern | Python (Imperative) | Python (Functional) | Haskell Support |
|---------|---------------------|---------------------|-----------------|
| Array swap | `arr[i] = arr[j]` | N/A | NOT SUPPORTED |
| Create new list | N/A | `[x for x in arr if ...]` | SUPPORTED |
| In-place append | `arr.append(x)` | N/A | NOT SUPPORTED |
| Concatenation | N/A | `list1 + list2` | SUPPORTED |
| Recursive decomposition | N/A | `arr[0]`, `arr[1:]` | SUPPORTED |

---

## Other Patterns to Avoid

### 1. In-Place List Operations

```python
# NOT SUPPORTED
arr.append(x)
arr.pop()
arr.insert(0, x)
arr.remove(x)
arr.sort()
```

**Alternative**: Use list comprehensions or create new lists:

```python
# SUPPORTED
new_arr: list = arr + [x]  # Instead of append
new_arr: list = [a for a in arr if a != x]  # Instead of remove
```

### 2. Dictionary Mutations

```python
# NOT SUPPORTED (in most cases)
d[key] = value
del d[key]
```

**Alternative**: Use dictionary comprehensions or functional updates:

```python
# SUPPORTED (for initial construction)
d: dict = {k: v for k, v in items}
```

---

## Recommended Patterns for Haskell

### Accumulator Pattern

```python
def sum_list(arr: list) -> int:
    """Use accumulator instead of mutation."""
    result: int = 0
    for x in arr:
        result += x  # This is supported (local variable)
    return result
```

### Filter-Map Pattern

```python
def process_list(arr: list) -> list:
    """Use comprehensions for transformations."""
    filtered: list = [x for x in arr if x > 0]
    mapped: list = [x * 2 for x in filtered]
    return mapped
```

### Recursive Decomposition

```python
def sum_recursive(arr: list) -> int:
    """Use recursion instead of loops with mutations."""
    if len(arr) == 0:
        return 0
    return arr[0] + sum_recursive(arr[1:])
```

---

## Benchmark Status

| Benchmark | Status | Notes |
|-----------|--------|-------|
| fibonacci | PASS | Pure recursion |
| matmul | PASS | Nested comprehensions |
| quicksort | FAIL | Requires functional approach |
| wordcount | PASS | Dict comprehensions |
| list_ops | PASS | Functional list operations |
| dict_ops | PASS | Functional dict operations |
| set_ops | PASS | Functional set operations |

**Overall**: 6/7 (86%)

---

## Future Improvements

1. **Automatic transformation**: Detect imperative patterns and suggest functional alternatives
2. **IORef support**: Optional mutable references for advanced users
3. **ST monad**: Safe local mutation within pure functions
4. **Array mutation detection**: Better error messages with specific fix suggestions

---

## See Also

- [Backend Selection Guide](backend_selection_guide.md) - Choosing the right backend
- [Haskell Build Fix](haskell_build_fix.md) - Build troubleshooting
- [Test File](../tests/test_backend_haskell_functional_quicksort.py) - Working examples

---

## Summary

The Haskell backend is fully functional for pure functional patterns. When using
Python code that relies on in-place mutations:

1. Rewrite using list comprehensions
2. Use recursive decomposition (`arr[0]`, `arr[1:]`)
3. Create new collections instead of mutating existing ones

This aligns with Haskell's philosophy and produces idiomatic, efficient code.
