"""Functional Quicksort - Works with all backends including Haskell.

This implementation uses list comprehensions instead of in-place array mutations,
making it compatible with pure functional languages like Haskell.

Usage:
    mgen --target haskell convert tests/examples/algorithms/functional_quicksort.py
    mgen --target rust convert tests/examples/algorithms/functional_quicksort.py

Note: The imperative quicksort in benchmarks/algorithms/quicksort.py uses in-place
array swaps which are NOT supported by the Haskell backend. Use this functional
version instead for Haskell targets.
"""


def quicksort(arr: list) -> list:
    """Sort a list using functional quicksort.

    This implementation creates new lists instead of mutating the input,
    making it compatible with pure functional languages like Haskell.

    Args:
        arr: List of integers to sort

    Returns:
        New sorted list (original is unchanged)
    """
    if len(arr) <= 1:
        return arr

    pivot: int = arr[0]
    rest: list = arr[1:]

    # Use list comprehensions - no mutations
    less: list = [x for x in rest if x < pivot]
    greater: list = [x for x in rest if x >= pivot]

    # Concatenate sorted sublists
    return quicksort(less) + [pivot] + quicksort(greater)


def main() -> int:
    """Demonstrate functional quicksort."""
    # Unsorted list
    arr: list = [64, 34, 25, 12, 22, 11, 90, 45, 33, 21]

    print(arr[0])  # Print first element before sort

    # Sort creates a new list - original unchanged
    sorted_arr: list = quicksort(arr)

    print(sorted_arr[0])  # Print smallest element: 11
    print(sorted_arr[9])  # Print largest element: 90

    # Verify sorting works
    for i in range(9):
        current: int = sorted_arr[i]
        next_val: int = sorted_arr[i + 1]
        if current > next_val:
            print(0)  # Error: not sorted
            return 1

    print(1)  # Success: sorted correctly
    return 0
