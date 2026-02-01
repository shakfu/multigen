"""Quicksort benchmark - Functional version for Haskell backend.

This implementation uses list comprehensions instead of in-place array mutations,
making it compatible with Haskell's pure functional paradigm while testing the
same algorithmic concept as the imperative version.

The output is identical to the imperative quicksort benchmark (prints "5" - the
smallest element after sorting [100, 95, 90, ... 5]).
"""


def quicksort(arr: list[int]) -> list[int]:
    """Sort a list using functional quicksort.

    Creates new lists instead of mutating the input, compatible with Haskell.

    Args:
        arr: List of integers to sort

    Returns:
        New sorted list
    """
    if len(arr) <= 1:
        return arr

    pivot: int = arr[0]
    rest: list[int] = arr[1:]

    # Use list comprehensions - no mutations
    less: list[int] = [x for x in rest if x < pivot]
    greater: list[int] = [x for x in rest if x >= pivot]

    # Concatenate sorted sublists
    return quicksort(less) + [pivot] + quicksort(greater)


def main() -> int:
    """Run quicksort benchmark."""
    # Create array with numbers in reverse order (same as imperative version)
    arr: list[int] = [100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5]

    # Sort the array and print first element (should be 5, the smallest)
    sorted_arr: list[int] = quicksort(arr)
    print(sorted_arr[0])

    # Additional sorts for benchmarking (result used to avoid optimization)
    sorted2: list[int] = quicksort(sorted_arr)
    sorted3: list[int] = quicksort(sorted2)
    print(sorted3[0])

    return 0
