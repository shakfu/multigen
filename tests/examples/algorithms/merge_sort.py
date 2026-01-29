"""Merge Sort - Classic divide-and-conquer sorting algorithm.

This example demonstrates:
- Recursive algorithms
- List manipulation
- Efficient sorting implementation
- Classic computer science algorithm
"""


def merge(left: list, right: list) -> list:
    """Merge two sorted lists into one sorted list.

    Args:
        left: First sorted list
        right: Second sorted list

    Returns:
        Merged sorted list
    """
    result: list = []
    i: int = 0
    j: int = 0

    # Merge while both lists have elements
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    # Append remaining elements from left
    while i < len(left):
        result.append(left[i])
        i += 1

    # Append remaining elements from right
    while j < len(right):
        result.append(right[j])
        j += 1

    return result


def merge_sort(arr: list) -> list:
    """Sort a list using merge sort algorithm.

    Args:
        arr: List to sort

    Returns:
        Sorted list
    """
    # Base case: lists with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # Divide: split list in half
    mid: int = len(arr) // 2
    left: list = []
    right: list = []

    # Copy elements to left half
    for i in range(mid):
        left.append(arr[i])

    # Copy elements to right half
    for i in range(mid, len(arr)):
        right.append(arr[i])

    # Conquer: recursively sort both halves
    left = merge_sort(left)
    right = merge_sort(right)

    # Combine: merge the sorted halves
    return merge(left, right)


def main() -> int:
    """Main entry point for merge sort example."""
    # Create unsorted list
    numbers: list = []
    numbers.append(64)
    numbers.append(34)
    numbers.append(25)
    numbers.append(12)
    numbers.append(22)
    numbers.append(11)
    numbers.append(90)

    print(64)  # First element before sort

    # Sort the list
    sorted_numbers: list = merge_sort(numbers)

    # Print sorted results
    for num in sorted_numbers:
        print(num)

    return 0
