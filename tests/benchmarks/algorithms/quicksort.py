"""Quicksort benchmark - Array manipulation performance."""


def quicksort(arr: list, low: int, high: int) -> int:
    """Sort array using quicksort algorithm."""
    if low < high:
        # Partition the array
        pivot: int = arr[high]
        i: int = low - 1

        for j in range(low, high):
            if arr[j] <= pivot:
                i += 1
                # Swap arr[i] and arr[j]
                temp: int = arr[i]
                arr[i] = arr[j]
                arr[j] = temp

        # Swap arr[i+1] and arr[high]
        temp2: int = arr[i + 1]
        arr[i + 1] = arr[high]
        arr[high] = temp2

        pi: int = i + 1

        # Recursively sort elements
        quicksort(arr, low, pi - 1)
        quicksort(arr, pi + 1, high)

    return 0


def main() -> int:
    """Run quicksort benchmark."""
    # Create array with numbers in reverse order
    arr: list = [100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5]

    # Sort the array multiple times for benchmarking
    for iteration in range(100):
        # Reset array
        test_arr: list = [100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5]
        quicksort(test_arr, 0, 19)

    # Final sort and print first element
    quicksort(arr, 0, 19)
    print(arr[0])

    return 0
