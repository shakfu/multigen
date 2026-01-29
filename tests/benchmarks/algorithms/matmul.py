"""Matrix multiplication benchmark - Numeric computation performance."""

from __future__ import annotations


def create_matrix(rows: int, cols: int, value: int) -> list[list[int]]:
    """Create a matrix filled with a value."""
    matrix: list[list[int]] = []
    for i in range(rows):
        row: list[int] = []
        for j in range(cols):
            row.append(value)
        matrix.append(row)
    return matrix


def matrix_multiply(a: list[list[int]], b: list[list[int]], size: int) -> list[list[int]]:
    """Multiply two square matrices."""
    result: list[list[int]] = create_matrix(size, size, 0)

    for i in range(size):
        for j in range(size):
            sum_val: int = 0
            for k in range(size):
                sum_val += a[i][k] * b[k][j]
            result[i][j] = sum_val

    return result


def main() -> int:
    """Run matrix multiplication benchmark."""
    # Create two 20x20 matrices
    size: int = 20
    matrix_a: list[list[int]] = create_matrix(size, size, 2)
    matrix_b: list[list[int]] = create_matrix(size, size, 3)

    # Multiply matrices 10 times for benchmarking
    result: list[list[int]] = []
    for iteration in range(10):
        result = matrix_multiply(matrix_a, matrix_b, size)

    # Print result from center of matrix
    center: int = size // 2
    print(result[center][center])

    return 0
