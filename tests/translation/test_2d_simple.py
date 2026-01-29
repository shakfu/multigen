"""Simple 2D list test"""

from __future__ import annotations


def main() -> int:
    """Test simple 2D list operations."""
    matrix: list[list[int]] = []

    # Create first row
    row1: list[int] = []
    row1.append(1)
    row1.append(2)

    # Add to matrix
    matrix.append(row1)

    # Create second row
    row2: list[int] = []
    row2.append(3)
    row2.append(4)

    # Add to matrix
    matrix.append(row2)

    # Access element
    result: int = matrix[0][0]
    print(result)

    return 0
