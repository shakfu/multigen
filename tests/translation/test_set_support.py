#!/usr/bin/env python3
"""Comprehensive test for set support in CGen."""


def test_empty_set() -> int:
    """Test empty set creation."""
    numbers: set[int] = set()
    return len(numbers)  # Should return 0


def test_set_with_add() -> int:
    """Test set with add method."""
    numbers: set[int] = set()
    numbers.add(1)
    numbers.add(2)
    numbers.add(1)  # Duplicate, should not be added
    return len(numbers)  # Should return 2


def test_set_membership() -> bool:
    """Test set membership checking."""
    numbers: set[int] = set()
    numbers.add(1)
    numbers.add(2)
    numbers.add(3)
    return 2 in numbers  # Should return true


def test_set_literal() -> int:
    """Test set literal initialization."""
    colors: set[int] = {1, 2, 3}  # Use int literals for now
    return len(colors)  # Should return 3 (duplicates removed)


def test_set_remove() -> int:
    """Test set remove method."""
    numbers: set[int] = set()
    numbers.add(1)
    numbers.add(2)
    numbers.add(3)
    numbers.remove(2)
    return len(numbers)  # Should return 2


def test_set_discard() -> int:
    """Test set discard method."""
    numbers: set[int] = set()
    numbers.add(1)
    numbers.add(2)
    numbers.discard(2)
    numbers.discard(99)  # Safe to discard non-existent element
    return len(numbers)  # Should return 1


def test_set_comprehension_simple() -> int:
    """Test simple set comprehension."""
    squares: set[int] = {x * x for x in range(5)}
    return len(squares)  # Should return 5


def test_set_comprehension_with_condition() -> int:
    """Test set comprehension with condition."""
    even_squares: set[int] = {x * x for x in range(10) if x % 2 == 0}
    return len(even_squares)  # Should return 5


def main() -> int:
    """Main test function."""
    result: int = 0

    result += test_empty_set()
    result += test_set_with_add()
    if test_set_membership():
        result += 1
    result += test_set_literal()
    result += test_set_remove()
    result += test_set_discard()
    result += test_set_comprehension_simple()
    result += test_set_comprehension_with_condition()

    return result
