"""Test list comprehension conversion to C loops."""


def test_simple_list_comprehension() -> int:
    """Test basic list comprehension with range."""
    # [x * 2 for x in range(5)]
    result: list[int] = [x * 2 for x in range(5)]
    return len(result)


def test_list_comprehension_with_condition() -> int:
    """Test list comprehension with condition."""
    # [x for x in range(10) if x % 2 == 0]
    evens: list[int] = [x for x in range(10) if x % 2 == 0]
    return len(evens)


def test_complex_list_comprehension() -> int:
    """Test list comprehension with complex expression."""
    # [x * x + 1 for x in range(1, 4)]
    squares_plus_one: list[int] = [x * x + 1 for x in range(1, 4)]
    return len(squares_plus_one)


def main() -> int:
    """Main function to test comprehensions."""
    count1: int = test_simple_list_comprehension()
    count2: int = test_list_comprehension_with_condition()
    count3: int = test_complex_list_comprehension()
    return count1 + count2 + count3
