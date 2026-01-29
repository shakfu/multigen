"""Test dictionary comprehension conversion to C loops."""


def test_simple_dict_comprehension() -> int:
    """Test basic dictionary comprehension with range."""
    # {x: x * 2 for x in range(3)}
    result: dict[str, int] = {str(x): x * 2 for x in range(3)}
    return len(result)


def test_dict_comprehension_with_condition() -> int:
    """Test dictionary comprehension with condition."""
    # {x: x * x for x in range(5) if x % 2 == 1}
    odd_squares: dict[str, int] = {str(x): x * x for x in range(5) if x % 2 == 1}
    return len(odd_squares)


def main() -> int:
    """Main function to test dict comprehensions."""
    count1: int = test_simple_dict_comprehension()
    count2: int = test_dict_comprehension_with_condition()
    return count1 + count2
