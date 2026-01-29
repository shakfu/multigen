def test_list_iteration() -> int:
    """Test iteration over lists"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)

    total: int = 0
    for num in numbers:
        total = total + num

    return total


def test_set_iteration() -> int:
    """Test iteration over sets"""
    unique_nums: set[int] = set()
    unique_nums.add(5)
    unique_nums.add(15)
    unique_nums.add(25)

    count: int = 0
    for value in unique_nums:
        count = count + 1

    return count


def test_string_list_iteration() -> int:
    """Test iteration over string lists"""
    names: list[str] = []
    names.append("Alice")
    names.append("Bob")
    names.append("Charlie")

    total_chars: int = 0
    for name in names:
        # Note: len() on strings not implemented yet, so we'll just count names
        total_chars = total_chars + 1

    return total_chars


def main() -> int:
    # Test all iteration functions
    assert test_list_iteration() == 60
    assert test_set_iteration() == 3
    assert test_string_list_iteration() == 3
    return 0
