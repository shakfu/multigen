def test_list_slicing() -> list[int]:
    """Test list slicing operations"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)
    numbers.append(40)
    numbers.append(50)

    # Basic slicing
    subset: list[int] = numbers[1:3]
    return subset


def test_slice_with_start_only() -> list[int]:
    """Test slicing with start only"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)

    # Slice from index 1 to end
    subset: list[int] = numbers[1:]
    return subset


def test_slice_with_end_only() -> list[int]:
    """Test slicing with end only"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)

    # Slice from start to index 2 (exclusive)
    subset: list[int] = numbers[:2]
    return subset


def main() -> int:
    result1: list[int] = test_list_slicing()
    result2: list[int] = test_slice_with_start_only()
    result3: list[int] = test_slice_with_end_only()

    assert len(result1) == 2
    assert len(result2) == 2
    assert len(result3) == 2

    return 0
