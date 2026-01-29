def test_simple_slice() -> list[int]:
    """Simple slice test"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)

    subset: list[int] = numbers[1:3]
    return subset


def main() -> int:
    result: list[int] = test_simple_slice()
    assert len(result) == 2
    return 0
