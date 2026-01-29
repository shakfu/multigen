def simple_test() -> int:
    numbers: list[int] = []
    numbers.append(10)
    return len(numbers)


def main() -> int:
    result: int = simple_test()
    assert result == 1
    return result
