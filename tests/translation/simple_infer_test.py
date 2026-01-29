def simple_test() -> int:
    numbers = []
    numbers.append(10)
    return len(numbers)


def main() -> int:
    result = simple_test()
    assert result == 1
    return result
