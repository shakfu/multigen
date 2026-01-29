def test_container_iteration() -> int:
    """Test container iteration"""
    numbers: list[int] = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)

    total: int = 0
    for num in numbers:
        total = total + num

    return total


def main() -> int:
    result: int = test_container_iteration()
    assert result == 60
    return result
