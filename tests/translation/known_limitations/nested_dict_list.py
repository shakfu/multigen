"""Test dict[str, list[int]] nested type."""


def main() -> int:
    """Test dictionary with list values."""
    # Create dict with list values
    groups: dict = {}
    evens: list = []
    odds: list = []

    # Populate lists
    for i in range(10):
        if i % 2 == 0:
            evens.append(i)
        else:
            odds.append(i)

    groups["evens"] = evens
    groups["odds"] = odds

    # Sum evens
    total: int = 0
    even_list: list = groups["evens"]
    for num in even_list:
        total += num

    print(total)
    return 0
