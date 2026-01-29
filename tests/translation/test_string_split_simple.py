def test_string_split_assignment() -> None:
    text: str = "hello,world"
    # Avoid direct return of list, assign to variable first
    words: list[str] = text.split(",")
    # Test that split worked correctly
    assert len(words) == 2
    assert words[0] == "hello"
    assert words[1] == "world"


def main() -> int:
    test_string_split_assignment()
    return 0
