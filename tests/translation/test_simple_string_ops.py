def test_string_replace() -> None:
    text: str = "hello world"
    new_text: str = text.replace("world", "python")
    assert new_text == "hello python"


def test_string_strip() -> None:
    text: str = "  hello world  "
    clean: str = text.strip()
    assert clean == "hello world"


def main() -> int:
    test_string_replace()
    test_string_strip()
    return 0
