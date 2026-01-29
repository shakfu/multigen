def test_string_methods() -> str:
    """Test string methods"""
    text: str = "Hello World"

    # Test membership
    has_hello: bool = "Hello" in text

    # Test string methods
    upper_text: str = text.upper()
    lower_text: str = text.lower()
    index: int = text.find("World")

    return upper_text


def main() -> int:
    result: str = test_string_methods()
    assert result == "HELLO WORLD"
    return 0
