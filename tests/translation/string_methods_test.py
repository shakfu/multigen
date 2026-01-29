def test_string_methods() -> str:
    """Test string methods"""
    text: str = "Hello World"

    # Test string methods
    upper_text: str = text.upper()
    index: int = text.find("World")

    return upper_text


def main() -> int:
    result: str = test_string_methods()
    assert result == "HELLO WORLD"
    return 0
