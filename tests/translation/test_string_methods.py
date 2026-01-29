def test_string_methods() -> int:
    """Test various string methods"""
    text: str = "Hello World"

    # String length
    length: int = len(text)

    # String methods that might be useful
    upper_text: str = text.upper()
    lower_text: str = text.lower()

    # Find operations
    index: int = text.find("World")

    # String contains
    has_hello: bool = "Hello" in text
    has_xyz: bool = "xyz" in text

    return length


def test_string_concatenation() -> str:
    """Test string concatenation"""
    first: str = "Hello"
    second: str = "World"
    result: str = first + " " + second
    return result


def test_string_comparison() -> bool:
    """Test string comparison"""
    text1: str = "Hello"
    text2: str = "Hello"
    text3: str = "World"

    same: bool = text1 == text2
    different: bool = text1 != text3

    return same and different


def main() -> int:
    # Test string methods with assertions
    assert test_string_methods() == 11
    assert test_string_concatenation() == "Hello World"
    assert test_string_comparison() == True
    return 0
