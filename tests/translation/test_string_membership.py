def test_string_membership() -> bool:
    """Test string membership operations"""
    text: str = "Hello World"

    # String contains
    has_hello: bool = "Hello" in text
    has_xyz: bool = "xyz" in text
    not_has_xyz: bool = "xyz" not in text

    return has_hello and not has_xyz and not_has_xyz


def main() -> int:
    result: bool = test_string_membership()
    assert result == True
    return 0
