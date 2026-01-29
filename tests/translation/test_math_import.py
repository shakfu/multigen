import math


def test_math_functions() -> None:
    """Test that math functions can be used and return expected values."""
    x: float = 4.0
    result: float = math.sqrt(x) + math.sin(x)
    # sqrt(4) = 2, sin(4) ≈ -0.7568, so result ≈ 1.2432
    # isinstance() check removed - not supported in C backend
    assert result > 1.0
    assert result < 3.0


def test_math_pow() -> None:
    """Test that math.pow works correctly."""
    base: float = 2.0
    exponent: float = 3.0
    result: float = math.pow(base, exponent)
    # 2^3 = 8
    assert result == 8.0


def main() -> int:
    test_math_functions()
    test_math_pow()
    return 0
