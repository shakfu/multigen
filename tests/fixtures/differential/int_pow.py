"""Exact integer **, and float ** when either operand is a float."""


def power(base: int, exp: int) -> int:
    return base**exp


def main() -> None:
    x: int = 2**40
    print(x)
    print(power(3, 5))
    print(power(-2, 3))
    print(power(7, 0))
    print(3**39)
    y: float = 2.0**3
    print(y)
    print(2**0.5)


if __name__ == "__main__":
    main()
