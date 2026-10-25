"""Uncaught ZeroDivisionError from int // after output has been printed."""


def floordiv(a: int, b: int) -> int:
    return a // b


def main() -> None:
    print(floordiv(7, 2))
    print(floordiv(1, 0))
    print("unreachable")


if __name__ == "__main__":
    main()
