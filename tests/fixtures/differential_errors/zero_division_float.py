"""Uncaught ZeroDivisionError from float //."""


def floordiv(a: float, b: float) -> float:
    return a // b


def main() -> None:
    print(1)
    print(floordiv(1.0, 0.0))
    print("unreachable")


if __name__ == "__main__":
    main()
