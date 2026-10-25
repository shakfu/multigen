"""Uncaught ZeroDivisionError from int %."""


def modulo(a: int, b: int) -> int:
    return a % b


def main() -> None:
    print(modulo(7, 2))
    print(modulo(1, 0))
    print("unreachable")


if __name__ == "__main__":
    main()
