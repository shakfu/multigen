"""Uncaught IndexError from reading past the end of a list."""


def main() -> None:
    xs: list[int] = [1, 2, 3]
    i: int = 2
    print(xs[i])
    i = 3
    print(xs[i])
    print("unreachable")


if __name__ == "__main__":
    main()
