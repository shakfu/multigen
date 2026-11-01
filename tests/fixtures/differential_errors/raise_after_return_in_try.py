"""A return inside try must not leave its frame behind."""


def f() -> int:
    try:
        return 1
    except ValueError:
        return 2


def main() -> None:
    print(f())
    raise ValueError("x")


if __name__ == "__main__":
    main()
