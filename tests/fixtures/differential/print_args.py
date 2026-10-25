"""print with several arguments of mixed types, and with none."""


def main() -> None:
    n: int = 3
    x: float = 2.5
    s: str = "hi"
    ok: bool = n > 2
    print(n, x)
    print(s, n, ok)
    print("total:", n + 1, x * 2)
    print()
    print(n)


if __name__ == "__main__":
    main()
