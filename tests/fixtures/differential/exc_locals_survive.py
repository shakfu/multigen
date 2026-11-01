"""Locals assigned in a try keep their values in the handler and after it."""


def bump(n: int) -> int:
    try:
        n = n + 1
        raise ValueError("x")
    except ValueError:
        return n


def main() -> None:
    x: int = 0
    s: str = "a"
    xs: list[int] = []
    try:
        x = 5
        s = "b"
        xs.append(1)
        xs.append(2)
        raise ValueError("x")
    except ValueError:
        print(x)
        print(s)
        print(len(xs))
    print(x)
    print(bump(1))


if __name__ == "__main__":
    main()
