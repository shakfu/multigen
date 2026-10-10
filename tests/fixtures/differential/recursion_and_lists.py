"""Recursion, list iteration and len()."""


def fib(n: int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)


def total(xs: list[int]) -> int:
    s: int = 0
    for x in xs:
        s += x
    return s


def main() -> None:
    xs: list[int] = [1, 2, 3, 4]
    print(fib(20))
    print(total(xs))
    print(len(xs))


if __name__ == "__main__":
    main()
