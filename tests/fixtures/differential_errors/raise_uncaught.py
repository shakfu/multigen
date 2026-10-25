"""Uncaught user raise of ValueError."""


def check(n: int) -> int:
    if n < 0:
        raise ValueError("negative")
    return n


def main() -> None:
    print(check(1))
    print(check(-1))
    print("unreachable")


if __name__ == "__main__":
    main()
