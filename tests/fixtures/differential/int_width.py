"""Integers past 2**31, and modulo of a negative."""


def big() -> int:
    x: int = 1
    for i in range(40):
        x = x * 2
    return x


def main() -> None:
    print(big())
    print(-7 % 3)


if __name__ == "__main__":
    main()
