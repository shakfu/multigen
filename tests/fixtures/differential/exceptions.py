"""raise, try/except and floor division of a negative."""


def safe_div(a: int, b: int) -> int:
    if b == 0:
        raise ValueError("div by zero")
    return a // b


def main() -> None:
    try:
        r: int = safe_div(10, 0)
        print(r)
    except ValueError:
        print("caught")
    print(safe_div(10, 3))
    print(-7 // 2)


if __name__ == "__main__":
    main()
