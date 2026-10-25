"""Uncaught KeyError from reading a missing dict key."""


def main() -> None:
    d: dict[str, int] = {"a": 1}
    key: str = "a"
    print(d[key])
    key = "b"
    print(d[key])
    print("unreachable")


if __name__ == "__main__":
    main()
