"""An exception no handler matches reaches the enclosing try."""


def g() -> None:
    try:
        raise ValueError("x")
    except KeyError:
        print("wrong")
    print("wrong")


def main() -> None:
    try:
        g()
    except ValueError:
        print("right")
    print("end")


if __name__ == "__main__":
    main()
