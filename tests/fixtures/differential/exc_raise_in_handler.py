"""A raise inside a handler reaches the enclosing try."""


def main() -> None:
    try:
        try:
            raise ValueError("a")
        except ValueError:
            print("h1")
            raise KeyError("b")
    except KeyError:
        print("h2")
    print("end")


if __name__ == "__main__":
    main()
