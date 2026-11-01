"""A bare raise reaches the enclosing try with the same type."""


def main() -> None:
    try:
        try:
            raise ValueError("x")
        except ValueError:
            print("h1")
            raise
    except ValueError:
        print("h2")
    print("end")


if __name__ == "__main__":
    main()
