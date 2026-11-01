"""An exception no handler matches ends the program."""


def main() -> None:
    try:
        raise ValueError("x")
    except KeyError:
        print("wrong")
    print("wrong")


if __name__ == "__main__":
    main()
