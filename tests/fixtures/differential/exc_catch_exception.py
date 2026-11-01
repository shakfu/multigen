"""except Exception catches every exception type."""


def main() -> None:
    try:
        raise ValueError("a")
    except Exception:
        print("caught")
    print("end")


if __name__ == "__main__":
    main()
