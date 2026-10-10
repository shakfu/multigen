"""A class whose method mutates a field."""


class Counter:
    def __init__(self, start: int) -> None:
        self.value: int = start

    def inc(self, by: int) -> None:
        self.value += by

    def get(self) -> int:
        return self.value


def main() -> None:
    c: Counter = Counter(5)
    c.inc(3)
    c.inc(2)
    print(c.get())


if __name__ == "__main__":
    main()
