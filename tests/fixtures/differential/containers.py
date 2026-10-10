"""Dict counting, comprehensions, string methods and slicing."""


def word_count(text: str) -> dict[str, int]:
    counts: dict[str, int] = {}
    for w in text.split(" "):
        if w in counts:
            counts[w] = counts[w] + 1
        else:
            counts[w] = 1
    return counts


def squares(n: int) -> list[int]:
    return [i * i for i in range(n) if i % 2 == 0]


def uniq(xs: list[int]) -> set[int]:
    return {x for x in xs}


def main() -> None:
    c: dict[str, int] = word_count("a b a c b a")
    print(c["a"])
    sq: list[int] = squares(10)
    print(sq[2])
    u: set[int] = uniq([1, 1, 2, 3])
    print(len(u))
    s: str = "Hello World"
    print(s.upper())
    print(s[0:5])


if __name__ == "__main__":
    main()
