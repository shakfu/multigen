"""Comprehension filters over every source kind, including two `if` clauses."""


def main() -> int:
    xs: list[int] = [1, 2, 3, 4, 5, 6]
    a: set[int] = {x for x in xs if x > 2}
    b: set[int] = {i for i in range(10) if i % 3 == 0}
    c: dict[int, int] = {i: i * i for i in range(6) if i % 2 == 1}
    d: dict[int, int] = {x: x + 1 for x in xs if x < 3}
    e: list[int] = [x for x in xs if x > 1 if x < 5]
    f: list[int] = [i for i in range(10) if i > 2 if i % 2 == 0]
    print(len(a))
    print(len(b))
    print(len(c))
    print(len(d))
    print(len(e))
    print(len(f))
    return 0
