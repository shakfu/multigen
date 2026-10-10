"""Filtered comprehensions, a set built from a set, and reuse of a list after a comprehension."""


def main() -> int:
    xs: list[int] = [3, 1, 3, 2]
    a: set[int] = {x for x in xs}
    b: set[int] = {x for x in xs if x > 1}
    c: list[int] = [x * 2 for x in xs]
    d: list[int] = [x for x in xs if x != 3]
    s: set[int] = {1, 2}
    e: set[int] = {y + 1 for y in s}
    print(len(a))
    print(len(b))
    print(c[3])
    print(len(d))
    print(len(e))
    print(len(xs))
    return 0
