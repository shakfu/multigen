"""Item assignment into lists, nested lists and dicts."""


def main() -> int:
    xs: list[int] = [1, 2, 3]
    xs[0] = 9
    xs[2] = 7
    m: list[list[int]] = [[0, 0], [0, 0]]
    m[1][0] = 5
    d: dict[str, int] = {}
    d["a"] = 1
    d["a"] = d["a"] + 1
    print(xs[0])
    print(xs[2])
    print(m[1][0])
    print(d["a"])
    return 0
