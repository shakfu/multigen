"""A variable rebound from itself inside for loops, as a plain and a mutated local."""


def doubled(n: int) -> int:
    x: int = 1
    for i in range(n):
        x = x * 2
    return x


def counted(n: int) -> int:
    x: int = 0
    while x < 3:
        x += 1
    for i in range(n):
        x = x + i
    return x


def main() -> int:
    print(doubled(10))
    print(counted(4))
    return 0
