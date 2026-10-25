"""Float arithmetic, int promotion, true division and float printing."""


def mean(a: float, b: float) -> float:
    return (a + b) / 2


def scale(x: float, k: int) -> float:
    return x * k


def main() -> None:
    x: float = 2.5
    n: int = 4
    print(x + n)
    print(n - x)
    print(scale(x, 3))
    print(mean(1.0, 2.0))
    print(7 / 2)
    print(-x)
    print(x > n)
    print(x < 3)
    y: float = 1.0
    y += 0.5
    y *= 2
    print(y)
    print(0.1)
    print(1.0 / 3.0)
    print(0.00001)
    print(-7.5 // 2.0)
    print(-7.5 % 2.0)


if __name__ == "__main__":
    main()
