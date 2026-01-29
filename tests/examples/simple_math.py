"""Simple mathematical operations example."""


def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y


def multiply(x: int, y: int) -> int:
    """Multiply two integers."""
    return x * y


def fibonacci(n: int) -> int:
    """Calculate nth Fibonacci number."""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)


def main():
    """Main function."""
    a = 10
    b = 5

    result = add(a, b)
    product = multiply(a, b)
    fib = fibonacci(8)

    print(f"{a} + {b} = {result}")
    print(f"{a} * {b} = {product}")
    print(f"fibonacci(8) = {fib}")


if __name__ == "__main__":
    main()
