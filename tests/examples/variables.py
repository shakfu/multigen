"""Variable declarations and basic operations."""


def process_numbers(x: int, y: float) -> float:
    """Process numbers with different types."""
    total: float = x + y
    doubled: float = total * 2.0
    return doubled


def boolean_logic(a: bool, b: bool) -> bool:
    """Demonstrate boolean operations."""
    result: bool = a and b
    return result


def string_example(name: str) -> str:
    """Simple string operations."""
    greeting: str = "Hello, " + name + "!"
    return greeting


def main():
    """Main function demonstrating different types."""
    num_result = process_numbers(42, 3.14)
    bool_result = boolean_logic(True, False)
    str_result = string_example("World")

    print(f"Number result: {num_result}")
    print(f"Boolean result: {bool_result}")
    print(f"String result: {str_result}")


if __name__ == "__main__":
    main()
