"""Demonstration of automatic type inference in MultiGen.

MultiGen can infer types for local variables from their usage,
eliminating the need for explicit type annotations in many cases.
"""


def infer_from_empty_list() -> int:
    """Type inferred from list.append() usage."""
    # numbers is inferred as list[int] from append(10)
    numbers = []
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)
    return len(numbers)


def infer_from_function_return() -> int:
    """Type inferred from function return value."""
    # result is inferred as int from infer_from_empty_list() -> int
    result = infer_from_empty_list()
    return result * 2


def infer_from_literal() -> str:
    """Type inferred from literal assignment."""
    # message is inferred as str from literal
    message = "Hello, type inference!"
    return message


def infer_from_operations() -> int:
    """Type inferred from arithmetic operations."""
    # x is inferred as int from literal
    x = 5
    # y is inferred as int from arithmetic
    y = x * 2
    # z is inferred as int from addition
    z = x + y
    return z


def main() -> int:
    """Main function demonstrating type inference."""
    count: int = infer_from_empty_list()
    print(count)

    doubled: int = infer_from_function_return()
    print(doubled)

    msg: str = infer_from_literal()
    print(msg)

    result: int = infer_from_operations()
    print(result)

    return 0
