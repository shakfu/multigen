"""
F-String Demo - MultiGen v0.1.86+

Demonstrates MultiGen's f-string support across all backends.
"""

def greet(name: str) -> str:
    """Simple f-string with string interpolation."""
    return f"Hello, {name}!"

def format_numbers(x: int, y: float) -> str:
    """F-string with multiple types."""
    return f"Integer: {x}, Float: {y}"

def calculate_and_format(a: int, b: int) -> str:
    """F-string with expressions."""
    return f"Sum of {a} and {b} is {a + b}"

def format_list_info(items: list[int]) -> str:
    """F-string with function calls."""
    return f"List has {len(items)} items"

def format_boolean(flag: bool) -> str:
    """F-string with boolean values."""
    return f"Flag is {flag}"

def complex_example(name: str, age: int, score: float, active: bool) -> str:
    """Complex f-string with multiple parts."""
    return f"User {name} (age {age}) has score {score} - Active: {active}"

def main() -> None:
    """Demonstrate all f-string features."""
    # Simple string interpolation
    greeting: str = greet("World")
    print(greeting)

    # Multiple types
    numbers: str = format_numbers(42, 3.14)
    print(numbers)

    # Expression in f-string
    calculation: str = calculate_and_format(10, 20)
    print(calculation)

    # Function call in f-string
    my_list: list[int] = [1, 2, 3, 4, 5]
    list_info: str = format_list_info(my_list)
    print(list_info)

    # Boolean in f-string
    bool_result: str = format_boolean(True)
    print(bool_result)

    # Complex example
    user_info: str = complex_example("Alice", 30, 95.5, True)
    print(user_info)

# Run the demo
main()
