"""Hello World in MultiGen."""


def greet(name: str) -> str:
    """Return a greeting message."""
    return "Hello, " + name + "!"


def main() -> int:
    """Main function."""
    message: str = greet("World")
    print(message)
    return 0
