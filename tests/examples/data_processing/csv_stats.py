"""CSV Statistics - Calculate basic statistics from CSV-like data.

This example demonstrates:
- Data parsing and processing
- Numerical computations
- List operations and aggregations
- Practical data analysis
"""


def parse_csv_line(line: str) -> list:
    """Parse a CSV line into fields.

    Args:
        line: CSV line string

    Returns:
        List of field strings
    """
    # Simple split on comma (no quote handling for simplicity)
    fields: list = []
    parts: list = line.split()  # Split on whitespace for simplicity

    for part in parts:
        fields.append(part)

    return fields


def calculate_stats(numbers: list) -> dict:
    """Calculate basic statistics for a list of numbers.

    Args:
        numbers: List of integers

    Returns:
        Dictionary with min, max, sum, and count
    """
    if len(numbers) == 0:
        stats: dict = {}
        stats["min"] = 0
        stats["max"] = 0
        stats["sum"] = 0
        stats["count"] = 0
        return stats

    min_val: int = numbers[0]
    max_val: int = numbers[0]
    total: int = 0

    for num in numbers:
        if num < min_val:
            min_val = num
        if num > max_val:
            max_val = num
        total += num

    result: dict = {}
    result["min"] = min_val
    result["max"] = max_val
    result["sum"] = total
    result["count"] = len(numbers)

    return result


def main() -> int:
    """Main entry point for CSV statistics."""
    # Sample data: scores from a test
    scores: list = []
    scores.append(85)
    scores.append(92)
    scores.append(78)
    scores.append(95)
    scores.append(88)
    scores.append(91)

    # Calculate statistics
    stats: dict = calculate_stats(scores)

    # Print results
    print(stats["min"])
    print(stats["max"])
    print(stats["sum"])
    print(stats["count"])

    return 0
