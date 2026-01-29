"""Data Pipeline - Multi-stage data transformation pipeline.

This example demonstrates:
- Multi-stage data processing
- Function composition
- List comprehensions
- Dictionary aggregations
- Practical ETL (Extract, Transform, Load) pattern
"""


def extract_numbers(text: str) -> list:
    """Extract numeric values from text (simplified).

    Args:
        text: Input text

    Returns:
        List of integers found in text
    """
    # Simplified: just return some sample data
    numbers: list = []
    numbers.append(10)
    numbers.append(25)
    numbers.append(15)
    numbers.append(30)
    numbers.append(20)
    return numbers


def transform_data(numbers: list, threshold: int) -> list:
    """Filter and transform numbers above threshold.

    Args:
        numbers: Input list of numbers
        threshold: Minimum value to keep

    Returns:
        Transformed list
    """
    result: list = []

    for num in numbers:
        if num >= threshold:
            # Transform: double the value
            transformed: int = num * 2
            result.append(transformed)

    return result


def aggregate_stats(numbers: list) -> dict:
    """Calculate aggregated statistics.

    Args:
        numbers: List of numbers

    Returns:
        Dictionary with statistics
    """
    if len(numbers) == 0:
        empty: dict = {}
        empty["total"] = 0
        empty["count"] = 0
        empty["average"] = 0
        return empty

    total: int = 0
    for num in numbers:
        total += num

    stats: dict = {}
    stats["total"] = total
    stats["count"] = len(numbers)
    stats["average"] = total // len(numbers)  # Integer division

    return stats


def run_pipeline(text: str, threshold: int) -> dict:
    """Run complete data processing pipeline.

    Args:
        text: Input text data
        threshold: Filter threshold

    Returns:
        Final statistics
    """
    # Stage 1: Extract
    raw_data: list = extract_numbers(text)

    # Stage 2: Transform
    processed_data: list = transform_data(raw_data, threshold)

    # Stage 3: Aggregate
    final_stats: dict = aggregate_stats(processed_data)

    return final_stats


def main() -> int:
    """Main entry point for data pipeline."""
    # Sample input
    input_text: str = "sample data"
    filter_threshold: int = 20

    # Run pipeline
    results: dict = run_pipeline(input_text, filter_threshold)

    # Print results
    print(results["total"])
    print(results["count"])
    print(results["average"])

    return 0
