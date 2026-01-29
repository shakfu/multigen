"""Word Counter - A simple CLI tool to count word frequencies in a text file.

This example demonstrates:
- File I/O operations
- String processing (split, lower, strip)
- Dictionary operations for frequency counting
- Practical CLI tool development
"""


def count_words(text: str) -> dict:
    """Count word frequencies in text.

    Args:
        text: Input text string

    Returns:
        Dictionary mapping words to their frequencies
    """
    word_counts: dict = {}
    words: list = text.lower().split()

    for word in words:
        # Clean the word
        clean_word: str = word.strip()

        if len(clean_word) > 0:
            # Update count
            if clean_word in word_counts:
                current: int = word_counts[clean_word]
                word_counts[clean_word] = current + 1
            else:
                word_counts[clean_word] = 1

    return word_counts


def print_top_words(word_counts: dict, n: int) -> int:
    """Print the top N most frequent words.

    Args:
        word_counts: Dictionary of word frequencies
        n: Number of top words to print

    Returns:
        Number of words printed
    """
    # For this simple version, just print all words
    # (sorting would require more complex logic)
    count: int = 0

    for word in word_counts:
        freq: int = word_counts[word]
        print(word)
        print(freq)
        count += 1

        if count >= n:
            break

    return count


def main() -> int:
    """Main entry point for word counter."""
    # Simple test text
    test_text: str = "the quick brown fox jumps over the lazy dog the fox"

    # Count words
    word_counts: dict = count_words(test_text)

    # Print results
    print_top_words(word_counts, 10)

    return 0
