"""Number Guessing Game - Simple interactive game.

This example demonstrates:
- Game logic and state management
- Conditional logic
- User interaction simulation
- Fun practical application
"""


def check_guess(guess: int, target: int) -> int:
    """Check if guess matches target.

    Args:
        guess: The guessed number
        target: The target number

    Returns:
        -1 if guess is too low, 1 if too high, 0 if correct
    """
    if guess < target:
        return -1
    elif guess > target:
        return 1
    else:
        return 0


def play_game(target: int, guesses: list) -> int:
    """Play the number guessing game with a list of guesses.

    Args:
        target: The number to guess
        guesses: List of guesses to try

    Returns:
        Number of attempts needed to find the answer
    """
    attempts: int = 0

    for guess in guesses:
        attempts += 1
        result: int = check_guess(guess, target)

        if result == 0:
            # Correct guess!
            return attempts
        elif result == -1:
            # Too low
            print(0)  # 0 represents "too low"
        else:
            # Too high
            print(1)  # 1 represents "too high"

    return attempts


def main() -> int:
    """Main entry point for number guessing game."""
    # Secret number
    secret: int = 42

    # Pre-defined sequence of guesses (simulating user input)
    guesses: list = []
    guesses.append(25)
    guesses.append(50)
    guesses.append(37)
    guesses.append(43)
    guesses.append(40)
    guesses.append(42)

    # Play the game
    attempts: int = play_game(secret, guesses)

    # Print results
    print(attempts)

    return 0
