"""Colored error formatting for MultiGen errors.

This module provides beautiful, colored error output similar to modern compilers
like Rust's rustc or TypeScript's tsc.
"""

import os
import sys
from pathlib import Path
from typing import IO, Optional

from .errors import MultiGenError, SourceLocation

# ANSI color codes
RESET = "\033[0m"
BOLD = "\033[1m"
DIM = "\033[2m"

# Colors
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
BLUE = "\033[34m"
MAGENTA = "\033[35m"
CYAN = "\033[36m"
WHITE = "\033[37m"

# Bright colors
BRIGHT_RED = "\033[91m"
BRIGHT_GREEN = "\033[92m"
BRIGHT_YELLOW = "\033[93m"
BRIGHT_BLUE = "\033[94m"
BRIGHT_MAGENTA = "\033[95m"
BRIGHT_CYAN = "\033[96m"


def supports_color() -> bool:
    """Check if terminal supports ANSI color codes."""
    # Disable colors on Windows unless explicitly enabled
    if sys.platform == "win32":
        # Check for Windows Terminal or ConEmu
        return "WT_SESSION" in sys.environ or "ConEmuANSI" in sys.environ

    # Check if stdout is a TTY
    if not hasattr(sys.stdout, "isatty"):
        return False

    if not sys.stdout.isatty():
        return False

    # Check TERM environment variable
    term = os.environ.get("TERM", "")
    if term == "dumb":
        return False

    return True


class ErrorFormatter:
    """Formats errors with colors and helpful context."""

    def __init__(self, use_color: Optional[bool] = None):
        """Initialize formatter.

        Args:
            use_color: Force color on/off, or None for auto-detect
        """
        if use_color is None:
            self.use_color = supports_color()
        else:
            self.use_color = use_color

    def colorize(self, text: str, color: str) -> str:
        """Apply color to text if colors enabled."""
        if not self.use_color:
            return text
        return f"{color}{text}{RESET}"

    def format_error(self, error: MultiGenError) -> str:
        """Format error with full context and colors.

        Args:
            error: The error to format

        Returns:
            Formatted error message
        """
        lines = []

        # Error header
        error_label = self.colorize("error", BOLD + BRIGHT_RED)
        error_code = ""
        if error.context.error_code:
            error_code = self.colorize(f"[{error.context.error_code.name}]", DIM)

        lines.append(f"{error_label}{error_code}: {str(error)}")

        # Location
        if error.context.location:
            location_str = self._format_location(error.context.location)
            lines.append(f"  {self.colorize('-->', BRIGHT_BLUE)} {location_str}")

            # Source snippet
            if error.context.source_line:
                snippet = self._format_source_snippet(error.context.location, error.context.source_line)
                lines.append(snippet)

        # Suggestion
        if error.context.suggestion:
            suggestion_label = self.colorize("help", BOLD + BRIGHT_GREEN)
            lines.append(f"{suggestion_label}: {error.context.suggestion}")

        # Help text
        if error.context.help_text:
            help_label = self.colorize("note", BOLD + BRIGHT_CYAN)
            lines.append(f"{help_label}: {error.context.help_text}")

        return "\n".join(lines)

    def _format_location(self, location: SourceLocation) -> str:
        """Format source location."""
        # Make filename relative to current directory if possible
        try:
            path = Path(location.filename)
            cwd = Path.cwd()
            if path.is_absolute() and path.is_relative_to(cwd):
                rel_path = path.relative_to(cwd)
                filename = str(rel_path)
            else:
                filename = location.filename
        except (ValueError, OSError):
            filename = location.filename

        location_str = f"{filename}:{location.line}"
        if location.column > 0:
            location_str += f":{location.column}"

        return self.colorize(location_str, BOLD)

    def _format_source_snippet(self, location: SourceLocation, source_line: str) -> str:
        """Format source code snippet with error indicator.

        Args:
            location: Source location
            source_line: The source code line

        Returns:
            Formatted snippet with line number and error indicator
        """
        lines = []

        # Line number width
        line_num_width = len(str(location.line))

        # Blank line
        blank_prefix = self.colorize(" " * line_num_width + " |", BRIGHT_BLUE)
        lines.append(blank_prefix)

        # Source line with line number
        line_num = self.colorize(f"{location.line:>{line_num_width}}", BRIGHT_BLUE)
        separator = self.colorize(" | ", BRIGHT_BLUE)
        lines.append(f"{line_num}{separator}{source_line.rstrip()}")

        # Error indicator (caret)
        if location.column > 0:
            # Calculate spaces before caret (accounting for line number and separator)
            spaces_before = " " * (line_num_width + 3 + location.column)
            caret = self.colorize("^", BRIGHT_RED)

            # If we have end_column, show range
            if location.end_column and location.end_column > location.column:
                length = location.end_column - location.column
                underline = self.colorize("^" * (length - 1), BRIGHT_RED)
                lines.append(f"{spaces_before}{caret}{underline}")
            else:
                lines.append(f"{spaces_before}{caret}")

        # Blank line
        lines.append(blank_prefix)

        return "\n".join(lines)

    def format_simple(self, error: Exception) -> str:
        """Format a regular exception (non-MultiGen error).

        Args:
            error: The exception to format

        Returns:
            Formatted error message
        """
        error_label = self.colorize("error", BOLD + BRIGHT_RED)
        error_type = type(error).__name__
        return f"{error_label}: {error_type}: {str(error)}"


# Global formatter instance
_formatter: Optional[ErrorFormatter] = None


def get_formatter() -> ErrorFormatter:
    """Get global error formatter instance."""
    global _formatter
    if _formatter is None:
        _formatter = ErrorFormatter()
    return _formatter


def set_color_mode(use_color: bool) -> None:
    """Set color mode globally.

    Args:
        use_color: Whether to use colors in error output
    """
    global _formatter
    _formatter = ErrorFormatter(use_color=use_color)


def format_error(error: Exception) -> str:
    """Format any error with appropriate formatter.

    Args:
        error: Error to format

    Returns:
        Formatted error string
    """
    formatter = get_formatter()
    if isinstance(error, MultiGenError):
        return formatter.format_error(error)
    else:
        return formatter.format_simple(error)


def print_error(error: Exception, file: Optional[IO[str]] = None) -> None:
    """Print formatted error to stderr or specified file.

    Args:
        error: Error to print
        file: File to write to (default: stderr)
    """
    output_file: IO[str] = file if file is not None else sys.stderr

    formatted = format_error(error)
    print(formatted, file=output_file)
