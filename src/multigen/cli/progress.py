"""Progress indicators for MultiGen CLI operations."""

import sys
import time
from collections.abc import Generator
from contextlib import contextmanager
from typing import Optional


class ProgressIndicator:
    """Simple progress indicator for CLI operations."""

    def __init__(self, enabled: bool = True, verbose: bool = False):
        """Initialize progress indicator.

        Args:
            enabled: Whether to show progress indicators
            verbose: Whether to show verbose output
        """
        self.enabled = enabled
        self.verbose = verbose
        self._current_step = 0
        self._total_steps = 0
        self._start_time: Optional[float] = None
        self._step_start_time: Optional[float] = None

    def start(self, total_steps: int) -> None:
        """Start progress tracking.

        Args:
            total_steps: Total number of steps in the operation
        """
        if not self.enabled:
            return

        self._total_steps = total_steps
        self._current_step = 0
        self._start_time = time.time()
        self._step_start_time = self._start_time

    def step(self, message: str) -> None:
        """Advance to next step with message.

        Args:
            message: Description of the current step
        """
        if not self.enabled:
            return

        self._current_step += 1

        # Calculate progress
        progress_pct = (self._current_step / self._total_steps * 100) if self._total_steps > 0 else 0

        # Calculate timing
        current_time = time.time()
        step_elapsed = current_time - (self._step_start_time or current_time)
        total_elapsed = current_time - (self._start_time or current_time)

        # Format progress bar
        bar_width = 30
        filled = int(bar_width * self._current_step / self._total_steps) if self._total_steps > 0 else 0
        bar = "█" * filled + "░" * (bar_width - filled)

        # Print progress
        sys.stderr.write(f"\r[{bar}] {progress_pct:5.1f}% | {message}")
        sys.stderr.flush()

        if self.verbose:
            sys.stderr.write(f" ({step_elapsed:.2f}s)\n")

        self._step_start_time = current_time

    def finish(self, message: str = "Complete") -> None:
        """Finish progress tracking.

        Args:
            message: Final completion message
        """
        if not self.enabled:
            return

        total_elapsed = time.time() - (self._start_time or time.time())
        sys.stderr.write(f"\r✓ {message} (total: {total_elapsed:.2f}s)\n")
        sys.stderr.flush()

    def fail(self, message: str = "Failed") -> None:
        """Mark progress as failed.

        Args:
            message: Failure message
        """
        if not self.enabled:
            return

        sys.stderr.write(f"\r✗ {message}\n")
        sys.stderr.flush()


@contextmanager
def progress_context(
    title: str, enabled: bool = True, verbose: bool = False
) -> Generator[ProgressIndicator, None, None]:
    """Context manager for simple progress tracking.

    Args:
        title: Title of the operation
        enabled: Whether to show progress
        verbose: Whether to show verbose output

    Yields:
        ProgressIndicator instance

    Example:
        with progress_context("Converting Python to C", enabled=True) as progress:
            progress.start(5)
            progress.step("Parsing source code")
            progress.step("Validating AST")
            progress.step("Analyzing types")
            progress.step("Generating C code")
            progress.step("Writing output")
            progress.finish()
    """
    if enabled:
        sys.stderr.write(f"⚙ {title}...\n")
        sys.stderr.flush()

    progress = ProgressIndicator(enabled=enabled, verbose=verbose)

    try:
        yield progress
    except Exception as e:
        progress.fail(f"Failed: {e}")
        raise


class Spinner:
    """Simple spinner for indeterminate progress."""

    FRAMES = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

    def __init__(self, message: str = "Working", enabled: bool = True):
        """Initialize spinner.

        Args:
            message: Message to display
            enabled: Whether to show spinner
        """
        self.message = message
        self.enabled = enabled
        self._frame = 0
        self._start_time: Optional[float] = None

    def start(self) -> None:
        """Start the spinner."""
        if not self.enabled:
            return
        self._start_time = time.time()
        self._update()

    def _update(self) -> None:
        """Update spinner frame."""
        if not self.enabled:
            return
        frame = self.FRAMES[self._frame % len(self.FRAMES)]
        elapsed = time.time() - (self._start_time or time.time())
        sys.stderr.write(f"\r{frame} {self.message}... ({elapsed:.1f}s)")
        sys.stderr.flush()
        self._frame += 1

    def tick(self) -> None:
        """Advance spinner one frame."""
        self._update()

    def stop(self, message: str = "Done") -> None:
        """Stop the spinner.

        Args:
            message: Completion message
        """
        if not self.enabled:
            return
        elapsed = time.time() - (self._start_time or time.time())
        sys.stderr.write(f"\r✓ {message} ({elapsed:.2f}s)\n")
        sys.stderr.flush()


@contextmanager
def spinner_context(message: str = "Working", enabled: bool = True) -> Generator[Spinner, None, None]:
    """Context manager for spinner.

    Args:
        message: Message to display
        enabled: Whether to show spinner

    Yields:
        Spinner instance

    Example:
        with spinner_context("Compiling", enabled=True) as spinner:
            # Do work, optionally calling spinner.tick() periodically
            compile_code()
            spinner.stop("Compilation complete")
    """
    spinner = Spinner(message, enabled=enabled)
    spinner.start()

    try:
        yield spinner
    except Exception as e:
        if enabled:
            sys.stderr.write(f"\r✗ Failed: {e}\n")
            sys.stderr.flush()
        raise
