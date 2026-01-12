"""Demonstration of strict verification mode.

This example shows how strict verification mode can catch unsafe memory
accesses during Python→C translation, before compilation or testing.

Usage:
    python examples/strict_verification_demo.py
"""

import sys
import tempfile
from pathlib import Path

# Add src to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root / "src"))

try:
    import z3  # noqa: F401

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    print("ERROR: Z3 not installed. Install with: pip install multigen[z3]")
    sys.exit(1)

from multigen.pipeline import MultiGenPipeline, PipelineConfig


def demo_safe_code():
    """Demonstrate that safe code passes strict verification."""
    print("=" * 70)
    print("DEMO 1: Safe Code (Passes Strict Verification)")
    print("=" * 70)
    print()

    safe_code = """
def safe_array_sum(arr: list[int], n: int) -> int:
    '''This is safe: we only access arr[i] for i in range(n).
    If caller ensures len(arr) >= n, Z3 can prove safety.'''
    total: int = 0
    for i in range(n):
        total = total + arr[i]
    return total
"""

    print("Python Code:")
    print(safe_code)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(safe_code)
        temp_path = Path(f.name)

    try:
        # Run with strict verification
        config = PipelineConfig(
            target_language="c", enable_formal_verification=True, strict_verification=True
        )
        pipeline = MultiGenPipeline(config=config)
        result = pipeline.convert(temp_path)

        if result.success:
            print("✓ VERIFICATION PASSED")
            print("  Code generation succeeded in strict mode")
            print("  Memory safety verified by Z3")
        else:
            print("✗ VERIFICATION FAILED")
            for error in result.errors:
                print(f"  Error: {error}")
    finally:
        temp_path.unlink()

    print()


def demo_unsafe_code():
    """Demonstrate that unsafe code is blocked by strict verification."""
    print("=" * 70)
    print("DEMO 2: Unsafe Code (Blocked by Strict Verification)")
    print("=" * 70)
    print()

    unsafe_code = """
def unsafe_array_access(arr: list[int], n: int) -> int:
    '''This is UNSAFE: range(n+1) but no guarantee arr has n+1 elements.
    This is an off-by-one error - accessing arr[n] when arr might only have n elements.'''
    total: int = 0
    for i in range(n + 1):  # BUG: should be range(n)
        total = total + arr[i]
    return total
"""

    print("Python Code (with off-by-one bug):")
    print(unsafe_code)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(unsafe_code)
        temp_path = Path(f.name)

    try:
        # Run with strict verification
        config = PipelineConfig(
            target_language="c", enable_formal_verification=True, strict_verification=True
        )
        pipeline = MultiGenPipeline(config=config)
        result = pipeline.convert(temp_path)

        if result.success:
            print("✓ Code generation succeeded (verification didn't catch the bug)")
        else:
            print("✗ CODE GENERATION BLOCKED")
            print("  Strict verification prevented unsafe code from being generated")
            print()
            print("Errors:")
            for error in result.errors:
                print(f"  - {error}")
            print()
            print("This off-by-one error was caught BEFORE:")
            print("  - C code generation")
            print("  - Compilation")
            print("  - Testing")
            print("  - Production deployment")
    finally:
        temp_path.unlink()

    print()


def demo_warning_mode():
    """Demonstrate warning mode (non-strict)."""
    print("=" * 70)
    print("DEMO 3: Warning Mode (Non-Strict)")
    print("=" * 70)
    print()

    unsafe_code = """
def maybe_unsafe(arr: list[int], n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + arr[i]
    return total
"""

    print("Same potentially unsafe code, but in WARNING MODE:")
    print(unsafe_code)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(unsafe_code)
        temp_path = Path(f.name)

    try:
        # Run WITHOUT strict verification
        config = PipelineConfig(
            target_language="c",
            enable_formal_verification=True,
            strict_verification=False,  # Warning mode
        )
        pipeline = MultiGenPipeline(config=config)
        result = pipeline.convert(temp_path)

        print(f"Success: {result.success}")
        print(f"Warnings: {len(result.warnings)}")
        print(f"Errors: {len(result.errors)}")
        print()

        if result.warnings:
            print("Warnings issued (but code generation continued):")
            for warning in result.warnings:
                if "FORMAL_VERIFICATION" in warning:
                    print(f"  - {warning}")
        else:
            print("No verification warnings issued.")

        print()
        print("In warning mode:")
        print("  - Verification runs and reports issues")
        print("  - But code generation continues anyway")
        print("  - Useful for development/debugging")

    finally:
        temp_path.unlink()

    print()


if __name__ == "__main__":
    print()
    print("╔" + "═" * 68 + "╗")
    print("║" + " " * 15 + "STRICT VERIFICATION MODE DEMO" + " " * 24 + "║")
    print("╚" + "═" * 68 + "╝")
    print()

    demo_safe_code()
    demo_unsafe_code()
    demo_warning_mode()

    print("=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print()
    print("Strict Verification Mode provides:")
    print("  1. Mathematical proof of memory safety (via Z3)")
    print("  2. Prevents unsafe code from reaching production")
    print("  3. Catches bugs before compilation/testing")
    print("  4. CI/CD integration for safety-critical code")
    print()
    print("Enable with:")
    print("  config = PipelineConfig(")
    print("      enable_formal_verification=True,")
    print("      strict_verification=True")
    print("  )")
    print()
