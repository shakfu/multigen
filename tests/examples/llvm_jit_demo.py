#!/usr/bin/env python3
"""Demo script comparing LLVM JIT vs AOT compilation.

This script demonstrates two compilation modes:
1. AOT (Ahead-of-Time): Generate .ll → llc → clang → executable
2. JIT (Just-in-Time): Generate .ll → execute in-memory

Usage:
    python examples/llvm_jit_demo.py
"""

import subprocess
import tempfile
import time
from pathlib import Path

from multigen.backends.llvm.jit_executor import jit_compile_and_run


def generate_llvm_ir(python_file: Path, output_dir: Path) -> Path:
    """Generate LLVM IR from Python file.

    Args:
        python_file: Path to Python source
        output_dir: Directory for output

    Returns:
        Path to generated .ll file
    """
    # Run multigen to generate LLVM IR
    result = subprocess.run(
        ["uv", "run", "multigen", "convert", "-t", "llvm", str(python_file)],
        cwd=output_dir,
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        raise RuntimeError(f"LLVM IR generation failed: {result.stderr}")

    # Find generated .ll file
    ll_files = list(output_dir.glob("build/src/*.ll"))
    if not ll_files:
        raise RuntimeError("No .ll file generated")

    return ll_files[0]


def aot_compile_and_run(python_file: Path) -> tuple[float, float, int]:
    """AOT compile and execute Python file.

    Args:
        python_file: Path to Python source

    Returns:
        Tuple of (compile_time, execution_time, exit_code)
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        output_dir = Path(tmpdir)

        # Compile
        compile_start = time.time()
        result = subprocess.run(
            ["uv", "run", "multigen", "build", "-t", "llvm", str(python_file)],
            cwd=output_dir,
            capture_output=True,
            text=True,
        )
        compile_time = time.time() - compile_start

        if result.returncode != 0:
            raise RuntimeError(f"Compilation failed: {result.stderr}")

        # Find executable
        executables = list(output_dir.glob("build/*"))
        executables = [e for e in executables if e.is_file() and not e.suffix]

        if not executables:
            raise RuntimeError("No executable found")

        executable = executables[0]

        # Execute
        exec_start = time.time()
        result = subprocess.run(
            [str(executable)],
            capture_output=True,
            text=True,
        )
        exec_time = time.time() - exec_start

        return compile_time, exec_time, result.returncode


def jit_compile_and_execute(python_file: Path) -> tuple[float, float, int]:
    """JIT compile and execute Python file.

    Args:
        python_file: Path to Python source

    Returns:
        Tuple of (compile_time, execution_time, exit_code)
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        output_dir = Path(tmpdir)

        # Generate LLVM IR
        compile_start = time.time()
        ll_file = generate_llvm_ir(python_file, output_dir)
        compile_time = time.time() - compile_start

        # JIT execute
        exec_start = time.time()
        exit_code = jit_compile_and_run(str(ll_file), verbose=False)
        exec_time = time.time() - exec_start

        return compile_time, exec_time, exit_code


def main():
    """Run LLVM JIT vs AOT comparison."""
    project_root = Path(__file__).parent.parent
    benchmark_file = project_root / "tests/benchmarks/algorithms/fibonacci.py"

    if not benchmark_file.exists():
        print(f"Error: Benchmark file not found: {benchmark_file}")
        return

    print("=" * 70)
    print("LLVM Backend: JIT vs AOT Compilation Comparison")
    print("=" * 70)
    print(f"\nBenchmark: {benchmark_file.name}")
    print()

    # AOT Compilation
    print("1. AOT (Ahead-of-Time) Compilation:")
    print("   Pipeline: Python → LLVM IR → llc (machine code) → clang (link)")
    print("   " + "-" * 60)

    try:
        aot_compile, aot_exec, aot_code = aot_compile_and_run(benchmark_file)
        print(f"   Compile time: {aot_compile*1000:.1f} ms")
        print(f"   Execution time: {aot_exec*1000:.1f} ms")
        print(f"   Total time: {(aot_compile + aot_exec)*1000:.1f} ms")
        print(f"   Exit code: {aot_code}")
        print("   Benefits: Standalone executable, optimal performance")
    except Exception as e:
        print(f"   Error: {e}")
        aot_compile = aot_exec = float("inf")

    print()

    # JIT Compilation
    print("2. JIT (Just-in-Time) Compilation:")
    print("   Pipeline: Python → LLVM IR → Execute in-memory")
    print("   " + "-" * 60)

    try:
        jit_compile, jit_exec, jit_code = jit_compile_and_execute(benchmark_file)
        print(f"   Compile time: {jit_compile*1000:.1f} ms")
        print(f"   Execution time: {jit_exec*1000:.1f} ms")
        print(f"   Total time: {(jit_compile + jit_exec)*1000:.1f} ms")
        print(f"   Exit code: {jit_code}")
        print("   Benefits: Fast development cycle, no intermediate files")
    except Exception as e:
        print(f"   Error: {e}")
        jit_compile = jit_exec = float("inf")

    print()

    # Comparison
    print("=" * 70)
    print("Summary:")
    print("=" * 70)

    if aot_compile != float("inf") and jit_compile != float("inf"):
        compile_speedup = aot_compile / jit_compile
        exec_speedup = aot_exec / jit_exec
        total_speedup = (aot_compile + aot_exec) / (jit_compile + jit_exec)

        print(f"\nJIT compile time: {compile_speedup:.2f}x faster than AOT")
        print(f"JIT execution time: {exec_speedup:.2f}x {'faster' if exec_speedup > 1 else 'slower'} than AOT")
        print(f"JIT total time: {total_speedup:.2f}x {'faster' if total_speedup > 1 else 'slower'} than AOT")

        print("\nRecommendations:")
        if total_speedup > 1.5:
            print("- Use JIT for development, testing, and rapid iteration")
            print("- Use AOT for production deployment (standalone executables)")
        else:
            print("- Both modes have similar performance")
            print("- Choose based on deployment requirements")

    print()
    print("Note: JIT mode requires llvmlite runtime dependency")
    print("      AOT mode produces standalone executables")
    print()


if __name__ == "__main__":
    main()
