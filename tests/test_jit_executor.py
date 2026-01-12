"""Tests for LLVM JIT executor.

This module tests the JIT compilation and execution functionality
of the LLVM backend using llvmlite's execution engine.
"""

import subprocess
import tempfile
from pathlib import Path

import pytest

# Check if llvmlite binding is available
try:
    import llvmlite.binding as llvm

    LLVM_AVAILABLE = True
except ImportError:
    LLVM_AVAILABLE = False


@pytest.mark.skipif(not LLVM_AVAILABLE, reason="llvmlite binding not available")
def test_jit_simple_function():
    """Test JIT compilation of simple LLVM IR function."""
    from multigen.backends.llvm.jit_executor import LLVMJITExecutor

    # Simple LLVM IR that adds two numbers
    llvm_ir = """
    define i64 @add(i64 %a, i64 %b) {
        %sum = add i64 %a, %b
        ret i64 %sum
    }
    """

    executor = LLVMJITExecutor()
    try:
        # Compile IR
        mod = executor.compile_ir(llvm_ir)
        assert mod is not None

        # Execute function
        result = executor.execute_function("add", 10, 20)
        assert result == 30

    finally:
        executor.cleanup()


@pytest.mark.skipif(not LLVM_AVAILABLE, reason="llvmlite binding not available")
def test_jit_fibonacci():
    """Test JIT compilation of fibonacci benchmark."""
    from multigen.backends.llvm.jit_executor import jit_compile_and_run

    # Generate LLVM IR for fibonacci
    project_root = Path(__file__).parent.parent
    benchmark_file = project_root / "tests/benchmarks/algorithms/fibonacci.py"

    if not benchmark_file.exists():
        pytest.skip("Fibonacci benchmark not found")

    # Generate LLVM IR using multigen
    with tempfile.TemporaryDirectory() as tmpdir:
        output_dir = Path(tmpdir)
        ll_file = output_dir / "src" / "fibonacci.ll"

        # Run multigen to generate LLVM IR (outputs to <build-dir>/src/)
        result = subprocess.run(
            [
                "uv", "run", "multigen",
                "--build-dir", str(output_dir),
                "convert", "-t", "llvm", str(benchmark_file),
            ],
            capture_output=True,
            text=True,
            cwd=project_root,
        )

        if result.returncode != 0:
            pytest.skip(f"Could not generate LLVM IR: {result.stderr}")

        if not ll_file.exists():
            pytest.skip("LLVM IR file not generated")

        # JIT compile and execute
        try:
            exit_code = jit_compile_and_run(str(ll_file), verbose=False)
            # Fibonacci should return 0 on success
            assert exit_code == 0
        except Exception as e:
            pytest.fail(f"JIT execution failed: {e}")


@pytest.mark.skipif(not LLVM_AVAILABLE, reason="llvmlite binding not available")
def test_jit_main_function():
    """Test JIT compilation of main() function."""
    from multigen.backends.llvm.jit_executor import LLVMJITExecutor

    # Simple main that returns 42
    llvm_ir = """
    define i64 @main() {
        ret i64 42
    }
    """

    executor = LLVMJITExecutor()
    try:
        # Compile IR
        executor.compile_ir(llvm_ir)

        # Execute main
        result = executor.execute_main()
        assert result == 42

    finally:
        executor.cleanup()


@pytest.mark.skipif(not LLVM_AVAILABLE, reason="llvmlite binding not available")
def test_jit_invalid_ir():
    """Test that invalid LLVM IR raises error."""
    from multigen.backends.llvm.jit_executor import LLVMJITExecutor

    # Invalid LLVM IR (bad syntax)
    llvm_ir = """
    define i64 @bad_function {
        this is not valid LLVM IR
    }
    """

    executor = LLVMJITExecutor()
    try:
        with pytest.raises(Exception):
            executor.compile_ir(llvm_ir)

    finally:
        executor.cleanup()


@pytest.mark.skipif(not LLVM_AVAILABLE, reason="llvmlite binding not available")
def test_jit_missing_function():
    """Test that calling missing function raises error."""
    from multigen.backends.llvm.jit_executor import LLVMJITExecutor

    llvm_ir = """
    define i64 @existing_function() {
        ret i64 0
    }
    """

    executor = LLVMJITExecutor()
    try:
        executor.compile_ir(llvm_ir)

        # Try to get address of non-existent function
        with pytest.raises(RuntimeError, match="not found"):
            executor.get_function_address("nonexistent_function")

    finally:
        executor.cleanup()
