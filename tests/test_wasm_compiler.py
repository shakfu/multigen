"""Tests for WebAssembly compilation functionality."""

import pytest
from pathlib import Path

# Check if llvmlite is available before importing LLVM backend
try:
    import llvmlite  # noqa: F401
    LLVMLITE_AVAILABLE = True
except ImportError:
    LLVMLITE_AVAILABLE = False

pytestmark = pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")

if LLVMLITE_AVAILABLE:
    from multigen.backends.llvm.wasm_compiler import (
        WebAssemblyCompiler,
        compile_to_webassembly,
    )


@pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")
class TestWebAssemblyCompiler:
    """Tests for WebAssembly compiler."""

    def test_compiler_initialization(self):
        """Test that WebAssembly compiler initializes correctly."""
        compiler = WebAssemblyCompiler()
        assert compiler.target_triple == "wasm32-unknown-unknown"
        assert compiler.target is not None

    def test_wasm32_wasi_target(self):
        """Test WASI target initialization."""
        compiler = WebAssemblyCompiler(target_triple="wasm32-wasi")
        assert compiler.target_triple == "wasm32-wasi"

    def test_extract_pure_functions(self):
        """Test extraction of user-defined functions from MultiGen IR."""
        # Simulated MultiGen IR with runtime declarations and user functions
        multigen_ir = """
; ModuleID = "multigen_module"
target triple = ""

declare void @vec_int_push(i64* %vec, i64 %value)
declare i64 @vec_int_at(i64* %vec, i64 %index)

define i64 @fibonacci(i64 %n) {
entry:
  %cmp = icmp sle i64 %n, 1
  br i1 %cmp, label %base, label %recurse
base:
  ret i64 %n
recurse:
  %n1 = sub i64 %n, 1
  %fib1 = call i64 @fibonacci(i64 %n1)
  %n2 = sub i64 %n, 2
  %fib2 = call i64 @fibonacci(i64 %n2)
  %result = add i64 %fib1, %fib2
  ret i64 %result
}

@str_1 = internal constant [20 x i8] c"Fibonacci function.\\00"

define i64 @main() {
entry:
  %result = call i64 @fibonacci(i64 10)
  ret i64 %result
}
"""
        compiler = WebAssemblyCompiler()
        pure_ir = compiler.extract_pure_functions(multigen_ir)

        # Should extract both functions
        assert "define i64 @fibonacci" in pure_ir
        assert "define i64 @main" in pure_ir

        # Should include string constant
        assert "@str_1 = internal constant" in pure_ir

        # Should NOT include runtime declarations
        assert "declare void @vec_int_push" not in pure_ir
        assert "declare i64 @vec_int_at" not in pure_ir

    def test_extract_pure_functions_no_functions(self):
        """Test extraction with IR that has no function definitions."""
        multigen_ir = """
; Only declarations
declare void @vec_int_push(i64* %vec, i64 %value)
declare i64 @vec_int_at(i64* %vec, i64 %index)
"""
        compiler = WebAssemblyCompiler()

        with pytest.raises(ValueError, match="No function definitions found"):
            compiler.extract_pure_functions(multigen_ir)

    def test_compile_simple_function(self, tmp_path):
        """Test compiling simple LLVM IR to WebAssembly."""
        simple_ir = """
target triple = "wasm32-unknown-unknown"

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}
"""
        compiler = WebAssemblyCompiler()
        output_path = tmp_path / "add.wasm"

        success = compiler.compile_to_wasm(simple_ir, output_path, opt_level=2)

        assert success is True
        assert output_path.exists()
        assert output_path.stat().st_size > 0

    def test_compile_text_format(self, tmp_path):
        """Test compiling to WebAssembly text format (.wat)."""
        simple_ir = """
target triple = "wasm32-unknown-unknown"

define i32 @multiply(i32 %a, i32 %b) {
entry:
  %result = mul i32 %a, %b
  ret i32 %result
}
"""
        compiler = WebAssemblyCompiler()
        output_path = tmp_path / "multiply.wat"

        success = compiler.compile_to_wasm(
            simple_ir, output_path, opt_level=2, text_format=True
        )

        assert success is True
        assert output_path.exists()

        # Verify it's text format
        wat_content = output_path.read_text()
        assert ".file" in wat_content or ".functype" in wat_content
        assert "multiply" in wat_content

    def test_compile_invalid_ir(self, tmp_path):
        """Test that invalid IR raises appropriate error."""
        invalid_ir = """
target triple = "wasm32-unknown-unknown"

define i32 @broken(i32 %a) {
  ; Missing entry label and return statement
}
"""
        compiler = WebAssemblyCompiler()
        output_path = tmp_path / "broken.wasm"

        with pytest.raises(ValueError, match="WebAssembly compilation failed"):
            compiler.compile_to_wasm(invalid_ir, output_path)

    def test_compile_with_optimization_levels(self, tmp_path):
        """Test compilation with different optimization levels."""
        simple_ir = """
target triple = "wasm32-unknown-unknown"

define i64 @factorial(i64 %n) {
entry:
  %cmp = icmp sle i64 %n, 1
  br i1 %cmp, label %base, label %recurse
base:
  ret i64 1
recurse:
  %n1 = sub i64 %n, 1
  %fact1 = call i64 @factorial(i64 %n1)
  %result = mul i64 %n, %fact1
  ret i64 %result
}
"""
        compiler = WebAssemblyCompiler()

        # Test all optimization levels
        sizes = {}
        for opt_level in [0, 1, 2, 3]:
            output_path = tmp_path / f"factorial_O{opt_level}.wasm"
            success = compiler.compile_to_wasm(simple_ir, output_path, opt_level=opt_level)
            assert success is True
            sizes[opt_level] = output_path.stat().st_size

        # Higher optimization should generally produce similar or smaller code
        # (though this isn't guaranteed for all functions)
        assert all(size > 0 for size in sizes.values())

    def test_get_info(self):
        """Test getting WebAssembly compiler information."""
        info = WebAssemblyCompiler.get_info()

        assert "llvmlite_available" in info
        assert info["llvmlite_available"] == "True"
        assert "status" in info
        assert info["status"] == "experimental"

        if LLVMLITE_AVAILABLE:
            assert "available_targets" in info
            # Should have at least wasm32-unknown-unknown
            assert "wasm32" in info["available_targets"]

    def test_compile_multigen_ir_integration(self, tmp_path):
        """Integration test with MultiGen-style IR (fibonacci without I/O)."""
        # Create fibonacci IR file dynamically (pure function, no I/O)
        ir_path = tmp_path / "fibonacci.ll"
        ir_path.write_text(
            """
; ModuleID = "multigen_fibonacci"
target triple = ""

define i64 @fibonacci(i64 %n) {
entry:
  %cmp = icmp sle i64 %n, 1
  br i1 %cmp, label %base, label %recurse
base:
  ret i64 %n
recurse:
  %n1 = sub i64 %n, 1
  %fib1 = call i64 @fibonacci(i64 %n1)
  %n2 = sub i64 %n, 2
  %fib2 = call i64 @fibonacci(i64 %n2)
  %result = add i64 %fib1, %fib2
  ret i64 %result
}

define i64 @main() {
entry:
  %result = call i64 @fibonacci(i64 10)
  ret i64 %result
}
"""
        )

        compiler = WebAssemblyCompiler()

        wasm_path = tmp_path / "fibonacci.wasm"
        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()
        assert wasm_path.stat().st_size > 0

        # Also test text format
        wat_path = tmp_path / "fibonacci.wat"
        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wat_path,
            opt_level=2,
            pure_functions_only=True,
            text_format=True,
        )

        assert success is True
        assert wat_path.exists()

        # Verify functions are in the output
        wat_content = wat_path.read_text()
        assert "fibonacci" in wat_content
        assert "main" in wat_content

    def test_compile_to_webassembly_helper(self, tmp_path):
        """Test high-level compile_to_webassembly helper function."""
        # Create a simple IR file
        ir_path = tmp_path / "test.ll"
        ir_path.write_text(
            """
; ModuleID = "test"
target triple = ""

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}
"""
        )

        output_dir = tmp_path / "output"
        output_dir.mkdir()

        success, error = compile_to_webassembly(
            ir_path=ir_path,
            output_dir=output_dir,
            target="wasm32-unknown-unknown",
            opt_level=2,
        )

        assert success is True
        assert error is None
        assert (output_dir / "test.wasm").exists()
        assert (output_dir / "test.wat").exists()

    def test_compile_to_webassembly_nonexistent_file(self, tmp_path):
        """Test helper function with nonexistent file."""
        ir_path = tmp_path / "nonexistent.ll"
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        success, error = compile_to_webassembly(
            ir_path=ir_path, output_dir=output_dir
        )

        assert success is False
        assert error is not None
        assert "not found" in error.lower() or "no such file" in error.lower()


@pytest.mark.skipif(LLVMLITE_AVAILABLE, reason="Test missing llvmlite error")
class TestWebAssemblyCompilerNoLLVMLite:
    """Tests for error handling when llvmlite is not available."""

    def test_compiler_init_without_llvmlite(self):
        """Test that compiler raises ImportError without llvmlite."""
        # This test only runs if llvmlite is NOT available
        # In practice, this is hard to test since the test suite requires llvmlite
        # When llvmlite is unavailable, WebAssemblyCompiler.__init__ raises ImportError
        from multigen.backends.llvm.wasm_compiler import WebAssemblyCompiler

        with pytest.raises(ImportError, match="llvmlite is required"):
            WebAssemblyCompiler()
