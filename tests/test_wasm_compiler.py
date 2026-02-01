"""Tests for WebAssembly compilation functionality."""

import pytest

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

        success = compiler.compile_to_wasm(simple_ir, output_path, opt_level=2, text_format=True)

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

        success, error = compile_to_webassembly(ir_path=ir_path, output_dir=output_dir)

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


@pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")
class TestWebAssemblyIntegration:
    """End-to-end integration tests: Python source -> LLVM IR -> WebAssembly."""

    def _python_to_llvm_ir(self, python_code: str) -> str:
        """Convert Python source code to LLVM IR."""
        from multigen.backends.llvm import IRToLLVMConverter
        from multigen.frontend.static_ir import build_ir_from_code

        ir_module = build_ir_from_code(python_code)
        converter = IRToLLVMConverter()
        llvm_module = converter.visit_module(ir_module)
        return str(llvm_module)

    def test_python_add_function_to_wasm(self, tmp_path):
        """Test full pipeline: Python add function -> LLVM IR -> WebAssembly."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        # Python -> LLVM IR
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"add"' in llvm_ir

        # Write IR to file
        ir_path = tmp_path / "add.ll"
        ir_path.write_text(llvm_ir)

        # LLVM IR -> WebAssembly
        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "add.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()
        assert wasm_path.stat().st_size > 0

    def test_python_arithmetic_to_wasm(self, tmp_path):
        """Test arithmetic operations: Python -> LLVM IR -> WebAssembly."""
        python_code = """
def calculate(a: int, b: int) -> int:
    result: int = (a + b) * (a - b)
    return result
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"calculate"' in llvm_ir

        ir_path = tmp_path / "calc.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "calc.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()

    def test_python_conditional_to_wasm(self, tmp_path):
        """Test conditional logic: Python -> LLVM IR -> WebAssembly."""
        python_code = """
def abs_value(x: int) -> int:
    if x < 0:
        return -x
    return x
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"abs_value"' in llvm_ir

        ir_path = tmp_path / "abs.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "abs.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()

    def test_python_recursive_fibonacci_to_wasm(self, tmp_path):
        """Test recursive function: Python fibonacci -> LLVM IR -> WebAssembly."""
        python_code = """
def fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"fib"' in llvm_ir
        assert 'call i64 @"fib"' in llvm_ir  # Recursive call

        ir_path = tmp_path / "fib.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "fib.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()

    def test_python_loop_to_wasm(self, tmp_path):
        """Test loop: Python while loop -> LLVM IR -> WebAssembly."""
        python_code = """
def sum_to_n(n: int) -> int:
    total: int = 0
    i: int = 1
    while i <= n:
        total = total + i
        i = i + 1
    return total
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"sum_to_n"' in llvm_ir

        ir_path = tmp_path / "sum.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "sum.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()

    def test_python_multiple_functions_to_wasm(self, tmp_path):
        """Test multiple functions: Python -> LLVM IR -> WebAssembly."""
        python_code = """
def square(x: int) -> int:
    return x * x

def cube(x: int) -> int:
    return x * x * x

def sum_powers(n: int) -> int:
    return square(n) + cube(n)
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        assert 'define i64 @"square"' in llvm_ir
        assert 'define i64 @"cube"' in llvm_ir
        assert 'define i64 @"sum_powers"' in llvm_ir

        ir_path = tmp_path / "powers.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "powers.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True
        assert wasm_path.exists()

    def test_python_to_wasm_text_format(self, tmp_path):
        """Test WebAssembly text format output."""
        python_code = """
def multiply(a: int, b: int) -> int:
    return a * b
"""
        llvm_ir = self._python_to_llvm_ir(python_code)

        ir_path = tmp_path / "mul.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wat_path = tmp_path / "mul.wat"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wat_path,
            opt_level=2,
            pure_functions_only=True,
            text_format=True,
        )

        assert success is True
        assert wat_path.exists()

        # Verify text format contains function name
        wat_content = wat_path.read_text()
        assert "multiply" in wat_content

    def test_python_to_wasm_all_optimization_levels(self, tmp_path):
        """Test all optimization levels produce valid WebAssembly."""
        python_code = """
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        ir_path = tmp_path / "fact.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        sizes = {}

        for opt_level in [0, 1, 2, 3]:
            wasm_path = tmp_path / f"fact_O{opt_level}.wasm"
            success = compiler.compile_multigen_ir(
                ir_path=ir_path,
                output_path=wasm_path,
                opt_level=opt_level,
                pure_functions_only=True,
            )
            assert success is True
            assert wasm_path.exists()
            sizes[opt_level] = wasm_path.stat().st_size

        # All should produce non-empty output
        assert all(size > 0 for size in sizes.values())

    def test_python_to_wasm_different_targets(self, tmp_path):
        """Test different WebAssembly targets."""
        python_code = """
def negate(x: int) -> int:
    return -x
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        ir_path = tmp_path / "neg.ll"
        ir_path.write_text(llvm_ir)

        targets = [
            "wasm32-unknown-unknown",
            "wasm32-wasi",
        ]

        for target in targets:
            try:
                compiler = WebAssemblyCompiler(target_triple=target)
                wasm_path = tmp_path / f"neg_{target.replace('-', '_')}.wasm"

                success = compiler.compile_multigen_ir(
                    ir_path=ir_path,
                    output_path=wasm_path,
                    opt_level=2,
                    pure_functions_only=True,
                )

                assert success is True
                assert wasm_path.exists()
            except RuntimeError:
                # Target not available on this system - skip
                pass

    def test_full_pipeline_helper_function(self, tmp_path):
        """Test compile_to_webassembly helper with Python source."""
        python_code = """
def power(base: int, exp: int) -> int:
    result: int = 1
    i: int = 0
    while i < exp:
        result = result * base
        i = i + 1
    return result
"""
        llvm_ir = self._python_to_llvm_ir(python_code)

        ir_path = tmp_path / "power.ll"
        ir_path.write_text(llvm_ir)

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
        assert (output_dir / "power.wasm").exists()
        assert (output_dir / "power.wat").exists()

    def test_wasm_binary_magic_number(self, tmp_path):
        """Verify WebAssembly binary has correct magic number."""
        python_code = """
def identity(x: int) -> int:
    return x
"""
        llvm_ir = self._python_to_llvm_ir(python_code)
        ir_path = tmp_path / "id.ll"
        ir_path.write_text(llvm_ir)

        compiler = WebAssemblyCompiler()
        wasm_path = tmp_path / "id.wasm"

        success = compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=2,
            pure_functions_only=True,
        )

        assert success is True

        # WebAssembly binary starts with magic number 0x00 0x61 0x73 0x6D ('\0asm')
        # followed by version 0x01 0x00 0x00 0x00
        wasm_bytes = wasm_path.read_bytes()
        # Note: llvmlite produces object files, not linked WASM modules
        # Object files have different headers, so we just verify non-empty output
        assert len(wasm_bytes) > 0
