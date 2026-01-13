"""Basic tests for LLVM backend."""

import shutil
import tempfile
from pathlib import Path

import pytest

# Check if llvmlite is available before importing LLVM backend
try:
    import llvmlite  # noqa: F401
    LLVMLITE_AVAILABLE = True
except ImportError:
    LLVMLITE_AVAILABLE = False

pytestmark = pytest.mark.skipif(not LLVMLITE_AVAILABLE, reason="llvmlite not installed")

if LLVMLITE_AVAILABLE:
    from multigen.backends.llvm import IRToLLVMConverter, LLVMBackend
    from multigen.frontend.static_ir import build_ir_from_code

# Check if LLVM tools are available
LLVM_TOOLS_AVAILABLE = shutil.which("llc") is not None and shutil.which("clang") is not None

# Try Homebrew LLVM if standard tools not found
if not LLVM_TOOLS_AVAILABLE:
    homebrew_llvm = Path("/opt/homebrew/opt/llvm/bin")
    if homebrew_llvm.exists():
        LLVM_TOOLS_AVAILABLE = (homebrew_llvm / "llc").exists() and (homebrew_llvm / "clang").exists()
        if LLVM_TOOLS_AVAILABLE:
            LLVM_LLC_PATH = str(homebrew_llvm / "llc")
            LLVM_CLANG_PATH = str(homebrew_llvm / "clang")
    else:
        LLVM_LLC_PATH = "llc"
        LLVM_CLANG_PATH = "clang"
else:
    LLVM_LLC_PATH = "llc"
    LLVM_CLANG_PATH = "clang"


class TestLLVMIRGeneration:
    """Test LLVM IR generation from Static IR."""

    def test_simple_add_function(self):
        """Test generating LLVM IR for a simple add function."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""

        # Build Static IR from Python
        ir_module = build_ir_from_code(python_code)

        # Convert to LLVM IR
        converter = IRToLLVMConverter()
        llvm_module = converter.visit_module(ir_module)

        # Get LLVM IR as string
        llvm_ir = str(llvm_module)

        # Verify LLVM IR contains expected elements
        assert "define" in llvm_ir  # Function definition
        assert "add" in llvm_ir  # Function name
        assert "i64" in llvm_ir  # 64-bit integer type
        assert "ret" in llvm_ir  # Return instruction

    def test_arithmetic_operations(self):
        """Test LLVM IR generation for arithmetic operations."""
        python_code = """
def calc(a: int, b: int) -> int:
    result: int = a + b - a * b
    return result
"""

        ir_module = build_ir_from_code(python_code)
        converter = IRToLLVMConverter()
        llvm_module = converter.visit_module(ir_module)
        llvm_ir = str(llvm_module)

        # Check for arithmetic instructions
        assert "add" in llvm_ir or "sub" in llvm_ir or "mul" in llvm_ir
        assert "alloca" in llvm_ir  # Local variable allocation
        assert "store" in llvm_ir  # Variable assignment
        assert "load" in llvm_ir  # Variable read

    def test_function_with_local_variable(self):
        """Test LLVM IR generation with local variables."""
        python_code = """
def square(x: int) -> int:
    result: int = x * x
    return result
"""

        ir_module = build_ir_from_code(python_code)
        converter = IRToLLVMConverter()
        llvm_module = converter.visit_module(ir_module)
        llvm_ir = str(llvm_module)

        assert "alloca i64" in llvm_ir  # Variable allocation
        assert "store" in llvm_ir
        assert "mul" in llvm_ir  # Multiplication


class TestLLVMBackend:
    """Test LLVM backend integration."""

    def test_backend_initialization(self):
        """Test LLVM backend initializes correctly."""
        backend = LLVMBackend()

        assert backend.get_name() == "llvm"
        assert backend.get_file_extension() == ".ll"
        assert backend.get_emitter() is not None
        assert backend.get_factory() is not None
        assert backend.get_builder() is not None

    def test_backend_feature_support(self):
        """Test feature support queries."""
        backend = LLVMBackend()

        assert backend.supports_feature("functions")
        assert backend.supports_feature("variables")
        assert backend.supports_feature("arithmetic")
        assert backend.supports_feature("control_flow")
        assert backend.supports_feature("loops")

    def test_emit_simple_function(self):
        """Test emitting LLVM IR via backend."""
        backend = LLVMBackend()

        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""

        llvm_ir = backend.get_emitter().emit_module(python_code)

        assert llvm_ir is not None
        assert len(llvm_ir) > 0
        assert "define" in llvm_ir
        assert '@"add"' in llvm_ir or "@add" in llvm_ir


class TestLLVMBuilder:
    """Test LLVM builder functionality."""

    def test_generate_makefile(self):
        """Test Makefile generation."""
        backend = LLVMBackend()
        builder = backend.get_builder()

        makefile = builder.generate_build_file(["test.ll"], "test_program")

        assert "LLC" in makefile
        assert "CLANG" in makefile
        assert "test_program" in makefile
        assert "test.ll" in makefile
        assert ".PHONY" in makefile

    def test_get_compile_flags(self):
        """Test compile flags retrieval."""
        backend = LLVMBackend()
        builder = backend.get_builder()

        flags = builder.get_compile_flags()

        assert isinstance(flags, list)
        assert "-filetype=obj" in flags


class TestEndToEnd:
    """End-to-end tests for LLVM backend."""

    def test_generate_llvm_ir_file(self):
        """Test generating LLVM IR file from Python code."""
        python_code = """
def multiply(x: int, y: int) -> int:
    result: int = x * y
    return result
"""

        with tempfile.TemporaryDirectory() as temp_dir:
            # Generate LLVM IR
            backend = LLVMBackend()
            llvm_ir = backend.get_emitter().emit_module(python_code)

            # Write to file
            output_file = Path(temp_dir) / "multiply.ll"
            output_file.write_text(llvm_ir)

            # Verify file exists and has content
            assert output_file.exists()
            content = output_file.read_text()
            assert len(content) > 0
            assert "define" in content
            assert '@"multiply"' in content or "@multiply" in content


class TestLLVMCompilation:
    """Test LLVM compilation (requires llc and clang)."""

    @pytest.mark.skipif(not LLVM_TOOLS_AVAILABLE, reason="LLVM tools (llc, clang) not available")
    def test_compile_to_binary(self):
        """Test compiling LLVM IR to native binary."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y

def multiply(a: int, b: int) -> int:
    result: int = a * b
    return result

def main() -> int:
    sum_val: int = add(5, 3)
    product: int = multiply(sum_val, 2)
    return product
"""

        with tempfile.TemporaryDirectory() as temp_dir:
            # Generate LLVM IR
            backend = LLVMBackend()
            llvm_ir = backend.get_emitter().emit_module(python_code)

            # Write IR to file
            ir_file = Path(temp_dir) / "test_program.ll"
            ir_file.write_text(llvm_ir)

            # Compile to binary with configured paths
            builder = backend.get_builder()
            builder.llc_path = LLVM_LLC_PATH
            builder.clang_path = LLVM_CLANG_PATH

            output_dir = Path(temp_dir)
            success = builder.compile_direct(str(ir_file), str(output_dir))

            # Verify compilation succeeded
            assert success, "Compilation should succeed"

            # Verify binary exists (named after source file stem)
            binary_path = output_dir / "test_program"
            assert binary_path.exists(), f"Binary should be created at {binary_path}"

            # Run the binary and check exit code (should be 16 = (5+3)*2)
            import subprocess

            result = subprocess.run([str(binary_path)], capture_output=True)
            assert result.returncode == 16, f"Expected exit code 16, got {result.returncode}"

    @pytest.mark.skipif(not LLVM_TOOLS_AVAILABLE, reason="LLVM tools (llc, clang) not available")
    def test_makefile_generation(self):
        """Test Makefile generation for LLVM IR."""
        backend = LLVMBackend()
        builder = backend.get_builder()

        makefile = builder.generate_build_file(["test.ll"], "test_program")

        # Verify Makefile has required components
        assert "LLC" in makefile
        assert "CLANG" in makefile
        assert "test_program" in makefile
        assert "test.ll" in makefile


class TestLLVMControlFlow:
    """Test LLVM control flow (if/while/for) - requires fixed Static IR."""

    @pytest.mark.skipif(not LLVM_TOOLS_AVAILABLE, reason="LLVM tools (llc, clang) not available")
    def test_if_statement_compilation(self):
        """Test if statement with comparison."""
        python_code = """
def max_val(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b

def main() -> int:
    return max_val(10, 5)
"""

        with tempfile.TemporaryDirectory() as temp_dir:
            backend = LLVMBackend()
            llvm_ir = backend.get_emitter().emit_module(python_code)

            # Verify comparison instruction is generated
            assert "icmp sgt" in llvm_ir, "Should generate signed greater than comparison"
            assert "br i1" in llvm_ir, "Should generate conditional branch with i1"

            # Compile and test
            ir_file = Path(temp_dir) / "test_if.ll"
            ir_file.write_text(llvm_ir)

            builder = backend.get_builder()
            builder.llc_path = LLVM_LLC_PATH
            builder.clang_path = LLVM_CLANG_PATH

            success = builder.compile_direct(str(ir_file), str(temp_dir))
            assert success, "Compilation should succeed"

            binary = Path(temp_dir) / "test_if"
            assert binary.exists()

            # Run and check result (max(10, 5) = 10)
            import subprocess

            result = subprocess.run([str(binary)], capture_output=True)
            assert result.returncode == 10

    @pytest.mark.skipif(not LLVM_TOOLS_AVAILABLE, reason="LLVM tools (llc, clang) not available")
    def test_while_loop_compilation(self):
        """Test while loop with comparison (fibonacci)."""
        python_code = """
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    a: int = 0
    b: int = 1
    i: int = 2
    while i <= n:
        temp: int = a + b
        a = b
        b = temp
        i = i + 1
    return b

def main() -> int:
    return fibonacci(10)
"""

        with tempfile.TemporaryDirectory() as temp_dir:
            backend = LLVMBackend()
            llvm_ir = backend.get_emitter().emit_module(python_code)

            # Verify while loop structure
            assert "while.cond:" in llvm_ir
            assert "while.body:" in llvm_ir
            assert "while.exit:" in llvm_ir
            assert "icmp sle" in llvm_ir  # Less than or equal comparison

            # Compile and test
            ir_file = Path(temp_dir) / "test_while.ll"
            ir_file.write_text(llvm_ir)

            builder = backend.get_builder()
            builder.llc_path = LLVM_LLC_PATH
            builder.clang_path = LLVM_CLANG_PATH

            success = builder.compile_direct(str(ir_file), str(temp_dir))
            assert success

            binary = Path(temp_dir) / "test_while"
            assert binary.exists()

            # Run and check result (fibonacci(10) = 55)
            import subprocess

            result = subprocess.run([str(binary)], capture_output=True)
            assert result.returncode == 55


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
