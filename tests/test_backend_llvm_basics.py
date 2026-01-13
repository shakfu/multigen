"""Tests for LLVM backend functionality."""

import shutil
import subprocess
import tempfile
from pathlib import Path

import pytest

# Check if llvmlite is available before importing LLVM backend
try:
    import llvmlite  # noqa: F401
    LLVMLITE_AVAILABLE = True
except ImportError:
    LLVMLITE_AVAILABLE = False

# Check if LLVM tools are available
LLVM_LLC_PATH = shutil.which("llc")
LLVM_LLI_PATH = shutil.which("lli")
LLVM_CLANG_PATH = shutil.which("clang")

# Try Homebrew LLVM if standard tools not found
if not all([LLVM_LLC_PATH, LLVM_LLI_PATH, LLVM_CLANG_PATH]):
    homebrew_llvm = Path("/opt/homebrew/opt/llvm/bin")
    if homebrew_llvm.exists():
        LLVM_LLC_PATH = str(homebrew_llvm / "llc") if (homebrew_llvm / "llc").exists() else None
        LLVM_LLI_PATH = str(homebrew_llvm / "lli") if (homebrew_llvm / "lli").exists() else None
        LLVM_CLANG_PATH = str(homebrew_llvm / "clang") if (homebrew_llvm / "clang").exists() else None

LLVM_TOOLS_AVAILABLE = all([LLVM_LLC_PATH, LLVM_LLI_PATH])
pytestmark = pytest.mark.skipif(
    not LLVMLITE_AVAILABLE or not LLVM_TOOLS_AVAILABLE,
    reason="llvmlite not installed or LLVM tools not available"
)

if LLVMLITE_AVAILABLE:
    from multigen.backends.llvm import IRToLLVMConverter
    from multigen.frontend.static_ir import IRBuilder


class TestLLVMBasicConversion:
    """Test basic LLVM IR generation functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.ir_builder = IRBuilder()
        self.llvm_converter = IRToLLVMConverter()

    def _convert_to_llvm(self, python_code: str) -> str:
        """Convert Python code to LLVM IR."""
        import ast
        tree = ast.parse(python_code)
        ir_module = self.ir_builder.build_from_ast(tree)
        llvm_module = self.llvm_converter.visit_module(ir_module)
        return str(llvm_module)

    def test_simple_arithmetic(self):
        """Test simple arithmetic operations."""
        python_code = """
def add(a: int, b: int) -> int:
    return a + b
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Check for function definition
        assert 'define i64 @"add"' in llvm_ir
        # Check for addition instruction
        assert "add i64" in llvm_ir

    def test_function_call_return_types(self):
        """Test that function calls have proper return types."""
        python_code = """
def get_one() -> int:
    return 1

def get_two() -> int:
    return 2

def main() -> int:
    return get_one() + get_two()
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Should have proper function call types and addition
        assert 'call i64 @"get_one"' in llvm_ir
        assert 'call i64 @"get_two"' in llvm_ir
        assert "add i64" in llvm_ir


class TestLLVMControlFlow:
    """Test control flow constructs in LLVM backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.ir_builder = IRBuilder()
        self.llvm_converter = IRToLLVMConverter()

    def _convert_to_llvm(self, python_code: str) -> str:
        """Convert Python code to LLVM IR."""
        import ast
        tree = ast.parse(python_code)
        ir_module = self.ir_builder.build_from_ast(tree)
        llvm_module = self.llvm_converter.visit_module(ir_module)
        return str(llvm_module)

    def test_if_statement_with_comparison(self):
        """Test if statement with comparison."""
        python_code = """
def max_val(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b
"""
        llvm_ir = self._convert_to_llvm(python_code)
        

        # Check for comparison instruction
        assert "icmp sgt" in llvm_ir
        # Check for conditional branch
        assert "br i1" in llvm_ir
        # Check for labels
        assert "if.then:" in llvm_ir
        assert "if.else:" in llvm_ir

    def test_while_loop(self):
        """Test while loop."""
        python_code = """
def count_down(n: int) -> int:
    i: int = n
    while i > 0:
        i = i - 1
    return i
"""
        llvm_ir = self._convert_to_llvm(python_code)
        

        # Check for loop structure
        assert "while.cond:" in llvm_ir
        assert "while.body:" in llvm_ir
        assert "while.exit:" in llvm_ir
        # Check for comparison and branch
        assert "icmp sgt" in llvm_ir
        assert "br i1" in llvm_ir

    def test_for_loop(self):
        """Test for loop with range."""
        python_code = """
def sum_range(n: int) -> int:
    total: int = 0
    i: int
    for i in range(n):
        total = total + i
    return total
"""
        llvm_ir = self._convert_to_llvm(python_code)
        

        # Check for loop structure
        assert "for.cond:" in llvm_ir
        assert "for.body:" in llvm_ir
        assert "for.inc:" in llvm_ir
        assert "for.exit:" in llvm_ir


class TestLLVMBooleanOperations:
    """Test boolean operations in LLVM backend."""

    def setup_method(self):
        """Set up test fixtures."""
        self.ir_builder = IRBuilder()
        self.llvm_converter = IRToLLVMConverter()

    def _convert_to_llvm(self, python_code: str) -> str:
        """Convert Python code to LLVM IR."""
        import ast
        tree = ast.parse(python_code)
        ir_module = self.ir_builder.build_from_ast(tree)
        llvm_module = self.llvm_converter.visit_module(ir_module)
        return str(llvm_module)

    def test_and_operation(self):
        """Test boolean and operation with short-circuit evaluation."""
        python_code = """
def test_and(a: int, b: int) -> int:
    if a > 5 and b > 5:
        return 1
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Check for comparison instructions
        assert "icmp sgt" in llvm_ir
        # Check for short-circuit blocks
        assert "and.eval_right" in llvm_ir
        assert "and.merge" in llvm_ir
        # Check for phi node
        assert "phi" in llvm_ir

    def test_or_operation(self):
        """Test boolean or operation with short-circuit evaluation."""
        python_code = """
def test_or(a: int, b: int) -> int:
    if a > 5 or b > 5:
        return 1
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Check for comparison instructions
        assert "icmp sgt" in llvm_ir
        # Check for short-circuit blocks
        assert "or.eval_right" in llvm_ir
        assert "or.merge" in llvm_ir
        # Check for phi node
        assert "phi" in llvm_ir

    def test_not_operation(self):
        """Test boolean not operation."""
        python_code = """
def test_not(a: int) -> int:
    if not a > 5:
        return 1
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)
        

        # Check for comparison (not is implemented as comparison with false/0)
        assert "icmp" in llvm_ir


@pytest.mark.skipif(not LLVM_LLI_PATH, reason="LLVM lli not available")
class TestLLVMExecution:
    """Test execution of generated LLVM IR."""

    def setup_method(self):
        """Set up test fixtures."""
        self.ir_builder = IRBuilder()
        self.llvm_converter = IRToLLVMConverter()
        self.temp_dir = None

    def _convert_to_llvm(self, python_code: str) -> str:
        """Convert Python code to LLVM IR."""
        import ast
        tree = ast.parse(python_code)
        ir_module = self.ir_builder.build_from_ast(tree)
        llvm_module = self.llvm_converter.visit_module(ir_module)
        return str(llvm_module)

    def teardown_method(self):
        """Clean up temporary files."""
        if self.temp_dir and Path(self.temp_dir).exists():
            shutil.rmtree(self.temp_dir)

    def _execute_llvm_ir(self, llvm_ir: str) -> int:
        """Execute LLVM IR and return exit code."""
        self.temp_dir = tempfile.mkdtemp()
        ll_file = Path(self.temp_dir) / "test.ll"
        ll_file.write_text(llvm_ir)

        result = subprocess.run(
            [LLVM_LLI_PATH, str(ll_file)],
            capture_output=True,
            text=True
        )
        return result.returncode

    def test_simple_return(self):
        """Test simple return value."""
        python_code = """
def main() -> int:
    return 42
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 42

    def test_addition(self):
        """Test addition execution."""
        python_code = """
def main() -> int:
    return 1 + 2
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 3

    def test_function_call_addition(self):
        """Test addition of function call results."""
        python_code = """
def get_one() -> int:
    return 1

def get_two() -> int:
    return 2

def main() -> int:
    return get_one() + get_two()
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 3

    def test_if_statement_execution(self):
        """Test if statement execution."""
        python_code = """
def max_val(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b

def main() -> int:
    return max_val(10, 5)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 10

    def test_boolean_and_execution(self):
        """Test boolean and execution."""
        python_code = """
def test_and(a: int, b: int) -> int:
    if a > 5 and b > 5:
        return 1
    return 0

def main() -> int:
    return test_and(10, 10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 1

    def test_boolean_or_execution(self):
        """Test boolean or execution."""
        python_code = """
def test_or(a: int, b: int) -> int:
    if a > 5 or b > 5:
        return 1
    return 0

def main() -> int:
    return test_or(3, 10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 1

    def test_boolean_not_execution(self):
        """Test boolean not execution."""
        python_code = """
def test_not(a: int) -> int:
    if not a > 5:
        return 1
    return 0

def main() -> int:
    return test_not(3)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 1

    def test_for_loop_execution(self):
        """Test for loop execution."""
        python_code = """
def sum_range(n: int) -> int:
    total: int = 0
    i: int
    for i in range(n):
        total = total + i
    return total

def main() -> int:
    return sum_range(10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # sum(0..9) = 45
        assert exit_code == 45

    def test_break_statement_execution(self):
        """Test break statement execution."""
        python_code = """
def test_break(n: int) -> int:
    i: int = 0
    while i < n:
        if i == 5:
            break
        i = i + 1
    return i

def main() -> int:
    return test_break(10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 5

    def test_continue_statement_execution(self):
        """Test continue statement execution."""
        python_code = """
def test_continue(n: int) -> int:
    total: int = 0
    i: int = 0
    while i < n:
        i = i + 1
        if i % 2 == 0:
            continue
        total = total + i
    return total

def main() -> int:
    return test_continue(10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # Sum of odd numbers 1,3,5,7,9 = 25
        assert exit_code == 25

    def test_elif_chain_execution(self):
        """Test elif chain execution."""
        python_code = """
def classify(x: int) -> int:
    if x > 10:
        return 1
    elif x > 5:
        return 2
    elif x > 0:
        return 3
    else:
        return 4

def main() -> int:
    return classify(7)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 2

    def test_comparison_chaining_execution(self):
        """Test comparison chaining execution."""
        python_code = """
def is_between(x: int, low: int, high: int) -> int:
    if low < x < high:
        return 1
    return 0

def main() -> int:
    return is_between(5, 3, 10)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 1

    def test_boolean_chaining_execution(self):
        """Test boolean chaining execution."""
        python_code = """
def test_and_chain(a: int, b: int, c: int) -> int:
    if a > 0 and b > 0 and c > 0:
        return 1
    return 0

def test_or_chain(a: int, b: int, c: int) -> int:
    if a > 10 or b > 10 or c > 10:
        return 1
    return 0

def main() -> int:
    return test_and_chain(1, 2, 3) + test_or_chain(1, 2, 15)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 2

    def test_augmented_assignment_execution(self):
        """Test augmented assignment execution."""
        python_code = """
def test_augmented(x: int) -> int:
    y: int = 10
    y += x
    y -= 2
    y *= 2
    return y

def main() -> int:
    return test_augmented(5)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # (10 + 5 - 2) * 2 = 26
        assert exit_code == 26

    def test_modulo_operation_execution(self):
        """Test modulo operation execution."""
        python_code = """
def test_mod(a: int, b: int) -> int:
    return a % b

def main() -> int:
    return test_mod(17, 5)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 2

    def test_bitwise_operations_execution(self):
        """Test bitwise operations execution."""
        python_code = """
def test_bitwise(a: int, b: int) -> int:
    x: int = a << 2
    y: int = b >> 1
    z: int = a & b
    w: int = a | b
    v: int = a ^ b
    return x + y + z + w + v

def main() -> int:
    return test_bitwise(12, 5)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 48 + 2 + 4 + 13 + 9 = 76
        assert exit_code == 76

    def test_integer_division_execution(self):
        """Test integer division execution."""
        python_code = """
def test_intdiv(a: int, b: int) -> int:
    return a // b

def main() -> int:
    return test_intdiv(17, 5)
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 3

    def test_type_cast_execution(self):
        """Test type casting execution."""
        python_code = """
def int_to_float(x: int) -> float:
    return float(x)

def float_to_int(x: float) -> int:
    return int(x)

def main() -> int:
    a: float = int_to_float(42)
    b: int = float_to_int(3.7)
    return b
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 3

    def test_comprehensive_program_execution(self):
        """Test comprehensive program with multiple algorithms."""
        python_code = """
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    a: int = 0
    b: int = 1
    i: int
    for i in range(2, n + 1):
        temp: int = a + b
        a = b
        b = temp
    return b

def factorial(n: int) -> int:
    result: int = 1
    i: int
    for i in range(1, n + 1):
        result *= i
    return result

def is_prime(n: int) -> bool:
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    i: int = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True

def main() -> int:
    fib10: int = fibonacci(10)
    fact5: int = factorial(5)
    prime: bool = is_prime(17)
    prime_int: int = int(prime)
    return fib10 + fact5 + prime_int
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # fib(10)=55, fact(5)=120, is_prime(17)=1 => 55+120+1=176
        assert exit_code == 176

    def test_unary_operators_execution(self):
        """Test unary operators execution."""
        python_code = """
def test_unary(x: int) -> int:
    a: int = -x
    b: int = +x
    c: int = ~x
    return a + b + c

def test_negative() -> int:
    x: int = -42
    y: int = -10
    return x + y

def main() -> int:
    result1: int = test_unary(5)
    result2: int = test_negative()
    return result1 + result2
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # -5+5-6 + -42-10 = -6 + -52 = -58 => 256-58=198 (unsigned byte wrapping)
        assert exit_code == 198

    def test_boolean_literals_execution(self):
        """Test boolean literals execution."""
        python_code = """
def test_bool() -> int:
    a: bool = True
    b: bool = False
    if a and not b:
        return 1
    return 0

def main() -> int:
    return test_bool()
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 1

    def test_multiple_early_returns_execution(self):
        """Test multiple early return paths."""
        python_code = """
def classify_number(x: int) -> int:
    if x < 0:
        return -1
    if x == 0:
        return 0
    if x < 10:
        return 1
    if x < 100:
        return 2
    return 3

def main() -> int:
    a: int = classify_number(-5)
    b: int = classify_number(0)
    c: int = classify_number(5)
    d: int = classify_number(50)
    e: int = classify_number(200)
    return a + b + c + d + e
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # -1 + 0 + 1 + 2 + 3 = 5
        assert exit_code == 5

    def test_range_two_args_execution(self):
        """Test range(start, stop) - should iterate from start to stop-1."""
        python_code = """
def main() -> int:
    total: int = 0
    # range(2, 7) should give 2, 3, 4, 5, 6
    for i in range(2, 7):
        total += i
    return total
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 2 + 3 + 4 + 5 + 6 = 20
        assert exit_code == 20

    def test_range_three_args_execution(self):
        """Test range(start, stop, step) - should iterate with custom step."""
        python_code = """
def main() -> int:
    total: int = 0
    # range(0, 10, 2) should give 0, 2, 4, 6, 8
    for i in range(0, 10, 2):
        total += i
    return total
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 0 + 2 + 4 + 6 + 8 = 20
        assert exit_code == 20

    def test_range_negative_step_execution(self):
        """Test range with negative step - should iterate backwards."""
        python_code = """
def main() -> int:
    total: int = 0
    # range(10, 0, -2) should give 10, 8, 6, 4, 2
    for i in range(10, 0, -2):
        total += i
    return total
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 10 + 8 + 6 + 4 + 2 = 30
        assert exit_code == 30

    def test_void_function_call_execution(self):
        """Test calling void functions (expression statements)."""
        python_code = """
def increment_global(x: int) -> None:
    # For this test, we'll use a global-style approach with return value
    # Since we can't test true global state, we'll use a helper that returns a value
    pass

def add_values(a: int, b: int) -> int:
    return a + b

def main() -> int:
    # Test that void function calls work (even if they do nothing)
    increment_global(5)
    # Test that we can still call and use return values
    result: int = add_values(10, 20)
    return result
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 30

    def test_recursion_execution(self):
        """Test recursive function calls."""
        python_code = """
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def main() -> int:
    fact5: int = factorial(5)
    fib7: int = fibonacci(7)
    return fact5 + fib7
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 5! = 120, fib(7) = 13, total = 133
        assert exit_code == 133

    def test_float_arithmetic_execution(self):
        """Test floating point arithmetic operations."""
        python_code = """
def main() -> int:
    a: float = 10.5
    b: float = 3.0

    # Basic arithmetic
    c: float = a + b
    d: float = a - b
    e: float = a * b
    f: float = a / b

    # Convert results to int for testing
    result: int = int(c) + int(d) + int(e) + int(f)
    return result
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # c=13.5->13, d=7.5->7, e=31.5->31, f=3.5->3, total=54
        assert exit_code == 54

    def test_float_comparisons_execution(self):
        """Test floating point comparison operations."""
        python_code = """
def main() -> int:
    a: float = 5.5
    b: float = 3.2

    result: int = 0

    if a > b:
        result += 1

    if a >= 5.5:
        result += 2

    if b < a:
        result += 4

    if b <= 3.2:
        result += 8

    if a == 5.5:
        result += 16

    if a != b:
        result += 32

    return result
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # All conditions true: 1 + 2 + 4 + 8 + 16 + 32 = 63
        assert exit_code == 63

    def test_float_int_conversion_execution(self):
        """Test conversions between float and int."""
        python_code = """
def main() -> int:
    # Int to float
    i: int = 42
    f: float = float(i)

    # Float to int (truncation)
    f2: float = 3.7
    i2: int = int(f2)

    f3: float = -2.9
    i3: int = int(f3)

    # Should be 42 + 3 + (-2) = 43
    return int(f) + i2 + i3
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 43

    def test_while_loop_execution(self):
        """Test while loops with various conditions."""
        python_code = """
def main() -> int:
    # Simple while loop
    count: int = 0
    i: int = 0
    while i < 5:
        count += 1
        i += 1

    # While with complex condition
    j: int = 10
    k: int = 0
    while j > 0 and k < 5:
        k += 1
        j -= 1

    return count + k
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # count=5, k=5, total=10
        assert exit_code == 10

    def test_nested_loops_execution(self):
        """Test nested for and while loops."""
        python_code = """
def main() -> int:
    total: int = 0

    # Nested for loops
    for i in range(3):
        for j in range(2):
            total += 1

    # Nested while
    x: int = 0
    while x < 2:
        y: int = 0
        while y < 3:
            total += 10
            y += 1
        x += 1

    return total
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # Nested for: 3*2=6, Nested while: 2*3*10=60, total=66
        assert exit_code == 66

    def test_loop_break_continue_execution(self):
        """Test break and continue in various loop contexts."""
        python_code = """
def main() -> int:
    # Break in for loop
    count1: int = 0
    for i in range(10):
        if i == 5:
            break
        count1 += 1

    # Continue in for loop
    count2: int = 0
    for j in range(10):
        if j % 2 == 0:
            continue
        count2 += 1

    # Break in while loop
    count3: int = 0
    k: int = 0
    while k < 10:
        k += 1
        if k == 3:
            break
        count3 += 1

    # Continue in while loop
    count4: int = 0
    m: int = 0
    while m < 5:
        m += 1
        if m == 3:
            continue
        count4 += 1

    return count1 + count2 + count3 + count4
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # count1=5, count2=5, count3=2, count4=4, total=16
        assert exit_code == 16

    def test_complex_conditional_execution(self):
        """Test complex conditional expressions."""
        python_code = """
def main() -> int:
    x: int = 10
    y: int = 5
    z: int = 3

    # Nested if with multiple conditions
    result: int = 0

    if x > y:
        if y > z:
            result += 1
        if x > z:
            result += 2

    # elif chains
    if x < 0:
        result += 100
    elif x < 5:
        result += 200
    elif x < 15:
        result += 4
    else:
        result += 400

    # Complex boolean expressions
    if (x > y and y > z) or x == 10:
        result += 8

    return result
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # result: 1 + 2 + 4 + 8 = 15
        assert exit_code == 15

    def test_mathematical_expressions_execution(self):
        """Test complex mathematical expressions."""
        python_code = """
def main() -> int:
    # Order of operations
    a: int = 2 + 3 * 4

    # Nested operations
    b: int = (5 + 3) * (10 - 6)

    # Division and modulo
    c: int = 17 // 5
    d: int = 17 % 5

    # Bitwise operations
    e: int = 12 & 10
    f: int = 12 | 3
    g: int = 12 ^ 10

    # Shifts
    h: int = 4 << 2
    i: int = 32 >> 3

    return a + b + c + d + e + f + g + h + i
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # a=14, b=32, c=3, d=2, e=8, f=15, g=6, h=16, i=4, total=100
        assert exit_code == 100

    def test_comprehensive_benchmark_execution(self):
        """Comprehensive benchmark testing multiple features together."""
        python_code = """
def is_prime(n: int) -> int:
    if n < 2:
        return 0
    if n == 2:
        return 1
    if n % 2 == 0:
        return 0

    i: int = 3
    while i * i <= n:
        if n % i == 0:
            return 0
        i += 2

    return 1

def count_primes(limit: int) -> int:
    count: int = 0
    for i in range(2, limit):
        if is_prime(i) == 1:
            count += 1
    return count

def sum_of_squares(n: int) -> int:
    total: int = 0
    for i in range(n):
        total += i * i
    return total

def gcd(a: int, b: int) -> int:
    while b != 0:
        temp: int = b
        b = a % b
        a = temp
    return a

def main() -> int:
    # Count primes up to 20: 2,3,5,7,11,13,17,19 = 8 primes
    primes: int = count_primes(20)

    # Sum of squares 0²+1²+2²+3²+4² = 0+1+4+9+16 = 30
    squares: int = sum_of_squares(5)

    # GCD of 48 and 18 = 6
    divisor: int = gcd(48, 18)

    # Combine results: 8 + 30 + 6 = 44
    return primes + squares + divisor
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 44

    def test_short_circuit_and_execution(self):
        """Test short-circuit evaluation for 'and' operator."""
        python_code = """
def divide_safe(x: int, y: int) -> int:
    # Should short-circuit before division by zero
    if y != 0 and x // y > 2:
        return 1
    return 0

def main() -> int:
    # Test 1: y=0, should short-circuit and not divide
    a: int = divide_safe(10, 0)

    # Test 2: y!=0 but x//y <= 2
    b: int = divide_safe(10, 5)

    # Test 3: y!=0 and x//y > 2
    c: int = divide_safe(10, 3)

    return a + b + c
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # a=0 (short-circuit), b=0 (false), c=1 (true), total=1
        assert exit_code == 1

    def test_short_circuit_or_execution(self):
        """Test short-circuit evaluation for 'or' operator."""
        python_code = """
def check_or(x: int, y: int) -> int:
    # Should short-circuit if x > 5, not evaluating y // 0
    if x > 5 or y // x > 0:
        return 1
    return 0

def main() -> int:
    # Test 1: x > 5, should short-circuit (y//x not evaluated, no div by zero)
    a: int = check_or(10, 0)

    # Test 2: x <= 5, need to evaluate second part
    b: int = check_or(2, 4)

    return a + b
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # a=1 (short-circuit), b=1 (2//4 gives 2>0 true), total=2
        assert exit_code == 2

    def test_modulo_negative_numbers_execution(self):
        """Test modulo operation with negative numbers.

        Tests Python-style modulo (floored division) semantics.
        """
        python_code = """
def main() -> int:
    # Test various modulo cases
    a: int = 17 % 5
    b: int = -17 % 5
    c: int = 17 % -5
    d: int = -17 % -5

    # Return sum of absolute values to fit in exit code
    result: int = a
    if b < 0:
        result += -b
    else:
        result += b
    if c < 0:
        result += -c
    else:
        result += c
    if d < 0:
        result += -d
    else:
        result += d

    return result
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # Python modulo: 17%5=2, -17%5=3, 17%-5=-3, -17%-5=-2
        # Sum of absolute values: 2+3+3+2 = 10
        assert exit_code == 10

    def test_mixed_int_float_operations_execution(self):
        """Test operations mixing int and float types."""
        python_code = """
def main() -> int:
    i: int = 10
    f: float = 3.5

    # Int to float conversion for operation
    result1: float = float(i) + f

    # Float to int conversion
    v1: int = int(result1)

    # Another mixed operation
    result2: float = float(i) * 2.5
    v2: int = int(result2)

    return v1 + v2
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # v1 = int(10.0 + 3.5) = 13, v2 = int(10.0 * 2.5) = 25, total = 38
        assert exit_code == 38

    def test_complex_elif_chains_execution(self):
        """Test complex elif chains with multiple branches."""
        python_code = """
def classify(x: int) -> int:
    if x < -10:
        return 1
    elif x < 0:
        return 2
    elif x == 0:
        return 3
    elif x < 10:
        return 4
    elif x < 100:
        return 5
    else:
        return 6

def main() -> int:
    a: int = classify(-20)
    b: int = classify(-5)
    c: int = classify(0)
    d: int = classify(5)
    e: int = classify(50)
    f: int = classify(200)

    return a + b + c + d + e + f
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # 1 + 2 + 3 + 4 + 5 + 6 = 21
        assert exit_code == 21

    def test_global_variable_execution(self) -> None:
        """Test global variable access and modification."""
        python_code = """
counter: int = 0

def increment() -> int:
    global counter
    counter = counter + 1
    return counter

def main() -> int:
    a: int = increment()
    b: int = increment()
    c: int = increment()
    return a + b + c
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # First call: counter = 1, Second: counter = 2, Third: counter = 3
        # Result: 1 + 2 + 3 = 6
        assert exit_code == 6

    def test_multiple_globals_execution(self) -> None:
        """Test multiple global variables with different types."""
        python_code = """
x: int = 10
y: int = 20
result: int = 0

def add_globals() -> int:
    global result
    result = x + y
    return result

def main() -> int:
    a: int = add_globals()
    return a
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        # x + y = 10 + 20 = 30
        assert exit_code == 30

    def test_print_integer_execution(self) -> None:
        """Test print statement with integers."""
        python_code = """
def main() -> int:
    print(42)
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)
        # Check that LLVM IR contains printf declaration and call
        assert "declare" in llvm_ir and "printf" in llvm_ir
        assert "call" in llvm_ir
        # Execute and verify it runs without error
        import subprocess
        with open('/tmp/test_print.ll', 'w') as f:
            f.write(llvm_ir)
        result = subprocess.run(['/opt/homebrew/opt/llvm/bin/lli', '/tmp/test_print.ll'],
                              capture_output=True, text=True)
        # Should print "42" and return 0
        assert result.returncode == 0
        assert "42" in result.stdout

    def test_string_literal_print_execution(self) -> None:
        """Test string literal with print."""
        python_code = """
def main() -> int:
    s: str = "Hello, World!"
    print(s)
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)
        # Execute and verify output
        import subprocess
        with open('/tmp/test_string_print.ll', 'w') as f:
            f.write(llvm_ir)
        result = subprocess.run(['/opt/homebrew/opt/llvm/bin/lli', '/tmp/test_string_print.ll'],
                              capture_output=True, text=True)
        assert result.returncode == 0
        assert "Hello, World!" in result.stdout

    def test_string_concatenation_execution(self) -> None:
        """Test string concatenation."""
        python_code = """
def main() -> int:
    a: str = "Hello"
    b: str = " World"
    c: str = a + b
    print(c)
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)
        # Execute and verify output
        import subprocess
        with open('/tmp/test_string_concat.ll', 'w') as f:
            f.write(llvm_ir)
        result = subprocess.run(['/opt/homebrew/opt/llvm/bin/lli', '/tmp/test_string_concat.ll'],
                              capture_output=True, text=True)
        assert result.returncode == 0
        assert "Hello World" in result.stdout

    def test_builtin_len_execution(self) -> None:
        """Test len() builtin function."""
        python_code = """
def main() -> int:
    s: str = "Hello"
    n: int = len(s)
    return n
"""
        llvm_ir = self._convert_to_llvm(python_code)
        exit_code = self._execute_llvm_ir(llvm_ir)
        assert exit_code == 5  # "Hello" has 5 characters

    def test_llvmlite_compilation_execution(self) -> None:
        """Test compilation using llvmlite instead of lli."""
        python_code = """
def main() -> int:
    x: int = 10
    y: int = 20
    return x + y
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Use llvmlite compiler
        from multigen.backends.llvm import LLVMCompiler
        compiler = LLVMCompiler()

        # Compile and run
        result = compiler.compile_and_run(llvm_ir, capture_output=True)
        assert result.returncode == 30  # 10 + 20 = 30

    def test_llvmlite_compile_to_executable(self) -> None:
        """Test compiling to standalone executable."""
        python_code = """
def main() -> int:
    print(42)
    return 0
"""
        llvm_ir = self._convert_to_llvm(python_code)

        # Use llvmlite compiler
        from multigen.backends.llvm import LLVMCompiler
        import tempfile
        compiler = LLVMCompiler()

        # Compile to executable
        with tempfile.NamedTemporaryFile(suffix="", delete=False) as exe_file:
            exe_path = exe_file.name

        try:
            success = compiler.compile_ir_to_executable(llvm_ir, exe_path)
            assert success

            # Verify executable exists
            from pathlib import Path
            assert Path(exe_path).exists()

            # Execute directly
            import subprocess
            result = subprocess.run([exe_path], capture_output=True, text=True)
            assert result.returncode == 0
            assert "42" in result.stdout
        finally:
            Path(exe_path).unlink(missing_ok=True)
