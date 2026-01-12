# LLVM JIT Executor

## Overview

The LLVM backend now supports **two compilation modes**:

1. **AOT (Ahead-of-Time)**: Traditional compilation using llc → clang → executable
2. **JIT (Just-in-Time)**: In-memory execution using llvmlite's MCJIT compiler

## When to Use Each Mode

### Use JIT Mode For

- **Development and testing** - 7.7x faster total time
- **Rapid iteration** - No intermediate files (*.o, executables)
- **Interactive debugging** - Execute code directly from IR
- **Quick prototyping** - Faster compile-test cycles

### Use AOT Mode For

- **Production deployment** - Standalone executables
- **Distribution** - No llvmlite runtime dependency
- **Cross-platform builds** - Target different architectures
- **Optimization** - Full LLVM optimization passes

## Performance Comparison

Benchmark: `fibonacci.py` (fibonacci(29))

| Metric | JIT Mode | AOT Mode | Speedup |
|--------|----------|----------|---------|
| Compile Time | 150ms | 730ms | **4.85x faster** |
| Execution Time | 11ms | 519ms | **45x faster** |
| Total Time | 162ms | 1249ms | **7.7x faster** |

**Note**: JIT execution is faster because:

- No subprocess overhead for running executable
- Code already "warm" in memory
- No disk I/O for executable creation

## Usage

### Basic Usage

```python
from multigen.backends.llvm.jit_executor import jit_compile_and_run

# Compile and execute LLVM IR file
result = jit_compile_and_run("fibonacci.ll", verbose=True)
print(f"Result: {result}")
```

### Advanced Usage

```python
from multigen.backends.llvm.jit_executor import LLVMJITExecutor

# Create executor
executor = LLVMJITExecutor()

try:
    # Compile LLVM IR
    executor.compile_ir_file("program.ll")

    # Execute main() function
    result = executor.execute_main()
    print(f"main() returned: {result}")

    # Execute arbitrary function with arguments
    result = executor.execute_function("fibonacci", 10)
    print(f"fibonacci(10) = {result}")

finally:
    executor.cleanup()
```

### From Python Source

```python
import subprocess
import tempfile
from pathlib import Path
from multigen.backends.llvm.jit_executor import jit_compile_and_run

# Generate LLVM IR from Python file
with tempfile.TemporaryDirectory() as tmpdir:
    output_dir = Path(tmpdir)

    # Convert Python to LLVM IR
    subprocess.run([
        "uv", "run", "multigen", "convert",
        "-t", "llvm", "program.py"
    ], cwd=output_dir)

    # Find generated .ll file
    ll_file = output_dir / "build/src/program.ll"

    # JIT compile and execute
    result = jit_compile_and_run(str(ll_file))
```

### Command Line

```bash
# Generate LLVM IR
uv run multigen convert -t llvm program.py

# JIT execute
uv run python -m multigen.backends.llvm.jit_executor build/src/program.ll
```

## API Reference

### `LLVMJITExecutor`

Main class for JIT compilation and execution.

#### Methods

- `__init__()` - Initialize executor and LLVM targets
- `create_execution_engine()` - Create MCJIT execution engine
- `compile_ir(llvm_ir: str)` - Compile LLVM IR string
- `compile_ir_file(llvm_ir_file: str)` - Compile LLVM IR from file
- `get_function_address(function_name: str)` - Get compiled function address
- `execute_main()` - Execute main() and return result
- `execute_function(function_name: str, *args)` - Execute arbitrary function
- `cleanup()` - Clean up resources

### `jit_compile_and_run(llvm_ir_file: str, verbose: bool = False) -> int`

Convenience function to compile and execute LLVM IR file.

**Args:**

- `llvm_ir_file`: Path to .ll file
- `verbose`: Print debug information

**Returns:**

- Return value from main() function

## Limitations

### Current Limitations

1. **Runtime Dependencies**: Requires llvmlite runtime
2. **No Standalone Executables**: Cannot produce distributable binaries
3. **In-Process Only**: Code executes in Python process
4. **No Cross-Compilation**: Executes on host architecture only

### Runtime Library Limitations

Currently, JIT mode works best with **self-contained programs** that don't require runtime libraries (vec_int, map_str_int, etc.). Programs with runtime dependencies need those libraries compiled and linked.

**Workaround**: For programs with runtime dependencies, use AOT mode:

```bash
uv run multigen build -t llvm program.py
```

## Examples

### Example 1: Simple Function

```python
from multigen.backends.llvm.jit_executor import LLVMJITExecutor

llvm_ir = """
define i64 @add(i64 %a, i64 %b) {
    %sum = add i64 %a, %b
    ret i64 %sum
}
"""

executor = LLVMJITExecutor()
try:
    executor.compile_ir(llvm_ir)
    result = executor.execute_function("add", 10, 20)
    print(f"10 + 20 = {result}")  # Output: 30
finally:
    executor.cleanup()
```

### Example 2: Fibonacci

```python
from multigen.backends.llvm.jit_executor import jit_compile_and_run

# Assuming fibonacci.ll exists
result = jit_compile_and_run("fibonacci.ll", verbose=True)
# Output: Compiling fibonacci.ll...
#         Compilation successful
#         Executing main()...
#         514229  (printed by program)
#         Execution complete. Result: 0
```

### Example 3: Performance Comparison

See `examples/llvm_jit_demo.py` for a complete comparison script.

```bash
uv run python examples/llvm_jit_demo.py
```

## Testing

Run JIT executor tests:

```bash
# All tests
make test

# JIT-specific tests
uv run pytest tests/test_jit_executor.py -v
```

## Technical Details

### Architecture

```text
Python AST → Static IR → LLVM IR → Execution Engine (JIT)
                                 ↘ llc → clang → Executable (AOT)
```

### LLVM Components

- **llvmlite**: Python binding for LLVM IR construction
- **MCJIT**: Machine Code JIT compiler (part of LLVM)
- **Execution Engine**: In-memory code execution

### Implementation

JIT mode uses llvmlite's execution engine:

1. Parse LLVM IR string to module
2. Verify module correctness
3. Add module to MCJIT compiler
4. Finalize object code in memory
5. Get function pointer via `get_function_address()`
6. Call function using ctypes

## Future Enhancements

### Planned Features

- [ ] Runtime library JIT linking (support vec_int, map_str_int, etc.)
- [ ] REPL mode for interactive Python-to-LLVM development
- [ ] Debug symbol generation for JIT code
- [ ] Profile-guided optimization (PGO) for JIT
- [ ] Caching compiled modules for faster reloads

### Potential Use Cases

- **Interactive REPL**: Python-to-native REPL
- **Runtime Code Generation**: Generate and execute code dynamically
- **Plugin Systems**: Load and execute plugins at runtime
- **Testing Framework**: Fast test execution without compilation overhead

## Resources

- [llvmlite Documentation](https://llvmlite.readthedocs.io/)
- [LLVM Execution Engine](https://llvm.org/docs/MCJITDesignAndImplementation.html)
- [MultiGen LLVM Backend Roadmap](../../../LLVM_BACKEND_ROADMAP.md)

## Contributing

To add JIT support for runtime libraries:

1. Compile runtime C files to LLVM IR (using clang)
2. Link LLVM IR modules before execution
3. Add module to execution engine
4. Test with benchmarks requiring runtime

See [LLVM_BACKEND_ROADMAP.md](../../../LLVM_BACKEND_ROADMAP.md) for development guidelines.
