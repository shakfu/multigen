# LLVM Backend Memory Testing

## Overview

The LLVM backend includes comprehensive memory leak detection using **AddressSanitizer (ASAN)**, a powerful runtime memory error detector built into Clang/LLVM.

## Test Results

**Status**: [x] **ALL TESTS PASSING**

All 7 benchmarks pass memory testing with zero leaks detected:

- [x] fibonacci
- [x] matmul
- [x] quicksort
- [x] wordcount
- [x] list_ops
- [x] dict_ops
- [x] set_ops

## Running Memory Tests

### Automated Testing

Use the provided script to test all benchmarks:

```bash
./scripts/test_llvm_memory.sh
```

This will:

1. Compile each benchmark with AddressSanitizer (`-fsanitize=address`)
2. Execute with leak detection enabled
3. Report any memory errors or leaks
4. Generate detailed logs in `build/memory_tests/`

### Manual Testing

To test a specific program with ASAN:

```bash
# 1. Build the LLVM IR
uv run multigen build -t llvm your_program.py

# 2. Compile with ASAN manually
LLC=/opt/homebrew/opt/llvm/bin/llc  # or just 'llc'
CLANG=/opt/homebrew/opt/llvm/bin/clang  # or just 'clang'

$LLC -filetype=obj build/your_program.ll -o build/your_program.o

# 3. Compile runtime with ASAN
$CLANG -fsanitize=address -g -c \
    src/multigen/backends/llvm/runtime/vec_int_minimal.c \
    src/multigen/backends/llvm/runtime/map_int_int_minimal.c \
    src/multigen/backends/llvm/runtime/set_int_minimal.c \
    # ... other runtime files

# 4. Link with ASAN
$CLANG -fsanitize=address -g build/your_program.o \
    build/*.o -o build/your_program_asan

# 5. Run with leak detection
ASAN_OPTIONS=detect_leaks=1 ./build/your_program_asan
```

## ASAN Integration

The LLVM backend now supports ASAN compilation through the builder API:

### Python API

```python
from multigen.backends.llvm import LLVMBuilder, LLVMCompiler

# Using builder
builder = LLVMBuilder()
builder.compile_direct(
    source_file="build/program.ll",
    output_dir="build",
    enable_asan=True  # Enable AddressSanitizer
)

# Using compiler
compiler = LLVMCompiler()
compiler.compile_ir_to_executable(
    llvm_ir=ir_code,
    output_path="build/program",
    enable_asan=True  # Enable AddressSanitizer
)
```

### Implementation Details

When `enable_asan=True`:

- Runtime C files compiled with `-fsanitize=address -g`
- Linker adds `-fsanitize=address -g` flags
- Debug symbols included for better stack traces

## What ASAN Detects

AddressSanitizer can detect:

1. **Use-after-free** - Accessing freed memory
2. **Heap buffer overflow** - Reading/writing past allocated bounds
3. **Stack buffer overflow** - Stack array overruns
4. **Double free** - Freeing memory twice
5. **Use-after-return** - Using stack variables after function returns
6. **Memory leaks** - Allocated memory not freed (with `detect_leaks=1`)

## ASAN Options

Configure ASAN behavior with environment variables:

```bash
# Detect leaks (default: enabled)
export ASAN_OPTIONS=detect_leaks=1

# Don't halt on first error (collect all errors)
export ASAN_OPTIONS=halt_on_error=0

# Save logs to file
export ASAN_OPTIONS=log_path=/path/to/asan.log

# Combined options
export ASAN_OPTIONS=detect_leaks=1:halt_on_error=0:log_path=asan.log
```

## Test Script Details

### Location

`scripts/test_llvm_memory.sh`

### Features

- Automatically finds LLVM tools (llc, clang)
- Compiles all runtime libraries with ASAN
- Runs benchmarks with timeout protection
- Generates detailed logs
- Color-coded output for easy reading
- Works with Homebrew LLVM or system LLVM

### Output

Successful run:

```text
======================================
LLVM Backend Memory Leak Testing
======================================

Testing: fibonacci
  Running with AddressSanitizer...
  [x]  No memory errors detected

...

======================================
Memory Testing Summary
======================================
Passed: 7
Failed: 0

All benchmarks passed memory testing!
```

## Runtime Library Memory Safety

All LLVM runtime libraries are memory-safe:

### vec_int_minimal.c (~130 lines)

- Proper `malloc`/`realloc`/`free` usage
- Bounds checking on all access
- `vec_int_free()` cleans up all allocations

### vec_vec_int_minimal.c (~200 lines)

- Nested container cleanup (frees inner vectors)
- No dangling pointers
- Proper deep copy semantics

### map_int_int_minimal.c (~216 lines)

- Hash table with linear probing
- Grows dynamically without leaks
- `map_int_int_free()` releases all entries

### set_int_minimal.c (~182 lines)

- Separate chaining for collisions
- Recursive chain cleanup
- No memory leaks on insertion/deletion

### String Operations

- `multigen_llvm_string.c` uses `malloc`/`free` correctly
- String arrays properly deallocated
- No string leaks in split/concat operations

## Performance Impact

ASAN has runtime overhead:

- **Slowdown**: ~2x execution time
- **Memory**: ~2-3x memory usage
- **Binary size**: Larger due to instrumentation

**Recommendation**: Use ASAN during development/testing, disable for production.

## Continuous Integration

Add memory testing to CI/CD:

```yaml
# .github/workflows/ci.yml
- name: LLVM Memory Tests
  run: |
    chmod +x scripts/test_llvm_memory.sh
    ./scripts/test_llvm_memory.sh
```

## Troubleshooting

### "clang: error: unsupported option '-fsanitize=address'"

Your clang version doesn't support ASAN. Install LLVM:

```bash
# macOS
brew install llvm

# Linux
sudo apt-get install clang
```

### "LeakSanitizer: detected memory leaks"

Check the stack trace in the output to find the leak source:

- Look for allocation site (where memory was allocated)
- Verify corresponding `free()` call exists
- Check all code paths call cleanup functions

### False Positives

Some libraries may report false leaks. Suppress with:

```bash
export LSAN_OPTIONS=suppressions=lsan_suppressions.txt
```

Create `lsan_suppressions.txt`:

```text
leak:library_function_name
```

## Future Enhancements

- [ ] Valgrind support (Linux-specific, more detailed analysis)
- [ ] Memory Sanitizer (MSan) for uninitialized memory
- [ ] Thread Sanitizer (TSan) for race conditions
- [ ] Automated memory testing in `make test`
- [ ] Leak sanitizer integration with CI
- [ ] Memory profiling tools (heap usage tracking)

## References

- [AddressSanitizer](https://clang.llvm.org/docs/AddressSanitizer.html) - Official LLVM docs
- [LeakSanitizer](https://clang.llvm.org/docs/LeakSanitizer.html) - Leak detection
- [Sanitizer Options](https://github.com/google/sanitizers/wiki/SanitizerCommonFlags) - Runtime flags

---

**Last Updated**: October 2025 (v0.1.80)
**Status**: All 7 benchmarks passing (0 leaks, 0 errors)
