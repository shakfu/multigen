#!/bin/bash
# Memory leak detection for LLVM backend benchmarks
# Tests all benchmarks with AddressSanitizer (ASAN)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/build"
MEMORY_TEST_DIR="$BUILD_DIR/memory_tests"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Benchmark files
ALGORITHM_BENCHMARKS=(
    "fibonacci"
    "matmul"
    "quicksort"
    "wordcount"
)

DATA_STRUCTURE_BENCHMARKS=(
    "list_ops"
    "dict_ops"
    "set_ops"
)

echo "======================================"
echo "LLVM Backend Memory Leak Testing"
echo "======================================"
echo ""
echo "Using AddressSanitizer (ASAN) for memory error detection"
echo ""

# Create memory test directory
mkdir -p "$MEMORY_TEST_DIR"

# Function to compile with ASAN and test
test_benchmark() {
    local bench_name=$1
    local bench_path=$2

    echo "Testing: $bench_name"

    # Compile with ASAN (we'll need to add this flag to the pipeline)
    # For now, manually compile the C runtime with ASAN

    # Look for .ll file in benchmark results directory first
    local ll_file="$BUILD_DIR/benchmark_results/llvm/${bench_name}/${bench_name}.ll"
    if [ ! -f "$ll_file" ]; then
        # Fallback to build root
        ll_file="$BUILD_DIR/${bench_name}.ll"
    fi

    local o_file="$MEMORY_TEST_DIR/${bench_name}_asan.o"
    local exe_file="$MEMORY_TEST_DIR/${bench_name}_asan"

    # Check if .ll file exists, if not build it
    if [ ! -f "$ll_file" ]; then
        echo "  ${YELLOW}⚠${NC}  LLVM IR not found, building..."
        cd "$PROJECT_ROOT"
        uv run multigen build -t llvm "$bench_path" > /dev/null 2>&1

        # Check again after build
        ll_file="$BUILD_DIR/benchmark_results/llvm/${bench_name}/${bench_name}.ll"
        if [ ! -f "$ll_file" ]; then
            ll_file="$BUILD_DIR/${bench_name}.ll"
        fi

        if [ ! -f "$ll_file" ]; then
            echo "  ${RED}✗${NC}  Build failed - .ll file not found"
            return 1
        fi
    fi

    # Compile to object with llc
    if [ -f "/opt/homebrew/opt/llvm/bin/llc" ]; then
        LLC="/opt/homebrew/opt/llvm/bin/llc"
        CLANG="/opt/homebrew/opt/llvm/bin/clang"
    else
        LLC="llc"
        CLANG="clang"
    fi

    $LLC -filetype=obj "$ll_file" -o "$o_file" 2>&1 || {
        echo "  ${RED}✗${NC}  Compilation failed"
        return 1
    }

    # Link with ASAN enabled
    # Find all C runtime files
    local runtime_files=(
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/vec_int_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/vec_vec_int_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/vec_str_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/map_int_int_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/map_str_int_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/set_int_minimal.c"
        "$PROJECT_ROOT/src/multigen/backends/llvm/runtime/multigen_llvm_string.c"
    )

    # Filter to only existing files
    local existing_runtime=()
    for file in "${runtime_files[@]}"; do
        if [ -f "$file" ]; then
            existing_runtime+=("$file")
        fi
    done

    $CLANG -fsanitize=address -g "$o_file" "${existing_runtime[@]}" -o "$exe_file" 2>&1 || {
        echo "  ${RED}✗${NC}  Linking with ASAN failed"
        return 1
    }

    # Run with ASAN
    echo "  Running with AddressSanitizer..."

    # Set ASAN options for better reporting
    export ASAN_OPTIONS=detect_leaks=1:halt_on_error=0:log_path="$MEMORY_TEST_DIR/${bench_name}_asan.log"

    if timeout 10 "$exe_file" > "$MEMORY_TEST_DIR/${bench_name}_output.txt" 2>&1; then
        # Check if ASAN found any issues
        if grep -q "ERROR: AddressSanitizer" "$MEMORY_TEST_DIR/${bench_name}_output.txt" 2>/dev/null; then
            echo "  ${RED}✗ MEMORY ERROR DETECTED${NC}"
            echo "    See: $MEMORY_TEST_DIR/${bench_name}_output.txt"
            return 1
        elif grep -q "LeakSanitizer" "$MEMORY_TEST_DIR/${bench_name}_output.txt" 2>/dev/null; then
            echo "  ${YELLOW}⚠ MEMORY LEAK DETECTED${NC}"
            echo "    See: $MEMORY_TEST_DIR/${bench_name}_output.txt"
            return 1
        else
            echo "  ${GREEN}✓${NC}  No memory errors detected"
            return 0
        fi
    else
        echo "  ${RED}✗${NC}  Runtime failed or timeout"
        cat "$MEMORY_TEST_DIR/${bench_name}_output.txt"
        return 1
    fi
}

# Test algorithm benchmarks
echo "Algorithm Benchmarks:"
echo "---------------------"
passed=0
failed=0

for bench in "${ALGORITHM_BENCHMARKS[@]}"; do
    if test_benchmark "$bench" "$PROJECT_ROOT/tests/benchmarks/algorithms/${bench}.py"; then
        ((passed++))
    else
        ((failed++))
    fi
done

# Test data structure benchmarks
echo ""
echo "Data Structure Benchmarks:"
echo "--------------------------"

for bench in "${DATA_STRUCTURE_BENCHMARKS[@]}"; do
    if test_benchmark "$bench" "$PROJECT_ROOT/tests/benchmarks/data_structures/${bench}.py"; then
        ((passed++))
    else
        ((failed++))
    fi
done

# Summary
echo ""
echo "======================================"
echo "Memory Testing Summary"
echo "======================================"
echo "Passed: ${GREEN}$passed${NC}"
echo "Failed: ${RED}$failed${NC}"
echo ""

if [ $failed -eq 0 ]; then
    echo "${GREEN}All benchmarks passed memory testing!${NC}"
    exit 0
else
    echo "${RED}Some benchmarks have memory issues.${NC}"
    echo "Check logs in: $MEMORY_TEST_DIR"
    exit 1
fi
