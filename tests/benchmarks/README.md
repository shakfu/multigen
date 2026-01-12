# MultiGen Benchmark Suite

This directory contains benchmarks for evaluating the performance of MultiGen-generated code across all supported backends (C, C++, Rust, Go, Haskell, OCaml).

## Structure

```text
benchmarks/
├── algorithms/          # Algorithm implementations
│   ├── fibonacci.py    # Recursive algorithm performance
│   ├── quicksort.py    # Array manipulation performance
│   ├── matmul.py       # Numeric computation performance
│   └── wordcount.py    # String/dict operations performance
├── data_structures/     # Container performance
│   ├── list_ops.py     # List operations and comprehensions
│   ├── dict_ops.py     # Dictionary operations and comprehensions
│   └── set_ops.py      # Set operations and comprehensions
└── real_world/         # Practical scenarios (planned)
```

## Benchmark Programs

### Algorithm Benchmarks

1. **fibonacci.py** - Tests recursive function call overhead
   - Calculates Fibonacci numbers up to n=30
   - Measures recursion performance
   - Expected output: `514229` (fib(29))

2. **quicksort.py** - Tests array manipulation and recursion
   - Sorts arrays of integers using quicksort
   - Runs 100 iterations for timing stability
   - Expected output: `5` (first element of sorted array)

3. **matmul.py** - Tests numeric computation
   - Multiplies 20x20 matrices
   - Runs 10 iterations
   - Expected output: `1200` (center element)

4. **wordcount.py** - Tests string and dictionary operations
   - Counts word frequencies in text
   - Runs 1000 iterations
   - Expected output: `4` (count of "the")

### Data Structure Benchmarks

1. **list_ops.py** - Tests list operations
   - List comprehensions with filtering
   - List append and access operations
   - Expected output: sum of results

2. **dict_ops.py** - Tests dictionary operations
   - Dictionary comprehensions
   - Dictionary insert and lookup
   - Expected output: sum of results

3. **set_ops.py** - Tests set operations
   - Set comprehensions
   - Set membership testing
   - Expected output: count + found_count

## Running Benchmarks

### Using Make (Recommended)

```bash
# Run all benchmarks
make benchmark

# Run specific categories
make benchmark-algorithms
make benchmark-data-structures

# Generate report from results
make benchmark-report

# Clean results
make benchmark-clean
```

### Manual Execution

```bash
# Run benchmark script
uv run python scripts/benchmark.py --category all --output benchmark_results

# Run specific category
uv run python scripts/benchmark.py --category algorithms --output benchmark_results

# Run specific backends only
uv run python scripts/benchmark.py --backends c cpp rust --output benchmark_results

# Generate Markdown report
uv run python scripts/generate_benchmark_report.py benchmark_results/benchmark_results.json --output benchmark_results/benchmark_report.md
```

### Direct Testing (Alternative)

You can also use the existing compilation test system to evaluate benchmarks:

```bash
# Copy benchmarks to test fixtures
cp benchmarks/algorithms/*.py tests/fixtures/compilation/

# Run compilation tests
make test-compilation
```

## Metrics Collected

The benchmark framework collects the following metrics:

1. **Compilation Time** - Time to compile generated code (wall clock)
2. **Execution Time** - Time to run the compiled program (wall clock)
3. **Binary Size** - Size of the compiled executable in bytes
4. **Lines of Code** - Number of lines in generated source code
5. **Success Rate** - Percentage of successful compilations/executions

## Output Format

### JSON Results

Results are saved as JSON in `benchmark_results/benchmark_results.json`:

```json
{
  "summary": {
    "total_benchmarks": 7,
    "total_backends": 6,
    "successful_runs": 42,
    "failed_runs": 0,
    "by_backend": {
      "c": {
        "total": 7,
        "successful": 7,
        "avg_compilation_time": 0.523,
        "avg_execution_time": 0.012,
        "avg_binary_size": 49152,
        "avg_lines_of_code": 127
      }
    }
  },
  "results": [...]
}
```

### Markdown Report

A human-readable Markdown report is generated showing:

- Summary statistics
- Backend comparison table
- Detailed results per benchmark
- Performance rankings (fastest execution, fastest compilation, smallest binary)
- Failed runs (if any)

## Adding New Benchmarks

To add a new benchmark:

1. Create a Python file in the appropriate directory
2. Implement using MultiGen-supported features only
3. Include type annotations for all variables
4. Ensure main() returns int (0 for success)
5. Print expected output for verification
6. Run through the benchmark suite

Example template:

```python
"""Description of benchmark."""

def benchmark_function(input: int) -> int:
    """Benchmark description."""
    # Implementation
    return result

def main() -> int:
    """Run benchmark."""
    result: int = 0
    for i in range(100):
        result = benchmark_function(i)
    print(result)
    return 0
```

## Current Status

**Phase 2: Performance Benchmarking Framework - IN PROGRESS**

✅ Completed:

- Benchmark directory structure
- 7 benchmark programs (4 algorithms, 3 data structures)
- Automated benchmark runner script
- Metrics collection system
- Markdown report generator
- Makefile targets

⚠️ Pending:

- Full pipeline integration for all backends
- Memory usage profiling
- Automated performance regression detection
- CI/CD integration
- Benchmark result history tracking

## Notes

- Benchmarks are designed to use only MultiGen-supported Python features
- Each benchmark includes expected output for verification
- Iteration counts are tuned to produce measurable timing differences
- All benchmarks should complete in < 30 seconds
