# MultiGen Examples

This directory contains real-world example applications demonstrating MultiGen's capabilities.

## Directory Structure

- **cli_tools/** - Command-line utilities
- **data_processing/** - Data analysis and processing applications  
- **algorithms/** - Classic algorithm implementations
- **games/** - Simple interactive games

## Examples

### CLI Tools

**wordcount.py** - Word frequency counter

- Demonstrates: File I/O, string processing, dictionaries
- Use case: Text analysis, content analytics
- Backends: C, C++, Rust, Go, Haskell, OCaml

### Data Processing

**csv_stats.py** - CSV statistics calculator

- Demonstrates: Data parsing, numerical computations, aggregations
- Use case: Data analysis, statistical reporting
- Backends: C, C++, Rust, Go, Haskell, OCaml

### Algorithms

**merge_sort.py** - Merge sort implementation

- Demonstrates: Recursive algorithms, list manipulation
- Use case: Sorting, algorithm study
- Backends: C, C++, Rust, Go, Haskell, OCaml

### Games

**number_guess.py** - Number guessing game

- Demonstrates: Game logic, conditional flow, user interaction
- Use case: Interactive applications, learning
- Backends: C, C++, Rust, Go, Haskell, OCaml

## Running Examples

### Convert to target language

```bash
# Convert to C
multigen --target c convert examples/cli_tools/wordcount.py

# Convert to C++  
multigen --target cpp convert examples/algorithms/merge_sort.py

# Convert to Rust
multigen --target rust convert examples/data_processing/csv_stats.py

# Convert to Go
multigen --target go convert examples/games/number_guess.py
```

### Build and run

```bash
# Build and run (C example)
multigen --target c build examples/cli_tools/wordcount.py
./build/wordcount

# Build with external build system
multigen --target rust convert examples/algorithms/merge_sort.py
cd build/src && cargo build --release && ./target/release/merge_sort
```

## Example Features

Each example demonstrates:

- ✅ Type annotations and type inference
- ✅ Container operations (lists, dicts, sets)
- ✅ Control flow (loops, conditionals)
- ✅ Functions and return values
- ✅ Practical real-world utility
- ✅ Cross-backend compatibility

## Adding New Examples

To add a new example:

1. Create Python file in appropriate category directory
2. Include docstring explaining the example
3. Use type annotations throughout
4. Add entry to this README
5. Test with multiple backends

## Performance Comparisons

All examples can be compiled and benchmarked across backends:

```bash
# Compare performance across backends
for backend in c cpp rust go haskell ocaml; do
    echo "=== $backend ==="
    multigen --target $backend build examples/algorithms/merge_sort.py
    time ./build/merge_sort
done
```
