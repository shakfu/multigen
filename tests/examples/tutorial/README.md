# MultiGen Tutorial Examples

These examples accompany the [Getting Started Tutorial](../../docs/GETTING_STARTED.md).

## Files

1. **hello.py** - Basic Hello World
2. **math_ops.py** - Mathematical operations (factorial, sum)
3. **string_ops.py** - String operations
4. **data_structures.py** - Dictionaries and data structures

## Running the Examples

### Hello World

```bash
# Convert to C++
multigen --target cpp convert hello.py

# Build and run
multigen --target cpp build hello.py
./build/hello
```

### Math Operations

```bash
# Try different backends
multigen --target cpp build math_ops.py
multigen --target rust build math_ops.py
multigen --target go build math_ops.py
./build/math_ops
```

### String Operations

```bash
multigen --target rust build string_ops.py
./build/string_ops
```

### Data Structures

```bash
multigen --target go build data_structures.py
./build/data_structures
```

## Notes

- All examples use type annotations (required)
- F-strings are converted to string concatenation
- Each example has a `main()` function
- Examples are tested with all backends

## See Also

- [Getting Started Guide](../../docs/GETTING_STARTED.md)
- [Error Handling](../../docs/ERROR_HANDLING.md)
- [More Examples](../../tests/examples/)
