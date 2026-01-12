# STC (Smart Template Containers) Integration

This directory contains the integration of the STC C library into CGen for high-performance container operations in generated C code.

## What is STC?

STC (Smart Template Containers) is a modern, header-only C99 container library that provides:

- **High-performance containers**: Vector, HashMap, HashSet, Deque, String, and more
- **Type safety**: Compile-time type checking with template-like macros
- **Memory management**: RAII-style automatic cleanup and memory safety
- **Modern C design**: Clean API that rivals C++ STL performance

**Repository**: <https://github.com/stclib/STC>
**License**: MIT
**Language**: C99/C11 compatible

## Directory Structure

```text
src/cgen/ext/stc/
├── include/           # STC header files
│   ├── stc/          # Core STC headers (vec.h, hmap.h, etc.)
│   └── c11/          # C11 compatibility headers
├── src/              # STC source files and utilities
├── __init__.py       # Python module initialization
├── containers.py     # Container type mappings and code generation
├── translator.py     # Enhanced Python-to-C translator with STC support
├── LICENSE           # STC MIT License
└── README.md         # This file
```

## Container Mappings

| Python Type | STC Container | Description |
|-------------|---------------|-------------|
| `list` | `vec` | Dynamic array with push/pop operations |
| `dict` | `hmap` | Unordered key-value mapping (hash map) |
| `set` | `hset` | Unordered unique element collection |
| `deque` | `deque` | Double-ended queue |
| `str` | `cstr` | String with UTF-8 support and short string optimization |
| Collections | Various | Stack, queue, priority queue, sorted containers |

## Usage Examples

### Python Code

```python
def process_numbers(data: List[int]) -> Dict[int, int]:
    numbers = []
    result = {}

    for item in data:
        numbers.append(item * 2)
        result[item] = item * item

    return result
```

### Generated C Code (with STC)

```c
#define T NumbersVec, int
#include <stc/vec.h>

#define T ResultMap, int, int
#include <stc/hmap.h>

ResultMap process_numbers(int* data, size_t data_len) {
    NumbersVec numbers = {0};
    ResultMap result = {0};

    for (size_t i = 0; i < data_len; i++) {
        int item = data[i];
        NumbersVec_push(&numbers, item * 2);
        ResultMap_insert(&result, item, item * item);
    }

    NumbersVec_drop(&numbers);
    return result;
}
```

## Benefits

### 1. Performance

- **Zero runtime overhead**: Template-based approach with compile-time specialization
- **Cache-friendly**: Contiguous memory allocation for better performance
- **Optimized algorithms**: Hand-tuned implementations that outperform many alternatives

### 2. Safety

- **Type safety**: Compile-time type checking prevents many runtime errors
- **Memory safety**: Automatic cleanup prevents memory leaks
- **Bounds checking**: Available in debug builds

### 3. Readability

- **High-level operations**: Generated code uses container operations instead of raw pointers
- **Standard patterns**: Familiar to C developers who know modern container libraries
- **Self-documenting**: Container types and operations are explicit

### 4. Maintainability

- **Less error-prone**: Eliminates manual memory management bugs
- **Easier debugging**: Clear container operations vs. complex pointer arithmetic
- **Standard API**: Consistent interface across all container types

## Integration with CGen

The STC integration enhances CGen's Python-to-C translation in several ways:

### 1. Enhanced SimplePythonToCTranslator

The `STCPythonToCTranslator` class extends the basic translator with:

- Automatic detection of Python container types
- Generation of appropriate STC type definitions
- Translation of Python container operations to STC calls
- Automatic cleanup code generation

### 2. Type System Integration

- Maps Python type annotations to STC container types
- Supports complex nested types (e.g., `List[Dict[str, int]]`)
- Provides fallback to traditional C for unsupported types

### 3. Memory Management

- Automatic generation of container initialization
- RAII-style cleanup for all containers
- No manual malloc/free required

## API Reference

### Container Code Generation

```python
from cgen.ext.stc.containers import STCCodeGenerator

generator = STCCodeGenerator()

# Generate type definition for List[int]
type_def, include = generator.generate_container_type_def("numbers", "List[int]")
# Result: "#define T NumbersVec, int", "#include <stc/vec.h>"

# Generate operation translation
operation = generator.generate_operation_translation("append", "NumbersVec", ["&numbers", "42"])
# Result: "NumbersVec_push(&numbers, 42)"
```

### Translator Integration

```python
from cgen.ext.stc.translator import STCPythonToCTranslator

translator = STCPythonToCTranslator()

# Analyze Python code for container types
import ast
code = "numbers: List[int] = []"
tree = ast.parse(code)
type_info = translator.analyze_variable_types(tree)

# Generate STC includes and type definitions
includes, type_defs = translator.generate_stc_includes_and_types(type_info)
```

## Installation and Setup

The STC library is already integrated into CGen. To use it:

1. **Import the STC modules**:

   ```python
   from cgen.ext.stc import get_stc_include_path, get_stc_headers
   from cgen.ext.stc.translator import STCPythonToCTranslator
   ```

2. **Use in code generation**:
   The STC translator can be integrated into existing CGen pipelines to automatically enhance container operations.

3. **Compile generated code**:

   ```bash
   # Include STC headers in compilation
   gcc -I$(python -c "from cgen.ext.stc import get_stc_include_path; print(get_stc_include_path())") \
       your_generated_code.c -o output
   ```

## Performance Benchmarks

STC containers have been benchmarked to be:

- **Significantly faster** than C++ STL for hash-based operations
- **Competitive** with hand-optimized C code
- **More cache-friendly** than many alternatives due to better memory layout

## Future Enhancements

Planned improvements for STC integration:

1. **Advanced Container Support**: Specialized containers for specific domains
2. **Optimization Hints**: Compiler hints for better optimization
3. **Custom Allocators**: Memory pool integration for performance-critical code
4. **Debugging Support**: Enhanced debug information for container operations

## License

The STC library is distributed under the MIT License. See the `LICENSE` file for full details.

## Contributing

To contribute to STC integration:

1. Understand the STC API and container patterns
2. Follow CGen coding standards and testing requirements
3. Add comprehensive tests for new container mappings
4. Update documentation with examples

## Support

For STC-related issues:

- Check the [STC documentation](https://github.com/stclib/STC)
- Review CGen's STC integration tests
- Consult the container mapping tables in `containers.py`
