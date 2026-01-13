# MultiGen: Multi-Language Code Generator

MultiGen is a Python-to-multiple-languages code generator that translates Python code to C, C++, Rust, Go, Haskell, and OCaml while preserving semantics and performance characteristics.

## Overview

MultiGen extends the CGen (Python-to-C) project into a multi-language translation system with enhanced runtime libraries, code generation, and a clean backend architecture.

## Key Features

- **Multi-Language Support**: Generate code for C, C++, Rust, Go, Haskell, and OCaml with language features
- **Universal Preference System**: Customize code generation for each backend with language-specific preferences
- **Advanced Python Support**: Object-oriented programming, comprehensions, string methods, augmented assignment
- **Modern Libraries**: C++ STL, Rust standard library, Go standard library, Haskell containers, OCaml standard library
- **Clean Architecture**: Extensible backend system with abstract interfaces for adding new target languages
- **Type-Safe Generation**: Leverages Python type annotations for accurate and safe code translation
- **Runtime Libraries**: Enhanced C backend with 50KB+ runtime libraries providing Python-like semantics
- **CLI Interface**: Simple command-line tool with preference customization for conversion and building
- **Production-Ready**: 821 passing tests ensuring translation accuracy and code quality

## Supported Languages

| Language | Status      | Extension | Build System      | Advanced Features | Preferences |
|----------|-------------|-----------|-------------------|-------------------|-------------|
| C        | Enhanced    | `.c`      | Makefile / gcc    | OOP, STC containers, string methods, comprehensions, runtime libraries | 12 settings |
| C++      | Enhanced    | `.cpp`    | Makefile / g++    | OOP, STL containers, string methods, comprehensions | 15 settings |
| Rust     | Enhanced    | `.rs`     | Cargo / rustc     | OOP, standard library, string methods, comprehensions, memory safety | 19 settings |
| Go       | Enhanced    | `.go`     | go.mod / go build | OOP, standard library, string methods, comprehensions | 18 settings |
| Haskell  | Enhanced    | `.hs`     | Cabal / ghc       | Functional programming, comprehensions, type safety | 12 settings |
| OCaml    | Enhanced    | `.ml`     | dune / ocamlc     | Functional programming, pattern matching, comprehensions | 17 settings |

## Quick Start

### Installation

Install from pypi

```sh
pip install multigen
```

Install from source

```bash
git clone https://github.com/shakfu/multigen
cd multigen
pip install -e .
```

### Optional Dependencies

MultiGen has zero required dependencies for core functionality (C, C++, Rust, Go, Haskell, OCaml backends). Optional features can be installed as needed:

```bash
# LLVM backend support (native compilation, WebAssembly)
pip install multigen[llvm]

# Z3 theorem prover (formal verification)
pip install multigen[z3]

# All optional dependencies
pip install multigen[all]
```

### Basic Usage

```bash
# List available backends
multigen backends

# Convert Python to C (with advanced features)
multigen --target c convert my_script.py

# Convert Python to C++ (with STL support)
multigen --target cpp convert my_script.py

# Convert Python to Rust with build
multigen --target rust build my_script.py

# Convert Python to Go (with enhanced features)
multigen --target go convert my_script.py

# Convert Python to Haskell (with functional programming features)
multigen --target haskell convert my_script.py

# Convert Python to OCaml (with functional programming and pattern matching)
multigen --target ocaml convert my_script.py

# Batch convert all Python files
multigen --target cpp batch --source-dir ./examples
```

### Backend Preferences

Customize code generation for each target language with the `--prefer` flag:

```bash
# Haskell with native comprehensions (idiomatic)
multigen --target haskell convert my_script.py --prefer use_native_comprehensions=true

# C with custom settings
multigen --target c convert my_script.py --prefer use_stc_containers=false --prefer indent_size=2

# C++ with modern features
multigen --target cpp convert my_script.py --prefer cpp_standard=c++20 --prefer use_modern_cpp=true

# Rust with specific edition
multigen --target rust convert my_script.py --prefer rust_edition=2018 --prefer clone_strategy=explicit

# Go with version targeting
multigen --target go convert my_script.py --prefer go_version=1.19 --prefer use_generics=false

# OCaml with functional programming preferences
multigen --target ocaml convert my_script.py --prefer prefer_immutable=true --prefer use_pattern_matching=true

# Multiple preferences
multigen --target haskell build my_script.py \
  --prefer use_native_comprehensions=true \
  --prefer camel_case_conversion=false \
  --prefer strict_data_types=true
```

## Preference System

MultiGen features a preference system that allows you to choose between **cross-language consistency** (default) and **language-specific idiomatic optimizations**.

### Design Philosophy

- **Default (Consistent)**: Uses runtime library functions for predictable behavior across all languages
- **Idiomatic (Optimized)**: Uses native language features for better performance and familiarity

### Available Preference Categories

| Backend | Key Preferences | Description |
|---------|-----------------|-------------|
| **Haskell** | `use_native_comprehensions`, `camel_case_conversion`, `strict_data_types` | Native vs runtime comprehensions, naming, type system |
| **C** | `use_stc_containers`, `brace_style`, `indent_size` | Container choice, code style, memory management |
| **C++** | `cpp_standard`, `use_modern_cpp`, `use_stl_containers` | Language standard, modern features, STL usage |
| **Rust** | `rust_edition`, `clone_strategy`, `use_iterators` | Edition targeting, ownership patterns, functional style |
| **Go** | `go_version`, `use_generics`, `naming_convention` | Version compatibility, language features, Go idioms |
| **OCaml** | `prefer_immutable`, `use_pattern_matching`, `curried_functions` | Functional style, pattern matching, function curry style |

### Example: Haskell Comprehensions

**Python Source:**

```python
def filter_numbers(numbers):
    return [x * 2 for x in numbers if x > 5]
```

**Default (Runtime Consistency):**

```haskell
filterNumbers numbers = listComprehensionWithFilter numbers (\x -> x > 5) (\x -> x * 2)
```

**Native (Idiomatic Haskell):**

```haskell
filterNumbers numbers = [x * 2 | x <- numbers, x > 5]
```

### Example: OCaml Functional Programming

**Python Source:**

```python
def process_items(items):
    return [item.upper() for item in items if len(item) > 3]
```

**Default (Runtime Consistency):**

```ocaml
let process_items items =
  list_comprehension_with_filter items (fun item -> len item > 3) (fun item -> upper item)
```

**Functional (Idiomatic OCaml):**

```ocaml
let process_items items =
  List.filter (fun item -> String.length item > 3) items
  |> List.map String.uppercase_ascii
```

For complete preference documentation, see [PREFERENCES.md](PREFERENCES.md).

## Examples

### Simple Functions

**Python Input:**

```python
def add(x: int, y: int) -> int:
    return x + y

def main() -> None:
    result = add(5, 3)
    print(result)
```

**Generated C++:**

```cpp
#include <iostream>
#include <vector>
#include <unordered_map>
#include "runtime/multigen_cpp_runtime.hpp"

using namespace std;
using namespace multigen;

int add(int x, int y) {
    return (x + y);
}

void main() {
    int result = add(5, 3);
    cout << result << endl;
}
```

**Generated C:**

```c
#include <stdio.h>
#include "multigen_runtime.h"

int add(int x, int y) {
    return (x + y);
}

void main() {
    int result = add(5, 3);
    printf("%d\n", result);
}
```

**Generated Go:**

```go
package main

import "multigen"

func add(x int, y int) int {
    return (x + y)
}

func main() {
    result := add(5, 3)
    multigen.Print(result)
}
```

**Generated Rust:**

```rust
// Include MultiGen Rust runtime
mod multigen_rust_runtime;
use multigen_rust_runtime::*;

fn add(x: i32, y: i32) -> i32 {
    (x + y)
}

fn main() {
    let mut result = add(5, 3);
    print_value(result);
}
```

**Generated Haskell:**

```haskell
module Main where

import MultiGenRuntime
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

add :: Int -> Int -> Int
add x y = (x + y)

main :: IO ()
main = printValue (add 5 3)
```

**Generated OCaml:**

```ocaml
(* Generated OCaml code from Python *)

open Mgen_runtime

let add x y =
  (x + y)

let main () =
  let result = add 5 3 in
  print_value result

let () = print_value "Generated OCaml code executed successfully"
```

### Advanced Features (Object-Oriented Programming)

**Python Input:**

```python
class Calculator:
    def __init__(self, name: str):
        self.name: str = name
        self.total: int = 0

    def add(self, value: int) -> None:
        self.total += value

    def get_result(self) -> str:
        return self.name.upper() + ": " + str(self.total)

def process() -> list:
    calc = Calculator("math")
    calc.add(10)
    return [calc.get_result() for _ in range(2)]
```

**Generated C++:**

```cpp
#include <iostream>
#include <string>
#include <vector>
#include "runtime/multigen_cpp_runtime.hpp"

using namespace std;
using namespace multigen;

class Calculator {
public:
    std::string name;
    int total;

    Calculator(std::string name) {
        this->name = name;
        this->total = 0;
    }

    void add(int value) {
        this->total += value;
    }

    std::string get_result() {
        return (StringOps::upper(this->name) + (": " + to_string(this->total)));
    }
};

std::vector<std::string> process() {
    Calculator calc("math");
    calc.add(10);
    return list_comprehension(Range(2), [&](auto _) {
        return calc.get_result();
    });
}
```

**Generated Go:**

```go
package main

import "multigen"

type Calculator struct {
    Name string
    Total int
}

func NewCalculator(name string) Calculator {
    obj := Calculator{}
    obj.Name = name
    obj.Total = 0
    return obj
}

func (obj *Calculator) Add(value int) {
    obj.Total += value
}

func (obj *Calculator) GetResult() string {
    return (multigen.StrOps.Upper(obj.Name) + (": " + multigen.ToStr(obj.Total)))
}

func process() []interface{} {
    calc := NewCalculator("math")
    calc.Add(10)
    return multigen.Comprehensions.ListComprehension(multigen.NewRange(2), func(item interface{}) interface{} {
        _ := item.(int)
        return calc.GetResult()
    })
}
```

**Generated Rust:**

```rust
use std::collections::{HashMap, HashSet};

// Include MultiGen Rust runtime
mod multigen_rust_runtime;
use multigen_rust_runtime::*;

#[derive(Clone)]
struct Calculator {
    name: String,
    total: i32,
}

impl Calculator {
    fn new(name: String) -> Self {
        Calculator {
            name: name,
            total: 0,
        }
    }

    fn add(&mut self, value: i32) {
        self.total += value;
    }

    fn get_result(&mut self) -> String {
        ((StrOps::upper(&self.name) + ": ".to_string()) + to_string(self.total))
    }
}

fn process() -> Vec<String> {
    let mut calc = Calculator::new("math".to_string());
    calc.add(10);
    Comprehensions::list_comprehension(new_range(2).collect(), |_| calc.get_result())
}
```

**Generated Haskell:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import MultiGenRuntime
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

data Calculator = Calculator
  { name :: String
  , total :: Int
  } deriving (Show, Eq)

newCalculator :: String -> Calculator
newCalculator name = Calculator { name = name, total = 0 }

add :: Calculator -> Int -> ()
add obj value = ()  -- Haskell immutable approach

getResult :: Calculator -> String
getResult obj = (upper (name obj)) + ": " + (toString (total obj))

process :: [String]
process =
  let calc = newCalculator "math"
  in listComprehension (rangeList (range 2)) (\_ -> getResult calc)
```

**Generated OCaml:**

```ocaml
(* Generated OCaml code from Python *)

open Mgen_runtime

type calculator = {
  name : string;
  total : int;
}

let create_calculator name =
  {
    name = name;
    total = 0;
  }

let calculator_add (calculator_obj : calculator) value =
  (* Functional update creating new record *)
  { calculator_obj with total = calculator_obj.total + value }

let calculator_get_result (calculator_obj : calculator) =
  (calculator_obj.name ^ ": " ^ string_of_int calculator_obj.total)

let process () =
  let calc = create_calculator "math" in
  let updated_calc = calculator_add calc 10 in
  list_comprehension (range_list (range 2)) (fun _ -> calculator_get_result updated_calc)
```

## Architecture

MultiGen follows a clean, extensible architecture with well-defined components:

### 7-Phase Translation Pipeline

1. **Validation**: Verify Python source compatibility
2. **Analysis**: Analyze code structure and dependencies
3. **Python Optimization**: Apply Python-level optimizations
4. **Mapping**: Map Python constructs to target language equivalents
5. **Target Optimization**: Apply target language-specific optimizations
6. **Generation**: Generate target language code
7. **Build**: Compile/build using target language toolchain

### Frontend (Language-Agnostic)

- **Type Inference**: Analyzes Python type annotations and infers types
- **Static Analysis**: Validates code compatibility and detects unsupported features
- **AST Processing**: Parses and transforms Python abstract syntax tree

### Backends (Language-Specific)

Each backend implements abstract interfaces:

- **AbstractEmitter**: Code generation for target language
- **AbstractFactory**: Factory for backend components
- **AbstractBuilder**: Build system integration
- **AbstractContainerSystem**: Container and collection handling

### Runtime Libraries (C Backend)

- **Error Handling** (`multigen_error_handling.h/.c`): Python-like exception system
- **Memory Management** (`multigen_memory_ops.h/.c`): Safe allocation and cleanup
- **Python Operations** (`multigen_python_ops.h/.c`): Python built-ins and semantics
- **String Operations** (`multigen_string_ops.h/.c`): String methods with memory safety
- **STC Integration** (`multigen_stc_bridge.h/.c`): Smart Template Container bridge

## CLI Commands

### Convert

Convert Python files to target language:

```bash
multigen --target <language> convert <input.py>
multigen --target rust convert example.py
```

### Build

Convert and compile/build the result:

```bash
multigen --target <language> build <input.py>
multigen --target go build --makefile example.py  # Generate build file
multigen --target c build example.py              # Direct compilation
```

### Batch

Process multiple files:

```bash
multigen --target <language> batch --source-dir <dir>
multigen --target rust batch --source-dir ./src --build
```

### Backends

List available language backends:

```bash
multigen backends
```

### Clean

Clean build artifacts:

```bash
multigen clean
```

## Development

### Running Tests

```bash
make test           # Run all 821 tests
make lint           # Run code linting with ruff
make type-check     # Run type checking with mypy
```

### Test Organization

MultiGen maintains a test suite organized into focused modules:

- `test_backend_c_*.py`: C backend tests (191 tests total)
  - Core functionality, OOP, comprehensions, string methods, runtime libraries
- `test_backend_cpp_*.py`: C++ backend tests (104 tests)
  - STL integration, modern C++ features, OOP support
- `test_backend_rust_*.py`: Rust backend tests (176 tests)
  - Ownership patterns, memory safety, standard library
- `test_backend_go_*.py`: Go backend tests (95 tests)
  - Go idioms, standard library, concurrency patterns
- `test_backend_haskell_*.py`: Haskell backend tests (93 tests)
  - Functional programming, type safety, comprehensions
- `test_backend_ocaml_*.py`: OCaml backend tests (25 tests)
  - Functional programming, pattern matching, immutability

### Adding New Backends

To add support for a new target language:

1. Create backend directory: `src/multigen/backends/mylang/`
2. Implement required abstract interfaces:
   - `MyLangBackend(LanguageBackend)`: Main backend class
   - `MyLangFactory(AbstractFactory)`: Component factory
   - `MyLangEmitter(AbstractEmitter)`: Code generation
   - `MyLangBuilder(AbstractBuilder)`: Build system integration
   - `MyLangContainerSystem(AbstractContainerSystem)`: Container handling
   - `MyLangPreferences(BasePreferences)`: Language-specific preferences
3. Register backend in `src/multigen/backends/registry.py`
4. Add tests in `tests/test_backend_mylang_*.py`
5. Update documentation

See existing backends (C, C++, Rust, Go, Haskell, OCaml) for implementation examples.

## Relationship with CGen

MultiGen extends the [CGen](https://github.com/shakfu/cgen) project by:

- Expanding Python-to-C capabilities into a multi-language translation system
- Integrating CGen's C runtime libraries (50KB+ of error handling, memory management, Python operations)
- Incorporating the STC (Smart Template Container) library for high-performance C containers
- Adding support for C++, Rust, Go, Haskell, and OCaml target languages
- Implementing a clean 7-phase translation pipeline with abstract backend interfaces
- Providing a universal preference system for language-specific code generation customization

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Advanced Features

### Supported Python Features

All backends support core Python features:

- **Object-Oriented Programming**: Classes, methods, constructors, instance variables, method calls
- **Augmented Assignment**: All operators (`+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `|=`, `^=`, `&=`, `<<=`, `>>=`)
- **String Operations**: `upper()`, `lower()`, `strip()`, `find()`, `replace()`, `split()`
- **Comprehensions**: List, dict, and set comprehensions with range iteration and conditional filtering
- **Control Structures**: if/elif/else, while loops, for loops with range()
- **Built-in Functions**: `abs()`, `bool()`, `len()`, `min()`, `max()`, `sum()`
- **Type Inference**: Automatic type detection from annotations and assignments

### Container Support by Language

- **C**: STC (Smart Template Container) library with optimized C containers (864KB integrated library)
- **C++**: STL containers (`std::vector`, `std::unordered_map`, `std::unordered_set`)
- **Rust**: Standard library collections (`Vec`, `HashMap`, `HashSet`) with memory safety
- **Go**: Standard library containers with idiomatic Go patterns
- **Haskell**: Standard library containers with type-safe functional operations
- **OCaml**: Standard library with immutable data structures and pattern matching

### Test Coverage

MultiGen maintains test coverage ensuring translation accuracy:

- **821 total tests** across all components and backends
- Comprehensive backend coverage testing all major Python features
- Test categories: basics, OOP, comprehensions, string methods, augmented assignment, control flow, integration
- All tests passing with zero regressions (100%)

## Development Roadmap

### Completed Milestones

- Multi-language backend system with C, C++, Rust, Go, Haskell, and OCaml support
- Advanced C runtime integration with 50KB+ of runtime libraries
- Sophisticated Python-to-C conversion with complete function and control flow support
- Object-oriented programming support across all backends
- Advanced Python language features: comprehensions, string methods, augmented assignment
- Complete STC library integration (864KB Smart Template Container library)
- Architecture consolidation with unified C backend module
- Professional test organization with 821 tests in focused, single-responsibility files
- Universal preference system with language-specific customization
- Production-ready code generation with clean, efficient output
- 4 production-ready backends (C++, C, Rust, Go) with 100% benchmark success

### Future Development

- **Advanced Frontend Analysis**: Integrate optimization detection and static analysis engine
- **STC Performance Optimization**: Container specialization and memory layout optimization
- **Formal Verification**: Theorem proving and memory safety proofs integration
- **Cross-Language Runtime**: Extend runtime concepts to other backends (C++, Rust, Go)
- **Performance Benchmarking**: Comprehensive performance analysis across all target languages
- **IDE Integration**: Language server protocol support for MultiGen syntax
- **Web Interface**: Online code conversion tool
- **Plugin System**: External backend support and extensibility
