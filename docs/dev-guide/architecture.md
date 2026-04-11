# Architecture

MultiGen's architecture is designed for extensibility, type safety, and clean separation of concerns.

## Overview

MultiGen consists of:

- **Frontend**: Python AST analysis, type inference, optimization
- **Pipeline**: 7-phase conversion pipeline
- **Backends**: Language-specific code generation (7 backends)
- **Runtime**: Self-contained runtime libraries for each backend
- **Verification**: Optional Z3-based formal verification

## 7-Phase Pipeline

All code conversion goes through these phases:

1. **Validation** -- Parse Python source to AST, validate syntax, check for unsupported constructs
2. **Analysis** -- Analyze types and dependencies, build symbol tables, detect variable scopes
3. **Python Optimization** -- Optimize Python AST, simplify expressions, constant folding
4. **Mapping** -- Map Python constructs to target language (types, operations, containers)
5. **Target Optimization** -- Backend-specific optimizations (inlining, loop unrolling, container selection)
6. **Generation** -- Emit target language code, format, add includes/imports
7. **Build** -- Optional compilation, Makefile generation, runtime library linking

## Core Abstractions

### LanguageBackend

Base class for all backends:

```python
class LanguageBackend(ABC):
    @abstractmethod
    def convert(self, source: str) -> str:
        """Convert Python source to target language"""

    @abstractmethod
    def get_emitter(self) -> AbstractEmitter:
        """Get code emitter"""

    @abstractmethod
    def get_factory(self) -> AbstractFactory:
        """Get object factory"""
```

### AbstractEmitter

Generates target language code:

```python
class AbstractEmitter(ABC):
    @abstractmethod
    def emit_function(self, node: ast.FunctionDef) -> str:
        """Emit a function definition"""

    @abstractmethod
    def emit_class(self, node: ast.ClassDef) -> str:
        """Emit a class definition"""
```

### AbstractFactory

Creates backend-specific objects:

```python
class AbstractFactory(ABC):
    @abstractmethod
    def create_type_mapper(self) -> TypeMapper:
        """Create type mapper"""

    @abstractmethod
    def create_builder(self) -> AbstractBuilder:
        """Create builder"""
```

## Backend Architecture

Each backend follows this structure:

```
backends/
  <language>/
    __init__.py          # Backend registration
    backend.py           # LanguageBackend implementation
    converter.py         # Code converter
    factory.py           # Object factory
    builder.py           # Build system
    type_inference.py    # Type inference (optional)
    runtime/             # Runtime library
```

## Design Patterns

### Visitor Pattern

Used for statement conversion in Haskell backend:

```python
class HaskellStatementVisitor(ABC):
    @abstractmethod
    def visit_assign(self, node: ast.Assign) -> str: ...

class MainFunctionVisitor(HaskellStatementVisitor):
    """IO-based visitor for main()"""

class PureFunctionVisitor(HaskellStatementVisitor):
    """Pure functional visitor"""
```

### Strategy Pattern

Used for type inference and loop conversion, shared across C++, Rust, Go backends (30% code reuse):

```python
class TypeInferenceStrategy(ABC):
    @abstractmethod
    def infer(self, node: ast.AST) -> Optional[str]: ...

class ConstantInferenceStrategy(TypeInferenceStrategy):
    """Infer from literal values"""

class BinOpInferenceStrategy(TypeInferenceStrategy):
    """Infer from binary operations"""
```

### Factory Pattern

Creates backend-specific components:

```python
class CFactory(AbstractFactory):
    def create_emitter(self) -> CEmitter:
        return CEmitter()

    def create_builder(self) -> CMakeBuilder:
        return CMakeBuilder()
```

## Type System

### Type Mapping

Python types map to backend types:

```
Python          C++              Rust             Go
------          ---              ----             --
int             int              i32              int
float           double           f64              float64
str             std::string      String           string
list[int]       vector<int>      Vec<i32>         []int
dict[str,int]   map<str,int>     HashMap<...>     map[string]int
set[int]        set<int>         HashSet<i32>     map[int]bool
```

## Runtime Libraries

Each backend includes a self-contained runtime:

| Backend  | Lines | Notes |
|----------|-------|-------|
| C        | ~2,500 | STC container templates, file I/O, string ops |
| C++      | 357   | Header-only, STL wrappers |
| Rust     | 304   | Pure std library |
| Go       | 413   | Reflection-based comprehensions |
| Haskell  | 214   | List/Map/Set helpers |
| OCaml    | 216   | Mutable references, association lists |
| LLVM     | ~8,300 | C runtime library for native compilation |

## Code Quality

- 1353 tests (100% passing)
- Strict mypy type checking
- 79% complexity reduction (design patterns)
- 49/49 benchmarks passing across all 7 backends
