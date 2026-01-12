Architecture
============

MultiGen's architecture is designed for extensibility, type safety, and clean separation of concerns.

Overview
--------

MultiGen consists of:

- **Frontend**: Python AST analysis, type inference, optimization
- **Pipeline**: 7-phase conversion pipeline
- **Backends**: Language-specific code generation (6 backends)
- **Runtime**: Self-contained runtime libraries for each backend
- **Verification**: Optional Z3-based formal verification

7-Phase Pipeline
----------------

All code conversion goes through these phases:

1. **Validation**

   - Parse Python source to AST
   - Validate syntax and structure
   - Check for unsupported constructs

2. **Analysis**

   - Analyze types and dependencies
   - Build symbol tables
   - Detect variable scopes
   - Analyze control flow

3. **Python Optimization**

   - Optimize Python AST
   - Simplify expressions
   - Constant folding
   - Dead code elimination

4. **Mapping**

   - Map Python constructs to target language
   - Type mapping (list → vector/array/List)
   - Operation mapping (comprehensions → loops/folds)

5. **Target Optimization**

   - Backend-specific optimizations
   - Inlining
   - Loop unrolling
   - Container selection

6. **Generation**

   - Emit target language code
   - Format and prettify
   - Add includes/imports

7. **Build**

   - Optional compilation
   - Generate Makefiles
   - Link runtime libraries

Core Abstractions
-----------------

LanguageBackend
~~~~~~~~~~~~~~~

Base class for all backends::

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

AbstractEmitter
~~~~~~~~~~~~~~~

Generates target language code::

   class AbstractEmitter(ABC):
       @abstractmethod
       def emit_function(self, node: ast.FunctionDef) -> str:
           """Emit a function definition"""

       @abstractmethod
       def emit_class(self, node: ast.ClassDef) -> str:
           """Emit a class definition"""

AbstractFactory
~~~~~~~~~~~~~~~

Creates backend-specific objects::

   class AbstractFactory(ABC):
       @abstractmethod
       def create_type_mapper(self) -> TypeMapper:
           """Create type mapper"""

       @abstractmethod
       def create_builder(self) -> AbstractBuilder:
           """Create builder"""

Backend Architecture
--------------------

Each backend follows this structure::

   backends/
     <language>/
       __init__.py          # Backend registration
       backend.py           # LanguageBackend implementation
       emitter.py           # Code emitter
       factory.py           # Object factory
       builder.py           # Build system
       type_mapper.py       # Type mapping
       type_inference.py    # Type inference (optional)
       runtime/             # Runtime library
         *.c / *.rs / etc.  # Runtime source files

Frontend Architecture
---------------------

Intelligence Layer
~~~~~~~~~~~~~~~~~~

The frontend uses a pluggable intelligence layer::

   IntelligencePipeline
     ├── Analyzers (BaseAnalyzer)
     │   ├── ASTAnalyzer
     │   └── TypeAnalyzer
     ├── Optimizers (BaseOptimizer)
     │   └── PythonOptimizer
     └── Verifiers (BaseVerifier)
         ├── BoundsProver
         ├── TheoremProver
         └── CorrectnessProver

Analysis Context
~~~~~~~~~~~~~~~~

Shared context for all analyses::

   @dataclass
   class AnalysisContext:
       source_code: str
       ast_node: ast.AST
       analysis_result: Optional[AnalysisResult]
       analysis_level: AnalysisLevel
       optimization_level: OptimizationLevel

Design Patterns
---------------

Visitor Pattern
~~~~~~~~~~~~~~~

Used for statement conversion in Haskell backend::

   class HaskellStatementVisitor(ABC):
       @abstractmethod
       def visit_assign(self, node: ast.Assign) -> str: ...

       @abstractmethod
       def visit_if(self, node: ast.If) -> str: ...

   class MainFunctionVisitor(HaskellStatementVisitor):
       """IO-based visitor for main()"""

   class PureFunctionVisitor(HaskellStatementVisitor):
       """Pure functional visitor"""

Strategy Pattern
~~~~~~~~~~~~~~~~

Used for type inference and loop conversion::

   class TypeInferenceStrategy(ABC):
       @abstractmethod
       def infer(self, node: ast.AST) -> Optional[str]: ...

   class ConstantInferenceStrategy(TypeInferenceStrategy):
       """Infer from literal values"""

   class BinOpInferenceStrategy(TypeInferenceStrategy):
       """Infer from binary operations"""

Shared across C++, Rust, Go backends (30% code reuse).

Factory Pattern
~~~~~~~~~~~~~~~

Creates backend-specific components::

   class CFactory(AbstractFactory):
       def create_emitter(self) -> CEmitter:
           return CEmitter()

       def create_builder(self) -> CMakeBuilder:
           return CMakeBuilder()

Type System
-----------

Type Inference
~~~~~~~~~~~~~~

Multi-pass type inference for complex containers::

   # Pass 1: Literals and annotations
   x: int = 42  # Known from annotation

   # Pass 2: Binary operations
   y = x + 1    # Inferred as int

   # Pass 3: Container operations
   arr = [1, 2, 3]        # Inferred as list[int]
   nested = [[1], [2]]    # Inferred as list[list[int]]

   # Pass 4: Function calls
   counts = {}
   counts[word] = 1       # Inferred as dict[str, int]

Type Mapping
~~~~~~~~~~~~

Python types map to backend types::

   Python          C++              Rust             Go
   ------          ---              ----             --
   int             int              i32              int
   float           double           f64              float64
   str             std::string      String           string
   list[int]       vector<int>      Vec<i32>         []int
   dict[str,int]   map<str,int>     HashMap<...>     map[string]int
   set[int]        set<int>         HashSet<i32>     map[int]bool

Runtime Libraries
-----------------

Each backend includes a self-contained runtime:

**C Runtime** (~2,500 lines)

- STC container templates
- File I/O helpers
- String utilities
- Math functions

**C++ Runtime** (357 lines, header-only)

- STL container wrappers
- String helpers
- File I/O utilities

**Rust Runtime** (304 lines)

- Collection helpers
- File I/O
- String utilities
- Pure std library

**Go Runtime** (413 lines)

- Reflection-based comprehensions
- File utilities
- Pure std library

**Haskell Runtime** (214 lines)

- List helpers
- Map/Set utilities
- Pure std library

**OCaml Runtime** (216 lines)

- Mutable references
- Association lists
- Pure std library

Formal Verification
-------------------

Z3 Integration
~~~~~~~~~~~~~~

Optional Z3-based verification::

   BoundsProver
     ├── verify_memory_safety()
     │   └── Z3FormulaGenerator
     │       ├── create_array_access_formula()
     │       ├── create_loop_bounds_formula()
     │       └── create_range_formula()
     └── generate_counterexample()

Verification Pipeline::

   1. Extract loop bounds and array accesses
   2. Generate Z3 formulas
   3. Check satisfiability
   4. Return proof or counterexample

Code Quality
------------

**Metrics**

- 961 tests (100% passing)
- Strict mypy type checking
- 2.93% code duplication
- 79% complexity reduction (design patterns)

**Testing Strategy**

- Unit tests (individual functions)
- Integration tests (full pipeline)
- Compilation tests (generated code)
- Benchmark tests (7 real-world examples)

Performance
-----------

**Pipeline Performance**

- Small files (<100 LOC): <100ms
- Medium files (100-1000 LOC): 100-500ms
- Large files (1000-5000 LOC): 500-2000ms

**Backend Performance**

Compilation times vary by backend:

- Go: 63ms (fastest)
- OCaml: 209ms
- C++: 422ms
- Haskell: 513ms
- C: 658ms
- Rust: ~1500ms

Extensibility
-------------

Adding Features
~~~~~~~~~~~~~~~

1. Add to frontend analysis
2. Update AST analyzer
3. Implement in each backend
4. Add tests
5. Update documentation

Adding Backends
~~~~~~~~~~~~~~~

1. Implement ``LanguageBackend``
2. Create emitter, factory, builder
3. Implement runtime library
4. Write comprehensive tests
5. Run benchmark suite
6. Update documentation

Future Enhancements
-------------------

Planned features:

- Exception handling (try/except)
- Context managers (with statement)
- Generators (yield)
- Async/await
- Additional verification properties
- IDE integration (LSP)
- Performance profiling

See ``PRODUCTION_ROADMAP.md`` for detailed roadmap.
