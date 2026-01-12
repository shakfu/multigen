Testing Guide
=============

MultiGen has a comprehensive test suite with 961 tests covering all aspects of code generation.

Running Tests
-------------

Run all tests::

   make test

Or with uv::

   uv run pytest

Run specific test file::

   uv run pytest tests/test_backend_c.py

Run specific test::

   uv run pytest tests/test_backend_c.py::TestCBackend::test_basic_function

Run with verbose output::

   uv run pytest -v

Run with coverage::

   make test-coverage

Test Organization
-----------------

Tests are organized by category::

   tests/
     test_pipeline.py              # Pipeline tests
     test_backend_c.py             # C backend tests
     test_backend_cpp.py           # C++ backend tests
     test_backend_rust.py          # Rust backend tests
     test_backend_go.py            # Go backend tests
     test_backend_haskell.py       # Haskell backend tests
     test_backend_ocaml.py         # OCaml backend tests
     test_type_inference.py        # Type inference tests
     test_ast_analyzer.py          # AST analyzer tests
     test_formal_verification.py   # Verification tests
     test_strict_verification.py   # Strict mode tests
     benchmarks/                   # Benchmark test cases
       algorithms/                 # Algorithm benchmarks
       data_structures/            # Data structure benchmarks

Test Categories
---------------

Unit Tests
~~~~~~~~~~

Test individual functions and classes::

   class TestTypeInference:
       def test_infer_from_literal(self):
           code = "x = 42"
           result = infer_type(code)
           assert result == "int"

       def test_infer_from_binop(self):
           code = "y = 1 + 2"
           result = infer_type(code)
           assert result == "int"

Integration Tests
~~~~~~~~~~~~~~~~~

Test end-to-end pipeline::

   class TestCBackend:
       def test_c_code_generation(self, tmp_path):
           input_file = tmp_path / "input.py"
           input_file.write_text("def foo(): pass")

           config = PipelineConfig(target_language="c")
           pipeline = MultiGenPipeline(config=config)
           result = pipeline.convert(input_file)

           assert result.success
           assert len(result.output_files) > 0

Compilation Tests
~~~~~~~~~~~~~~~~~

Verify generated code compiles::

   class TestCompilation:
       def test_c_compiles(self, tmp_path):
           # Generate C code
           result = pipeline.convert(input_file, output_dir=tmp_path)

           # Compile with gcc
           output = tmp_path / "output.c"
           subprocess.run(
               ["gcc", "-o", "program", str(output)],
               check=True
           )

           # Run compiled program
           result = subprocess.run(
               ["./program"],
               capture_output=True,
               text=True
           )
           assert result.returncode == 0

Benchmark Tests
~~~~~~~~~~~~~~~

Use real-world examples::

   class TestBenchmarks:
       def test_fibonacci(self):
           """Test recursive fibonacci implementation"""
           result = pipeline.convert("tests/benchmarks/algorithms/fibonacci.py")
           assert result.success

       def test_quicksort(self):
           """Test quicksort implementation"""
           result = pipeline.convert("tests/benchmarks/algorithms/quicksort.py")
           assert result.success

Verification Tests
~~~~~~~~~~~~~~~~~~

Test formal verification::

   class TestFormalVerification:
       def test_safe_array_access(self):
           code = '''
           def sum_array(arr: list[int]) -> int:
               total: int = 0
               for i in range(len(arr)):
                   total += arr[i]
               return total
           '''
           result = verify(code)
           assert result.success
           assert "SAFE" in result.verification_details

       def test_unsafe_array_access(self):
           code = '''
           def unsafe(arr: list[int], n: int) -> int:
               for i in range(n):
                   x = arr[i]  # Unsafe: n might be > len(arr)
               return x
           '''
           result = verify(code)
           assert not result.success

Test Fixtures
-------------

Common fixtures::

   @pytest.fixture
   def tmp_path():
       """Temporary directory for test files"""
       # Provided by pytest

   @pytest.fixture
   def sample_code():
       """Sample Python code for testing"""
       return '''
       def fibonacci(n: int) -> int:
           if n <= 1:
               return n
           return fibonacci(n - 1) + fibonacci(n - 2)
       '''

   @pytest.fixture
   def pipeline():
       """Configured pipeline instance"""
       config = PipelineConfig(target_language="c")
       return MultiGenPipeline(config=config)

Test Markers
------------

Use markers for test categorization::

   @pytest.mark.slow
   def test_large_file_conversion():
       """Test that takes >1s"""

   @pytest.mark.integration
   def test_full_pipeline():
       """Integration test"""

   @pytest.mark.unit
   def test_single_function():
       """Unit test"""

   @pytest.mark.skipif(not Z3_AVAILABLE, reason="Z3 not available")
   def test_formal_verification():
       """Requires Z3"""

Run specific markers::

   # Run only unit tests
   pytest -m unit

   # Skip slow tests
   pytest -m "not slow"

   # Run integration tests
   pytest -m integration

Writing Tests
-------------

Test Template
~~~~~~~~~~~~~

::

   class TestNewFeature:
       """Test suite for new feature"""

       def test_basic_case(self):
           """Test basic functionality"""
           # Arrange
           input_data = "test input"

           # Act
           result = process(input_data)

           # Assert
           assert result.success
           assert result.output == "expected output"

       def test_edge_case(self):
           """Test edge case"""
           # Test edge conditions

       def test_error_handling(self):
           """Test error conditions"""
           with pytest.raises(ValueError):
               process(invalid_input)

Assertions
~~~~~~~~~~

Common assertions::

   # Boolean assertions
   assert result.success
   assert not result.has_errors

   # Equality
   assert result.output == expected
   assert len(result.errors) == 0

   # Membership
   assert "error message" in result.errors
   assert file_path in result.output_files

   # Type checks
   assert isinstance(result, PipelineResult)

   # Exceptions
   with pytest.raises(ValueError) as exc_info:
       invalid_operation()
   assert "expected message" in str(exc_info.value)

Test Coverage
-------------

Generate coverage report::

   make test-coverage

View HTML report::

   open htmlcov/index.html

Coverage goals:

- Overall: >80%
- Critical paths: >95%
- Backends: >90%

Benchmark Suite
---------------

MultiGen includes 7 comprehensive benchmarks:

Algorithms
~~~~~~~~~~

1. **fibonacci.py**: Recursive function calls
2. **quicksort.py**: List operations, recursion
3. **matmul.py**: Nested loops, 2D arrays
4. **wordcount.py**: File I/O, dictionaries, string operations

Data Structures
~~~~~~~~~~~~~~~

5. **list_ops.py**: List comprehensions, operations
6. **dict_ops.py**: Dictionary operations, iteration
7. **set_ops.py**: Set operations, membership

Running Benchmarks
~~~~~~~~~~~~~~~~~~

Run all benchmarks::

   make benchmark

Run specific backend::

   uv run multigen convert -t c tests/benchmarks/algorithms/fibonacci.py
   gcc fibonacci.c -o fibonacci
   ./fibonacci

Generate benchmark report::

   make benchmark-report

Target: 7/7 passing (100%) for production-ready backends

Current Status
~~~~~~~~~~~~~~

- **C**: 7/7 (100%)
- **C++**: 7/7 (100%)
- **Rust**: 7/7 (100%)
- **Go**: 7/7 (100%)
- **OCaml**: 7/7 (100%)
- **Haskell**: 6/7 (86%)

Continuous Integration
----------------------

GitHub Actions workflow::

   .github/workflows/test.yml

Runs on every push:

1. Run all tests
2. Type checking (mypy)
3. Linting (ruff)
4. Coverage report
5. Benchmark validation

Test Quality Standards
----------------------

**Zero Tolerance**

- ALL tests must pass
- No flaky tests
- No disabled tests (unless marked with reason)

**Fast Tests**

- Unit tests: <100ms each
- Integration tests: <1s each
- Full suite: <20s

**Comprehensive Coverage**

- Test happy path
- Test edge cases
- Test error conditions
- Test all backends

Debugging Tests
---------------

Run with verbose output::

   pytest -vv

Show print statements::

   pytest -s

Run single test with debugging::

   pytest tests/test_file.py::test_name -vv -s

Use pytest debugging::

   pytest --pdb  # Drop into debugger on failure

Troubleshooting
---------------

**Import errors**::

   # Ensure PYTHONPATH is set
   export PYTHONPATH=/Users/sa/projects/multigen/src
   pytest

**Z3 not available**::

   # Install Z3
   pip install z3-solver

   # Or skip Z3 tests
   pytest -m "not verification"

**Compilation tests fail**::

   # Ensure compilers are installed
   gcc --version
   rustc --version
   go version

Next Steps
----------

- :doc:`contributing` - Contributing guidelines
- :doc:`architecture` - Understanding architecture
- ``CLAUDE.md`` - Development notes
