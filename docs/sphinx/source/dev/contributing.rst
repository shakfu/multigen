Contributing Guide
==================

Thank you for considering contributing to MultiGen!

Development Setup
-----------------

1. Clone the repository::

      git clone https://github.com/shakfu/multigen.git
      cd multigen

2. Install development dependencies::

      uv sync --group dev --group extra

3. Verify installation::

      make test

This runs 961 tests and should complete in ~13 seconds.

Development Workflow
--------------------

1. Create a feature branch::

      git checkout -b feature/your-feature-name

2. Make your changes

3. Run tests::

      make test

4. Type check::

      make type-check

5. Lint and format::

      make lint
      make format

6. Update documentation::

      # Update CHANGELOG.md
      # Update CLAUDE.md if architecture changes
      # Update docs/sphinx/source/ for user-facing changes

7. Commit and push::

      git add .
      git commit -m "Description of changes"
      git push origin feature/your-feature-name

8. Create a pull request on GitHub

Code Quality Standards
-----------------------

**Zero Tolerance for Test Failures**

- ALL 961 tests must pass, no exceptions
- Never weaken tests without significant certainty
- Find and fix root causes, don't ignore failures

**Type Safety**

- Strict mypy type checking (``disallow_untyped_defs``)
- Use Python 3.9+ type annotations (PEP 585)
- No ``# type: ignore`` without good reason

**Code Style**

- Follow PEP 8
- Use ruff for linting
- Use isort for imports
- 120 character line length
- Google-style docstrings

**Testing**

- Write tests for all new functionality
- Use pytest-style tests
- Maintain 100% test pass rate
- Tests should be fast (<1s each)

Architecture Guidelines
-----------------------

**7-Phase Pipeline**

All code generation goes through:

1. Validation
2. Analysis
3. Python Optimization
4. Mapping
5. Target Optimization
6. Generation
7. Build

**Backend Abstractions**

- Backends implement ``LanguageBackend``
- Use ``AbstractEmitter`` for code generation
- Use ``AbstractFactory`` for object creation
- Use ``AbstractBuilder`` for build processes

**Language-Agnostic Design**

- Core pipeline is language-agnostic
- No backend-specific code in core modules
- All language details in backend implementations

Adding a New Backend
--------------------

1. Create backend directory::

      mkdir -p src/multigen/backends/newlang

2. Implement core classes::

      # backend.py
      class NewLangBackend(LanguageBackend):
          ...

      # emitter.py
      class NewLangEmitter(AbstractEmitter):
          ...

      # factory.py
      class NewLangFactory(AbstractFactory):
          ...

3. Create runtime library::

      mkdir -p src/multigen/backends/newlang/runtime
      # Implement runtime helper functions

4. Write tests::

      # tests/test_backend_newlang.py
      class TestNewLangBackend:
          def test_basic_function(self):
              ...

5. Add to benchmark suite::

      # Run all 7 benchmarks
      make benchmark

6. Update documentation::

      # docs/sphinx/source/guide/backends.rst
      # Add NewLang section

See ``CLAUDE.md`` for detailed architecture notes.

Running Benchmarks
------------------

Run all benchmarks::

   make benchmark

Run specific backend::

   uv run multigen convert -t newlang tests/benchmarks/algorithms/fibonacci.py

Generate benchmark report::

   make benchmark-report

Target: 7/7 benchmarks passing (100%)

Testing Guidelines
------------------

**Unit Tests**

Test individual functions and classes::

   def test_function_conversion(self):
       code = "def foo(): pass"
       result = converter.convert(code)
       assert result.success

**Integration Tests**

Test end-to-end pipeline::

   def test_c_code_generation(self, tmp_path):
       result = pipeline.convert(input_file, output_dir=tmp_path)
       assert result.success
       assert (tmp_path / "output.c").exists()

**Compilation Tests**

Verify generated code compiles::

   def test_compiled_output(self, tmp_path):
       pipeline.convert(input_file, output_dir=tmp_path)
       subprocess.run(["gcc", "output.c"], check=True)

**Benchmark Tests**

Use existing benchmarks for regression testing.

Documentation
-------------

Update these files for user-facing changes:

- ``README.md`` - Main project description
- ``CHANGELOG.md`` - Version history
- ``docs/sphinx/source/`` - Sphinx documentation
- ``CLAUDE.md`` - Architecture notes (for developers)

Build documentation::

   cd docs/sphinx
   uv run sphinx-build -b html source build

Pull Request Guidelines
------------------------

**PR Title**

Use conventional commits format::

   feat: Add NewLang backend
   fix: Resolve type inference bug in Rust backend
   docs: Update verification guide
   test: Add integration tests for strict mode

**PR Description**

Include:

- What changed
- Why it changed
- How to test it
- Any breaking changes
- Related issues

**Review Process**

- All tests must pass
- Type checking must pass
- Code review by maintainer
- Documentation updated
- CHANGELOG.md updated

Community
---------

- GitHub Issues: https://github.com/shakfu/multigen/issues
- Discussions: https://github.com/shakfu/multigen/discussions

Code of Conduct
---------------

Be respectful, constructive, and professional in all interactions.

License
-------

MultiGen is MIT licensed. All contributions must be compatible with MIT license.
