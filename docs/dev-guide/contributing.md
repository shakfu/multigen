# Contributing Guide

## Development Setup

1. Clone the repository:

```bash
git clone https://github.com/shakfu/multigen.git
cd multigen
```

2. Install development dependencies:

```bash
uv sync --group dev --group extra
```

3. Verify installation:

```bash
make test
```

This runs 1353 tests and should complete in ~21 seconds.

## Development Workflow

1. Create a feature branch
2. Make your changes
3. Run tests: `make test`
4. Type check: `make typecheck`
5. Lint and format: `make lint` / `make format`
6. Update documentation (CHANGELOG.md, CLAUDE.md, docs/)
7. Create a pull request

## Code Quality Standards

**Zero Tolerance for Test Failures**

- ALL tests must pass, no exceptions
- Never weaken tests without significant certainty
- Find and fix root causes, don't ignore failures

**Type Safety**

- Strict mypy type checking (`disallow_untyped_defs`)
- Use Python 3.9+ type annotations (PEP 585)

**Code Style**

- Follow PEP 8
- Use ruff for linting
- 120 character line length
- Google-style docstrings

## Adding a New Backend

1. Create backend directory:

```bash
mkdir -p src/multigen/backends/newlang
```

2. Implement core classes (`backend.py`, `converter.py`, `factory.py`, `builder.py`)

3. Create runtime library in `runtime/` subdirectory

4. Write tests in `tests/test_backend_newlang_*.py`

5. Run all 7 benchmarks: `make benchmark`

6. Update documentation

See existing backends (C, C++, Rust, Go, Haskell, OCaml, LLVM) for implementation examples.

## Pull Request Guidelines

**PR Title**: Use conventional commits format:

```
feat: Add NewLang backend
fix: Resolve type inference bug in Rust backend
docs: Update verification guide
```

**Requirements**:

- All tests pass
- Type checking passes
- CHANGELOG.md updated
- Documentation updated for user-facing changes

## Community

- GitHub Issues: <https://github.com/shakfu/multigen/issues>
- Discussions: <https://github.com/shakfu/multigen/discussions>

## License

MultiGen is MIT licensed. All contributions must be compatible with MIT license.
