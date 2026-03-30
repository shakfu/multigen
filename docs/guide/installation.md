# Installation

## Requirements

- Python 3.9 or higher
- pip or uv package manager

## Basic Installation

Install MultiGen using pip:

```bash
pip install multigen
```

Or using uv:

```bash
uv pip install multigen
```

## Optional Dependencies

### LLVM Backend

For native compilation via LLVM IR:

```bash
pip install multigen[llvm]
```

### Formal Verification

To enable Z3-based formal verification:

```bash
pip install multigen[z3]
```

### All Dependencies

```bash
pip install multigen[all]
```

## Development Installation

Clone the repository and install in development mode:

```bash
git clone https://github.com/shakfu/multigen.git
cd multigen
uv sync --group dev
```

This installs all development dependencies including:

- pytest (testing)
- mypy (type checking)
- ruff (linting)
- mkdocs (documentation)

## Verify Installation

Check that MultiGen is installed correctly:

```bash
multigen --version
```

Run the test suite:

```bash
make test
```

Or with uv:

```bash
uv run pytest
```
