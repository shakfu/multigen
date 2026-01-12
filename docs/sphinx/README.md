# MultiGen Sphinx Documentation

This directory contains the Sphinx-based documentation for MultiGen.

## Building Documentation

### Prerequisites

Install development dependencies:

```bash
uv sync --group dev
```

### Build HTML Documentation

```bash
# From project root
make docs

# Or from this directory
make html
```

The built documentation will be in `build/index.html`.

### View Documentation

```bash
# From project root
make docs-serve

# Or open manually
open build/index.html
```

### Clean Build

```bash
# From project root
make docs-clean

# Or from this directory
make clean
```

## Documentation Structure

```text
source/
├── index.rst              # Main index page
├── conf.py                # Sphinx configuration
├── guide/                 # User guides
│   ├── installation.rst
│   ├── quickstart.rst
│   ├── backends.rst
│   └── verification.rst
├── api/                   # API reference
│   ├── pipeline.rst
│   ├── backends.rst
│   ├── frontend.rst
│   └── verification.rst
└── dev/                   # Developer documentation
    ├── contributing.rst
    ├── architecture.rst
    └── testing.rst
```

## Adding Documentation

### New User Guide

1. Create `.rst` file in `source/guide/`
2. Add to `index.rst` toctree:

   ```rst
   .. toctree::
      :maxdepth: 2
      :caption: User Guide

      guide/your-new-guide
   ```

### New API Reference

1. Create `.rst` file in `source/api/`
2. Use autodoc directives:

   ```rst
   .. automodule:: multigen.your_module
      :members:
      :undoc-members:
      :show-inheritance:
   ```

### Markdown Support

MyST parser is enabled, so you can use `.md` files:

```rst
.. toctree::
   :maxdepth: 2

   guide/your-markdown-file.md
```

## Sphinx Configuration

Key settings in `conf.py`:

- **Theme**: Read the Docs theme (`sphinx_rtd_theme`)
- **Extensions**:
  - `sphinx.ext.autodoc` - Auto-generate API docs from docstrings
  - `sphinx.ext.napoleon` - Support for Google/NumPy style docstrings
  - `sphinx.ext.viewcode` - Links to source code
  - `sphinx.ext.intersphinx` - Links to other documentation
  - `myst_parser` - Markdown support

## Tips

### Docstring Format

Use Google-style docstrings:

```python
def example(arg1: int, arg2: str) -> bool:
    """Short description.

    Longer description with more details.

    Args:
        arg1: First argument description
        arg2: Second argument description

    Returns:
        Return value description

    Raises:
        ValueError: When something is wrong
    """
```

### Cross-References

Link to other docs:

```rst
:doc:`/guide/quickstart`
:ref:`genindex`
```

Link to Python objects:

```rst
:class:`multigen.pipeline.MultiGenPipeline`
:func:`multigen.convert`
```

## Building for Production

For ReadTheDocs or other hosting:

```bash
sphinx-build -b html source build
```

## Troubleshooting

**Import errors**: Ensure `sys.path` includes `src/` in `conf.py`

**Missing modules**: Install with `uv sync --group dev`

**Duplicate warnings**: Normal when same class documented multiple ways

**Build errors**: Run `make clean` then rebuild
