Installation
============

Requirements
------------

- Python 3.9 or higher
- pip or uv package manager

Basic Installation
------------------

Install MultiGen using pip::

   pip install multigen

Or using uv::

   uv pip install multigen

With Formal Verification
-------------------------

To enable Z3-based formal verification, install with the z3 extra::

   pip install multigen[z3]

Or::

   uv pip install multigen[z3]

Development Installation
------------------------

Clone the repository and install in development mode::

   git clone https://github.com/shakfu/multigen.git
   cd multigen
   uv sync --group dev

This installs all development dependencies including:

- pytest (testing)
- mypy (type checking)
- ruff (linting)
- sphinx (documentation)

Verify Installation
-------------------

Check that MultiGen is installed correctly::

   multigen --version

Run the test suite::

   make test

Or with uv::

   uv run pytest
