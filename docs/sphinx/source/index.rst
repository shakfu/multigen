MultiGen Documentation
==================

**MultiGen** (Multi-Language Generator) translates Python code to C, C++, Rust, Go, Haskell, and OCaml with zero external runtime dependencies.

Features
--------

- **6 Production Backends**: C, C++, Rust, Go, Haskell, OCaml
- **Formal Verification**: Optional Z3-based memory safety proofs
- **Type Inference**: Automatic type detection for containers and functions
- **Zero Dependencies**: Self-contained runtime libraries
- **961 Tests**: Comprehensive test suite with 98% benchmark pass rate

Quick Start
-----------

Installation::

   pip install multigen

Basic Usage::

   # Convert Python to C
   multigen convert -t c example.py

   # Build with Makefile generation
   multigen build -t c example.py -m

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   guide/installation
   guide/quickstart
   guide/backends
   guide/verification

.. toctree::
   :maxdepth: 2
   :caption: API Reference

   api/pipeline
   api/backends
   api/frontend
   api/verification

.. toctree::
   :maxdepth: 1
   :caption: Development

   dev/contributing
   dev/architecture
   dev/testing

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
