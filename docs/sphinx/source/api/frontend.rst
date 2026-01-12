Frontend API
============

Frontend analysis components for AST analysis, type inference, and optimization.

AST Analyzer
------------

.. automodule:: multigen.frontend.ast_analyzer
   :members:
   :undoc-members:
   :show-inheritance:

Type Inference
--------------

.. automodule:: multigen.frontend.type_inference
   :members:
   :undoc-members:
   :show-inheritance:

Intelligence Layer
------------------

Base Classes
~~~~~~~~~~~~

.. automodule:: multigen.frontend.base
   :members:
   :undoc-members:
   :show-inheritance:

Analyzers
~~~~~~~~~

.. autoclass:: multigen.frontend.base.BaseAnalyzer
   :members:
   :undoc-members:
   :show-inheritance:

Optimizers
~~~~~~~~~~

.. autoclass:: multigen.frontend.base.BaseOptimizer
   :members:
   :undoc-members:
   :show-inheritance:

Verifiers
~~~~~~~~~

.. autoclass:: multigen.frontend.base.BaseVerifier
   :members:
   :undoc-members:
   :show-inheritance:

Intelligence Pipeline
~~~~~~~~~~~~~~~~~~~~~

.. autoclass:: multigen.frontend.base.IntelligencePipeline
   :members:
   :undoc-members:
   :show-inheritance:

Analysis Context
----------------

.. autoclass:: multigen.frontend.base.AnalysisContext
   :members:
   :undoc-members:
   :show-inheritance:

Analysis Results
----------------

.. autoclass:: multigen.frontend.base.AnalysisReport
   :members:
   :undoc-members:
   :show-inheritance:

.. autoclass:: multigen.frontend.base.OptimizationResult
   :members:
   :undoc-members:
   :show-inheritance:

.. autoclass:: multigen.frontend.base.VerificationResult
   :members:
   :undoc-members:
   :show-inheritance:

Example Usage
-------------

Using the AST analyzer::

   from multigen.frontend.ast_analyzer import ASTAnalyzer
   import ast

   code = "def foo(x: int) -> int: return x * 2"
   tree = ast.parse(code)
   analyzer = ASTAnalyzer()
   result = analyzer.analyze(tree, code)

   print(f"Functions: {result.functions}")
   print(f"Type annotations: {result.type_annotations}")

Using the intelligence pipeline::

   from multigen.frontend.base import (
       IntelligencePipeline,
       AnalysisContext,
   )
   import ast

   pipeline = IntelligencePipeline()
   # Add analyzers, optimizers, verifiers
   # pipeline.add_analyzer(...)
   # pipeline.add_optimizer(...)
   # pipeline.add_verifier(...)

   code = "def example(): pass"
   tree = ast.parse(code)
   context = AnalysisContext(source_code=code, ast_node=tree)
   results = pipeline.process(context)

See Also
--------

- :doc:`verification` - Formal verification APIs
- :doc:`pipeline` - Main pipeline API
