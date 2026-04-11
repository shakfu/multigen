# Frontend API

Frontend analysis components for AST analysis, type inference, and optimization.

## AST Analyzer

::: multigen.frontend.ast_analyzer

## Type Inference

::: multigen.frontend.type_inference

## Intelligence Layer

### Base Classes

::: multigen.frontend.base

## Example Usage

Using the AST analyzer:

```python
from multigen.frontend.ast_analyzer import ASTAnalyzer
import ast

code = "def foo(x: int) -> int: return x * 2"
tree = ast.parse(code)
analyzer = ASTAnalyzer()
result = analyzer.analyze(tree, code)

print(f"Functions: {result.functions}")
print(f"Type annotations: {result.type_annotations}")
```

Using the intelligence pipeline:

```python
from multigen.frontend.base import (
    IntelligencePipeline,
    AnalysisContext,
)
import ast

pipeline = IntelligencePipeline()
code = "def example(): pass"
tree = ast.parse(code)
context = AnalysisContext(source_code=code, ast_node=tree)
results = pipeline.process(context)
```

## See Also

- [Verification API](verification.md) -- Formal verification APIs
- [Pipeline API](pipeline.md) -- Main pipeline API
