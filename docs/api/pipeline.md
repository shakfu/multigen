# Pipeline API

The pipeline module provides the main entry point for code conversion.

## MultiGenPipeline

::: multigen.pipeline.MultiGenPipeline

## PipelineConfig

::: multigen.pipeline.PipelineConfig

## PipelineResult

::: multigen.pipeline.PipelineResult

## Example Usage

Basic conversion:

```python
from multigen.pipeline import MultiGenPipeline, PipelineConfig

config = PipelineConfig(target_language="c")
pipeline = MultiGenPipeline(config=config)
result = pipeline.convert("example.py")

if result.success:
    print("Conversion successful")
    for path in result.output_files:
        print(f"Generated: {path}")
else:
    print("Errors:", result.errors)
```

With verification:

```python
config = PipelineConfig(
    target_language="rust",
    enable_formal_verification=True,
    strict_verification=True,
)
pipeline = MultiGenPipeline(config=config)
result = pipeline.convert("example.py")
```

## Pipeline Phases

The pipeline executes in 7 phases:

1. **Validation**: Parse and validate Python AST
2. **Analysis**: Analyze types, dependencies, control flow
3. **Python Optimization**: Optimize Python AST
4. **Mapping**: Map Python constructs to target language
5. **Target Optimization**: Optimize target language constructs
6. **Generation**: Generate target language code
7. **Build**: Optionally compile/build the generated code

## See Also

- [Backend API](backends.md) -- Backend-specific APIs
- [Verification API](verification.md) -- Verification APIs
- [Frontend API](frontend.md) -- Frontend analysis APIs
