# Supported Python Syntax

MultiGen translates a statically analysable subset of Python. This page is the
contract for that subset: what the frontend accepts, what it reports, and what
it refuses.

Two things decide the outcome for a given construct:

- **The feature rule** says what the construct means and how well the backends
  support it. Rules are declared in `multigen.frontend.subset_validator`.
- **The validation profile** says whether that level of support is good enough.
  `portable` reports weakly supported constructs and continues; `strict-static`
  fails on them.

Check a file against a profile with:

```bash
multigen check --profile strict-static program.py
multigen check --format json program.py
```

The same profile applies during translation, via `--profile` on `convert` and
`build`, or `PipelineConfig(validation_profile=...)`. Validation runs in phase 1,
so a rejected construct never reaches a backend.

## Constructs with no rule

The validator is an allowlist. A construct that no rule classifies is rejected
rather than passed through, because a backend given an unclassified construct
may drop it silently. `async def`, `await`, `global` at module scope, `nonlocal`,
the walrus operator and `del` are rejected on these grounds.

## Type annotations versus values

Some node types mean different things by position. `dict[str, int]` and
`(3, 4)` are both tuples to Python's parser, but the first is type syntax every
backend handles and the second is a runtime value that only the TypeScript
backend builds. Rules that describe runtime constructs are not applied inside
type annotations.

<!-- BEGIN GENERATED: feature-support -->

<!-- Generated from the feature registry. Do not edit by hand:
     run `make docs-syntax` after changing a rule or a profile. -->

### Profiles

| Profile | Description |
|---|---|
| `portable` | The common subset across the supported targets. Reports weakly inferred types and experimental features without failing on them. |
| `strict-static` | The conservative statically translatable subset. Fails on anything whose type could not be established, on any construct that is only experimentally supported, and on generators and exceptions, whose semantics the backends do not fully model. |

A feature is **accepted** when a profile allows it, **warned** when it is
reported but not fatal, and **rejected** when it fails validation.

### Feature support

| Feature | Tier | Declared status | portable | strict-static | Notes |
|---|---|---|---|---|---|
| Arithmetic Operations | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Basic arithmetic and comparison operations |
| Basic Types | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Basic Python types: int, float, bool, str |
| Control Flow | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Basic control flow: if/else, while, for with range/containers, assert statements |
| F-Strings | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | F-string literals for string formatting. Format specs: .Nf, d, x, X, o, e, E, %; No conversion flags (!r, !s, !a) |
| Function Calls | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Static function calls (no eval/exec) |
| Function Definitions | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Type-annotated function definitions. Must have type annotations; No decorators except allowed ones |
| Variable Declarations | 1 - fundamental | FULLY_SUPPORTED | accepted | accepted | Annotated variable declarations |
| Context Managers | 2 - structured | PARTIALLY_SUPPORTED | accepted | accepted | Basic with statement for file I/O (single context manager). Single context manager only; File operations only; Requires 'as' binding |
| Data Classes | 2 - structured | FULLY_SUPPORTED | accepted | accepted | Dataclasses mapped to C structs |
| Dict Items Unpacking | 2 - structured | FULLY_SUPPORTED | accepted | accepted | `for k, v in d.items()` in loops and comprehensions; no tuple value is built |
| Enumerations | 2 - structured | PLANNED | rejected | rejected | Python enums mapped to C enums |
| Exception Handling | 2 - structured | PARTIALLY_SUPPORTED | accepted | accepted | try/except/else/finally for explicitly raised exceptions. Operations do not raise: a ZeroDivisionError or IndexError crashes instead of reaching the handler. No exception chaining (raise ... from ...) |
| Generator Expressions | 2 - structured | FULLY_SUPPORTED | accepted | accepted | Generator expressions normalized to list comprehensions (eager collection) |
| Generator Functions | 2 - structured | PARTIALLY_SUPPORTED | accepted | accepted | Generator functions with yield/yield from (eager collection to list). No .send() or .throw() |
| Lists | 2 - structured | PARTIALLY_SUPPORTED | accepted | accepted | Lists as arrays with size tracking. Fixed size or bounded growth; Homogeneous element types |
| Named Tuples | 2 - structured | FULLY_SUPPORTED | accepted | accepted | NamedTuple classes mapped to C structs |
| Tuples | 2 - structured | NOT_SUPPORTED | rejected | rejected | Tuple values are not translatable; tuples in type annotations are |
| Union Types | 2 - structured | EXPERIMENTAL | warned | rejected | Union types as tagged unions |
| Yield From | 2 - structured | PARTIALLY_SUPPORTED | accepted | accepted | yield from for extending accumulator with iterable (eager collection). Function calls, range(), and variables only; No .send() or .throw() |
| Comprehensions | 3 - advanced | FULLY_SUPPORTED | accepted | accepted | List, dict, and set comprehensions converted to C loops with STC containers. One `for` clause per comprehension |
| Generic Types | 3 - advanced | PARTIALLY_SUPPORTED | accepted | accepted | Generic types via monomorphization (not supported by the LLVM backend) |
| Pattern Matching | 3 - advanced | PLANNED | rejected | rejected | Python 3.10+ match statements |
| Duck Typing | 4 - unsupported | NOT_SUPPORTED | rejected | rejected | Duck typing requires runtime type checks |
| Lambda Functions | 4 - unsupported | NOT_SUPPORTED | rejected | rejected | Lambda functions require function pointer support |
| Metaclasses | 4 - unsupported | NOT_SUPPORTED | rejected | rejected | Metaclasses require runtime introspection |

### Diagnostic identifiers

These are stable and append-only: they appear in `--format json` output
and are safe to filter on.

| Identifier | Meaning |
|---|---|
| `STATIC.ANALYSIS_ERROR` | Analysis error |
| `STATIC.ANALYSIS_WARNING` | Analysis warning |
| `STATIC.BACKEND_UNSUPPORTED` | Backend unsupported |
| `STATIC.CONSTRAINT_VIOLATION` | Constraint violation |
| `STATIC.CONTEXT_MANAGER_BINDING` | Context manager binding |
| `STATIC.EXCEPTION_CHAINING` | Exception chaining |
| `STATIC.EXPERIMENTAL_FEATURE` | Experimental feature |
| `STATIC.FEATURE_NOT_IMPLEMENTED` | Feature not implemented |
| `STATIC.FSTRING_CONVERSION_FLAG` | Fstring conversion flag |
| `STATIC.FSTRING_FORMAT_SPEC` | Fstring format spec |
| `STATIC.LOW_CONFIDENCE_TYPE` | Low confidence type |
| `STATIC.MISSING_RETURN_ANNOTATION` | Missing return annotation |
| `STATIC.MULTIPLE_CONTEXT_MANAGERS` | Multiple context managers |
| `STATIC.SYNTAX_ERROR` | Syntax error |
| `STATIC.UNANNOTATED_PARAMETER` | Unannotated parameter |
| `STATIC.UNKNOWN_TYPE` | Unknown type |
| `STATIC.UNRECOGNIZED_CONSTRUCT` | Unrecognized construct |
| `STATIC.UNSUPPORTED_DECORATOR` | Unsupported decorator |
| `STATIC.UNSUPPORTED_FEATURE` | Unsupported feature |
| `STATIC.UNSUPPORTED_YIELD_FROM` | Unsupported yield from |

Universal constraint checks keep their own codes under the `CONSTRAINT.` prefix.

<!-- END GENERATED: feature-support -->
