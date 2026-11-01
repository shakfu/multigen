"""Structured diagnostics for static validation.

Validation used to report bare strings, which meant callers could print a
problem but not act on one: no stable identity to suppress or test against, no
position to jump to, no machine-readable form. A `Diagnostic` carries the
identity, the position, and the remediation alongside the message.

The string-based `violations` and `warnings` accessors on validation results are
derived from these, so existing callers keep working unchanged.
"""

import ast
import json
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional


class DiagnosticSeverity(Enum):
    """How a diagnostic affects the validity of the analysed source."""

    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


class RuleId:
    """Stable identifiers for validation diagnostics.

    These are part of the public contract: they appear in JSON output and in
    suppression lists, so treat them as append-only. Rename one and you break
    every caller filtering on it.
    """

    SYNTAX_ERROR = "STATIC.SYNTAX_ERROR"
    UNRECOGNIZED_CONSTRUCT = "STATIC.UNRECOGNIZED_CONSTRUCT"
    UNSUPPORTED_FEATURE = "STATIC.UNSUPPORTED_FEATURE"
    FEATURE_NOT_IMPLEMENTED = "STATIC.FEATURE_NOT_IMPLEMENTED"
    CONSTRAINT_VIOLATION = "STATIC.CONSTRAINT_VIOLATION"
    EXPERIMENTAL_FEATURE = "STATIC.EXPERIMENTAL_FEATURE"
    MISSING_RETURN_ANNOTATION = "STATIC.MISSING_RETURN_ANNOTATION"
    UNANNOTATED_PARAMETER = "STATIC.UNANNOTATED_PARAMETER"
    UNSUPPORTED_DECORATOR = "STATIC.UNSUPPORTED_DECORATOR"
    FSTRING_CONVERSION_FLAG = "STATIC.FSTRING_CONVERSION_FLAG"
    FSTRING_FORMAT_SPEC = "STATIC.FSTRING_FORMAT_SPEC"
    EXCEPTION_CHAINING = "STATIC.EXCEPTION_CHAINING"
    EXCEPTION_FORM = "STATIC.EXCEPTION_FORM"
    MULTIPLE_CONTEXT_MANAGERS = "STATIC.MULTIPLE_CONTEXT_MANAGERS"
    CONTEXT_MANAGER_BINDING = "STATIC.CONTEXT_MANAGER_BINDING"
    UNSUPPORTED_YIELD_FROM = "STATIC.UNSUPPORTED_YIELD_FROM"
    # Emitted by the integrated pipeline rather than the subset validator.
    ANALYSIS_ERROR = "STATIC.ANALYSIS_ERROR"
    ANALYSIS_WARNING = "STATIC.ANALYSIS_WARNING"
    UNKNOWN_TYPE = "STATIC.UNKNOWN_TYPE"
    LOW_CONFIDENCE_TYPE = "STATIC.LOW_CONFIDENCE_TYPE"
    BACKEND_UNSUPPORTED = "STATIC.BACKEND_UNSUPPORTED"
    # Universal constraint checks keep their own identifiers under a namespace.
    CONSTRAINT_PREFIX = "CONSTRAINT."


@dataclass(frozen=True)
class SourceSpan:
    """A region of source, one-indexed by line and zero-indexed by column."""

    line: int
    column: int = 0
    end_line: Optional[int] = None
    end_column: Optional[int] = None

    @classmethod
    def from_node(cls, node: ast.AST) -> Optional["SourceSpan"]:
        """Build a span from an AST node, or None if it carries no position."""
        line = getattr(node, "lineno", None)
        if line is None:
            return None
        return cls(
            line=line,
            column=getattr(node, "col_offset", 0),
            end_line=getattr(node, "end_lineno", None),
            end_column=getattr(node, "end_col_offset", None),
        )

    def __str__(self) -> str:
        """Render as `line:column`, the form editors and tooling expect."""
        return f"{self.line}:{self.column}"


@dataclass(frozen=True)
class Diagnostic:
    """A single validation finding."""

    rule_id: str
    severity: DiagnosticSeverity
    message: str
    span: Optional[SourceSpan] = None
    node_type: Optional[str] = None
    feature: Optional[str] = None
    remediation: Optional[str] = None
    # Supporting detail such as an inferred type or the offending identifier.
    evidence: dict[str, str] = field(default_factory=dict)

    @property
    def is_error(self) -> bool:
        """Whether this diagnostic invalidates the source."""
        return self.severity is DiagnosticSeverity.ERROR

    def format(self, path: Optional[str] = None) -> str:
        """Render as a conventional compiler diagnostic line."""
        location = path or "<source>"
        if self.span is not None:
            location = f"{location}:{self.span}"
        return f"{location}: {self.severity.value}: [{self.rule_id}] {self.message}"

    def to_dict(self) -> dict[str, Any]:
        """Render as a JSON-serialisable mapping."""
        payload: dict[str, Any] = {
            "rule_id": self.rule_id,
            "severity": self.severity.value,
            "message": self.message,
        }
        if self.span is not None:
            payload["span"] = {
                "line": self.span.line,
                "column": self.span.column,
                "end_line": self.span.end_line,
                "end_column": self.span.end_column,
            }
        for name, value in (
            ("node_type", self.node_type),
            ("feature", self.feature),
            ("remediation", self.remediation),
        ):
            if value is not None:
                payload[name] = value
        if self.evidence:
            payload["evidence"] = dict(self.evidence)
        return payload


def diagnostics_to_json(diagnostics: list[Diagnostic], indent: Optional[int] = 2) -> str:
    """Serialise diagnostics, preserving the order they were reported in."""
    return json.dumps([d.to_dict() for d in diagnostics], indent=indent)
