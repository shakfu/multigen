"""Tests for the integrated static validation pass."""

import glob
import json

import pytest

from multigen.frontend.ast_analyzer import ASTAnalyzer
from multigen.frontend.diagnostics import DiagnosticSeverity, RuleId
from multigen.frontend.static_validation import StaticValidator


class TestASTAnalyzerReuse:
    """The analyzer is run per validation, so it must not accumulate."""

    def test_state_does_not_leak_between_analyses(self):
        analyzer = ASTAnalyzer()

        first = analyzer.analyze("def f(x: int) -> int:\n    return x\n")
        second = analyzer.analyze("def g(y: int) -> int:\n    return y\n")

        assert list(first.functions) == ["f"]
        assert list(second.functions) == ["g"]
        assert first is not second


class TestStaticValidator:
    """One parse, every analysis, one merged report."""

    def test_accepts_the_translation_corpus(self):
        """Every program the backends translate must validate clean."""
        offenders = {}
        validator = StaticValidator()
        for path in sorted(glob.glob("tests/translation/*.py")):
            report = validator.validate_code(open(path, encoding="utf-8").read())
            if report.errors():
                offenders[path] = [d.message for d in report.errors()]

        assert offenders == {}

    @pytest.mark.parametrize(
        "literal,valid",
        [("[-7, 7]", True), ("[1.5, -2.5]", True), ("[1, 2]", True), ("[1, 'a']", False), ("[True, 1]", False)],
    )
    def test_list_homogeneity_compares_element_types(self, literal, valid):
        """-7 is a UnaryOp node, but it is an int like 7; 1 and 'a' are both Constant nodes."""
        code = f"def f() -> int:\n    a = {literal}\n    return len(a)\n"

        assert StaticValidator().validate_code(code).is_valid is valid

    def test_merges_findings_from_several_analyses(self):
        code = (
            "from enum import Enum\n\n\nclass S(Enum):\n    IDLE = 0\n\n\ndef f(x, y: int) -> int:\n    return x + y\n"
        )

        report = StaticValidator().validate_code(code)

        rule_ids = {d.rule_id for d in report.diagnostics}
        # The subset validator's finding and the AST analyzer's finding.
        assert RuleId.FEATURE_NOT_IMPLEMENTED in rule_ids
        assert RuleId.UNANNOTATED_PARAMETER in rule_ids
        assert RuleId.ANALYSIS_ERROR in rule_ids
        assert not report.is_valid

    def test_method_receivers_need_no_annotation(self):
        """`self` and `cls` take the enclosing class's type."""
        code = (
            "class Counter:\n"
            "    def __init__(self, start: int) -> None:\n"
            "        self.value: int = start\n\n"
            "    def inc(self, by: int) -> None:\n"
            "        self.value += by\n\n"
            "    @classmethod\n"
            "    def zero(cls) -> int:\n"
            "        return 0\n"
        )

        report = StaticValidator().validate_code(code)

        assert report.errors() == []

    @pytest.mark.parametrize(
        "code",
        [
            "def f(self) -> int:\n    return 0\n",
            "class C:\n    @staticmethod\n    def f(x) -> int:\n        return 0\n",
            "class C:\n    def f(self, x) -> int:\n        return 0\n",
        ],
        ids=["free-function", "staticmethod", "second-param"],
    )
    def test_non_receiver_parameters_still_need_annotations(self, code):
        rule_ids = {d.rule_id for d in StaticValidator().validate_code(code).diagnostics}

        assert RuleId.UNANNOTATED_PARAMETER in rule_ids

    @pytest.mark.parametrize(
        "expression,valid",
        [("xs[-1]", False), ("xs[-2]", False), ("xs[0]", True), ("xs[len(xs) - 1]", True)],
    )
    def test_negative_constant_index_is_rejected(self, expression, valid):
        """Every backend read xs[-1] out of range or failed to compile it."""
        code = f"def f(xs: list[int]) -> int:\n    return {expression}\n"

        assert StaticValidator().validate_code(code).is_valid is valid

    def test_located_findings_precede_unlocated_ones(self):
        """A report reads top to bottom; positionless findings trail it."""
        code = "def f(x, y: int) -> int:\n    return x + y\n"

        diagnostics = StaticValidator().validate_code(code).diagnostics

        located = [i for i, d in enumerate(diagnostics) if d.span is not None]
        unlocated = [i for i, d in enumerate(diagnostics) if d.span is None]
        assert not located or not unlocated or max(located) < min(unlocated)

    def test_identical_findings_are_reported_once(self):
        code = "def f(x, y: int) -> int:\n    return x + y\n"

        diagnostics = StaticValidator().validate_code(code).diagnostics

        keys = [(d.rule_id, d.message, d.span) for d in diagnostics]
        assert len(keys) == len(set(keys))

    def test_syntax_error_stops_the_pass(self):
        report = StaticValidator().validate_code("def f(:\n")

        assert not report.is_valid
        assert [d.rule_id for d in report.diagnostics] == [RuleId.SYNTAX_ERROR]

    def test_low_confidence_types_are_policy_not_rule(self):
        """The same source can be acceptable or not, depending on strictness."""
        permissive = StaticValidator(reject_low_confidence=False, confidence_threshold=1.01)
        strict = StaticValidator(reject_low_confidence=True, confidence_threshold=1.01)
        code = "def g(n: int) -> int:\n    total = 0\n    for i in range(n):\n        total += i\n    return total\n"

        lenient_report = permissive.validate_code(code)
        strict_report = strict.validate_code(code)

        weak = [RuleId.LOW_CONFIDENCE_TYPE, RuleId.UNKNOWN_TYPE]
        lenient_weak = [d for d in lenient_report.diagnostics if d.rule_id in weak]
        strict_weak = [d for d in strict_report.diagnostics if d.rule_id in weak]

        assert lenient_weak, "threshold above 1.0 should flag every inferred name"
        assert all(d.severity is DiagnosticSeverity.WARNING for d in lenient_weak)
        assert all(d.severity is DiagnosticSeverity.ERROR for d in strict_weak)
        assert lenient_report.is_valid
        assert not strict_report.is_valid

    def test_constraint_ids_are_namespaced(self):
        """Checker codes like SA001 must not collide with STATIC.* ids."""
        report = StaticValidator().validate_code("def f() -> int:\n    return eval('1')\n")

        for diagnostic in report.diagnostics:
            assert diagnostic.rule_id.startswith(("STATIC.", "CONSTRAINT."))

    def test_report_serialises_to_json(self):
        report = StaticValidator().validate_code("async def f(x: int) -> int:\n    return x\n")

        payload = json.loads(report.to_json())

        assert payload
        assert {"rule_id", "severity", "message"} <= set(payload[0])

    @pytest.mark.parametrize("attribute", ["violations", "warnings"])
    def test_string_views_derive_from_diagnostics(self, attribute):
        report = StaticValidator().validate_code("def f(x) -> int:\n    return x\n")

        expected = [
            d.message for d in report.diagnostics if (d.is_error if attribute == "violations" else not d.is_error)
        ]
        assert getattr(report, attribute) == expected
