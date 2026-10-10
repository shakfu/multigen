"""Feature statuses must match what the backends actually do.

A status that overstates support lets a construct through to a backend that
drops or mangles it; one that understates support makes the strict profile
reject working code. Both were present before these tests existed.
"""

import pytest

from multigen.backends.registry import registry
from multigen.errors import UnsupportedFeatureError
from multigen.frontend.subset_validator import FeatureStatus, StaticPythonSubsetValidator


def _emit(backend_name: str, source: str) -> str:
    return registry.get_backend(backend_name).get_emitter().emit_module(source)


class TestGenericsAreSupported:
    """list[int] and dict[str, int] map to real container types."""

    SOURCE = "def f(xs: list[int], d: dict[str, int]) -> int:\n    return len(xs) + len(d)\n"

    def test_status_is_not_experimental(self):
        """Experimental made the strict profile reject most of the corpus."""
        rule = StaticPythonSubsetValidator().feature_rules["generics"]

        assert rule.status is FeatureStatus.PARTIALLY_SUPPORTED

    @pytest.mark.parametrize(
        "backend,expected",
        [("c", "vec_int"), ("rust", "Vec<i64>"), ("typescript", "number[]"), ("go", "[]int")],
    )
    def test_backends_emit_real_container_types(self, backend, expected):
        if backend not in registry.list_backends():
            pytest.skip(f"{backend} backend unavailable")

        assert expected in _emit(backend, self.SOURCE)


class TestCRefusesRatherThanEmitsInvalidCode:
    """An unsupported expression must not become a comment in a value position."""

    def test_tuple_value_is_refused(self):
        source = "def f() -> int:\n    p: tuple[int, int] = (3, 4)\n    return p[0]\n"

        with pytest.raises(UnsupportedFeatureError, match="Tuple"):
            _emit("c", source)

    def test_no_unsupported_comment_reaches_generated_c(self):
        """The old fallback produced `/* Unsupported expression ... */` as a value."""
        source = "def f(xs: list[int]) -> int:\n    return len(xs)\n"

        assert "Unsupported expression" not in _emit("c", source)


class TestPlannedFeaturesAreDroppedByBackends:
    """Why PLANNED must reject: the backends do not fail loudly on these."""

    def test_every_backend_drops_enum_members(self):
        source = "from enum import Enum\n\n\nclass Colour(Enum):\n    RED = 0\n    GREEN = 1\n\n\ndef f() -> int:\n    return 0\n"
        dropped = []
        for backend in sorted(registry.list_backends()):
            try:
                if "GREEN" not in _emit(backend, source):
                    dropped.append(backend)
            except Exception:
                # An honest refusal is fine; silent loss is not.
                continue

        assert dropped, "expected the enum audit to still demonstrate silent loss"
        assert StaticPythonSubsetValidator().feature_rules["enums"].status is FeatureStatus.PLANNED


class TestBackendsRefuseRatherThanDrop:
    """Constructs a backend cannot represent must raise, not vanish.

    Each of these previously produced output that omitted the construct while
    the pipeline reported success.
    """

    DATACLASS = "from dataclasses import dataclass\n\n\n@dataclass\nclass Pt:\n    x: int\n    y: int\n\n\ndef f() -> int:\n    return 0\n"
    NAMEDTUPLE = "from typing import NamedTuple\n\n\nclass Pt(NamedTuple):\n    x: int\n    y: int\n\n\ndef f() -> int:\n    return 0\n"
    MATCH = "def f(x: int) -> int:\n    match x:\n        case 1:\n            return 10\n    return 0\n"

    @pytest.mark.parametrize("source", [DATACLASS, NAMEDTUPLE, MATCH])
    def test_llvm_refuses_what_it_cannot_represent(self, source):
        if "llvm" not in registry.list_backends():
            pytest.skip("llvm backend unavailable")

        with pytest.raises(UnsupportedFeatureError):
            _emit("llvm", source)

    @pytest.mark.parametrize("source", [DATACLASS, NAMEDTUPLE])
    def test_ocaml_refuses_classes_whose_fields_it_would_discard(self, source):
        with pytest.raises(UnsupportedFeatureError, match="fields declared in the class body"):
            _emit("ocaml", source)

    def test_ocaml_still_accepts_a_genuinely_empty_class(self):
        """The refusal must key on discarded fields, not on classes generally."""
        assert "type marker = unit" in _emit("ocaml", "class Marker:\n    pass\n\n\ndef f() -> int:\n    return 0\n")

    def test_llvm_still_accepts_global_declarations(self):
        """`global` restates where a name lives; nothing is lost by ignoring it."""
        if "llvm" not in registry.list_backends():
            pytest.skip("llvm backend unavailable")

        source = "counter: int = 0\n\n\ndef bump() -> int:\n    global counter\n    counter = counter + 1\n    return counter\n"
        assert "counter" in _emit("llvm", source)


class TestSettledStrictStaticExclusions:
    """The three constructs the plan left open, settled by measurement.

    See STATIC-PYTHON-PLAN.md. Each exclusion records a divergence from CPython
    that was observed by running generated C, not inferred.
    """

    GENERATOR = (
        "def counter(n: int) -> int:\n    i: int = 0\n    while i < n:\n        yield i\n        i += 1\n"
        "\n\ndef entry() -> int:\n    total: int = 0\n    for v in counter(5):\n        total += v\n    return total\n"
    )
    EXCEPTIONS = "def entry(x: int) -> int:\n    try:\n        return 10 // x\n    except ZeroDivisionError:\n        return -1\n"
    CONTEXT_MANAGER = (
        "def entry() -> int:\n    total: int = 0\n    with open('p') as handle:\n        total = 5\n    return total\n"
    )

    def test_generators_and_yield_from_are_excluded(self):
        from multigen.frontend.static_profile import STRICT_STATIC

        assert {"generators", "yield_from"} <= STRICT_STATIC.rejected_features

    def test_exceptions_are_excluded(self):
        """Operations do not raise, so a handler can silently never fire."""
        from multigen.frontend.static_profile import STRICT_STATIC
        from multigen.frontend.static_validation import StaticValidator

        assert "exceptions" in STRICT_STATIC.rejected_features
        assert not StaticValidator(STRICT_STATIC).validate_code(self.EXCEPTIONS).is_valid

    def test_context_managers_are_kept(self):
        """Measured to execute and agree with CPython, so not excluded."""
        from multigen.frontend.static_profile import PORTABLE, STRICT_STATIC
        from multigen.frontend.static_validation import StaticValidator

        assert "context_managers" not in STRICT_STATIC.rejected_features
        assert StaticValidator(STRICT_STATIC).validate_code(self.CONTEXT_MANAGER).is_valid
        assert StaticValidator(PORTABLE).validate_code(self.CONTEXT_MANAGER).is_valid

    def test_exceptions_remain_usable_under_portable(self):
        """The exclusion is a strict-profile policy, not a global ban."""
        from multigen.frontend.static_profile import PORTABLE
        from multigen.frontend.static_validation import StaticValidator

        assert StaticValidator(PORTABLE).validate_code(self.EXCEPTIONS).is_valid

    def test_try_except_generates_compilable_c(self):
        """A duplicate macro family made every try/except program uncompilable."""
        emitted = _emit("c", self.EXCEPTIONS)

        assert "MGEN_TRY" in emitted
        # The removed family defined MGEN_EXCEPT; the surviving one does not.
        assert "MGEN_EXCEPT" not in emitted

    def test_generator_probe_actually_consumes(self):
        """The probe used to define a generator without iterating one."""
        from multigen.frontend.capability_probes import PROBES

        for key in ("generators", "yield_from"):
            assert "for v in" in PROBES[key].source, f"{key} probe does not consume"

    def test_every_probe_is_a_runnable_program(self):
        """A probe must print an answer, or the run stage has nothing to compare."""
        from multigen.frontend.capability_probes import BASELINE, PROBES

        for key, probe in list(PROBES.items()) + [("baseline", BASELINE)]:
            assert "def main() -> int:" in probe.source, f"{key} has no entry point"
            assert "print(compute())" in probe.source, f"{key} prints no answer"
