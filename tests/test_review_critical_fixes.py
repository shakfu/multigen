"""Regression tests for the critical defects recorded in REVIEW.md.

Each class covers one finding (C-1 .. C-7). The assertions are about emitted
source rather than compiler output so they run without every toolchain
installed; the corresponding programs were compiled and run against CPython
when the fixes were made.
"""

import ast

import pytest

from multigen.backends.base import CompilationResult
from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.converter_utils import (
    escape_string_for_c_family,
    escape_string_for_haskell,
    escape_string_for_ocaml,
    escape_string_for_rust,
)
from multigen.backends.cpp.converter import MultiGenPythonToCppConverter
from multigen.backends.go.converter import MultiGenPythonToGoConverter
from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.ocaml.converter import MultiGenPythonToOCamlConverter
from multigen.backends.rust.converter import MultiGenPythonToRustConverter
from multigen.frontend.base import AnalysisContext
from multigen.frontend.optimizers.compile_time_evaluator import CompileTimeEvaluator
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig


class TestFailedBuildIsNotSuccess:
    """C-1: a failing build must clear the result's success flag."""

    def test_build_failure_marks_result_unsuccessful(self, tmp_path, monkeypatch):
        source = tmp_path / "prog.py"
        source.write_text("def main() -> int:\n    return 0\n")

        config = PipelineConfig(
            target_language="c",
            build_mode=BuildMode.DIRECT,
            output_dir=str(tmp_path / "out"),
        )
        pipeline = MultiGenPipeline(config=config)
        monkeypatch.setattr(pipeline.builder, "compile_direct", lambda *args, **kwargs: False)

        result = pipeline.convert(source)

        assert not result.success
        assert "Direct compilation failed" in result.errors
        assert result.executable_path is None

    def test_build_failure_reports_compiler_output(self, tmp_path, monkeypatch):
        source = tmp_path / "prog.py"
        source.write_text("def main() -> int:\n    return 0\n")

        config = PipelineConfig(target_language="c", build_mode=BuildMode.DIRECT, output_dir=str(tmp_path / "out"))
        pipeline = MultiGenPipeline(config=config)
        failed = CompilationResult(success=False, stderr="prog.c:1: error: boom\n", return_code=1)
        monkeypatch.setattr(pipeline.builder, "_execute", lambda *args: failed)

        result = pipeline.convert(source)

        assert not result.success
        assert "Direct compilation failed:\nprog.c:1: error: boom" in result.errors


class TestStringEscaping:
    """C-2: string literals must be escaped for the target language."""

    QUOTED = 'he said "hi"'

    def test_escape_helpers_cover_control_characters(self):
        # A hex escape would be greedy in C, so control characters use octal.
        assert escape_string_for_c_family("\x01" + "7") == "\\0017"
        assert escape_string_for_rust("\x01") == "\\u{1}"
        # Haskell decimal escapes need \& before a following digit.
        assert escape_string_for_haskell("\x01" + "7") == "\\1\\&7"
        assert escape_string_for_ocaml("\x01") == "\\001"

    @pytest.mark.parametrize(
        ("converter_class", "expected"),
        [
            (MultiGenPythonToCConverter, '"he said \\"hi\\""'),
            (MultiGenPythonToCppConverter, '"he said \\"hi\\""'),
            (MultiGenPythonToGoConverter, '"he said \\"hi\\""'),
            (MultiGenPythonToRustConverter, '"he said \\"hi\\""'),
            (MultiGenPythonToHaskellConverter, '"he said \\"hi\\""'),
            (MultiGenPythonToOCamlConverter, '"he said \\"hi\\""'),
        ],
    )
    def test_quotes_in_literals_are_escaped(self, converter_class, expected):
        python_code = f"def main() -> int:\n    msg: str = {self.QUOTED!r}\n    return 0\n"
        emitted = converter_class().convert_code(python_code)

        assert expected in emitted


class TestGoMapLiterals:
    """C-3: a non-empty dict literal must be valid Go."""

    def setup_method(self):
        self.converter = MultiGenPythonToGoConverter()

    def test_dict_literal_uses_single_braces(self):
        go_code = self.converter.convert_code(
            "def main() -> int:\n    d: dict[str, int] = {'a': 1, 'b': 2}\n    return 0\n"
        )

        assert 'map[string]int{"a": 1, "b": 2}' in go_code
        assert "{{" not in go_code

    def test_set_literal_keeps_its_element_type(self):
        go_code = self.converter.convert_code("def main() -> int:\n    s: set[int] = {1, 2}\n    return 0\n")

        assert "map[int]bool{1: true, 2: true}" in go_code


class TestOCamlLoopsAndRefs:
    """C-4: OCaml must emit real loops and declare its refs."""

    def setup_method(self):
        self.converter = MultiGenPythonToOCamlConverter()

    def test_while_loop_body_is_emitted(self):
        ocaml_code = self.converter.convert_code(
            "def countdown(n: int) -> int:\n"
            "    i: int = n\n"
            "    count: int = 0\n"
            "    while i > 0:\n"
            "        count += i\n"
            "        i = i - 1\n"
            "    return count\n"
        )

        assert "while (!i > 0) do" in ocaml_code
        assert "done" in ocaml_code
        assert "(* while" not in ocaml_code

    def test_mutable_variable_without_annotation_declares_a_ref(self):
        ocaml_code = self.converter.convert_code(
            "def accumulate(n: int) -> int:\n    total = 0\n    for i in range(n):\n        total += i\n    return total\n"
        )

        assert "let total = ref (0) in" in ocaml_code

    def test_function_call_argument_is_parenthesized(self):
        ocaml_code = self.converter.convert_code(
            "def double(n: int) -> int:\n    return n * 2\n\n\ndef main() -> int:\n    print(double(5))\n    return 0\n"
        )

        assert "(double 5)" in ocaml_code


class TestTryExceptControlFlow:
    """C-5: a return inside try must exit the function, not the closure."""

    SOURCE = (
        "def safe(a: int, b: int) -> int:\n"
        "    try:\n"
        "        y: int = a + b\n"
        "        return y\n"
        "    except ValueError:\n"
        "        return -1\n"
    )

    def test_go_carries_the_return_out_of_the_closure(self):
        go_code = MultiGenPythonToGoConverter().convert_code(self.SOURCE)

        # The value leaves the closure through named results, and the binding is
        # hoisted so it is still in scope afterwards.
        assert "var y int" in go_code
        assert "__mgen_returned0 bool)" in go_code
        assert "if __mgen_try_returned0 {" in go_code
        assert "return __mgen_try_value0" in go_code

    def test_rust_carries_the_return_out_of_the_closure(self):
        rust_code = MultiGenPythonToRustConverter().convert_code(self.SOURCE)

        assert "-> Option<i64>" in rust_code
        assert "return Some(y);" in rust_code
        assert "if let Some(__mgen_returned0) = __mgen_try_value0 {" in rust_code

    def test_rust_raise_uses_panic_any(self):
        rust_code = MultiGenPythonToRustConverter().convert_code("def fail() -> int:\n    raise ValueError('bad')\n")

        # panic! only accepts a format literal, and downcast_ref in the handler
        # needs the typed payload panic_any preserves.
        assert "std::panic::panic_any(ValueError::new(" in rust_code
        assert "panic!(" not in rust_code


class TestBoundsProverSoundness:
    """C-6: the bounds prover must not disprove what it cannot model."""

    def _proof(self, code):
        pytest.importorskip("z3")
        from multigen.frontend.verifiers.bounds_prover import BoundsProver

        context = AnalysisContext(source_code=code, ast_node=ast.parse(code), analysis_result=None)
        return BoundsProver().verify_memory_safety(context)

    def test_guarded_access_is_not_reported_unsafe(self):
        proof = self._proof(
            "def head(a: list[int], n: int) -> int:\n    if n <= len(a):\n        return a[0]\n    return 0\n"
        )

        assert proof.is_safe
        assert proof.unsafe_accesses == []

    def test_annotation_subscript_creates_no_memory_region(self):
        proof = self._proof("def size(a: list[int]) -> int:\n    return len(a)\n")

        assert proof.proof_results == []

    def test_provable_violation_is_still_reported(self):
        proof = self._proof("def bad() -> int:\n    a = [1, 2, 3]\n    return a[5]\n")

        assert not proof.is_safe
        assert [access.region.name for access in proof.unsafe_accesses] == ["a"]

    def test_in_bounds_access_is_proved(self):
        proof = self._proof("def good() -> int:\n    a = [1, 2, 3]\n    return a[1]\n")

        assert proof.is_safe
        assert proof.unsafe_accesses == []


class TestCorrectnessProverReportsUndecided:
    """C-6: unmodelled properties are undecided, not violations."""

    def test_correct_algorithm_has_no_failed_properties(self):
        pytest.importorskip("z3")
        from multigen.frontend.verifiers.correctness_prover import CorrectnessProver

        code = (
            "def factorial(n: int) -> int:\n"
            "    result: int = 1\n"
            "    i: int = 1\n"
            "    while i <= n:\n"
            "        result = result * i\n"
            "        i = i + 1\n"
            "    return result\n"
        )
        context = AnalysisContext(source_code=code, ast_node=ast.parse(code), analysis_result=None)
        proof = CorrectnessProver().verify_algorithm_correctness(context)

        assert proof.failed_properties == []


class TestOptimizerReporting:
    """C-7: only transformations that happened may be reported."""

    def test_pipeline_reports_analyses_not_applied_optimizations(self, tmp_path):
        source = tmp_path / "prog.py"
        source.write_text("def main() -> int:\n    for i in range(5):\n        pass\n    return 0\n")
        config = PipelineConfig(
            target_language="c",
            output_dir=str(tmp_path / "out"),
            enable_advanced_analysis=True,
            enable_optimizations=True,
        )

        result = MultiGenPipeline(config=config).convert(source)
        from multigen.pipeline import PipelinePhase

        phase = result.phase_results[PipelinePhase.PYTHON_OPTIMIZATION]

        assert phase.analyses_run
        assert phase.optimizations_applied == []

    def test_loop_analyzer_reports_opportunities_not_transformations(self):
        from multigen.frontend.optimizers.loop_analyzer import LoopAnalyzer

        code = "def f() -> int:\n    total = 0\n    for i in range(5):\n        total += i\n    return total\n"
        context = AnalysisContext(source_code=code, ast_node=ast.parse(code), analysis_result=None)
        result = LoopAnalyzer().optimize(context)

        assert not any(transformation.startswith("Applied ") for transformation in result.transformations)

    def test_constant_is_not_substituted_after_reassignment(self):
        code = "def f(n: int) -> int:\n    x: int = 5\n    x = n * 2\n    return x\n"
        context = AnalysisContext(source_code=code, ast_node=ast.parse(code), analysis_result=None)
        result = CompileTimeEvaluator().optimize(context)

        assert "return x" in ast.unparse(result.optimized_ast)

    def test_single_assignment_constant_is_still_folded(self):
        code = "def g() -> int:\n    y: int = 3\n    return y + 1\n"
        context = AnalysisContext(source_code=code, ast_node=ast.parse(code), analysis_result=None)
        result = CompileTimeEvaluator().optimize(context)

        assert "return 4" in ast.unparse(result.optimized_ast)
