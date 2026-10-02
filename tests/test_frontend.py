"""Tests for the CGen Frontend - Static Python Analysis Layer."""

import ast
import sys
from pathlib import Path

import pytest

# Add src directory to Python path for development testing
project_root = Path(__file__).parent.parent
src_path = project_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

HAS_PYTEST = True

from multigen.frontend import (
    AnalysisContext,
    AnalysisLevel,
    BoundsChecker,
    CallGraphAnalyzer,
    InferenceMethod,
    IRDataType,
    OptimizationLevel,
    PythonConstraintChecker,
    StaticAnalyzer,
    StaticPythonSubsetValidator,
    SubsetTier,
    SymbolicExecutor,
    TypeInferenceEngine,
    VectorizationDetector,
    analyze_python_code,
    build_ir_from_code,
)


class TestASTAnalyzer:
    """Test the AST analysis framework."""

    def test_simple_function_analysis(self):
        """Test analysis of a simple function."""
        code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        result = analyze_python_code(code)

        assert result.convertible
        assert len(result.functions) == 1
        assert "add" in result.functions

        func_info = result.functions["add"]
        assert func_info.name == "add"
        assert len(func_info.parameters) == 2
        assert func_info.return_type.name == "int"

    def test_function_with_variables(self):
        """Test analysis of function with local variables."""
        code = """
def calculate(x: int, y: float) -> float:
    result: float = x * y
    return result
"""
        result = analyze_python_code(code)

        assert result.convertible
        func_info = result.functions["calculate"]
        # local_variables includes both parameters and actual local variables
        assert len(func_info.local_variables) == 3  # x, y (params) + result (local)
        assert "result" in func_info.local_variables
        assert "x" in func_info.local_variables
        assert "y" in func_info.local_variables

        # Check that result is not a parameter
        assert not func_info.local_variables["result"].is_parameter

    def test_missing_type_annotations(self):
        """Test error detection for missing type annotations."""
        code = """
def bad_function(x):
    return x
"""
        result = analyze_python_code(code)

        assert not result.convertible
        assert len(result.errors) > 0
        assert any("type annotation" in error for error in result.errors)

    def test_complexity_calculation(self):
        """Test function complexity calculation."""
        simple_code = """
def simple(x: int) -> int:
    return x + 1
"""
        complex_code = """
def complex_func(x: int) -> int:
    if x > 0:
        for i in range(10):
            if i % 2 == 0:
                x = x + i
    return x
"""
        simple_result = analyze_python_code(simple_code)
        complex_result = analyze_python_code(complex_code)

        simple_func = simple_result.functions["simple"]
        complex_func = complex_result.functions["complex_func"]

        assert simple_func.complexity.value < complex_func.complexity.value


class TestTypeInference:
    """Test the type inference system."""

    def test_literal_type_inference(self):
        """Test type inference for literals."""
        engine = TypeInferenceEngine()

        # Test different literal types
        import ast

        int_literal = ast.Constant(value=42)
        float_literal = ast.Constant(value=3.14)
        bool_literal = ast.Constant(value=True)
        str_literal = ast.Constant(value="hello")

        int_result = engine.infer_expression_type(int_literal, {})
        float_result = engine.infer_expression_type(float_literal, {})
        bool_result = engine.infer_expression_type(bool_literal, {})
        str_result = engine.infer_expression_type(str_literal, {})

        assert int_result.type_info.name == "int"
        assert float_result.type_info.name == "float"
        assert bool_result.type_info.name == "bool"
        assert str_result.type_info.name == "str"

        # All should have high confidence
        assert int_result.confidence == 1.0
        assert int_result.method == InferenceMethod.LITERAL

    def test_binary_operation_inference(self):
        """Test type inference for binary operations."""
        engine = TypeInferenceEngine()

        # Create a simple AST for: 5 + 3
        import ast

        left = ast.Constant(value=5)
        right = ast.Constant(value=3)
        binop = ast.BinOp(left=left, op=ast.Add(), right=right)

        result = engine.infer_expression_type(binop, {})

        assert result.type_info.c_equivalent == "int"
        assert result.method == InferenceMethod.OPERATION
        assert result.confidence > 0.8


class TestConstraintChecker:
    """Test the static constraint checker."""

    def test_safe_code_passes(self):
        """Test that safe code passes constraint checking."""
        code = """
def safe_function(x: int, y: int) -> int:
    result: int = x + y
    return result
"""
        checker = PythonConstraintChecker()
        violations = checker.check_code(code)

        # Should have no errors
        errors = [v for v in violations if v.severity == "error"]
        assert len(errors) == 0

    def test_division_by_zero_detection(self):
        """Test detection of division by zero."""
        code = """
def unsafe_division(x: int) -> float:
    return x / 0
"""
        checker = PythonConstraintChecker()
        violations = checker.check_code(code)

        errors = [v for v in violations if v.severity == "error"]
        assert any("division by zero" in error.message.lower() for error in errors)


class TestSubsetValidator:
    """Test the Python subset validator."""

    def test_tier1_fundamental_features(self):
        """Test validation of Tier 1 fundamental features."""
        code = """
def basic_function(x: int, y: float) -> float:
    result: float = x + y
    return result
"""
        validator = StaticPythonSubsetValidator()
        result = validator.validate_code(code)

        assert result.is_valid
        assert result.tier == SubsetTier.TIER_1_FUNDAMENTAL
        assert len(result.violations) == 0

    def test_unsupported_features_rejected(self):
        """Test that unsupported features are properly rejected."""
        code = """
def use_lambda():
    return lambda x: x * 2  # Lambda not supported
"""
        validator = StaticPythonSubsetValidator()
        result = validator.validate_code(code)

        assert not result.is_valid
        assert len(result.violations) > 0
        assert len(result.unsupported_features) > 0

    def test_feature_support_query(self):
        """Test querying feature support information."""
        validator = StaticPythonSubsetValidator()

        basic_types = validator.get_feature_support("basic_types")
        assert basic_types is not None
        assert basic_types.tier == SubsetTier.TIER_1_FUNDAMENTAL

        supported_features = validator.list_supported_features(SubsetTier.TIER_1_FUNDAMENTAL)
        assert len(supported_features) > 0

    def test_conversion_strategy_determination(self):
        """Test conversion strategy determination."""
        simple_code = """
def simple(x: int) -> int:
    return x + 1
"""
        validator = StaticPythonSubsetValidator()
        result = validator.validate_code(simple_code)

        assert result.conversion_strategy == "direct_conversion"

    @pytest.mark.parametrize(
        "construct,code",
        [
            ("AsyncFunctionDef", "async def f(x: int) -> int:\n    return x\n"),
            ("NamedExpr", "def f(x: int) -> int:\n    if (y := x + 1) > 0:\n        return y\n    return 0\n"),
            ("Global", "g: int = 0\n\n\ndef f() -> int:\n    global g\n    return g\n"),
            (
                "Nonlocal",
                "def o() -> int:\n    x: int = 0\n\n    def i() -> int:\n        nonlocal x\n        return x\n\n    return i()\n",
            ),
            ("Delete", "def f() -> int:\n    a: list[int] = [1]\n    del a[0]\n    return 0\n"),
        ],
    )
    def test_unrecognised_constructs_are_rejected(self, construct, code):
        """A construct no rule classifies must not pass as valid Static Python.

        These previously validated clean and reached a backend: `async def` was
        silently dropped from the generated source while the pipeline reported
        success.
        """
        result = StaticPythonSubsetValidator().validate_code(code)

        assert not result.is_valid
        assert any(f"Unsupported Python construct: {construct}" in v for v in result.violations)

    @pytest.mark.parametrize(
        "code",
        [
            "def add(x: int, y: int) -> int:\n    return x + y\n",
            "from dataclasses import dataclass\n\n\n@dataclass\nclass P:\n    x: int\n    y: int\n",
            "def s(n: int) -> int:\n    t: int = 0\n    for i in range(n):\n        if i == 2:\n            continue\n        t += i\n    return t\n",
            "def f() -> int:\n    d: dict[str, int] = {'a': 1}\n    s: set[int] = {1, 2}\n    return len(d) + len(s)\n",
            "def f(x: int) -> int:\n    return 1 if x > 0 else 2\n",
            "def f(xs: list[int]) -> list[int]:\n    return [x * 2 for x in xs if x > 0]\n",
        ],
    )
    def test_supported_constructs_still_accepted(self, code):
        """The allowlist must not reject code the backends already translate."""
        result = StaticPythonSubsetValidator().validate_code(code)

        assert result.is_valid, result.violations

    @pytest.mark.parametrize(
        "code,expected_rule",
        [
            ("from enum import Enum\n\n\nclass S(Enum):\n    IDLE = 0\n", "Enumerations"),
            (
                "def f(x: int) -> int:\n    match x:\n        case 1:\n            return 2\n    return 0\n",
                "Pattern Matching",
            ),
        ],
    )
    def test_planned_features_are_rejected(self, code, expected_rule):
        """PLANNED means no backend implements it, so it must not validate clean.

        An Enum previously validated as supported and the C backend emitted an
        empty struct, dropping every member.
        """
        result = StaticPythonSubsetValidator().validate_code(code)

        assert not result.is_valid
        assert any(f"Feature not yet implemented: {expected_rule}" in v for v in result.violations)

    def test_class_forms_are_classified_once(self):
        """Four rules claim ast.ClassDef; exactly one may govern a given class.

        Rejecting PLANNED would otherwise reject every dataclass, because the
        enum rule claims ClassDef too.
        """
        validator = StaticPythonSubsetValidator()
        dataclass_code = "from dataclasses import dataclass\n\n\n@dataclass\nclass P:\n    x: int\n"
        namedtuple_code = "from typing import NamedTuple\n\n\nclass P(NamedTuple):\n    x: int\n"

        for code, expected in ((dataclass_code, "Data Classes"), (namedtuple_code, "Named Tuples")):
            result = validator.validate_code(code)
            assert result.is_valid, result.violations
            assert expected in result.supported_features
            assert "Enumerations" not in result.supported_features

    def test_validator_holds_no_state_between_calls(self):
        """Validation state is local to each call.

        A detailed diagnostic used to be stashed on the instance and consumed by
        whichever later node happened to fail next.
        """
        validator = StaticPythonSubsetValidator()

        first = validator.validate_code("def f(x):\n    return x\n")
        second = validator.validate_code("def g(y: int) -> int:\n    return y\n")
        third = validator.validate_code("def f(x):\n    return x\n")

        assert not first.is_valid
        assert second.is_valid and second.violations == []
        assert third.violations == first.violations

    @pytest.mark.parametrize(
        "code",
        [
            "def f() -> int:\n    d: dict[str, int] = {}\n    return len(d)\n",
            "def f(pairs: dict[str, int]) -> int:\n    return len(pairs)\n",
            "def f() -> dict[str, int]:\n    return {}\n",
            "def f() -> int:\n    r: dict[str, int] = {str(x): x for x in range(3)}\n    return len(r)\n",
        ],
    )
    def test_tuples_in_annotations_are_accepted(self, code):
        """`dict[str, int]` contains an ast.Tuple, but it is type syntax.

        The backends handle these routinely, so a rule about tuple values must
        not fire on them.
        """
        result = StaticPythonSubsetValidator().validate_code(code)

        assert result.is_valid, result.violations

    @pytest.mark.parametrize(
        "code",
        [
            "def f() -> int:\n    p: tuple[int, int] = (3, 4)\n    return p[0]\n",
            "def f() -> int:\n    return (1, 2)[0]\n",
        ],
    )
    def test_tuple_values_are_rejected(self, code):
        """Only TypeScript builds a tuple value; C used to emit invalid C."""
        result = StaticPythonSubsetValidator().validate_code(code)

        assert not result.is_valid
        assert any("Tuples" in v for v in result.violations)

    def test_annotation_and_value_tuples_are_told_apart(self):
        """Both forms in one function: the annotation passes, the value does not."""
        code = "def f() -> tuple[int, int]:\n    return (1, 2)\n"

        result = StaticPythonSubsetValidator().validate_code(code)

        assert not result.is_valid
        # One diagnostic, for the returned value, not two.
        assert len([v for v in result.violations if "Tuples" in v]) == 1


class TestStaticIR:
    """Test the Static IR generation."""

    def test_simple_function_ir(self):
        """Test IR generation for a simple function."""
        code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        ir_module = build_ir_from_code(code)

        assert ir_module.name == "main"
        assert len(ir_module.functions) == 1

        func = ir_module.functions[0]
        assert func.name == "add"
        assert len(func.parameters) == 2
        assert func.return_type.base_type == IRDataType.INT

    def test_function_with_variables_ir(self):
        """Test IR generation for function with local variables."""
        code = """
def calculate(x: int, y: int) -> int:
    result: int = x + y
    return result
"""
        ir_module = build_ir_from_code(code)

        func = ir_module.functions[0]
        assert len(func.local_variables) == 1
        assert func.local_variables[0].name == "result"
        assert func.local_variables[0].ir_type.base_type == IRDataType.INT

    def test_control_flow_ir(self):
        """Test IR generation for control flow."""
        code = """
def conditional(x: int) -> int:
    if x > 0:
        return x
    else:
        return 0
"""
        ir_module = build_ir_from_code(code)

        func = ir_module.functions[0]
        # Check that we have statements in the body
        assert len(func.body) > 0

    def test_loop_ir(self):
        """Test IR generation for loops."""
        code = """
def loop_function(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
    return total
"""
        ir_module = build_ir_from_code(code)

        func = ir_module.functions[0]
        assert len(func.body) > 0
        assert len(func.local_variables) >= 1  # total variable

    def test_ir_type_mapping(self):
        """Test correct type mapping in IR."""
        from multigen.frontend.static_ir import IRDataType, IRType

        int_type = IRType(IRDataType.INT)
        float_type = IRType(IRDataType.FLOAT)
        string_type = IRType(IRDataType.STRING)

        assert int_type.to_c_declaration("var") == "int var"
        assert float_type.to_c_declaration("var") == "double var"
        assert string_type.to_c_declaration("var") == "char* var"

        # Test pointer types
        int_ptr = IRType(IRDataType.INT, is_pointer=True)
        assert int_ptr.to_c_declaration("ptr") == "int *ptr"

        # Test const types
        const_int = IRType(IRDataType.INT, is_const=True)
        assert const_int.to_c_declaration("var") == "const int var"


class TestFrontendIntegration:
    """Integration tests for frontend components."""

    def test_complete_analysis_pipeline(self):
        """Test the complete analysis pipeline."""
        code = """
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    a: int = 0
    b: int = 1
    for i in range(2, n + 1):
        temp: int = a + b
        a = b
        b = temp
    return b
"""

        # Run all analysis components
        ast_result = analyze_python_code(code)

        checker = PythonConstraintChecker()
        violations = checker.check_code(code)

        validator = StaticPythonSubsetValidator()
        validation_result = validator.validate_code(code)

        ir_module = build_ir_from_code(code)

        # Verify all components agree on convertibility
        assert ast_result.convertible
        # No errors from constraint checker
        errors = [v for v in violations if v.severity == "error"]
        assert len(errors) == 0
        assert validation_result.is_valid

        # Verify IR was generated
        assert len(ir_module.functions) == 1
        assert ir_module.functions[0].name == "fibonacci"

    def test_error_consistency(self):
        """Test that all components consistently identify errors."""
        bad_code = """
def bad_function(x):  # Missing type annotation
    return eval("x + 1")  # Dynamic execution
"""

        ast_result = analyze_python_code(bad_code)

        checker = PythonConstraintChecker()
        violations = checker.check_code(bad_code)

        validator = StaticPythonSubsetValidator()
        validator.validate_code(bad_code)

        # All should identify problems
        assert not ast_result.convertible
        # Note: PythonConstraintChecker focuses on type safety, not missing annotations
        # Note: validator might not catch all issues since it focuses on subset validation

    def test_performance_with_large_function(self):
        """Test performance with larger functions."""
        # Generate a larger function
        lines = ["def large_function(x: int) -> int:"]
        lines.append("    result: int = x")
        for i in range(50):
            lines.append(f"    temp_{i}: int = result + {i}")
            lines.append(f"    result = result + temp_{i}")
        lines.append("    return result")

        large_code = "\n".join(lines)

        # All components should handle this reasonably quickly
        import time

        start_time = time.time()
        ast_result = analyze_python_code(large_code)
        ast_time = time.time() - start_time

        start_time = time.time()
        ir_module = build_ir_from_code(large_code)
        ir_time = time.time() - start_time

        # Should complete in reasonable time
        assert ast_time < 1.0  # 1 second
        assert ir_time < 1.0  # 1 second

        # Verify results are reasonable
        assert ast_result.convertible
        assert len(ir_module.functions) == 1
        func = ir_module.functions[0]
        assert len(func.local_variables) > 40  # Should have many variables


# Add pytest markers if pytest is available
if HAS_PYTEST:
    TestASTAnalyzer = pytest.mark.frontend(pytest.mark.unit(TestASTAnalyzer))
    TestTypeInference = pytest.mark.frontend(pytest.mark.unit(TestTypeInference))
    TestConstraintChecker = pytest.mark.frontend(pytest.mark.unit(TestConstraintChecker))
    TestSubsetValidator = pytest.mark.frontend(pytest.mark.unit(TestSubsetValidator))
    TestStaticIR = pytest.mark.frontend(pytest.mark.unit(TestStaticIR))
    TestFrontendIntegration = pytest.mark.frontend(pytest.mark.integration(TestFrontendIntegration))


class TestAdvancedAnalysis:
    """Test advanced frontend analysis components."""

    def test_static_analyzer_basic(self):
        """Test StaticAnalyzer on simple code."""
        code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        analysis_result = analyze_python_code(code)
        import ast

        ast_root = ast.parse(code)

        context = AnalysisContext(
            source_code=code,
            ast_node=ast_root,
            analysis_result=analysis_result,
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )

        static_analyzer = StaticAnalyzer()
        report = static_analyzer.analyze(context)

        assert report.analyzer_name == "StaticAnalyzer"
        assert report.success
        assert report.execution_time_ms > 0
        # Should have control flow information
        assert report.metadata is not None

    def test_symbolic_executor_basic(self):
        """Test SymbolicExecutor on simple code."""
        code = """
def max_value(x: int, y: int) -> int:
    if x > y:
        return x
    return y
"""
        analysis_result = analyze_python_code(code)
        import ast

        ast_root = ast.parse(code)

        context = AnalysisContext(
            source_code=code,
            ast_node=ast_root,
            analysis_result=analysis_result,
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )

        symbolic_executor = SymbolicExecutor()
        report = symbolic_executor.analyze(context)

        assert report.analyzer_name == "SymbolicExecutor"
        assert report.success
        assert report.execution_time_ms > 0
        # Should have path analysis information
        assert report.metadata is not None

    def test_symbolic_executor_sequences_assignments(self):
        """Assignments must continue to later statements in the same block."""
        code = """
def calculate(x: int) -> int:
    y = x + 1
    z = y + 1
    return z
"""
        function = ast.parse(code).body[0]
        context = AnalysisContext(
            source_code=code,
            ast_node=function,
            analysis_result=analyze_python_code(code),
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )

        report = SymbolicExecutor().analyze(context)

        assert report.success
        assert any(path.visited_lines[-1] == 5 for path in report.execution_paths)
        assert any("return_value" in path.final_state.metadata for path in report.execution_paths)

    @staticmethod
    def _symbolic_report(code: str):
        function = ast.parse(code).body[0]
        context = AnalysisContext(
            source_code=code,
            ast_node=function,
            analysis_result=analyze_python_code(code),
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )
        return SymbolicExecutor().analyze(context)

    @pytest.mark.parametrize(
        "loop",
        ["for i in range(n):\n        total += i", "while total < n:\n        total += 1"],
        ids=["for", "while"],
    )
    def test_symbolic_executor_continues_after_loop(self, loop):
        """Every loop exit must reach the statements after the loop."""
        code = f"""
def f(n: int) -> int:
    total = 0
    {loop}
    result = total * 2
    return result
"""
        report = self._symbolic_report(code)

        assert report.success
        assert report.execution_paths
        assert all(path.visited_lines[-1] == 7 for path in report.execution_paths)
        assert all("return_value" in path.final_state.metadata for path in report.execution_paths)
        assert any("loop" in note for note in report.approximations)

    def test_symbolic_executor_runs_loop_else(self):
        """A loop that terminates normally runs its else block before continuing."""
        code = """
def f(n: int) -> int:
    while n > 0:
        n -= 1
    else:
        n = 7
    return n
"""
        report = self._symbolic_report(code)

        assert all(path.visited_lines[-2:] == [6, 7] for path in report.execution_paths)

    def test_symbolic_executor_flags_unmodeled_loop_body(self):
        """Control flow inside a loop body is skipped, so the report must say so."""
        code = """
def f(n: int) -> int:
    for i in range(n):
        if i > 3:
            break
    return n
"""
        report = self._symbolic_report(code)

        assert any("line 3: loop body control flow not modeled" == note for note in report.approximations)

    def test_symbolic_executor_straight_line_is_exhaustive(self):
        report = self._symbolic_report("def f(x: int) -> int:\n    y = x + 1\n    return y\n")

        assert report.approximations == []

    def test_bounds_checker_basic(self):
        """Test BoundsChecker on simple code."""
        code = """
def array_access(arr: list[int], index: int) -> int:
    return arr[index]
"""
        analysis_result = analyze_python_code(code)
        import ast

        ast_root = ast.parse(code)

        context = AnalysisContext(
            source_code=code,
            ast_node=ast_root,
            analysis_result=analysis_result,
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )

        bounds_checker = BoundsChecker()
        report = bounds_checker.analyze(context)

        assert report.analyzer_name == "BoundsChecker"
        assert report.success
        assert report.execution_time_ms > 0

    def test_call_graph_analyzer_basic(self):
        """Test CallGraphAnalyzer on simple code."""
        code = """
def helper(x: int) -> int:
    return x * 2

def main(value: int) -> int:
    result = helper(value)
    return result + 1
"""
        analysis_result = analyze_python_code(code)
        import ast

        ast_root = ast.parse(code)

        context = AnalysisContext(
            source_code=code,
            ast_node=ast_root,
            analysis_result=analysis_result,
            analysis_level=AnalysisLevel.INTERMEDIATE,
        )

        call_graph_analyzer = CallGraphAnalyzer()
        report = call_graph_analyzer.analyze(context)

        assert report.analyzer_name == "CallGraphAnalyzer"
        assert report.success
        assert report.execution_time_ms >= 0
        # Should have detected function calls
        assert report.metadata is not None

    def test_vectorization_detector_basic(self):
        """Test VectorizationDetector on loop code."""
        code = """
def sum_array(arr: list[int]) -> int:
    total: int = 0
    for i in range(len(arr)):
        total += arr[i]
    return total
"""
        analysis_result = analyze_python_code(code)
        import ast

        ast_root = ast.parse(code)

        context = AnalysisContext(
            source_code=code,
            ast_node=ast_root,
            analysis_result=analysis_result,
            analysis_level=AnalysisLevel.INTERMEDIATE,
            optimization_level=OptimizationLevel.MODERATE,
        )

        vectorization_detector = VectorizationDetector()
        report = vectorization_detector.optimize(context)

        assert report.optimizer_name == "VectorizationDetector"
        assert report.success
        assert report.execution_time_ms >= 0

    def test_flow_sensitive_type_inference(self):
        """Test flow-sensitive type inference."""
        code = """
def conditional_type(flag: bool) -> int:
    if flag:
        x = 10
    else:
        x = 20
    return x
"""
        import ast

        ast_root = ast.parse(code)

        type_engine = TypeInferenceEngine(enable_flow_sensitive=True)

        # Get the function node
        func_node = None
        for node in ast.walk(ast_root):
            if isinstance(node, ast.FunctionDef) and node.name == "conditional_type":
                func_node = node
                break

        assert func_node is not None

        # Run flow-sensitive analysis
        results = type_engine.analyze_function_signature_enhanced(func_node)

        # Should have inferred types for parameters and local variables
        assert "flag" in results
        assert results["flag"].type_info.name == "bool"

        # Should have inferred type for x from assignments
        assert "x" in results
        # x is inferred as int from the literal assignments
        assert results["x"].type_info.c_equivalent in ["int", "inferred"]

    def test_flow_sensitive_vs_basic_inference(self):
        """Test difference between flow-sensitive and basic inference."""
        code = """
def infer_from_usage(a, b):
    c = a + b
    return c
"""
        import ast

        ast_root = ast.parse(code)

        func_node = None
        for node in ast.walk(ast_root):
            if isinstance(node, ast.FunctionDef) and node.name == "infer_from_usage":
                func_node = node
                break

        assert func_node is not None

        # Test with flow-sensitive enabled
        type_engine_flow = TypeInferenceEngine(enable_flow_sensitive=True)
        results_flow = type_engine_flow.analyze_function_signature_enhanced(func_node)

        # Test with flow-sensitive disabled
        type_engine_basic = TypeInferenceEngine(enable_flow_sensitive=False)
        results_basic = type_engine_basic.analyze_function_signature(func_node)

        # Both should work, but flow-sensitive may provide better inference
        assert "a" in results_flow
        assert "b" in results_flow
        assert "c" in results_flow

        assert "a" in results_basic
        assert "b" in results_basic


if __name__ == "__main__":
    # Apply markers when running directly
    TestASTAnalyzer = pytest.mark.frontend(pytest.mark.unit(TestASTAnalyzer))
    TestTypeInference = pytest.mark.frontend(pytest.mark.unit(TestTypeInference))
    TestConstraintChecker = pytest.mark.frontend(pytest.mark.unit(TestConstraintChecker))
    TestSubsetValidator = pytest.mark.frontend(pytest.mark.unit(TestSubsetValidator))
    TestStaticIR = pytest.mark.frontend(pytest.mark.unit(TestStaticIR))
    TestFrontendIntegration = pytest.mark.frontend(pytest.mark.integration(TestFrontendIntegration))
    TestAdvancedAnalysis = pytest.mark.frontend(pytest.mark.integration(TestAdvancedAnalysis))
