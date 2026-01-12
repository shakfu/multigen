"""Test cases for flow-sensitive type inference."""

import ast

from multigen.frontend.flow_sensitive_inference import FLOW_BOOL, FLOW_FLOAT, FLOW_INT, FlowSensitiveInferencer, TypeUnifier
from multigen.frontend.type_inference import TypeInferenceEngine


class TestTypeUnifier:
    """Test the TypeUnifier class."""

    def test_basic_unification(self):
        """Test basic type unification."""
        unifier = TypeUnifier()

        # Same types unify to themselves
        assert unifier.unify(FLOW_INT, FLOW_INT) == FLOW_INT

        # Unknown types unify to known type
        from multigen.frontend.flow_sensitive_inference import FLOW_UNKNOWN
        assert unifier.unify(FLOW_INT, FLOW_UNKNOWN) == FLOW_INT

        # Numeric coercion
        result = unifier.unify(FLOW_INT, FLOW_FLOAT)
        assert result == FLOW_FLOAT

    def test_bool_int_coercion(self):
        """Test bool-int type coercion."""
        unifier = TypeUnifier()
        result = unifier.unify(FLOW_BOOL, FLOW_INT)
        assert result == FLOW_INT


class TestFlowSensitiveInferencer:
    """Test the FlowSensitiveInferencer class."""

    def setUp(self):
        """Set up test fixtures."""
        self.fallback_engine = TypeInferenceEngine(enable_flow_sensitive=False)
        self.inferencer = FlowSensitiveInferencer(self.fallback_engine)

    def test_parameter_inference_from_usage(self):
        """Test inferring parameter types from usage patterns."""
        self.setUp()

        # Test function with unannotated parameter used in arithmetic
        code = """
def test_func(x, y: int):
    if x > 0:
        result = x + y
    else:
        result = x * 2
    return result
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # Should infer x as int from arithmetic usage
        assert "x" in results
        assert results["x"].confidence > 0.0
        assert results["x"].type_info.name in ["int", "union"]  # May be union with float

    def test_comparison_type_propagation(self):
        """Test type propagation through comparisons."""
        self.setUp()

        code = """
def test_func(a, b):
    if a > b:
        return a + 1
    return b + 2
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # Both parameters should be inferred as compatible numeric types
        assert "a" in results
        assert "b" in results
        assert results["a"].confidence > 0.0
        assert results["b"].confidence > 0.0

    def test_return_type_inference(self):
        """Test return type inference."""
        self.setUp()

        code = """
def test_func(x: int):
    if x > 0:
        return x + 1
    else:
        return x * 2
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # Should infer return type as int
        assert "__return__" in results
        assert results["__return__"].type_info.name == "int"

    def test_flow_sensitive_branching(self):
        """Test flow-sensitive analysis with branching."""
        self.setUp()

        code = """
def test_func(flag: bool):
    if flag:
        result = 42
    else:
        result = 3.14
    return result
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # result should be unified as union or float (depends on unification strategy)
        # At minimum, should have reasonable confidence
        assert "result" in results
        assert results["result"].confidence > 0.0


class TestIntegrationWithExistingEngine:
    """Test integration with existing TypeInferenceEngine."""

    def test_enhanced_analysis_fallback(self):
        """Test that enhanced analysis falls back gracefully."""
        engine = TypeInferenceEngine(enable_flow_sensitive=True)

        # Test with well-formed function
        code = """
def test_func(x: int, y: int) -> int:
    return x + y
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = engine.analyze_function_signature_enhanced(func_node)

        # Should work and provide results
        assert len(results) > 0
        assert "x" in results
        assert "y" in results

    def test_disable_flow_sensitive(self):
        """Test disabling flow-sensitive analysis."""
        engine = TypeInferenceEngine(enable_flow_sensitive=False)

        code = """
def test_func(x: int, y: int) -> int:
    return x + y
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        # Should use regular analysis
        results = engine.analyze_function_signature_enhanced(func_node)
        assert len(results) > 0


class TestLocalVariableInference:
    """Test automatic inference for local variables without annotations."""

    def setUp(self):
        """Set up test fixtures."""
        self.fallback_engine = TypeInferenceEngine(enable_flow_sensitive=True)
        self.inferencer = FlowSensitiveInferencer(self.fallback_engine)

    def test_local_variable_without_annotation_allowed(self):
        """Test that local variables without annotations are allowed to pass analysis."""
        from multigen.frontend.ast_analyzer import ASTAnalyzer

        code = """
def test_func() -> int:
    numbers = []
    numbers.append(10)
    return len(numbers)
"""
        analyzer = ASTAnalyzer()
        result = analyzer.analyze(code)

        # Should not have errors about missing type annotations for local variables
        # (This is the key fix - ast_analyzer now creates placeholders for locals)
        assert result.convertible
        assert "test_func" in result.functions
        assert "numbers" in result.functions["test_func"].local_variables

    def test_infer_from_function_return(self):
        """Test inferring variable type from function return type."""
        self.setUp()

        code = """
def get_number() -> int:
    return 42

def test_func() -> int:
    result = get_number()
    return result
"""
        tree = ast.parse(code)
        # Test the second function
        func_node = tree.body[1]

        results = self.inferencer.analyze_function_flow(func_node)

        # Should infer result as int
        assert "result" in results
        assert results["result"].type_info is not None

    def test_infer_from_literal(self):
        """Test inferring variable type from literal values."""
        self.setUp()

        code = """
def test_func() -> int:
    x = 5
    s = "hello"
    f = 3.14
    b = True
    return x
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # Should infer x as int
        assert "x" in results
        assert results["x"].type_info is not None
        assert results["x"].type_info.name == "int"

        # Should infer s as str
        assert "s" in results
        assert results["s"].type_info is not None
        assert results["s"].type_info.name == "str"

        # Should infer f as float
        assert "f" in results
        assert results["f"].type_info is not None
        assert results["f"].type_info.name == "float"

        # Should infer b as bool
        assert "b" in results
        assert results["b"].type_info is not None
        assert results["b"].type_info.name == "bool"

    def test_infer_from_arithmetic(self):
        """Test inferring variable type from arithmetic operations."""
        self.setUp()

        code = """
def test_func() -> int:
    x = 5
    y = x * 2
    z = x + y
    return z
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # All should be inferred as int
        assert "x" in results
        assert "y" in results
        assert "z" in results
        assert results["x"].type_info.name == "int"
        # y and z should be numeric (int or union with float)
        assert results["y"].type_info is not None
        assert results["z"].type_info is not None

    def test_mixed_inference(self):
        """Test complex case with multiple inference patterns."""
        self.setUp()

        code = """
def test_func() -> int:
    numbers = []
    numbers.append(10)
    numbers.append(20)

    count = len(numbers)
    doubled = count * 2

    return doubled
"""
        tree = ast.parse(code)
        func_node = tree.body[0]

        results = self.inferencer.analyze_function_flow(func_node)

        # Should infer all variables
        assert "numbers" in results
        assert "count" in results
        assert "doubled" in results

        # All should have type info
        assert results["numbers"].type_info is not None
        assert results["count"].type_info is not None
        assert results["doubled"].type_info is not None


class TestEndToEndInference:
    """End-to-end tests using the full MultiGen pipeline."""

    def test_simple_infer_test_file(self):
        """Test the simple_infer_test.py example works end-to-end."""
        import tempfile
        from pathlib import Path
        from multigen.pipeline import MultiGenPipeline, PipelineConfig

        code = """
def simple_test() -> int:
    numbers = []
    numbers.append(10)
    return len(numbers)

def main() -> int:
    result = simple_test()
    print(result)
    return 0
"""
        # Write to temp file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(code)
            temp_path = Path(f.name)

        try:
            # Create pipeline
            config = PipelineConfig(
                target_language="cpp",
                enable_advanced_analysis=True
            )
            pipeline = MultiGenPipeline(config)

            # Should convert without errors
            result = pipeline.convert(temp_path)

            # Should succeed
            assert result.success, f"Conversion failed with errors: {result.errors}"
            assert len(result.errors) == 0

        finally:
            # Clean up
            temp_path.unlink()

    def test_global_variable_still_requires_annotation(self):
        """Test that global variables still require explicit annotations."""
        from multigen.frontend.ast_analyzer import ASTAnalyzer

        code = """
global_var = 10

def test_func() -> int:
    return global_var
"""
        analyzer = ASTAnalyzer()
        result = analyzer.analyze(code)

        # Should have error about global variable
        assert not result.convertible
        assert any("global_var" in error.lower() for error in result.errors)
