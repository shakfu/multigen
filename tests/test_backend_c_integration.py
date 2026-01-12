"""Integration tests for C backend components (emitter, builder, containers, factory)."""

import tempfile
from pathlib import Path

import pytest

from multigen.backends.c.builder import CBuilder
from multigen.backends.c.containers import CContainerSystem
from multigen.backends.c.converter import MultiGenPythonToCConverter
from multigen.backends.c.emitter import CEmitter
from multigen.backends.c.factory import CFactory


class TestEnhancedCEmitter:
    """Test enhanced C emitter with py2c integration."""

    def setup_method(self):
        """Set up test fixtures."""
        self.emitter = CEmitter()

    def test_emit_module_with_py2c(self):
        """Test module emission using py2c converter."""
        python_code = """
def add(x: int, y: int) -> int:
    return x + y
"""
        c_code = self.emitter.emit_module(python_code)

        assert "int add(int x, int y)" in c_code
        assert '#include "multigen_error_handling.h"' in c_code

    def test_emit_module_fallback(self):
        """Test fallback to basic emission on py2c failure."""
        # This should trigger fallback
        python_code = """
def complex_unsupported():
    # Some unsupported feature that causes py2c to fail
    pass
"""
        c_code = self.emitter.emit_module(python_code)

        # Should contain fallback code
        assert c_code is not None
        assert len(c_code) > 0

    def test_can_use_simple_emission(self):
        """Test simple emission detection."""
        # With runtime support, should prefer sophisticated conversion
        import ast
        func_node = ast.FunctionDef(name="test", args=ast.arguments(posonlyargs=[], args=[], kwonlyargs=[], kw_defaults=[], defaults=[]), body=[], decorator_list=[])
        result = self.emitter.can_use_simple_emission(func_node, {})
        assert isinstance(result, bool)

    def test_py2c_converter_initialization(self):
        """Test that py2c converter is properly initialized."""
        assert self.emitter.py2c_converter is not None
        assert isinstance(self.emitter.py2c_converter, MultiGenPythonToCConverter)


class TestCContainerSystemEnhanced:
    """Test enhanced container system with STC support."""

    def setup_method(self):
        """Set up test fixtures."""
        self.container_system = CContainerSystem()

    def test_stc_container_types(self):
        """Test STC container type generation."""
        list_type = self.container_system.get_list_type("int")
        assert list_type == "vec_int"

        dict_type = self.container_system.get_dict_type("str", "int")
        assert dict_type == "map_str_int"

        set_type = self.container_system.get_set_type("int")
        assert set_type == "set_int"

    def test_container_operations_stc(self):
        """Test STC container operations generation."""
        operations = ["append", "get", "size", "contains"]
        code = self.container_system.generate_container_operations("vec_int", operations)

        assert "vec_int_push" in code
        assert "vec_int_at" in code
        assert "vec_int_size" in code
        assert "MGEN_VEC_AT_SAFE" in code

    def test_container_declarations(self):
        """Test STC container declarations."""
        containers = [("my_list", "int"), ("my_set", "double")]
        declarations = self.container_system.generate_container_declarations(containers)

        assert "#define STC_ENABLED" in declarations
        assert "vec_int" in declarations
        assert "set_double" in declarations
        assert '#include "stc/vec.h"' in declarations

    def test_required_imports_stc(self):
        """Test required imports for STC."""
        imports = self.container_system.get_required_imports()

        assert '#include "multigen_stc_bridge.h"' in imports
        assert '#include "multigen_error_handling.h"' in imports

    def test_type_name_sanitization(self):
        """Test type name sanitization for STC."""
        assert self.container_system._sanitize_type_name("char*") == "str"
        assert self.container_system._sanitize_type_name("const char*") == "cstr"
        assert self.container_system._sanitize_type_name("int") == "int"


class TestCBuilderEnhanced:
    """Test enhanced C builder with runtime integration."""

    def setup_method(self):
        """Set up test fixtures."""
        self.builder = CBuilder()

    def test_runtime_detection(self):
        """Test runtime directory detection."""
        # Runtime should be detected
        assert hasattr(self.builder, "use_runtime")
        assert hasattr(self.builder, "runtime_dir")

    def test_makefile_generation_with_runtime(self):
        """Test Makefile generation with runtime sources."""
        source_files = ["test.c"]
        makefile = self.builder.generate_build_file(source_files, "test_app")

        if self.builder.use_runtime:
            assert "multigen_error_handling.c" in makefile
            assert "multigen_python_ops.c" in makefile
            assert "multigen_memory_ops.c" in makefile
            assert "-I" in makefile  # Include path for runtime

    def test_compile_flags_with_runtime(self):
        """Test compile flags include runtime support."""
        flags = self.builder.get_compile_flags()

        assert "-Wall" in flags
        assert "-Wextra" in flags
        assert "-std=c11" in flags

        if self.builder.use_runtime:
            # Should include runtime include path
            runtime_flag = any("-I" in flag for flag in flags)
            assert runtime_flag

    def test_runtime_sources_detection(self):
        """Test runtime sources detection."""
        sources = self.builder.get_runtime_sources()

        if self.builder.use_runtime:
            assert len(sources) > 0
            source_names = [Path(src).name for src in sources]
            assert "multigen_error_handling.c" in source_names

    def test_runtime_headers_detection(self):
        """Test runtime headers detection."""
        headers = self.builder.get_runtime_headers()

        if self.builder.use_runtime:
            assert len(headers) > 0
            assert "multigen_error_handling.h" in headers


class TestCFactoryEnhanced:
    """Test enhanced C factory with integrated capabilities."""

    def setup_method(self):
        """Set up test fixtures."""
        self.factory = CFactory()

    def test_variable_creation(self):
        """Test variable creation."""
        var_decl = self.factory.create_variable("x", "int", "42")
        assert "int x = 42;" in var_decl

    def test_function_signature_creation(self):
        """Test function signature creation."""
        params = [("x", "int"), ("y", "float")]
        signature = self.factory.create_function_signature("test_func", params, "double")
        assert "double test_func(int x, float y)" in signature

    def test_comment_creation(self):
        """Test comment creation."""
        comment = self.factory.create_comment("Test comment")
        assert "/* Test comment */" in comment

    def test_include_creation(self):
        """Test include statement creation."""
        include = self.factory.create_include("stdio.h")
        assert "#include <stdio.h>" in include

        include = self.factory.create_include('"myheader.h"')
        assert '#include "myheader.h"' in include


class TestIntegrationEnhancedC:
    """Integration tests for enhanced C backend."""

    def test_full_pipeline_enhanced_c(self):
        """Test complete pipeline with enhanced C backend."""
        python_code = """
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)
"""

        emitter = CEmitter()
        c_code = emitter.emit_module(python_code)

        # Should contain proper C function
        assert "int fibonacci(int n)" in c_code
        assert "if (n <= 1)" in c_code or "if ((n <= 1))" in c_code
        assert "return n;" in c_code
        assert "fibonacci((n - 1))" in c_code

    def test_container_usage_integration(self):
        """Test integration with container usage."""
        python_code = """
def use_list():
    my_list = [1, 2, 3]
    return len(my_list)
"""

        emitter = CEmitter()
        c_code = emitter.emit_module(python_code)

        # Should handle containers appropriately
        assert c_code is not None
        assert len(c_code) > 0

    def test_runtime_function_integration(self):
        """Test integration with runtime functions."""
        python_code = """
def use_builtin(x: int) -> int:
    return abs(x)
"""

        emitter = CEmitter()
        c_code = emitter.emit_module(python_code)

        # Should use runtime function
        assert "multigen_abs_int" in c_code or "abs" in c_code

    def test_build_system_integration(self):
        """Test build system integration."""
        with tempfile.TemporaryDirectory() as temp_dir:
            builder = CBuilder()
            source_files = [str(Path(temp_dir) / "test.c")]

            # Create a dummy source file
            Path(source_files[0]).write_text("int main() { return 0; }")

            makefile = builder.generate_build_file(source_files, "test_app")

            # With new makefilegen, sources are collected via wildcard
            assert "$(wildcard $(SRCDIR)/*.c)" in makefile or "test.c" in makefile
            assert "test_app" in makefile
            assert "CC = gcc" in makefile


# Fixtures for testing
@pytest.fixture
def sample_python_functions():
    """Sample Python functions for testing."""
    return {
        "simple_add": """
def add(x: int, y: int) -> int:
    return x + y
""",
        "with_variables": """
def calculate(x: int, y: float) -> float:
    result: float = x * y
    return result
""",
        "with_control_flow": """
def max_value(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b
""",
        "with_loop": """
def sum_to_n(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
    return total
""",
    }


@pytest.mark.integration
def test_enhanced_c_backend_comprehensive(sample_python_functions):
    """Comprehensive test of enhanced C backend."""
    emitter = CEmitter()

    for _name, code in sample_python_functions.items():
        c_code = emitter.emit_module(code)

        # Basic sanity checks
        assert c_code is not None
        assert len(c_code) > 100  # Should be substantial
        assert "#include" in c_code  # Should have includes
        assert "(" in c_code and ")" in c_code  # Should have function syntax
