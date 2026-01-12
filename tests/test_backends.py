"""Tests for MultiGen backend system."""

import pytest
from multigen.backends.registry import registry
from multigen.backends.base import LanguageBackend
from multigen.backends.c.backend import CBackend
from multigen.backends.rust.backend import RustBackend
from multigen.backends.go.backend import GoBackend
from multigen.backends.cpp.backend import CppBackend


class TestBackendRegistry:
    """Test the backend registry system."""

    def test_registry_lists_available_backends(self):
        """Test that registry lists available backends."""
        backends = registry.list_backends()

        # At minimum, we should have C backend
        assert "c" in backends

        # Check if other backends are available
        expected_backends = {"c", "rust", "go", "cpp"}
        available_backends = set(backends)

        # At least C should be available
        assert "c" in available_backends

        # Other backends should be available if implemented
        for backend in expected_backends:
            if backend in available_backends:
                assert registry.has_backend(backend)

    def test_get_backend_instances(self):
        """Test getting backend instances."""
        # Test C backend
        c_backend = registry.get_backend("c")
        assert isinstance(c_backend, CBackend)
        assert c_backend.get_name() == "c"
        assert c_backend.get_file_extension() == ".c"

        # Test other backends if available
        if registry.has_backend("rust"):
            rust_backend = registry.get_backend("rust")
            assert isinstance(rust_backend, RustBackend)
            assert rust_backend.get_name() == "rust"
            assert rust_backend.get_file_extension() == ".rs"

        if registry.has_backend("go"):
            go_backend = registry.get_backend("go")
            assert isinstance(go_backend, GoBackend)
            assert go_backend.get_name() == "go"
            assert go_backend.get_file_extension() == ".go"

        if registry.has_backend("cpp"):
            cpp_backend = registry.get_backend("cpp")
            assert isinstance(cpp_backend, CppBackend)
            assert cpp_backend.get_name() == "cpp"
            assert cpp_backend.get_file_extension() == ".cpp"

    def test_unknown_backend_raises_error(self):
        """Test that unknown backend raises appropriate error."""
        with pytest.raises(ValueError, match="Unknown backend: nonexistent"):
            registry.get_backend("nonexistent")


class TestBackendInterfaces:
    """Test backend interface implementations."""

    @pytest.mark.parametrize("backend_name", ["c", "rust", "go", "cpp"])
    def test_backend_implements_interface(self, backend_name):
        """Test that backend properly implements LanguageBackend interface."""
        if not registry.has_backend(backend_name):
            pytest.skip(f"Backend {backend_name} not available")

        backend = registry.get_backend(backend_name)

        # Test LanguageBackend interface
        assert isinstance(backend, LanguageBackend)
        assert isinstance(backend.get_name(), str)
        assert isinstance(backend.get_file_extension(), str)
        assert backend.get_file_extension().startswith(".")

        # Test that backend components can be instantiated
        factory = backend.get_factory()
        emitter = backend.get_emitter()
        builder = backend.get_builder()
        container_system = backend.get_container_system()

        assert factory is not None
        assert emitter is not None
        assert builder is not None
        assert container_system is not None

    @pytest.mark.parametrize("backend_name", ["c", "rust", "go", "cpp"])
    def test_factory_creates_code_elements(self, backend_name):
        """Test that factory can create basic code elements."""
        if not registry.has_backend(backend_name):
            pytest.skip(f"Backend {backend_name} not available")

        backend = registry.get_backend(backend_name)
        factory = backend.get_factory()

        # Test variable creation
        var_decl = factory.create_variable("x", "int", "42")
        assert "x" in var_decl
        assert "int" in var_decl
        assert "42" in var_decl

        # Test function signature creation
        func_sig = factory.create_function_signature("add", [("a", "int"), ("b", "int")], "int")
        assert "add" in func_sig
        assert "a" in func_sig
        assert "b" in func_sig

        # Test comment creation
        comment = factory.create_comment("test comment")
        assert "test comment" in comment

    @pytest.mark.parametrize("backend_name", ["c", "rust", "go", "cpp"])
    def test_type_mapping(self, backend_name):
        """Test Python to target language type mapping."""
        if not registry.has_backend(backend_name):
            pytest.skip(f"Backend {backend_name} not available")

        backend = registry.get_backend(backend_name)
        emitter = backend.get_emitter()

        # Test basic type mappings
        int_type = emitter.map_python_type("int")
        float_type = emitter.map_python_type("float")
        bool_type = emitter.map_python_type("bool")
        str_type = emitter.map_python_type("str")

        # Types should be non-empty strings
        assert isinstance(int_type, str) and int_type
        assert isinstance(float_type, str) and float_type
        assert isinstance(bool_type, str) and bool_type
        assert isinstance(str_type, str) and str_type

        # Types should be different (in most cases)
        types = {int_type, float_type, bool_type, str_type}
        assert len(types) >= 3  # At least 3 different types

    @pytest.mark.parametrize("backend_name", ["c", "rust", "go", "cpp"])
    def test_container_system(self, backend_name):
        """Test container system provides appropriate types."""
        if not registry.has_backend(backend_name):
            pytest.skip(f"Backend {backend_name} not available")

        backend = registry.get_backend(backend_name)
        containers = backend.get_container_system()

        # Test container type generation
        list_type = containers.get_list_type("int")
        dict_type = containers.get_dict_type("string", "int")
        set_type = containers.get_set_type("int")

        assert isinstance(list_type, str) and list_type
        assert isinstance(dict_type, str) and dict_type
        assert isinstance(set_type, str) and set_type

        # Should contain element types
        assert "int" in list_type
        assert "int" in dict_type
        assert "int" in set_type

    @pytest.mark.parametrize("backend_name", ["c", "rust", "go", "cpp"])
    def test_builder_configuration(self, backend_name):
        """Test builder provides build configuration."""
        if not registry.has_backend(backend_name):
            pytest.skip(f"Backend {backend_name} not available")

        backend = registry.get_backend(backend_name)
        builder = backend.get_builder()

        # Test build file generation
        build_content = builder.generate_build_file(["test.ext"], "testproject")
        assert isinstance(build_content, str) and build_content
        # Go uses a fixed module name for consistency
        if backend_name == "go":
            assert "multigenproject" in build_content
        else:
            assert "testproject" in build_content

        # Test build filename
        build_filename = builder.get_build_filename()
        assert isinstance(build_filename, str) and build_filename

        # Test compile flags
        flags = builder.get_compile_flags()
        assert isinstance(flags, list)