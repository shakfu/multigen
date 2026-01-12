"""Tests for C backend fallback container system."""

import pytest
from multigen.backends.c.containers import CContainerSystem


class TestCContainerSystemFallback:
    """Test fallback container system functionality."""

    def test_stc_availability_check(self):
        """Test STC availability detection."""
        container_system = CContainerSystem()
        is_available = container_system.check_stc_availability()

        # STC should be available in the repository
        assert isinstance(is_available, bool)

    def test_manual_stc_toggle(self):
        """Test manual STC enable/disable."""
        container_system = CContainerSystem()

        # Default should be True
        assert container_system.use_stc is True

        # Manually disable STC
        container_system.set_use_stc(False)
        assert container_system.use_stc is False

        # Re-enable STC
        container_system.set_use_stc(True)
        assert container_system.use_stc is True

    def test_auto_detect_stc(self):
        """Test automatic STC detection."""
        container_system = CContainerSystem()
        container_system.auto_detect_stc()

        # Should detect based on actual availability
        assert isinstance(container_system.use_stc, bool)

    def test_fallback_list_type(self):
        """Test fallback list type generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        list_type = container_system.get_list_type("int")
        assert list_type == "int*"

    def test_fallback_dict_type(self):
        """Test fallback dict type generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        dict_type = container_system.get_dict_type("char*", "int")
        assert "struct" in dict_type
        assert "key" in dict_type
        assert "value" in dict_type

    def test_fallback_set_type(self):
        """Test fallback set type generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        set_type = container_system.get_set_type("int")
        assert set_type == "int*"

    def test_fallback_imports(self):
        """Test fallback required imports."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        imports = container_system.get_required_imports()

        assert any("multigen_error_handling.h" in imp for imp in imports)
        assert any("multigen_memory_ops.h" in imp for imp in imports)
        assert any("multigen_containers_fallback.h" in imp for imp in imports)
        assert not any("multigen_stc_bridge.h" in imp for imp in imports)

    def test_stc_imports(self):
        """Test STC required imports."""
        container_system = CContainerSystem()
        container_system.set_use_stc(True)

        imports = container_system.get_required_imports()

        assert any("multigen_stc_bridge.h" in imp for imp in imports)
        assert any("multigen_error_handling.h" in imp for imp in imports)

    def test_fallback_operations_append(self):
        """Test fallback append operation generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        operations = container_system.generate_container_operations("vec_int", ["append"])

        assert "multigen_dyn_array_append" in operations
        assert "Fallback operations" in operations

    def test_fallback_operations_all(self):
        """Test all fallback operations generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        operations = ["append", "insert", "remove", "get", "set", "size", "clear", "contains"]
        code = container_system.generate_container_operations("vec_int", operations)

        assert "multigen_dyn_array_append" in code
        assert "multigen_dyn_array_insert" in code
        assert "multigen_dyn_array_remove" in code
        assert "multigen_dyn_array_get" in code
        assert "multigen_dyn_array_set" in code
        assert "multigen_dyn_array_size" in code
        assert "multigen_dyn_array_clear" in code
        assert "multigen_dyn_array_contains" in code

    def test_fallback_container_includes(self):
        """Test fallback container includes generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        includes = container_system.generate_container_includes()

        assert "multigen_containers_fallback.h" in includes
        assert "basic fallback" in includes
        assert "STC" not in includes

    def test_stc_container_includes(self):
        """Test STC container includes generation."""
        container_system = CContainerSystem()
        container_system.set_use_stc(True)

        includes = container_system.generate_container_includes()

        assert "multigen_stc_bridge.h" in includes
        assert "STC" in includes

    def test_fallback_container_declarations(self):
        """Test fallback container declarations."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        containers = [("my_vec", "int"), ("my_other_vec", "float")]
        declarations = container_system.generate_container_declarations(containers)

        assert "Basic array-based containers" in declarations
        assert "no declarations needed" in declarations

    def test_stc_container_declarations(self):
        """Test STC container declarations."""
        container_system = CContainerSystem()
        container_system.set_use_stc(True)

        containers = [("my_vec", "int")]
        declarations = container_system.generate_container_declarations(containers)

        assert "STC container template declarations" in declarations
        assert "vec_int" in declarations

    def test_type_name_sanitization(self):
        """Test type name sanitization for container names."""
        container_system = CContainerSystem()

        # Test various type names
        assert container_system._sanitize_type_name("int") == "int"
        assert container_system._sanitize_type_name("char*") == "str"
        assert container_system._sanitize_type_name("const char*") == "cstr"
        assert container_system._sanitize_type_name("float") == "float"
        assert container_system._sanitize_type_name("double") == "double"

    def test_fallback_operations_example_code(self):
        """Test that fallback operations include example usage."""
        container_system = CContainerSystem()
        container_system.set_use_stc(False)

        code = container_system.generate_container_operations("vec_int", ["append"])

        # Check for example usage
        assert "multigen_dyn_array_new" in code
        assert "sizeof(int)" in code
        assert "multigen_dyn_array_free" in code


class TestCFallbackRuntimeIntegration:
    """Test that fallback runtime files exist and are accessible."""

    def test_fallback_header_exists(self):
        """Test that fallback header file exists."""
        from pathlib import Path

        runtime_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime"
        header_file = runtime_dir / "multigen_containers_fallback.h"

        assert header_file.exists(), "Fallback header file should exist"

    def test_fallback_implementation_exists(self):
        """Test that fallback implementation file exists."""
        from pathlib import Path

        runtime_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime"
        impl_file = runtime_dir / "multigen_containers_fallback.c"

        assert impl_file.exists(), "Fallback implementation file should exist"

    def test_fallback_header_content(self):
        """Test that fallback header has expected content."""
        from pathlib import Path

        runtime_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime"
        header_file = runtime_dir / "multigen_containers_fallback.h"

        content = header_file.read_text()

        # Check for key structures and functions
        assert "multigen_dyn_array" in content
        assert "multigen_dyn_array_new" in content
        assert "multigen_dyn_array_append" in content
        assert "multigen_dyn_array_get" in content
        assert "multigen_dyn_array_size" in content

    def test_fallback_implementation_content(self):
        """Test that fallback implementation has expected content."""
        from pathlib import Path

        runtime_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime"
        impl_file = runtime_dir / "multigen_containers_fallback.c"

        content = impl_file.read_text()

        # Check for key function implementations
        assert "multigen_dyn_array_new" in content
        assert "multigen_dyn_array_append" in content
        assert "multigen_dyn_array_reserve" in content
        assert "multigen_dyn_array_contains" in content
        assert "realloc" in content
        assert "memmove" in content
