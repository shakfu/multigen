"""Tests for template-based container code generation."""

import pytest
from multigen.backends.c.container_codegen import ContainerCodeGenerator


class TestContainerCodeGenTemplate:
    """Test template-based container code generation."""

    def setup_method(self):
        """Set up test fixtures."""
        self.codegen = ContainerCodeGenerator()

    def test_generate_from_template_vec_int(self):
        """Test generating vec_int from template."""
        code = self.codegen.generate_from_template("vec_int")
        assert code is not None
        assert "vec_int" in code
        assert "int* data;" in code
        assert "void vec_int_push(vec_int* vec, int value)" in code
        assert "vec_int_drop" in code
        assert "Generated Container: vec_int" in code

    def test_generate_from_template_vec_float(self):
        """Test generating vec_float from template."""
        code = self.codegen.generate_from_template("vec_float")
        assert code is not None
        assert "vec_float" in code
        assert "float* data;" in code
        assert "void vec_float_push(vec_float* vec, float value)" in code
        assert "vec_float_drop" in code

    def test_generate_from_template_vec_str(self):
        """Test generating vec_str from template with string ownership."""
        code = self.codegen.generate_from_template("vec_str")
        assert code is not None
        assert "vec_str" in code
        assert "char** data;" in code
        assert "void vec_str_push(vec_str* vec, const char* value)" in code
        # Should have strdup for ownership
        assert "strdup" in code
        assert "vec_str_drop" in code

    def test_generate_from_template_map_str_int(self):
        """Test generating map_str_int from template."""
        code = self.codegen.generate_from_template("map_str_int")
        assert code is not None
        assert "map_str_int" in code
        assert "char* key;" in code
        assert "int value;" in code
        assert "void map_str_int_insert" in code
        assert "map_str_int_get" in code
        assert "map_str_int_drop" in code

    def test_generate_from_template_map_int_int(self):
        """Test generating map_int_int from template."""
        code = self.codegen.generate_from_template("map_int_int")
        assert code is not None
        assert "map_int_int" in code
        assert "int key;" in code
        assert "int value;" in code
        assert "void map_int_int_insert" in code

    def test_generate_from_template_set_int(self):
        """Test generating set_int from template."""
        code = self.codegen.generate_from_template("set_int")
        assert code is not None
        assert "set_int" in code
        assert "int value;" in code
        assert "bool set_int_insert" in code
        assert "set_int_contains" in code
        assert "set_int_drop" in code

    def test_generate_from_template_set_str(self):
        """Test generating set_str from template."""
        code = self.codegen.generate_from_template("set_str")
        assert code is not None
        assert "set_str" in code
        assert "char* value;" in code
        assert "bool set_str_insert(set_str* set, const char* value)" in code
        # Should have strdup for ownership
        assert "strdup" in code

    def test_generate_from_template_vec_vec_int_returns_none(self):
        """Test that vec_vec_int returns None (not yet supported by templates)."""
        code = self.codegen.generate_from_template("vec_vec_int")
        assert code is None  # Nested vectors not yet supported by templates

    def test_generate_from_template_invalid_type(self):
        """Test that invalid type returns None."""
        code = self.codegen.generate_from_template("vec_unknown")
        assert code is None

    def test_generate_container_uses_templates(self):
        """Test that generate_container uses templates for supported types."""
        # vec_int should use template
        code = self.codegen.generate_container("vec_int")
        assert code is not None
        assert "parameterized template" in code.lower()

        # vec_vec_int should fall back to hardcoded method
        code = self.codegen.generate_container("vec_vec_int")
        assert code is not None
        # Should NOT have "parameterized template" marker
        # (fallback to old method)

    def test_template_code_has_no_includes(self):
        """Test that generated code has includes stripped."""
        code = self.codegen.generate_from_template("vec_int")
        assert code is not None
        # Includes should be stripped from generated code
        assert "#include" not in code

    def test_template_code_has_no_error_macros(self):
        """Test that generated code has error macros removed."""
        code = self.codegen.generate_from_template("vec_int")
        assert code is not None
        # Error macros should be removed
        assert "MGEN_SET_ERROR" not in code

    def test_template_code_has_no_header_guards(self):
        """Test that generated code has header guards stripped."""
        code = self.codegen.generate_from_template("vec_int")
        assert code is not None
        # Header guards should be stripped (but not #define constants)
        assert "#ifndef" not in code
        assert "MGEN_VEC" not in code  # No header guard defines
        # Note: #define constants like DEFAULT_CAPACITY are OK


class TestTemplateVsHardcoded:
    """Compare template-generated code with hardcoded code."""

    def setup_method(self):
        """Set up test fixtures."""
        self.codegen = ContainerCodeGenerator()

    def test_vec_int_template_vs_hardcoded(self):
        """Test that vec_int template generates similar code to hardcoded."""
        template_code = self.codegen.generate_from_template("vec_int")
        hardcoded_code = self.codegen.generate_vec_int()

        assert template_code is not None
        assert hardcoded_code is not None

        # Both should define vec_int type
        assert "typedef struct" in template_code
        assert "typedef struct" in hardcoded_code

        # Both should have push function
        assert "vec_int_push" in template_code
        assert "vec_int_push" in hardcoded_code

        # Both should have drop function
        assert "vec_int_drop" in template_code
        assert "vec_int_drop" in hardcoded_code

    def test_map_str_int_template_vs_hardcoded(self):
        """Test that map_str_int template generates similar code to hardcoded."""
        template_code = self.codegen.generate_from_template("map_str_int")
        hardcoded_code = self.codegen.generate_str_int_map()

        assert template_code is not None
        assert hardcoded_code is not None

        # Both should have hash map structure
        assert "hash" in template_code.lower()
        assert "hash" in hardcoded_code.lower()

        # Template should have map_str_int naming
        assert "map_str_int_insert" in template_code
        assert "map_str_int_get" in template_code

        # Hardcoded uses str_int_map naming (different convention)
        assert "str_int_map_insert" in hardcoded_code
        assert "str_int_map_get" in hardcoded_code
