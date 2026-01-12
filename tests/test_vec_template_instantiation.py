"""Test instantiation of generic vec_T template."""

import pytest
from pathlib import Path
from multigen.backends.c.template_substitution import TemplateSubstitutionEngine


class TestVecTemplateInstantiation:
    """Test vec_T template instantiation for different types."""

    def setup_method(self):
        """Set up test fixtures."""
        self.engine = TemplateSubstitutionEngine()
        self.template_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "c" / "runtime" / "templates"

    def test_vec_T_h_template_exists(self):
        """Test that vec_T.h.tmpl template file exists."""
        template_path = self.template_dir / "vec_T.h.tmpl"
        assert template_path.exists(), f"Template file not found: {template_path}"

    def test_vec_T_c_template_exists(self):
        """Test that vec_T.c.tmpl template file exists."""
        template_path = self.template_dir / "vec_T.c.tmpl"
        assert template_path.exists(), f"Template file not found: {template_path}"

    def test_instantiate_vec_int_header(self):
        """Test instantiating vec_T.h.tmpl for int type."""
        template_path = self.template_dir / "vec_T.h.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "int")

        # Check basic structure
        assert "typedef struct {" in result
        assert "int* data;" in result
        assert "vec_int" in result
        assert "vec_int_init(void)" in result
        assert "vec_int_push(vec_int* vec, int value)" in result
        assert "vec_int_at(vec_int* vec, size_t index)" in result
        assert "vec_int_drop(vec_int* vec)" in result

        # Check header guard
        assert "#ifndef MGEN_VEC_int_H" in result
        assert "#define MGEN_VEC_int_H" in result

    def test_instantiate_vec_str_header(self):
        """Test instantiating vec_T.h.tmpl for str type."""
        template_path = self.template_dir / "vec_T.h.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "str")

        # Check basic structure
        assert "typedef struct {" in result
        assert "char** data;" in result
        assert "vec_str" in result
        assert "vec_str_init(void)" in result
        assert "vec_str_push(vec_str* vec, const char* value)" in result  # const for pointer types
        assert "vec_str_drop(vec_str* vec)" in result

        # Check header guard
        assert "#ifndef MGEN_VEC_str_H" in result

    def test_instantiate_vec_int_implementation(self):
        """Test instantiating vec_T.c.tmpl for int type."""
        template_path = self.template_dir / "vec_T.c.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "int")

        # Check includes
        assert '#include "multigen_vec_int.h"' in result
        assert '#include "multigen_error_handling.h"' in result

        # Check init function
        assert "vec_int vec_int_init(void)" in result
        assert "malloc(DEFAULT_CAPACITY * sizeof(int))" in result

        # Check push function - should use simple assignment for int
        assert "vec_int_push(vec_int* vec, int value)" in result
        assert "vec->data[vec->size++] = value;" in result
        # Should NOT have strdup for int
        assert "strdup" not in result

        # Check drop function - should NOT free individual elements for int
        assert "vec_int_drop(vec_int* vec)" in result
        assert "free(vec->data);" in result
        # Should not have loop to free elements
        assert result.count("for (size_t i = 0; i < vec->size; i++)") == 0

    def test_instantiate_vec_str_implementation(self):
        """Test instantiating vec_T.c.tmpl for str type."""
        template_path = self.template_dir / "vec_T.c.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "str")

        # Check includes
        assert '#include "multigen_vec_str.h"' in result

        # Check init function
        assert "vec_str vec_str_init(void)" in result
        assert "malloc(DEFAULT_CAPACITY * sizeof(char*))" in result

        # Check push function - should use strdup for strings
        assert "vec_str_push(vec_str* vec, const char* value)" in result
        assert "strdup(value)" in result
        assert "if (value)" in result
        assert "vec->data[vec->size] = NULL;" in result  # NULL handling

        # Check drop function - should free individual strings
        assert "vec_str_drop(vec_str* vec)" in result
        assert "for (size_t i = 0; i < vec->size; i++)" in result
        assert "free(vec->data[i]);" in result
        assert "free(vec->data);" in result

        # Check clear function - should also free strings
        assert "vec_str_clear(vec_str* vec)" in result
        assert result.count("for (size_t i = 0; i < vec->size; i++)") >= 2  # In both clear and drop

    def test_instantiate_vec_float_header(self):
        """Test instantiating vec_T.h.tmpl for float type."""
        template_path = self.template_dir / "vec_T.h.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "float")

        assert "float* data;" in result
        assert "vec_float" in result
        assert "vec_float_push(vec_float* vec, float value)" in result
        assert "#ifndef MGEN_VEC_float_H" in result

    def test_vec_T_no_placeholders_remain(self):
        """Test that all placeholders are substituted."""
        template_path = self.template_dir / "vec_T.c.tmpl"
        template = template_path.read_text()

        result = self.engine.substitute_vec_template(template, "int")

        # Check that no template placeholders remain (except comments)
        # These should all be replaced
        assert "{{T}}" not in result or result.count("{{T}}") == 0
        assert "{{T_SUFFIX}}" not in result or result.count("{{T_SUFFIX}}") == 0
        assert "{{#T_NEEDS_DROP}}" not in result
        assert "{{/T_NEEDS_DROP}}" not in result
        assert "{{#T_NEEDS_COPY}}" not in result
        assert "{{/T_NEEDS_COPY}}" not in result
        assert "{{#T_IS_POINTER}}" not in result
        assert "{{/T_IS_POINTER}}" not in result
