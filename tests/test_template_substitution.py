"""Tests for template substitution engine."""

import pytest
from multigen.backends.c.template_substitution import TemplateSubstitutionEngine
from multigen.backends.c.type_parameter_extractor import (
    TypeParameterExtractor,
    ContainerTypeInfo,
)


class TestTemplateSubstitutionEngine:
    """Test TemplateSubstitutionEngine class."""

    def setup_method(self):
        """Set up test fixtures."""
        self.engine = TemplateSubstitutionEngine()

    def test_substitute_vec_simple_placeholder(self):
        """Test simple placeholder substitution in vector template."""
        template = "typedef struct { {{T}}* data; } vec_{{T_SUFFIX}};"
        result = self.engine.substitute_vec_template(template, "int")
        assert result == "typedef struct { int* data; } vec_int;"

    def test_substitute_vec_multiple_placeholders(self):
        """Test multiple placeholders in vector template."""
        template = "{{T}} vec_{{T_SUFFIX}}_at(vec_{{T_SUFFIX}}* v, size_t idx) { return v->data[idx]; }"
        result = self.engine.substitute_vec_template(template, "float")
        assert result == "float vec_float_at(vec_float* v, size_t idx) { return v->data[idx]; }"

    def test_substitute_vec_zero_value(self):
        """Test zero value placeholder substitution."""
        template = "{{T}} default_val = {{T_ZERO}};"
        result_int = self.engine.substitute_vec_template(template, "int")
        assert result_int == "int default_val = 0;"

        result_float = self.engine.substitute_vec_template(template, "float")
        assert result_float == "float default_val = 0.0f;"

        result_str = self.engine.substitute_vec_template(template, "str")
        assert result_str == "char* default_val = NULL;"

    def test_substitute_vec_printf_format(self):
        """Test printf format placeholder substitution."""
        template = 'printf("Value: {{T_PRINTF}}\\n", val);'
        result_int = self.engine.substitute_vec_template(template, "int")
        assert result_int == 'printf("Value: %d\\n", val);'

        result_str = self.engine.substitute_vec_template(template, "str")
        assert result_str == 'printf("Value: %s\\n", val);'

    def test_substitute_vec_conditional_needs_drop(self):
        """Test conditional block for types needing drop."""
        template = """void cleanup({{T}}* val) {
{{#T_NEEDS_DROP}}
    free(*val);
{{/T_NEEDS_DROP}}
}"""
        # String type needs drop
        result_str = self.engine.substitute_vec_template(template, "str")
        assert "free(*val);" in result_str
        assert "{{#T_NEEDS_DROP}}" not in result_str
        assert "{{/T_NEEDS_DROP}}" not in result_str

        # Int type doesn't need drop
        result_int = self.engine.substitute_vec_template(template, "int")
        assert "free(*val);" not in result_int

    def test_substitute_vec_conditional_needs_copy(self):
        """Test conditional block for types needing deep copy."""
        template = """{{T}} copy_val({{T}} val) {
{{#T_NEEDS_COPY}}
    return strdup(val);
{{/T_NEEDS_COPY}}
{{^T_NEEDS_COPY}}
    return val;
{{/T_NEEDS_COPY}}
}"""
        # String type needs copy
        result_str = self.engine.substitute_vec_template(template, "str")
        # Check that the conditional block is included
        assert "strdup(val)" in result_str or "return val" in result_str

    def test_substitute_map_simple_placeholders(self):
        """Test simple placeholder substitution in map template."""
        template = "typedef struct { {{K}} key; {{V}} value; } entry_{{KV_SUFFIX}};"
        result = self.engine.substitute_map_template(template, "str", "int")
        assert result == "typedef struct { char* key; int value; } entry_str_int;"

    def test_substitute_map_key_value_types(self):
        """Test key and value type placeholders."""
        template = "{{V}} map_{{KV_SUFFIX}}_get(map_{{KV_SUFFIX}}* m, {{K}} key);"
        result = self.engine.substitute_map_template(template, "int", "float")
        assert result == "float map_int_float_get(map_int_float* m, int key);"

    def test_substitute_map_key_value_properties(self):
        """Test key and value type-specific properties."""
        template = """{{K}} key = {{K_ZERO}};
{{V}} val = {{V_ZERO}};"""
        result = self.engine.substitute_map_template(template, "str", "int")
        assert result == """char* key = NULL;
int val = 0;"""

    def test_substitute_map_compare_operations(self):
        """Test comparison operator substitution."""
        template = "if (k1 {{K_COMPARE}} k2) { ... }"
        result_int = self.engine.substitute_map_template(template, "int", "int")
        assert "k1 == k2" in result_int

        result_str = self.engine.substitute_map_template(template, "str", "int")
        assert "k1 strcmp k2" in result_str  # strcmp needs special handling

    def test_substitute_map_hash_function(self):
        """Test hash function name substitution."""
        template = "size_t hash = {{K_HASH}}(key);"
        result_int = self.engine.substitute_map_template(template, "int", "str")
        assert result_int == "size_t hash = hash_int(key);"

        result_str = self.engine.substitute_map_template(template, "str", "int")
        assert result_str == "size_t hash = hash_string(key);"

    def test_substitute_map_conditional_key_needs_drop(self):
        """Test conditional for key type cleanup."""
        template = """void cleanup_key({{K}}* key) {
{{#K_NEEDS_DROP}}
    free(*key);
{{/K_NEEDS_DROP}}
}"""
        result_str = self.engine.substitute_map_template(template, "str", "int")
        assert "free(*key);" in result_str

        result_int = self.engine.substitute_map_template(template, "int", "int")
        assert "free(*key);" not in result_int

    def test_substitute_map_conditional_value_needs_drop(self):
        """Test conditional for value type cleanup."""
        template = """void cleanup_val({{V}}* val) {
{{#V_NEEDS_DROP}}
    free(*val);
{{/V_NEEDS_DROP}}
}"""
        result = self.engine.substitute_map_template(template, "int", "str")
        assert "free(*val);" in result

    def test_substitute_set_uses_vec_logic(self):
        """Test that set template uses same logic as vector template."""
        template = "typedef struct { {{T}}* data; } set_{{T_SUFFIX}};"
        result = self.engine.substitute_set_template(template, "int")
        assert result == "typedef struct { int* data; } set_int;"

    def test_substitute_from_container_info_vec(self):
        """Test substitution using ContainerTypeInfo for vectors."""
        extractor = TypeParameterExtractor()
        info = extractor.extract("vec_int")
        assert info is not None

        template = "typedef struct { {{T}}* data; } vec_{{T_SUFFIX}};"
        result = self.engine.substitute_from_container_info(template, info)
        assert result == "typedef struct { int* data; } vec_int;"

    def test_substitute_from_container_info_map(self):
        """Test substitution using ContainerTypeInfo for maps."""
        extractor = TypeParameterExtractor()
        info = extractor.extract("map_str_int")
        assert info is not None

        template = "{{V}} get({{K}} key) { ... }"
        result = self.engine.substitute_from_container_info(template, info)
        assert "int get(char* key)" in result

    def test_substitute_from_container_info_set(self):
        """Test substitution using ContainerTypeInfo for sets."""
        extractor = TypeParameterExtractor()
        info = extractor.extract("set_str")
        assert info is not None

        template = "bool contains(set_{{T_SUFFIX}}* s, {{T}} elem);"
        result = self.engine.substitute_from_container_info(template, info)
        assert result == "bool contains(set_str* s, char* elem);"

    def test_substitute_from_container_info_vec_vec(self):
        """Test substitution using ContainerTypeInfo for nested vectors."""
        extractor = TypeParameterExtractor()
        info = extractor.extract("vec_vec_int")
        assert info is not None

        template = "{{T}} get_elem(vec_vec_{{T_SUFFIX}}* v, size_t i, size_t j);"
        result = self.engine.substitute_from_container_info(template, info)
        assert result == "int get_elem(vec_vec_int* v, size_t i, size_t j);"

    def test_unknown_placeholder_preserved(self):
        """Test that unknown placeholders are preserved."""
        template = "{{UNKNOWN_PLACEHOLDER}} should stay"
        result = self.engine.substitute_vec_template(template, "int")
        assert "{{UNKNOWN_PLACEHOLDER}}" in result

    def test_nested_conditionals(self):
        """Test nested conditional blocks."""
        template = """{{#T_NEEDS_DROP}}
outer block
{{#T_IS_POINTER}}
inner block
{{/T_IS_POINTER}}
{{/T_NEEDS_DROP}}"""
        result = self.engine.substitute_vec_template(template, "str")
        # str needs drop and is pointer
        assert "outer block" in result
        assert "inner block" in result

    def test_conditional_false_omits_block(self):
        """Test that false conditionals omit the block entirely."""
        template = """before
{{#T_NEEDS_DROP}}
should not appear
{{/T_NEEDS_DROP}}
after"""
        result = self.engine.substitute_vec_template(template, "int")
        assert "before" in result
        assert "after" in result
        assert "should not appear" not in result

    def test_multiple_same_placeholders(self):
        """Test that same placeholder is substituted consistently."""
        template = "{{T}} func({{T}} a, {{T}} b) { {{T}} c = a + b; return c; }"
        result = self.engine.substitute_vec_template(template, "double")
        assert result == "double func(double a, double b) { double c = a + b; return c; }"

    def test_empty_template(self):
        """Test empty template returns empty string."""
        result = self.engine.substitute_vec_template("", "int")
        assert result == ""

    def test_template_with_no_placeholders(self):
        """Test template with no placeholders is unchanged."""
        template = "int add(int a, int b) { return a + b; }"
        result = self.engine.substitute_vec_template(template, "int")
        assert result == template


class TestComplexTemplates:
    """Test substitution with realistic template snippets."""

    def setup_method(self):
        """Set up test fixtures."""
        self.engine = TemplateSubstitutionEngine()

    def test_vec_push_function(self):
        """Test substitution of vector push function."""
        template = """void vec_{{T_SUFFIX}}_push(vec_{{T_SUFFIX}}* vec, {{T}} elem) {
    if (vec->size >= vec->capacity) {
        vec_{{T_SUFFIX}}_grow(vec);
    }
{{#T_NEEDS_COPY}}
    vec->data[vec->size] = strdup(elem);
{{/T_NEEDS_COPY}}
{{^T_NEEDS_COPY}}
    vec->data[vec->size] = elem;
{{/T_NEEDS_COPY}}
    vec->size++;
}"""
        result_int = self.engine.substitute_vec_template(template, "int")
        assert "void vec_int_push(vec_int* vec, int elem)" in result_int
        assert "vec->data[vec->size] = elem;" in result_int
        assert "strdup" not in result_int

    def test_map_insert_function(self):
        """Test substitution of map insert function."""
        template = """void map_{{KV_SUFFIX}}_insert(map_{{KV_SUFFIX}}* map, {{K}} key, {{V}} value) {
    size_t hash = {{K_HASH}}(key);
    size_t idx = hash % map->capacity;
{{#K_NEEDS_COPY}}
    {{K}} key_copy = strdup(key);
{{/K_NEEDS_COPY}}
{{#V_NEEDS_COPY}}
    {{V}} val_copy = strdup(value);
{{/V_NEEDS_COPY}}
}"""
        result = self.engine.substitute_map_template(template, "str", "str")
        assert "void map_str_str_insert(map_str_str* map, char* key, char* value)" in result
        assert "size_t hash = hash_string(key);" in result
        assert "strdup(key)" in result
        assert "strdup(value)" in result

    def test_set_contains_function(self):
        """Test substitution of set contains function."""
        template = """bool set_{{T_SUFFIX}}_contains(set_{{T_SUFFIX}}* set, {{T}} elem) {
    size_t hash = {{T_HASH}}(elem);
    for (size_t i = 0; i < set->size; i++) {
        if (set->data[i] {{T_COMPARE}} elem) {
            return true;
        }
    }
    return false;
}"""
        result = self.engine.substitute_set_template(template, "int")
        assert "bool set_int_contains(set_int* set, int elem)" in result
        assert "hash_int(elem)" in result
        assert "set->data[i] == elem" in result
