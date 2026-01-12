"""Tests for type parameter extraction."""

import pytest
from multigen.backends.c.type_parameter_extractor import (
    TypeParameterExtractor,
    ContainerTypeInfo,
)


class TestTypeParameterExtractor:
    """Test TypeParameterExtractor class."""

    def setup_method(self):
        """Set up test fixtures."""
        self.extractor = TypeParameterExtractor()

    def test_extract_vec_int(self):
        """Test extracting parameters from vec_int."""
        info = self.extractor.extract("vec_int")
        assert info is not None
        assert info.family == "vec"
        assert info.full_name == "vec_int"
        assert info.type_params == ["int"]
        assert len(info.type_properties) == 1
        assert info.type_properties[0].name == "int"
        assert info.is_nested is False

    def test_extract_vec_float(self):
        """Test extracting parameters from vec_float."""
        info = self.extractor.extract("vec_float")
        assert info is not None
        assert info.family == "vec"
        assert info.type_params == ["float"]
        assert info.type_properties[0].name == "float"

    def test_extract_map_str_int(self):
        """Test extracting parameters from map_str_int."""
        info = self.extractor.extract("map_str_int")
        assert info is not None
        assert info.family == "map"
        assert info.full_name == "map_str_int"
        assert info.type_params == ["str", "int"]
        assert len(info.type_properties) == 2
        assert info.type_properties[0].name == "str"
        assert info.type_properties[1].name == "int"
        assert info.is_nested is False

    def test_extract_map_int_int(self):
        """Test extracting parameters from map_int_int."""
        info = self.extractor.extract("map_int_int")
        assert info is not None
        assert info.family == "map"
        assert info.type_params == ["int", "int"]

    def test_extract_set_int(self):
        """Test extracting parameters from set_int."""
        info = self.extractor.extract("set_int")
        assert info is not None
        assert info.family == "set"
        assert info.full_name == "set_int"
        assert info.type_params == ["int"]
        assert len(info.type_properties) == 1
        assert info.is_nested is False

    def test_extract_set_str(self):
        """Test extracting parameters from set_str."""
        info = self.extractor.extract("set_str")
        assert info is not None
        assert info.family == "set"
        assert info.type_params == ["str"]
        assert info.type_properties[0].name == "str"

    def test_extract_vec_vec_int(self):
        """Test extracting parameters from vec_vec_int."""
        info = self.extractor.extract("vec_vec_int")
        assert info is not None
        assert info.family == "vec_vec"
        assert info.full_name == "vec_vec_int"
        assert info.type_params == ["vec_int", "int"]
        assert len(info.type_properties) == 1  # Only inner type properties
        assert info.type_properties[0].name == "int"
        assert info.is_nested is True

    def test_extract_invalid_type_returns_none(self):
        """Test that invalid type returns None."""
        info = self.extractor.extract("vec_unknown")
        assert info is None

    def test_extract_invalid_pattern_returns_none(self):
        """Test that invalid pattern returns None."""
        info = self.extractor.extract("invalid_pattern")
        assert info is None

    def test_is_parameterized_container(self):
        """Test is_parameterized_container method."""
        assert self.extractor.is_parameterized_container("vec_int") is True
        assert self.extractor.is_parameterized_container("map_str_int") is True
        assert self.extractor.is_parameterized_container("set_int") is True
        assert self.extractor.is_parameterized_container("vec_unknown") is False
        assert self.extractor.is_parameterized_container("invalid") is False

    def test_get_template_name_vec(self):
        """Test get_template_name for vectors."""
        assert self.extractor.get_template_name("vec_int") == "vec_T"
        assert self.extractor.get_template_name("vec_float") == "vec_T"
        assert self.extractor.get_template_name("vec_double") == "vec_T"

    def test_get_template_name_map(self):
        """Test get_template_name for maps."""
        assert self.extractor.get_template_name("map_str_int") == "map_K_V"
        assert self.extractor.get_template_name("map_int_int") == "map_K_V"

    def test_get_template_name_set(self):
        """Test get_template_name for sets."""
        assert self.extractor.get_template_name("set_int") == "set_T"
        assert self.extractor.get_template_name("set_str") == "set_T"

    def test_get_template_name_nested(self):
        """Test get_template_name for nested containers."""
        assert self.extractor.get_template_name("vec_vec_int") == "vec_vec_T"

    def test_get_template_name_invalid_returns_none(self):
        """Test get_template_name returns None for invalid types."""
        assert self.extractor.get_template_name("invalid") is None
        assert self.extractor.get_template_name("vec_unknown") is None


class TestAllContainerCombinations:
    """Test extraction works for all common container combinations."""

    def setup_method(self):
        """Set up test fixtures."""
        self.extractor = TypeParameterExtractor()

    def test_all_vec_types(self):
        """Test all vector type combinations."""
        vec_types = ["vec_int", "vec_float", "vec_double", "vec_bool", "vec_char", "vec_str"]
        for vec_type in vec_types:
            info = self.extractor.extract(vec_type)
            assert info is not None, f"Should extract {vec_type}"
            assert info.family == "vec"

    def test_all_set_types(self):
        """Test all set type combinations."""
        set_types = ["set_int", "set_float", "set_double", "set_str"]
        for set_type in set_types:
            info = self.extractor.extract(set_type)
            assert info is not None, f"Should extract {set_type}"
            assert info.family == "set"

    def test_common_map_types(self):
        """Test common map type combinations."""
        map_types = [
            ("map_str_int", "str", "int"),
            ("map_int_int", "int", "int"),
            ("map_str_str", "str", "str"),
            ("map_int_str", "int", "str"),
        ]
        for map_type, expected_key, expected_val in map_types:
            info = self.extractor.extract(map_type)
            assert info is not None, f"Should extract {map_type}"
            assert info.family == "map"
            assert info.type_params == [expected_key, expected_val]
