"""Tests for type properties system."""

import pytest
from multigen.backends.c.type_properties import (
    TypeProperties,
    get_type_properties,
    is_registered_type,
    register_type,
)


class TestTypeProperties:
    """Test TypeProperties dataclass."""

    def test_int_properties(self):
        """Test integer type properties."""
        props = get_type_properties("int")
        assert props.name == "int"
        assert props.c_type == "int"
        assert props.suffix == "int"
        assert props.is_pointer is False
        assert props.needs_drop is False
        assert props.needs_copy is False
        assert props.printf_fmt == "%d"
        assert props.zero_value == "0"
        assert props.compare_op == "=="
        assert props.hash_fn == "hash_int"

    def test_float_properties(self):
        """Test float type properties."""
        props = get_type_properties("float")
        assert props.c_type == "float"
        assert props.suffix == "float"
        assert props.is_pointer is False
        assert props.printf_fmt == "%f"
        assert props.zero_value == "0.0f"

    def test_string_properties(self):
        """Test string type properties."""
        props = get_type_properties("str")
        assert props.c_type == "char*"
        assert props.suffix == "str"
        assert props.is_pointer is True
        assert props.needs_drop is True
        assert props.needs_copy is True
        assert props.printf_fmt == "%s"
        assert props.zero_value == "NULL"
        assert props.compare_op == "strcmp"
        assert props.hash_fn == "hash_string"

    def test_unknown_type_raises(self):
        """Test that unknown type raises ValueError."""
        with pytest.raises(ValueError, match="Unknown type: unknown"):
            get_type_properties("unknown")

    def test_is_registered(self):
        """Test type registration check."""
        assert is_registered_type("int") is True
        assert is_registered_type("float") is True
        assert is_registered_type("str") is True
        assert is_registered_type("unknown") is False

    def test_register_custom_type(self):
        """Test registering a custom type."""
        custom = TypeProperties(
            name="custom",
            c_type="custom_t",
            suffix="custom",
            is_pointer=False,
            needs_drop=False,
            needs_copy=False,
            printf_fmt="%d",
            zero_value="0",
            compare_op="==",
            hash_fn="hash_custom",
        )
        register_type(custom)

        assert is_registered_type("custom") is True
        props = get_type_properties("custom")
        assert props.name == "custom"
        assert props.c_type == "custom_t"


class TestAllRegisteredTypes:
    """Test all registered types have correct properties."""

    def test_all_types_accessible(self):
        """Test all standard types are registered."""
        required_types = ["int", "float", "double", "bool", "char", "str", "cstr"]
        for type_name in required_types:
            assert is_registered_type(type_name), f"Type {type_name} should be registered"
            props = get_type_properties(type_name)
            assert props.name == type_name

    def test_pointer_types_consistency(self):
        """Test pointer types have consistent properties."""
        str_props = get_type_properties("str")
        cstr_props = get_type_properties("cstr")

        # Both should be pointers
        assert str_props.is_pointer is True
        assert cstr_props.is_pointer is True

        # Both should need drop and copy
        assert str_props.needs_drop is True
        assert cstr_props.needs_drop is True
        assert str_props.needs_copy is True
        assert cstr_props.needs_copy is True

    def test_numeric_types_consistency(self):
        """Test numeric types have consistent properties."""
        numeric_types = ["int", "float", "double"]
        for type_name in numeric_types:
            props = get_type_properties(type_name)
            assert props.is_pointer is False, f"{type_name} should not be a pointer"
            assert props.needs_drop is False, f"{type_name} should not need drop"
            assert props.needs_copy is False, f"{type_name} should not need copy"
