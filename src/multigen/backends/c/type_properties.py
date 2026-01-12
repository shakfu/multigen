"""Type properties system for parameterized container code generation.

This module defines type properties that control how code is generated
for different element/key/value types in containers.
"""

from dataclasses import dataclass


@dataclass(frozen=True)
class TypeProperties:
    """Properties of a type that affect code generation.

    Attributes:
        name: Python-style type name (e.g., "int", "str", "float")
        c_type: C type declaration (e.g., "int", "char*", "double")
        suffix: Type suffix for function names (e.g., "int", "str", "float")
        is_pointer: Whether type uses pointer semantics
        needs_drop: Whether type needs cleanup (free/drop)
        needs_copy: Whether type needs deep copy (strdup for strings)
        printf_fmt: Format specifier for printf (e.g., "%d", "%s", "%f")
        zero_value: Default/zero value (e.g., "0", "NULL", "0.0")
        compare_op: Comparison operator/function (e.g., "==", "strcmp")
        hash_fn: Hash function name (e.g., "hash_int", "hash_string")
    """

    name: str
    c_type: str
    suffix: str
    is_pointer: bool
    needs_drop: bool
    needs_copy: bool
    printf_fmt: str
    zero_value: str
    compare_op: str
    hash_fn: str


# Type registry: maps Python type names to their properties
TYPE_REGISTRY: dict[str, TypeProperties] = {
    "int": TypeProperties(
        name="int",
        c_type="int",
        suffix="int",
        is_pointer=False,
        needs_drop=False,
        needs_copy=False,
        printf_fmt="%d",
        zero_value="0",
        compare_op="==",
        hash_fn="hash_int",
    ),
    "float": TypeProperties(
        name="float",
        c_type="float",
        suffix="float",
        is_pointer=False,
        needs_drop=False,
        needs_copy=False,
        printf_fmt="%f",
        zero_value="0.0f",
        compare_op="==",  # Note: float equality is tricky, but we use == for now
        hash_fn="hash_float",
    ),
    "double": TypeProperties(
        name="double",
        c_type="double",
        suffix="double",
        is_pointer=False,
        needs_drop=False,
        needs_copy=False,
        printf_fmt="%lf",
        zero_value="0.0",
        compare_op="==",
        hash_fn="hash_double",
    ),
    "bool": TypeProperties(
        name="bool",
        c_type="bool",
        suffix="bool",
        is_pointer=False,
        needs_drop=False,
        needs_copy=False,
        printf_fmt="%d",
        zero_value="false",
        compare_op="==",
        hash_fn="hash_int",  # Reuse int hash for bool (0 or 1)
    ),
    "char": TypeProperties(
        name="char",
        c_type="char",
        suffix="char",
        is_pointer=False,
        needs_drop=False,
        needs_copy=False,
        printf_fmt="%c",
        zero_value="'\\0'",
        compare_op="==",
        hash_fn="hash_int",  # Reuse int hash for char
    ),
    "str": TypeProperties(
        name="str",
        c_type="char*",
        suffix="str",
        is_pointer=True,
        needs_drop=True,  # Needs free()
        needs_copy=True,  # Needs strdup()
        printf_fmt="%s",
        zero_value="NULL",
        compare_op="strcmp",
        hash_fn="hash_string",
    ),
    "cstr": TypeProperties(
        name="cstr",
        c_type="char*",
        suffix="cstr",
        is_pointer=True,
        needs_drop=True,
        needs_copy=True,
        printf_fmt="%s",
        zero_value="NULL",
        compare_op="strcmp",
        hash_fn="hash_string",
    ),
}


def get_type_properties(type_name: str) -> TypeProperties:
    """Get type properties for a given type name.

    Args:
        type_name: Type name (e.g., "int", "str", "float")

    Returns:
        TypeProperties for the type

    Raises:
        ValueError: If type is not registered
    """
    if type_name not in TYPE_REGISTRY:
        raise ValueError(f"Unknown type: {type_name}. Supported types: {list(TYPE_REGISTRY.keys())}")

    return TYPE_REGISTRY[type_name]


def register_type(props: TypeProperties) -> None:
    """Register a new type in the type registry.

    Args:
        props: Type properties to register
    """
    TYPE_REGISTRY[props.name] = props


def is_registered_type(type_name: str) -> bool:
    """Check if a type is registered.

    Args:
        type_name: Type name to check

    Returns:
        True if type is registered, False otherwise
    """
    return type_name in TYPE_REGISTRY


__all__ = ["TypeProperties", "get_type_properties", "register_type", "is_registered_type", "TYPE_REGISTRY"]
