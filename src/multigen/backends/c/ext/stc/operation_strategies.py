"""Container Operation Strategies for STC Translation.

This module implements the Strategy pattern to translate Python container
operations to STC operations, reducing complexity and improving maintainability.
"""

import ast
from abc import ABC, abstractmethod
from typing import Optional


class ContainerOperationStrategy(ABC):
    """Abstract base class for container operation translation strategies."""

    @abstractmethod
    def translate(self, method: str, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        """Translate a container method call to STC operation.

        Args:
            method: Method name being called
            obj_name: Variable name of the container
            container_type: STC container type name
            args: List of argument AST nodes

        Returns:
            Translated STC operation string, or None if method not supported
        """
        pass


class ListOperationStrategy(ContainerOperationStrategy):
    """Strategy for translating Python list operations to STC vector operations."""

    def translate(self, method: str, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        """Translate list method calls."""
        operations = {
            "append": self._translate_append,
            "pop": self._translate_pop,
            "insert": self._translate_insert,
            "remove": self._translate_remove,
            "clear": self._translate_clear,
            "copy": self._translate_copy,
            "reverse": self._translate_reverse,
            "sort": self._translate_sort,
            "index": self._translate_index,
            "count": self._translate_count,
            "extend": self._translate_extend,
        }

        translator = operations.get(method)
        if translator:
            return translator(obj_name, container_type, args)
        return None

    def _translate_append(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            arg = ast.unparse(args[0])
            return f"{container_type}_push(&{obj_name}, {arg})"
        return None

    def _translate_pop(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            # pop(index) - remove at specific index
            index = ast.unparse(args[0])
            return f"{container_type}_erase_at(&{obj_name}, {index})"
        else:
            # pop() - remove last element
            return f"{container_type}_pop(&{obj_name})"

    def _translate_insert(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if len(args) >= 2:
            index = ast.unparse(args[0])
            value = ast.unparse(args[1])
            return f"{container_type}_insert_at(&{obj_name}, {index}, {value})"
        return None

    def _translate_remove(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            value = ast.unparse(args[0])
            return f"{container_type}_erase_val(&{obj_name}, {value})"
        return None

    def _translate_clear(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clear(&{obj_name})"

    def _translate_copy(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clone({obj_name})"

    def _translate_reverse(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_reverse(&{obj_name})"

    def _translate_sort(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_sort(&{obj_name})"

    def _translate_index(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            value = ast.unparse(args[0])
            return f"{container_type}_find(&{obj_name}, {value})"
        return None

    def _translate_count(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            value = ast.unparse(args[0])
            return f"{container_type}_count(&{obj_name}, {value})"
        return None

    def _translate_extend(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_extend(&{obj_name}, &{other})"
        return None


class DictOperationStrategy(ContainerOperationStrategy):
    """Strategy for translating Python dict operations to STC map operations."""

    def translate(self, method: str, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        """Translate dict method calls."""
        operations = {
            "get": self._translate_get,
            "keys": self._translate_keys,
            "values": self._translate_values,
            "items": self._translate_items,
            "update": self._translate_update,
            "setdefault": self._translate_setdefault,
            "popitem": self._translate_popitem,
            "clear": self._translate_clear,
            "copy": self._translate_copy,
        }

        translator = operations.get(method)
        if translator:
            return translator(obj_name, container_type, args)
        return None

    def _translate_get(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            key = ast.unparse(args[0])
            if len(args) > 1:
                default = ast.unparse(args[1])
                return f"{container_type}_get_or(&{obj_name}, {key}, {default})"
            else:
                return f"{container_type}_get(&{obj_name}, {key})"
        return None

    def _translate_keys(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_keys({obj_name})"

    def _translate_values(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_values({obj_name})"

    def _translate_items(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_items({obj_name})"

    def _translate_update(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_update(&{obj_name}, &{other})"
        return None

    def _translate_setdefault(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if len(args) >= 2:
            key = ast.unparse(args[0])
            default = ast.unparse(args[1])
            return f"{container_type}_setdefault(&{obj_name}, {key}, {default})"
        return None

    def _translate_popitem(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_popitem(&{obj_name})"

    def _translate_clear(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clear(&{obj_name})"

    def _translate_copy(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clone({obj_name})"


class SetOperationStrategy(ContainerOperationStrategy):
    """Strategy for translating Python set operations to STC set operations."""

    def translate(self, method: str, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        """Translate set method calls."""
        operations = {
            "add": self._translate_add,
            "discard": self._translate_discard,
            "union": self._translate_union,
            "intersection": self._translate_intersection,
            "difference": self._translate_difference,
            "symmetric_difference": self._translate_symmetric_difference,
            "issubset": self._translate_issubset,
            "issuperset": self._translate_issuperset,
            "isdisjoint": self._translate_isdisjoint,
            "clear": self._translate_clear,
            "copy": self._translate_copy,
        }

        translator = operations.get(method)
        if translator:
            return translator(obj_name, container_type, args)
        return None

    def _translate_add(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            value = ast.unparse(args[0])
            return f"{container_type}_insert(&{obj_name}, {value})"
        return None

    def _translate_discard(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            value = ast.unparse(args[0])
            return f"{container_type}_erase(&{obj_name}, {value})"
        return None

    def _translate_union(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_union(&{obj_name}, &{other})"
        return None

    def _translate_intersection(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_intersection(&{obj_name}, &{other})"
        return None

    def _translate_difference(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_difference(&{obj_name}, &{other})"
        return None

    def _translate_symmetric_difference(
        self, obj_name: str, container_type: str, args: list[ast.expr]
    ) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_symmetric_difference(&{obj_name}, &{other})"
        return None

    def _translate_issubset(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_issubset(&{obj_name}, &{other})"
        return None

    def _translate_issuperset(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_issuperset(&{obj_name}, &{other})"
        return None

    def _translate_isdisjoint(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            other = ast.unparse(args[0])
            return f"{container_type}_isdisjoint(&{obj_name}, &{other})"
        return None

    def _translate_clear(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clear(&{obj_name})"

    def _translate_copy(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"{container_type}_clone({obj_name})"


class StringOperationStrategy(ContainerOperationStrategy):
    """Strategy for translating Python string operations to STC cstr operations."""

    def translate(self, method: str, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        """Translate string method calls."""
        operations = {
            "join": self._translate_join,
            "split": self._translate_split,
            "strip": self._translate_strip,
            "replace": self._translate_replace,
            "startswith": self._translate_startswith,
            "endswith": self._translate_endswith,
            "find": self._translate_find,
            "upper": self._translate_upper,
            "lower": self._translate_lower,
        }

        translator = operations.get(method)
        if translator:
            return translator(obj_name, container_type, args)
        return None

    def _translate_join(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            iterable = ast.unparse(args[0])
            return f"cstr_join(&{obj_name}, &{iterable})"
        return None

    def _translate_split(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            delimiter = ast.unparse(args[0])
            return f"cstr_split(&{obj_name}, {delimiter})"
        else:
            return f"cstr_split_whitespace(&{obj_name})"

    def _translate_strip(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"cstr_strip(&{obj_name})"

    def _translate_replace(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if len(args) >= 2:
            old = ast.unparse(args[0])
            new = ast.unparse(args[1])
            return f"cstr_replace(&{obj_name}, {old}, {new})"
        return None

    def _translate_startswith(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            prefix = ast.unparse(args[0])
            return f"cstr_startswith(&{obj_name}, {prefix})"
        return None

    def _translate_endswith(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            suffix = ast.unparse(args[0])
            return f"cstr_endswith(&{obj_name}, {suffix})"
        return None

    def _translate_find(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        if args:
            substring = ast.unparse(args[0])
            return f"cstr_find(&{obj_name}, {substring})"
        return None

    def _translate_upper(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"cstr_upper(&{obj_name})"

    def _translate_lower(self, obj_name: str, container_type: str, args: list[ast.expr]) -> Optional[str]:
        return f"cstr_lower(&{obj_name})"


class ContainerOperationTranslator:
    """Main translator using strategy pattern for container operations."""

    def __init__(self) -> None:
        """Initialize translator with all operation strategies."""
        self.list_strategy = ListOperationStrategy()
        self.dict_strategy = DictOperationStrategy()
        self.set_strategy = SetOperationStrategy()
        self.string_strategy = StringOperationStrategy()

    def translate_operation(
        self, method: str, obj_name: str, container_type: str, args: list[ast.expr]
    ) -> Optional[str]:
        """Translate a container operation using the appropriate strategy.

        Args:
            method: Method name being called
            obj_name: Variable name of the container
            container_type: STC container type name
            args: List of argument AST nodes

        Returns:
            Translated STC operation string, or None if not supported
        """
        # Determine which strategy to use based on container type
        if container_type.endswith("Vec"):
            return self.list_strategy.translate(method, obj_name, container_type, args)
        elif container_type.endswith("Map"):
            return self.dict_strategy.translate(method, obj_name, container_type, args)
        elif container_type.endswith("Set"):
            return self.set_strategy.translate(method, obj_name, container_type, args)
        elif container_type == "cstr":
            return self.string_strategy.translate(method, obj_name, container_type, args)

        return None


__all__ = [
    "ContainerOperationStrategy",
    "ListOperationStrategy",
    "DictOperationStrategy",
    "SetOperationStrategy",
    "StringOperationStrategy",
    "ContainerOperationTranslator",
]
