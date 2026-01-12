"""C++ container system implementation."""

from typing import Optional

from ..base import AbstractContainerSystem


class CppContainerSystem(AbstractContainerSystem):
    """Container system for C++ with STL containers."""

    def __init__(self) -> None:
        """Initialize the C++ container system."""
        self.container_mappings: dict[str, str] = {
            "list": "std::vector",
            "dict": "std::map",
            "set": "std::set",
            "tuple": "std::tuple",
            "deque": "std::deque",
            "stack": "std::stack",
            "queue": "std::queue",
            "priority_queue": "std::priority_queue",
            "unordered_map": "std::unordered_map",
            "unordered_set": "std::unordered_set",
        }

        self.required_headers: dict[str, str] = {
            "std::vector": "vector",
            "std::map": "map",
            "std::set": "set",
            "std::tuple": "tuple",
            "std::deque": "deque",
            "std::stack": "stack",
            "std::queue": "queue",
            "std::priority_queue": "queue",
            "std::unordered_map": "unordered_map",
            "std::unordered_set": "unordered_set",
            "std::string": "string",
        }

    def get_container_type(self, python_container: str, element_types: Optional[list[str]] = None) -> str:
        """Get C++ container type for Python container."""
        base_type = self.container_mappings.get(python_container, python_container)

        if element_types:
            if python_container == "dict" and len(element_types) >= 2:
                return f"{base_type}<{element_types[0]}, {element_types[1]}>"
            elif python_container == "tuple" and len(element_types) > 1:
                types_str = ", ".join(element_types)
                return f"{base_type}<{types_str}>"
            elif element_types:
                return f"{base_type}<{element_types[0]}>"

        return base_type

    def create_container(self, container_type: str, elements: Optional[list[str]] = None) -> str:
        """Create C++ container initialization."""
        if elements:
            elements_str = ", ".join(elements)
            return f"{{{elements_str}}}"
        return "{}"

    def get_container_method(self, container_type: str, method: str) -> str:
        """Get C++ method name for container operation."""
        method_mappings = {
            # List/Vector methods
            "append": "push_back",
            "extend": "insert",
            "insert": "insert",
            "remove": "erase",
            "pop": "pop_back",
            "clear": "clear",
            "size": "size",
            "empty": "empty",
            # Dict/Map methods
            "get": "at",
            "keys": "/* keys() not directly available */",
            "values": "/* values() not directly available */",
            "items": "/* items() not directly available */",
            # Set methods
            "add": "insert",
            "discard": "erase",
            "union": "/* union via set_union */",
            "intersection": "/* intersection via set_intersection */",
            # Common methods
            "find": "find",
            "count": "count",
        }

        return method_mappings.get(method, method)

    def get_iteration_pattern(self, container_var: str, container_type: str) -> str:
        """Get C++ iteration pattern for container."""
        if "std::map" in container_type or "std::unordered_map" in container_type:
            return f"for (auto& [key, value] : {container_var})"
        else:
            return f"for (auto& item : {container_var})"

    def get_required_headers(self) -> list[str]:
        """Get list of required headers for containers."""
        return list(set(self.required_headers.values()))

    def get_header_for_container(self, container_type: str) -> Optional[str]:
        """Get required header for specific container type."""
        return self.required_headers.get(container_type)

    def generate_container_includes(self, used_containers: list[str]) -> list[str]:
        """Generate include statements for used containers."""
        includes = []
        headers_needed = set()

        for container in used_containers:
            header = self.get_header_for_container(container)
            if header:
                headers_needed.add(header)

        for header in sorted(headers_needed):
            includes.append(f"#include <{header}>")

        return includes

    def create_list_operations(self) -> dict[str, str]:
        """Create C++ code patterns for common list operations."""
        return {
            "append": "vec.push_back(item);",
            "insert": "vec.insert(vec.begin() + index, item);",
            "remove": "vec.erase(std::remove(vec.begin(), vec.end(), item), vec.end());",
            "pop": "vec.pop_back();",
            "clear": "vec.clear();",
            "size": "vec.size()",
            "empty": "vec.empty()",
            "reverse": "std::reverse(vec.begin(), vec.end());",
            "sort": "std::sort(vec.begin(), vec.end());",
        }

    def create_dict_operations(self) -> dict[str, str]:
        """Create C++ code patterns for common dict operations."""
        return {
            "insert": "map[key] = value;",
            "get": "map.at(key)",
            "get_with_default": "map.count(key) ? map[key] : default_value",
            "erase": "map.erase(key);",
            "find": "map.find(key)",
            "contains": "map.count(key) > 0",
            "clear": "map.clear();",
            "size": "map.size()",
            "empty": "map.empty()",
        }

    def create_set_operations(self) -> dict[str, str]:
        """Create C++ code patterns for common set operations."""
        return {
            "add": "set.insert(item);",
            "remove": "set.erase(item);",
            "discard": "set.erase(item);",  # Same as remove but no exception
            "clear": "set.clear();",
            "size": "set.size()",
            "empty": "set.empty()",
            "contains": "set.count(item) > 0",
            "union": "std::set_union(set1.begin(), set1.end(), set2.begin(), set2.end(), std::inserter(result, result.begin()));",
            "intersection": "std::set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(), std::inserter(result, result.begin()));",
        }

    def get_container_literal_syntax(self, container_type: str) -> str:
        """Get the literal syntax for container initialization."""
        if "std::vector" in container_type:
            return "{1, 2, 3}"
        elif "std::map" in container_type:
            return '{{"key1", "value1"}, {"key2", "value2"}}'
        elif "std::set" in container_type:
            return "{1, 2, 3}"
        elif "std::tuple" in container_type:
            return '{1, "hello", 3.14}'
        else:
            return "{}"

    def optimize_container_usage(self, code: str) -> str:
        """Apply C++-specific container optimizations."""
        optimizations = {
            # Use emplace_back instead of push_back when constructing
            ".push_back(": ".emplace_back(",
            # Use reserve for vectors when size is known
            "std::vector": "std::vector  // Consider using reserve() for known sizes",
            # Use unordered containers for performance when order doesn't matter
            "std::map": "std::map  // Consider std::unordered_map for better performance",
            "std::set": "std::set  // Consider std::unordered_set for better performance",
        }

        optimized_code = code
        for old, new in optimizations.items():
            if old in optimized_code:
                # Add comment about optimization opportunity
                optimized_code = optimized_code.replace(old, new)

        return optimized_code

    def get_list_type(self, element_type: str) -> str:
        """Get C++ vector type for element type."""
        return f"std::vector<{element_type}>"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get C++ map type for key-value pair."""
        return f"std::map<{key_type}, {value_type}>"

    def get_set_type(self, element_type: str) -> str:
        """Get C++ set type for element type."""
        return f"std::set<{element_type}>"

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate container-specific operations code."""
        code_parts = []
        for operation in operations:
            if "vector" in container_type:
                ops = self.create_list_operations()
                if operation in ops:
                    code_parts.append(ops[operation])
            elif "map" in container_type:
                ops = self.create_dict_operations()
                if operation in ops:
                    code_parts.append(ops[operation])
            elif "set" in container_type:
                ops = self.create_set_operations()
                if operation in ops:
                    code_parts.append(ops[operation])
        return "\n".join(code_parts)

    def get_required_imports(self) -> list[str]:
        """Get imports required for container operations."""
        return [
            "#include <vector>",
            "#include <map>",
            "#include <set>",
            "#include <string>",
            "#include <algorithm>",
        ]
