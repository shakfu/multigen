"""STC-Enhanced Python-to-C Translator.

This module extends the SimplePythonToCTranslator with STC container support,
enabling translation of Python container operations to high-performance STC
container operations in generated C code.
"""

import ast
from typing import Any, Optional

from .containers import STCCodeGenerator, get_stc_container_for_python_type
from .enhanced_type_inference import EnhancedTypeInferenceEngine
from .operation_strategies import ContainerOperationTranslator


class STCPythonToCTranslator:
    """Enhanced Python-to-C translator with STC container support."""

    def __init__(self) -> None:
        self.stc_generator = STCCodeGenerator()
        self.container_variables: dict[str, str] = {}  # var_name -> container_type
        self.required_includes: set[str] = set()
        self.type_definitions: list[str] = []
        self.type_inference_engine = EnhancedTypeInferenceEngine()
        self.operation_translator = ContainerOperationTranslator()

    def analyze_variable_types(self, node: ast.AST) -> dict[str, str]:
        """Enhanced variable type analysis using advanced type inference.

        Returns:
            Dictionary mapping variable names to their Python types
        """
        # Use enhanced type inference engine
        # Cast to Module for type safety
        module_node = node if isinstance(node, ast.Module) else ast.Module(body=[], type_ignores=[])
        inferred_types = self.type_inference_engine.analyze_module(module_node)

        # Convert InferredType objects to simple string mapping
        type_info = {}
        for var_name, inferred_type in inferred_types.items():
            type_info[var_name] = inferred_type.python_type

        # Fallback to basic analysis if inference found nothing
        if not type_info:
            type_info = self._basic_type_analysis(node)

        return type_info

    def _basic_type_analysis(self, node: ast.AST) -> dict[str, str]:
        """Fallback basic type analysis for compatibility."""
        type_info = {}

        class TypeAnalyzer(ast.NodeVisitor):
            def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
                """Handle type-annotated assignments: var: List[int] = []."""
                if isinstance(node.target, ast.Name):
                    var_name = node.target.id
                    if isinstance(node.annotation, ast.Name):
                        type_info[var_name] = node.annotation.id
                    elif isinstance(node.annotation, ast.Subscript):
                        # Handle List[int], Dict[str, int], etc.
                        type_info[var_name] = ast.unparse(node.annotation)
                        # Also handle lowercase variants like list[int]
                        if isinstance(node.annotation.value, ast.Name):
                            base_type = node.annotation.value.id
                            if base_type in ["list", "dict", "set"]:
                                type_info[var_name] = ast.unparse(node.annotation)

            def visit_Assign(self, node: ast.Assign) -> None:
                """Handle regular assignments with type inference."""
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        var_name = target.id
                        # Try to infer type from assignment value
                        if isinstance(node.value, ast.List):
                            type_info[var_name] = "list"
                        elif isinstance(node.value, ast.Dict):
                            type_info[var_name] = "dict"
                        elif isinstance(node.value, ast.Set):
                            type_info[var_name] = "set"
                        elif isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
                            type_info[var_name] = "str"
                        elif isinstance(node.value, ast.Call):
                            # Handle list(), dict(), set() constructor calls
                            if isinstance(node.value.func, ast.Name):
                                func_name = node.value.func.id
                                if func_name in ["list", "dict", "set", "deque"]:
                                    type_info[var_name] = func_name

        analyzer = TypeAnalyzer()
        analyzer.visit(node)
        return type_info

    def get_type_inference_statistics(self) -> dict[str, Any]:
        """Get statistics about type inference accuracy."""
        return self.type_inference_engine.get_inference_statistics()

    def generate_stc_includes_and_types(self, type_info: dict[str, str]) -> tuple[list[str], list[str]]:
        """Generate STC include statements and type definitions.

        Args:
            type_info: Variable name to Python type mapping

        Returns:
            Tuple of (include_statements, type_definitions)
        """
        includes = []
        type_defs = []

        for var_name, python_type in type_info.items():
            container = get_stc_container_for_python_type(python_type)
            if container:
                # Generate type definition and include
                type_def, include = self.stc_generator.generate_container_type_def(var_name, python_type)

                if include and include not in includes:
                    includes.append(include)

                if type_def and type_def not in type_defs:
                    type_defs.append(type_def)

                # Store container type for later use
                if python_type.startswith("List["):
                    self.container_variables[var_name] = f"{var_name.capitalize()}Vec"
                elif python_type.startswith("Dict["):
                    self.container_variables[var_name] = f"{var_name.capitalize()}Map"
                elif python_type.startswith("Set["):
                    self.container_variables[var_name] = f"{var_name.capitalize()}Set"
                elif python_type == "list":
                    self.container_variables[var_name] = f"{var_name.capitalize()}Vec"
                elif python_type == "dict":
                    self.container_variables[var_name] = f"{var_name.capitalize()}Map"
                elif python_type == "set":
                    self.container_variables[var_name] = f"{var_name.capitalize()}Set"

        return includes, type_defs

    def translate_container_operation(self, call_node: ast.Call) -> Optional[str]:
        """Translate Python container method calls to STC operations.

        Uses the strategy pattern to delegate translation to specialized strategies
        based on container type (list/dict/set/string).

        Args:
            call_node: AST Call node representing a method call

        Returns:
            STC operation string or None if not a container operation
        """
        if not isinstance(call_node.func, ast.Attribute):
            return None

        # Get the object and method name
        if isinstance(call_node.func.value, ast.Name):
            obj_name = call_node.func.value.id
            method_name = call_node.func.attr

            # Check if this is a known container variable
            if obj_name in self.container_variables:
                container_type = self.container_variables[obj_name]

                # Delegate to strategy pattern translator
                return self.operation_translator.translate_operation(
                    method_name, obj_name, container_type, call_node.args
                )

        return None

    def translate_container_initialization(self, assign_node: ast.Assign) -> Optional[str]:
        """Translate container initialization to STC initialization.

        Args:
            assign_node: AST Assignment node

        Returns:
            STC initialization string or None
        """
        if len(assign_node.targets) == 1 and isinstance(assign_node.targets[0], ast.Name):
            var_name = assign_node.targets[0].id

            if var_name in self.container_variables:
                container_type = self.container_variables[var_name]
                return self.stc_generator.generate_initialization(var_name, container_type)

        return None

    def generate_cleanup_code(self) -> list[str]:
        """Generate cleanup code for all STC containers."""
        cleanup_lines = []
        for var_name, container_type in self.container_variables.items():
            cleanup_lines.append(self.stc_generator.generate_cleanup(var_name, container_type))
        return cleanup_lines

    def translate_len_builtin(self, call_node: ast.Call) -> Optional[str]:
        """Translate len() builtin for STC containers."""
        if isinstance(call_node.func, ast.Name) and call_node.func.id == "len" and len(call_node.args) == 1:
            arg = call_node.args[0]
            if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                container_type = self.container_variables[arg.id]
                return f"{container_type}_size(&{arg.id})"

        return None

    def translate_subscript_operation(self, subscript_node: ast.Subscript) -> Optional[str]:
        """Translate subscript operations (container[key]) to STC operations.

        Args:
            subscript_node: AST Subscript node

        Returns:
            STC operation string or None if not a container subscript
        """
        if isinstance(subscript_node.value, ast.Name):
            obj_name = subscript_node.value.id

            if obj_name in self.container_variables:
                container_type = self.container_variables[obj_name]
                key = ast.unparse(subscript_node.slice)

                # Handle different container types
                if container_type.endswith("Vec"):
                    # List/vector indexing: list[i] -> vec_at(&list, i)
                    return f"{container_type}_at(&{obj_name}, {key})"

                elif container_type.endswith("Map"):
                    # Dict indexing: dict[key] -> hmap_get(&dict, key)
                    return f"{container_type}_get(&{obj_name}, {key})"

                elif container_type.endswith("Set"):
                    # Set membership check: set[key] -> hset_contains(&set, key)
                    return f"{container_type}_contains(&{obj_name}, {key})"

                elif container_type == "cstr":
                    # String indexing: str[i] -> cstr_at(&str, i)
                    return f"cstr_at(&{obj_name}, {key})"

        return None

    def translate_membership_test(self, compare_node: ast.Compare) -> Optional[str]:
        """Translate membership tests (x in container) to STC operations.

        Args:
            compare_node: AST Compare node with 'in' operator

        Returns:
            STC operation string or None if not a container membership test
        """
        if (
            len(compare_node.ops) == 1
            and isinstance(compare_node.ops[0], ast.In)
            and len(compare_node.comparators) == 1
        ):
            left = compare_node.left
            right = compare_node.comparators[0]

            # Check if right side is a container
            if isinstance(right, ast.Name) and right.id in self.container_variables:
                container_type = self.container_variables[right.id]
                value = ast.unparse(left)

                if container_type.endswith(("Set", "Map")):
                    return f"{container_type}_contains(&{right.id}, {value})"
                elif container_type.endswith("Vec"):
                    # For vectors, we need to search
                    return f"{container_type}_find(&{right.id}, {value}) != {container_type}_end(&{right.id})"
                elif container_type == "cstr":
                    return f"cstr_find(&{right.id}, {value}) != cstr_npos"

        return None

    def translate_assignment_to_subscript(self, assign_node: ast.Assign) -> Optional[str]:
        """Translate assignments to subscripts (container[key] = value) to STC operations.

        Args:
            assign_node: AST Assign node with subscript target

        Returns:
            STC operation string or None if not a container subscript assignment
        """
        if len(assign_node.targets) == 1 and isinstance(assign_node.targets[0], ast.Subscript):
            target = assign_node.targets[0]

            if isinstance(target.value, ast.Name):
                obj_name = target.value.id

                if obj_name in self.container_variables:
                    container_type = self.container_variables[obj_name]
                    key = ast.unparse(target.slice)
                    value = ast.unparse(assign_node.value)

                    if container_type.endswith("Vec"):
                        # List assignment: list[i] = value -> vec_at(&list, i) = value
                        return f"*{container_type}_at_mut(&{obj_name}, {key}) = {value}"

                    elif container_type.endswith("Map"):
                        # Dict assignment: dict[key] = value -> hmap_insert(&dict, key, value)
                        return f"{container_type}_insert(&{obj_name}, {key}, {value})"

                    elif container_type == "cstr":
                        # String character assignment: str[i] = char -> cstr_set_at(&str, i, char)
                        return f"cstr_set_at(&{obj_name}, {key}, {value})"

        return None

    def translate_for_loop_iteration(self, for_node: ast.For) -> Optional[tuple[str, str, str]]:
        """Translate for loop iteration over STC containers.

        Args:
            for_node: AST For node

        Returns:
            Tuple of (init_code, condition_code, body_prefix) or None
        """
        if isinstance(for_node.iter, ast.Name) and for_node.iter.id in self.container_variables:
            container_name = for_node.iter.id
            container_type = self.container_variables[container_name]

            if isinstance(for_node.target, ast.Name):
                target_var = for_node.target.id
                iterator_var = f"{container_name}_it"

                if container_type.endswith("Vec"):
                    # Vector iteration
                    init_code = f"for (c_each({iterator_var}, {container_type}, {container_name}))"
                    body_prefix = f"    {target_var} = *{iterator_var}.ref;"
                    return init_code, "", body_prefix

                elif container_type.endswith("Set"):
                    # Set iteration
                    init_code = f"for (c_each({iterator_var}, {container_type}, {container_name}))"
                    body_prefix = f"    {target_var} = *{iterator_var}.ref;"
                    return init_code, "", body_prefix

                elif container_type.endswith("Map"):
                    # Map iteration (iterate over keys)
                    init_code = f"for (c_each({iterator_var}, {container_type}, {container_name}))"
                    body_prefix = f"    {target_var} = {iterator_var}.ref->first;"
                    return init_code, "", body_prefix

                elif container_type == "cstr":
                    # String character iteration
                    init_code = f"for (size_t i = 0; i < cstr_size(&{container_name}); i++)"
                    body_prefix = f"    {target_var} = cstr_at(&{container_name}, i);"
                    return init_code, "", body_prefix

        return None

    def translate_builtin_functions(self, call_node: ast.Call) -> Optional[str]:
        """Translate Python builtin functions for STC containers.

        Args:
            call_node: AST Call node

        Returns:
            STC operation string or None
        """
        if isinstance(call_node.func, ast.Name):
            func_name = call_node.func.id

            if func_name == "len":
                return self.translate_len_builtin(call_node)

            elif func_name == "max" and len(call_node.args) == 1:
                arg = call_node.args[0]
                if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                    container_type = self.container_variables[arg.id]
                    if container_type.endswith("Vec"):
                        return f"{container_type}_max(&{arg.id})"

            elif func_name == "min" and len(call_node.args) == 1:
                arg = call_node.args[0]
                if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                    container_type = self.container_variables[arg.id]
                    if container_type.endswith("Vec"):
                        return f"{container_type}_min(&{arg.id})"

            elif func_name == "sum" and len(call_node.args) == 1:
                arg = call_node.args[0]
                if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                    container_type = self.container_variables[arg.id]
                    if container_type.endswith("Vec"):
                        return f"{container_type}_sum(&{arg.id})"

            elif func_name == "sorted" and len(call_node.args) == 1:
                arg = call_node.args[0]
                if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                    container_type = self.container_variables[arg.id]
                    return f"{container_type}_sorted(&{arg.id})"

            elif func_name == "reversed" and len(call_node.args) == 1:
                arg = call_node.args[0]
                if isinstance(arg, ast.Name) and arg.id in self.container_variables:
                    container_type = self.container_variables[arg.id]
                    return f"{container_type}_reversed(&{arg.id})"

        return None

    def generate_iteration_code(self, for_node: ast.For) -> Optional[str]:
        """Generate STC iteration code for for loops.

        Args:
            for_node: AST For node

        Returns:
            STC iteration string or None
        """
        if isinstance(for_node.iter, ast.Name) and for_node.iter.id in self.container_variables:
            container_name = for_node.iter.id
            container_type = self.container_variables[container_name]

            if isinstance(for_node.target, ast.Name):
                iterator_var = "it"  # Standard STC iterator name
                target_var = for_node.target.id

                iteration_code = self.stc_generator.generate_iteration(container_name, container_type, iterator_var)

                # Generate code to access the iterator value
                if container_type.endswith("Vec") or container_type.endswith("Set"):
                    value_access = f"*{iterator_var}.ref"
                elif container_type.endswith("Map"):
                    value_access = f"{iterator_var}.ref->second"  # For key-value pairs
                else:
                    value_access = f"*{iterator_var}.ref"

                return f"{iteration_code} {{\n    {target_var} = {value_access};"

        return None


__all__ = ["STCPythonToCTranslator"]
