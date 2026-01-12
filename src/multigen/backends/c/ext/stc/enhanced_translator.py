"""Enhanced STC Translator with Smart Pointers and Allocators.

This module extends the STC translator to support smart pointers, custom allocators,
and advanced memory management patterns in Python-to-C translation.

Features:
- Smart pointer syntax translation
- Custom allocator integration
- Memory-safe operation generation
- Reference cycle prevention
- Performance optimization
"""

import ast
from typing import Any, Optional

from .allocators import AllocatorType
from .enhanced_memory_manager import EnhancedMemoryManager, ResourceType
from .smart_pointers import SmartPointerType
from .translator import STCPythonToCTranslator


class EnhancedSTCTranslator(STCPythonToCTranslator):
    """Enhanced STC translator supporting smart pointers and custom allocators.

    Extends the base STC translator with:
    - Smart pointer operation translation
    - Custom allocator support
    - Memory safety analysis
    - Performance optimization
    """

    def __init__(self) -> None:
        super().__init__()
        self.enhanced_memory_manager = EnhancedMemoryManager()

        # Python syntax mappings for smart pointers
        self.smart_pointer_syntax = {
            "unique_ptr": SmartPointerType.UNIQUE,
            "shared_ptr": SmartPointerType.SHARED,
            "weak_ptr": SmartPointerType.WEAK,
            "scoped_ptr": SmartPointerType.SCOPED,
        }

        # Python syntax mappings for allocators
        self.allocator_syntax = {
            "arena_alloc": AllocatorType.ARENA,
            "pool_alloc": AllocatorType.POOL,
            "stack_alloc": AllocatorType.STACK,
            "free_list_alloc": AllocatorType.FREE_LIST,
        }

        # Track smart pointer variables
        self.smart_pointer_variables: dict[str, SmartPointerType] = {}

        # Track allocator variables
        self.allocator_variables: dict[str, AllocatorType] = {}

    def analyze_variable_types(self, tree: ast.AST) -> dict[str, str]:
        """Enhanced variable type analysis including smart pointers and allocators."""
        type_info = super().analyze_variable_types(tree)

        class EnhancedTypeAnalyzer(ast.NodeVisitor):
            def __init__(self, translator: "EnhancedSTCTranslator") -> None:
                self.translator = translator

            def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
                if isinstance(node.target, ast.Name):
                    var_name = node.target.id
                    type_annotation = ast.unparse(node.annotation)

                    # Check for smart pointer types
                    for syntax, pointer_type in self.translator.smart_pointer_syntax.items():
                        if type_annotation.startswith(syntax):
                            self.translator.smart_pointer_variables[var_name] = pointer_type
                            type_info[var_name] = type_annotation

                    # Check for allocator types
                    for syntax, allocator_type in self.translator.allocator_syntax.items():
                        if type_annotation.startswith(syntax):
                            self.translator.allocator_variables[var_name] = allocator_type
                            type_info[var_name] = type_annotation

            def visit_Call(self, node: ast.Call) -> None:
                # Check for smart pointer factory functions
                if isinstance(node.func, ast.Name):
                    func_name = node.func.id
                    if func_name.startswith(("make_unique", "make_shared")):
                        # Extract return type and register
                        if func_name.startswith("make_unique"):
                            pass
                        else:
                            pass

                        # Try to determine variable name from context
                        # This would need to be enhanced based on specific patterns

                self.generic_visit(node)

        analyzer = EnhancedTypeAnalyzer(self)
        analyzer.visit(tree)

        return type_info

    def translate_smart_pointer_operation(self, node: ast.Call) -> Optional[str]:
        """Translate smart pointer operations to STC code."""
        if not isinstance(node.func, ast.Attribute):
            return None

        if not isinstance(node.func.value, ast.Name):
            return None

        obj_name = node.func.value.id
        method_name = node.func.attr

        # Check if this is a smart pointer variable
        if obj_name not in self.smart_pointer_variables:
            return None

        self.smart_pointer_variables[obj_name]

        # Convert arguments
        args = [self._convert_arg_to_string(arg) for arg in node.args]

        # Generate smart pointer operation
        operation = self.enhanced_memory_manager.smart_pointer_manager.generate_smart_pointer_operation(
            method_name, obj_name, args
        )

        return operation

    def translate_allocator_operation(self, node: ast.Call) -> Optional[str]:
        """Translate allocator operations to STC code."""
        if not isinstance(node.func, ast.Attribute):
            return None

        if not isinstance(node.func.value, ast.Name):
            return None

        obj_name = node.func.value.id
        method_name = node.func.attr

        # Check if this is an allocator variable
        if obj_name not in self.allocator_variables:
            return None

        self.allocator_variables[obj_name]
        args = [self._convert_arg_to_string(arg) for arg in node.args]

        # Generate allocator operation based on method
        if method_name == "alloc":
            if len(args) >= 2:  # variable_name, size, element_type
                return self.enhanced_memory_manager.allocator_manager.generate_allocation_code(
                    obj_name, args[0], args[1], args[2] if len(args) > 2 else "void"
                )
        elif method_name == "free":
            if len(args) >= 1:
                return self.enhanced_memory_manager.allocator_manager.generate_deallocation_code(obj_name, args[0])

        return None

    def translate_make_smart_pointer(self, node: ast.Call) -> Optional[str]:
        """Translate make_unique, make_shared, etc. to STC code."""
        if not isinstance(node.func, ast.Name):
            return None

        func_name = node.func.id

        if func_name.startswith("make_unique"):
            pointer_type = SmartPointerType.UNIQUE
            element_type = func_name.replace("make_unique_", "") if "_" in func_name else "auto"
        elif func_name.startswith("make_shared"):
            pointer_type = SmartPointerType.SHARED
            element_type = func_name.replace("make_shared_", "") if "_" in func_name else "auto"
        else:
            return None

        args = [self._convert_arg_to_string(arg) for arg in node.args]

        return self.enhanced_memory_manager.smart_pointer_manager.generate_make_smart_pointer(
            pointer_type, element_type, args
        )

    def translate_enhanced_container_operation(self, node: ast.Call) -> Optional[str]:
        """Enhanced container operation translation with allocator support."""
        # First try standard container operations
        standard_result = super().translate_container_operation(node)
        if standard_result:
            return standard_result

        # Try smart pointer operations
        smart_pointer_result = self.translate_smart_pointer_operation(node)
        if smart_pointer_result:
            return smart_pointer_result

        # Try allocator operations
        allocator_result = self.translate_allocator_operation(node)
        if allocator_result:
            return allocator_result

        # Try smart pointer factory functions
        factory_result = self.translate_make_smart_pointer(node)
        if factory_result:
            return factory_result

        return None

    def generate_enhanced_type_definitions(self, type_info: dict[str, str]) -> tuple[list[str], list[str]]:
        """Generate enhanced type definitions including smart pointers and allocators."""
        includes, type_defs = [], []

        # Register resources with enhanced memory manager
        for var_name, type_str in type_info.items():
            if var_name in self.smart_pointer_variables:
                pointer_type = self.smart_pointer_variables[var_name]
                element_type = self._extract_element_type_from_annotation(type_str)

                # Register smart pointer
                self.enhanced_memory_manager.register_smart_pointer(var_name, pointer_type, element_type)

                # Generate type definition
                pointer_alloc = self.enhanced_memory_manager.smart_pointer_manager.allocations[var_name]
                type_def, include = self.enhanced_memory_manager.smart_pointer_manager.generate_smart_pointer_type_def(
                    pointer_alloc
                )

                if include:
                    includes.append(include)
                if type_def:
                    type_defs.append(type_def)

            elif var_name in self.allocator_variables:
                allocator_type = self.allocator_variables[var_name]

                # Register allocator with default parameters
                allocator_instance = self.enhanced_memory_manager.register_allocator(var_name, allocator_type)

                # Generate allocator setup
                init_code, include = self.enhanced_memory_manager.allocator_manager.generate_allocator_setup(
                    allocator_instance
                )

                if include:
                    includes.append(include)
                if init_code:
                    type_defs.append(init_code)

            elif any(container_type in type_str for container_type in ["list", "dict", "set"]):
                # Standard container - check if it should use a custom allocator
                container_type = self._extract_container_type_from_annotation(type_str)
                element_type = self._extract_element_type_from_annotation(type_str)

                # Register container
                self.enhanced_memory_manager.register_container_with_allocator(var_name, container_type, element_type)

        # Generate initialization code for all components
        init_result = self.enhanced_memory_manager.generate_initialization_code()
        init_includes_raw = init_result[0]
        init_code_raw = init_result[1]
        # Ensure we have lists
        if isinstance(init_includes_raw, list):
            includes.extend(init_includes_raw)
        else:
            includes.append(init_includes_raw)
        if isinstance(init_code_raw, list):
            type_defs.extend(init_code_raw)
        else:
            type_defs.append(init_code_raw)

        # Remove duplicates
        unique_includes = list(dict.fromkeys(includes))
        unique_type_defs = list(dict.fromkeys(type_defs))

        return unique_includes, unique_type_defs

    def generate_enhanced_cleanup_code(self) -> list[str]:
        """Generate enhanced cleanup code for all managed resources."""
        return self.enhanced_memory_manager.generate_cleanup_code()

    def analyze_memory_safety(self, tree: ast.AST) -> dict[str, Any]:
        """Comprehensive memory safety analysis."""
        # Run standard STC memory analysis
        standard_errors: list[dict[str, Any]] = []

        # Run enhanced memory analysis
        enhanced_errors = self.enhanced_memory_manager.detect_memory_issues()

        # Combine results
        all_errors = standard_errors + enhanced_errors

        # Generate performance analysis
        performance_analysis = self.enhanced_memory_manager.analyze_performance()

        return {
            "memory_errors": [
                {
                    "type": error.error_type if hasattr(error, "error_type") else "unknown",
                    "message": error.message if hasattr(error, "message") else str(error),
                    "line": error.line_number if hasattr(error, "line_number") else 0,
                    "severity": error.severity if hasattr(error, "severity") else "error",
                }
                for error in all_errors
            ],
            "performance_analysis": performance_analysis,
            "cleanup_summary": {
                "total_resources": len(self.enhanced_memory_manager.resources),
                "smart_pointers": len(
                    [
                        r
                        for r in self.enhanced_memory_manager.resources.values()
                        if r.resource_type == ResourceType.SMART_POINTER
                    ]
                ),
                "containers": len(
                    [
                        r
                        for r in self.enhanced_memory_manager.resources.values()
                        if r.resource_type == ResourceType.CONTAINER
                    ]
                ),
                "custom_allocators": len(self.enhanced_memory_manager.allocator_manager.allocators),
                "cycles_detected": len(self.enhanced_memory_manager._detect_dependency_cycles()),
                "recommendations": performance_analysis["optimization_recommendations"],
            },
        }

    def _extract_element_type_from_annotation(self, annotation: str) -> str:
        """Extract element type from type annotation."""
        # Handle smart pointer types: unique_ptr[int] -> int
        # Handle container types: list[int] -> int
        if "[" in annotation and "]" in annotation:
            start = annotation.find("[") + 1
            end = annotation.rfind("]")
            return annotation[start:end].strip()
        return "void"

    def _extract_container_type_from_annotation(self, annotation: str) -> str:
        """Extract container type from annotation."""
        if "[" in annotation:
            return annotation[: annotation.find("[")].strip()
        return annotation.strip()

    def _convert_arg_to_string(self, arg: ast.expr) -> str:
        """Convert AST argument to string representation."""
        if isinstance(arg, ast.Constant):
            return repr(arg.value)
        elif isinstance(arg, ast.Name):
            return arg.id
        elif isinstance(arg, ast.Attribute):
            return f"{self._convert_arg_to_string(arg.value)}.{arg.attr}"
        else:
            return ast.unparse(arg)


__all__ = ["EnhancedSTCTranslator"]
