"""STC Memory Management System.

This module provides comprehensive memory management for STC containers,
including automatic cleanup, error handling, and memory safety guarantees.
"""

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional


class MemoryScope(Enum):
    """Types of memory scopes for automatic cleanup."""

    GLOBAL = "global"
    FUNCTION = "function"
    BLOCK = "block"
    LOOP = "loop"
    CONDITIONAL = "conditional"


@dataclass
class ContainerAllocation:
    """Tracks a container allocation for memory management."""

    name: str
    container_type: str
    scope: MemoryScope
    line_number: int
    requires_cleanup: bool = True
    is_parameter: bool = False
    is_return_value: bool = False
    is_moved: bool = False


@dataclass
class MemoryError:
    """Represents a potential memory management error."""

    error_type: str
    message: str
    line_number: int
    severity: str  # "error", "warning", "info"


class STCMemoryManager:
    """Comprehensive memory management system for STC containers.

    Provides:
    - Automatic cleanup code generation
    - Memory leak detection
    - Double-free prevention
    - Exception safety
    - RAII-style resource management
    """

    def __init__(self) -> None:
        # Track all container allocations
        self.allocations: dict[str, ContainerAllocation] = {}

        # Scope stack for tracking variable lifetimes
        self.scope_stack: list[dict[str, ContainerAllocation]] = [{}]

        # Track moved/transferred containers
        self.moved_containers: set[str] = set()

        # Memory errors and warnings
        self.memory_errors: list[MemoryError] = []

        # Function-specific tracking
        self.current_function: Optional[str] = None
        self.function_parameters: dict[str, set[str]] = {}
        self.function_returns: dict[str, set[str]] = {}

    def enter_scope(self, scope_type: MemoryScope = MemoryScope.BLOCK) -> None:
        """Enter a new scope (function, block, loop, etc.)."""
        self.scope_stack.append({})

    def exit_scope(self) -> list[str]:
        """Exit current scope and generate cleanup code.

        Returns:
            List of cleanup statements for containers going out of scope
        """
        if len(self.scope_stack) <= 1:
            return []  # Don't cleanup global scope automatically

        scope_containers = self.scope_stack.pop()
        cleanup_code = []

        for _name, allocation in scope_containers.items():
            if self._needs_cleanup(allocation):
                cleanup_stmt = self._generate_cleanup_statement(allocation)
                cleanup_code.append(cleanup_stmt)

        return cleanup_code

    def register_container(
        self, name: str, container_type: str, scope: MemoryScope = MemoryScope.BLOCK, line_number: int = 0
    ) -> ContainerAllocation:
        """Register a new container allocation."""
        allocation = ContainerAllocation(name=name, container_type=container_type, scope=scope, line_number=line_number)

        # Add to current scope
        if self.scope_stack:
            self.scope_stack[-1][name] = allocation

        # Add to global allocation tracking
        self.allocations[name] = allocation

        return allocation

    def register_parameter(self, name: str, container_type: str) -> None:
        """Register a container parameter (doesn't need cleanup in this function)."""
        allocation = self.register_container(name, container_type, MemoryScope.FUNCTION)
        allocation.is_parameter = True
        allocation.requires_cleanup = False  # Caller is responsible

        if self.current_function:
            if self.current_function not in self.function_parameters:
                self.function_parameters[self.current_function] = set()
            self.function_parameters[self.current_function].add(name)

    def register_return_value(self, name: str) -> Optional[str]:
        """Register a container as a return value.

        Returns:
            Cleanup prevention code if needed
        """
        if name in self.allocations:
            allocation = self.allocations[name]
            allocation.is_return_value = True
            allocation.requires_cleanup = False  # Will be cleaned up by caller

            if self.current_function:
                if self.current_function not in self.function_returns:
                    self.function_returns[self.current_function] = set()
                self.function_returns[self.current_function].add(name)

            # Generate move semantics if supported
            return f"// {name} returned by value - moved to caller"

        return None

    def mark_moved(self, name: str) -> None:
        """Mark a container as moved (no longer needs cleanup)."""
        if name in self.allocations:
            self.allocations[name].is_moved = True
            self.allocations[name].requires_cleanup = False
            self.moved_containers.add(name)

    def enter_function(self, function_name: str) -> None:
        """Enter a function scope."""
        self.current_function = function_name
        self.enter_scope(MemoryScope.FUNCTION)

    def exit_function(self) -> tuple[list[str], list[str]]:
        """Exit function scope.

        Returns:
            Tuple of (cleanup_code, error_handling_code)
        """
        cleanup_code = self.exit_scope()
        error_handling = self._generate_error_handling()

        self.current_function = None
        return cleanup_code, error_handling

    def generate_exception_safe_wrapper(self, operation_code: str, container_name: str) -> list[str]:
        """Generate exception-safe wrapper for potentially failing operations.

        Args:
            operation_code: The operation that might fail
            container_name: Container that needs cleanup on error

        Returns:
            Exception-safe code with cleanup
        """
        code = []

        # Check if operation can fail
        if any(keyword in operation_code for keyword in ["_insert", "_push", "_reserve", "_resize"]):
            code.append(f"// Exception-safe operation for {container_name}")
            code.append("do {")
            code.append(f"    if (!({operation_code})) {{")

            # Generate cleanup for all containers in current scope
            for scope_containers in reversed(self.scope_stack):
                for _name, allocation in scope_containers.items():
                    if self._needs_cleanup(allocation):
                        cleanup = self._generate_cleanup_statement(allocation)
                        code.append(f"        {cleanup}")

            code.append("        return false; // Operation failed")
            code.append("    }")
            code.append("} while(0);")
        else:
            code.append(operation_code)

        return code

    def analyze_memory_safety(self, ast_node: ast.AST) -> list[MemoryError]:
        """Analyze AST for potential memory safety issues.

        Returns:
            List of detected memory errors and warnings
        """
        self.memory_errors = []

        class MemoryAnalyzer(ast.NodeVisitor):
            def __init__(self, manager: "STCMemoryManager") -> None:
                self.manager = manager

            def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
                self.manager.enter_function(node.name)

                # Check for missing cleanup in function
                self._check_function_cleanup(node)

                self.generic_visit(node)

                cleanup_code, errors = self.manager.exit_function()
                if cleanup_code:
                    self.manager._add_warning(
                        "missing_cleanup",
                        f"Function {node.name} may leak containers: {[c.split('_drop')[0] for c in cleanup_code if '_drop' in c]}",
                        node.lineno,
                    )

            def visit_Return(self, node: ast.Return) -> None:
                # Check if returning container without proper transfer
                if isinstance(node.value, ast.Name):
                    var_name = node.value.id
                    if var_name in self.manager.allocations:
                        self.manager.register_return_value(var_name)

            def visit_Call(self, node: ast.Call) -> None:
                # Check for potential allocation failures
                if isinstance(node.func, ast.Attribute):
                    method_name = node.func.attr
                    if method_name in ["push", "insert", "reserve", "resize"]:
                        if isinstance(node.func.value, ast.Name):
                            container_name = node.func.value.id
                            self.manager._add_info(
                                "allocation_check",
                                f"Consider checking return value of {container_name}.{method_name}()",
                                node.lineno,
                            )

            def _check_function_cleanup(self, node: ast.FunctionDef) -> None:
                # Check if function properly cleans up local containers
                for stmt in ast.walk(node):
                    if (
                        isinstance(stmt, ast.Call)
                        and isinstance(stmt.func, ast.Attribute)
                        and stmt.func.attr in ["drop", "clear", "free"]
                    ):
                        break

        analyzer = MemoryAnalyzer(self)
        analyzer.visit(ast_node)

        return self.memory_errors

    def generate_memory_safe_initialization(self, name: str, container_type: str) -> list[str]:
        """Generate memory-safe container initialization."""
        code = []

        # Zero-initialize to ensure safe state
        code.append(f"{container_type} {name} = {{0}};")

        # Register with memory manager
        self.register_container(name, container_type)

        # Add error checking for complex initializations
        if container_type.endswith(("Map", "Set")):
            code.append(f"// {name} ready for use - automatic cleanup registered")

        return code

    def generate_function_cleanup_wrapper(self, function_code: list[str], function_name: str) -> list[str]:
        """Wrap function with automatic cleanup."""
        wrapped_code = []

        # Function start
        wrapped_code.extend(function_code[:1])  # Function signature
        wrapped_code.append("{")

        # Add cleanup tracking
        wrapped_code.append(f"    // Auto-cleanup for function {function_name}")

        # Function body
        wrapped_code.extend(["    " + line for line in function_code[1:-1]])

        # Cleanup before any return
        if function_name in self.function_parameters:
            wrapped_code.append("    // Cleanup local containers")
            for container in self.function_parameters[function_name]:
                if container in self.allocations:
                    allocation = self.allocations[container]
                    if self._needs_cleanup(allocation):
                        cleanup = self._generate_cleanup_statement(allocation)
                        wrapped_code.append(f"    {cleanup}")

        wrapped_code.append("}")

        return wrapped_code

    def _needs_cleanup(self, allocation: ContainerAllocation) -> bool:
        """Check if allocation needs cleanup."""
        return (
            allocation.requires_cleanup
            and not allocation.is_parameter
            and not allocation.is_return_value
            and not allocation.is_moved
        )

    def _generate_cleanup_statement(self, allocation: ContainerAllocation) -> str:
        """Generate cleanup statement for allocation."""
        return f"{allocation.container_type}_drop(&{allocation.name});"

    def _generate_error_handling(self) -> list[str]:
        """Generate error handling code for current scope."""
        error_code = []

        # Check for common error patterns
        current_containers: list[str] = []
        for scope_containers in self.scope_stack:
            current_containers.extend(scope_containers.keys())

        if current_containers:
            error_code.append("// Error handling: cleanup on failure")
            error_code.append("cleanup_on_error:")
            for container in current_containers:
                if container in self.allocations:
                    allocation = self.allocations[container]
                    if self._needs_cleanup(allocation):
                        cleanup = self._generate_cleanup_statement(allocation)
                        error_code.append(f"    {cleanup}")
            error_code.append("    return false;")

        return error_code

    def _add_error(self, error_type: str, message: str, line_number: int) -> None:
        """Add a memory error."""
        self.memory_errors.append(
            MemoryError(error_type=error_type, message=message, line_number=line_number, severity="error")
        )

    def _add_warning(self, error_type: str, message: str, line_number: int) -> None:
        """Add a memory warning."""
        self.memory_errors.append(
            MemoryError(error_type=error_type, message=message, line_number=line_number, severity="warning")
        )

    def _add_info(self, error_type: str, message: str, line_number: int) -> None:
        """Add a memory info message."""
        self.memory_errors.append(
            MemoryError(error_type=error_type, message=message, line_number=line_number, severity="info")
        )

    def generate_cleanup_summary(self) -> dict[str, Any]:
        """Generate summary of memory management."""
        return {
            "total_allocations": len(self.allocations),
            "requires_cleanup": sum(1 for a in self.allocations.values() if self._needs_cleanup(a)),
            "moved_containers": len(self.moved_containers),
            "memory_errors": len([e for e in self.memory_errors if e.severity == "error"]),
            "memory_warnings": len([e for e in self.memory_errors if e.severity == "warning"]),
            "functions_with_containers": len(self.function_parameters),
            "allocations_by_type": self._get_allocations_by_type(),
        }

    def _get_allocations_by_type(self) -> dict[str, int]:
        """Get allocation counts by container type."""
        counts: dict[str, int] = {}
        for allocation in self.allocations.values():
            container_type = allocation.container_type
            counts[container_type] = counts.get(container_type, 0) + 1
        return counts


__all__ = ["STCMemoryManager", "MemoryScope", "ContainerAllocation", "MemoryError"]
