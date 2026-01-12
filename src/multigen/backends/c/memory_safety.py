"""Memory Safety Checker for C/C++ Backends.

Analyzes Python code for patterns that could lead to memory safety issues
when translated to C/C++ (manual memory management, raw pointers).
"""

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Optional


class MemorySafetyViolation(Enum):
    """Types of memory safety violations."""

    BUFFER_OVERFLOW = "buffer_overflow"
    NULL_DEREFERENCE = "null_dereference"
    MEMORY_LEAK = "memory_leak"
    DANGLING_POINTER = "dangling_pointer"


@dataclass
class MemorySafetyWarning:
    """Memory safety warning for C/C++ code generation."""

    violation_type: MemorySafetyViolation
    message: str
    line: int
    severity: str  # "warning" | "error"
    suggestion: Optional[str] = None


class MemorySafetyChecker:
    """C/C++ memory safety analysis for Python code.

    Detects Python patterns that could lead to memory safety issues
    when translated to C/C++ with manual memory management.
    """

    def __init__(self, language: str = "c"):
        """Initialize checker.

        Args:
            language: Target language ("c" or "cpp")
        """
        self.language = language
        self.warnings: list[MemorySafetyWarning] = []

    def check_code(self, source_code: str) -> list[MemorySafetyWarning]:
        """Analyze Python code for C/C++ memory safety issues.

        Args:
            source_code: Python source code to analyze

        Returns:
            List of memory safety warnings
        """
        self.warnings = []

        try:
            tree = ast.parse(source_code)
            self._check_buffer_overflows(tree)
            self._check_null_dereferences(tree)
            self._check_memory_leaks(tree)
            self._check_dangling_pointers(tree)
        except SyntaxError:
            # Invalid Python - will be caught by validation phase
            pass

        return self.warnings

    def _check_buffer_overflows(self, tree: ast.AST) -> None:
        """Detect potential buffer overflow patterns (MS001)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.Subscript):
                # Skip type annotations
                if self._is_type_annotation(node):
                    continue

                # Variable index without bounds check
                if isinstance(node.slice, ast.Name):
                    self.warnings.append(
                        MemorySafetyWarning(
                            violation_type=MemorySafetyViolation.BUFFER_OVERFLOW,
                            message=f"Array access with variable index '{node.slice.id}' - ensure bounds checking",
                            line=node.lineno,
                            severity="warning",
                            suggestion="Add explicit bounds check before array access",
                        )
                    )

                # Index at array length (off-by-one)
                if isinstance(node.slice, ast.Call):
                    if isinstance(node.slice.func, ast.Name) and node.slice.func.id == "len":
                        if len(node.slice.args) == 1:
                            # arr[len(arr)] is always out of bounds
                            self.warnings.append(
                                MemorySafetyWarning(
                                    violation_type=MemorySafetyViolation.BUFFER_OVERFLOW,
                                    message="Array index equals length (off-by-one error)",
                                    line=node.lineno,
                                    severity="error",
                                    suggestion="Use len(arr) - 1 for last element",
                                )
                            )

    def _check_null_dereferences(self, tree: ast.AST) -> None:
        """Detect potential null pointer dereference patterns (MS002)."""
        for node in ast.walk(tree):
            # Check for attribute access on potentially None values
            if isinstance(node, ast.Attribute):
                if isinstance(node.value, ast.Call):
                    # result = get_object()
                    # result.method()  # Could be None
                    self.warnings.append(
                        MemorySafetyWarning(
                            violation_type=MemorySafetyViolation.NULL_DEREFERENCE,
                            message="Attribute access on function return value - may be None",
                            line=node.lineno,
                            severity="warning",
                            suggestion="Add None check before attribute access",
                        )
                    )

    def _check_memory_leaks(self, tree: ast.AST) -> None:
        """Detect potential memory leak patterns (MS003)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                # Check for allocations without cleanup
                has_allocation = False
                has_cleanup = False

                for subnode in ast.walk(node):
                    # Detect allocation patterns (list creation, dict creation, etc.)
                    if isinstance(subnode, (ast.List, ast.Dict, ast.ListComp, ast.DictComp)):
                        if isinstance(subnode, (ast.List, ast.Dict)):
                            # Only flag non-empty allocations
                            if (isinstance(subnode, ast.List) and subnode.elts) or (
                                isinstance(subnode, ast.Dict) and subnode.keys
                            ):
                                has_allocation = True
                        else:
                            has_allocation = True

                    # Check for explicit cleanup (del statement)
                    if isinstance(subnode, ast.Delete):
                        has_cleanup = True

                # Warn if allocation without cleanup in non-main functions
                if has_allocation and not has_cleanup and node.name != "main":
                    self.warnings.append(
                        MemorySafetyWarning(
                            violation_type=MemorySafetyViolation.MEMORY_LEAK,
                            message=f"Function '{node.name}' allocates memory without explicit cleanup",
                            line=node.lineno,
                            severity="warning",
                            suggestion="Add explicit cleanup or ensure caller handles cleanup",
                        )
                    )

    def _check_dangling_pointers(self, tree: ast.AST) -> None:
        """Detect potential dangling pointer patterns (MS004)."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                # Check for returning references to local variables
                for subnode in ast.walk(node):
                    if isinstance(subnode, ast.Return):
                        if subnode.value:
                            # Check if returning a locally created list/dict
                            if isinstance(subnode.value, (ast.List, ast.Dict, ast.Set)):
                                # This is actually OK in C++ (return by value)
                                # But in C with pointers, would be dangling
                                if self.language == "c":
                                    self.warnings.append(
                                        MemorySafetyWarning(
                                            violation_type=MemorySafetyViolation.DANGLING_POINTER,
                                            message=f"Function '{node.name}' returns local container - use heap allocation",
                                            line=subnode.lineno,
                                            severity="warning",
                                            suggestion="Allocate on heap or use output parameter",
                                        )
                                    )

    def _is_type_annotation(self, node: ast.Subscript) -> bool:
        """Check if subscript is a type annotation, not array access."""
        if isinstance(node.value, ast.Name):
            type_names = {
                "list",
                "dict",
                "set",
                "tuple",
                "List",
                "Dict",
                "Set",
                "Tuple",
                "Optional",
                "Union",
                "Callable",
                "Sequence",
                "Mapping",
            }
            return node.value.id in type_names

        if isinstance(node.value, ast.Attribute):
            # typing.List, collections.abc.Sequence, etc.
            return True

        return False
