"""Smart Pointer System for STC Integration.

This module provides C++ style smart pointers implemented using STC containers,
offering memory safety, RAII semantics, and automatic resource management.

Features:
- unique_ptr: Exclusive ownership, move semantics
- shared_ptr: Reference counting, shared ownership
- weak_ptr: Non-owning references, cycle breaking
- Integration with STC containers
- Custom deleters and allocators
"""

from dataclasses import dataclass
from enum import Enum
from typing import Optional


class SmartPointerType(Enum):
    """Types of smart pointers supported."""

    UNIQUE = "unique_ptr"
    SHARED = "shared_ptr"
    WEAK = "weak_ptr"
    SCOPED = "scoped_ptr"


@dataclass
class SmartPointerSpec:
    """Specification for a smart pointer type."""

    stc_name: str  # STC implementation name
    header_file: str  # Header to include
    pointer_type: SmartPointerType
    description: str
    supports_custom_deleter: bool = True
    supports_arrays: bool = False
    thread_safe: bool = False


# Smart pointer type mappings
SMART_POINTER_SPECS = {
    SmartPointerType.UNIQUE: SmartPointerSpec(
        stc_name="unique_ptr",
        header_file="stc/unique_ptr.h",
        pointer_type=SmartPointerType.UNIQUE,
        description="Exclusive ownership smart pointer with move semantics",
        supports_custom_deleter=True,
        supports_arrays=True,
        thread_safe=False,
    ),
    SmartPointerType.SHARED: SmartPointerSpec(
        stc_name="shared_ptr",
        header_file="stc/shared_ptr.h",
        pointer_type=SmartPointerType.SHARED,
        description="Reference-counted shared ownership smart pointer",
        supports_custom_deleter=True,
        supports_arrays=False,
        thread_safe=True,
    ),
    SmartPointerType.WEAK: SmartPointerSpec(
        stc_name="weak_ptr",
        header_file="stc/weak_ptr.h",
        pointer_type=SmartPointerType.WEAK,
        description="Non-owning weak reference to shared_ptr",
        supports_custom_deleter=False,
        supports_arrays=False,
        thread_safe=True,
    ),
    SmartPointerType.SCOPED: SmartPointerSpec(
        stc_name="scoped_ptr",
        header_file="stc/scoped_ptr.h",
        pointer_type=SmartPointerType.SCOPED,
        description="RAII scoped pointer for automatic cleanup",
        supports_custom_deleter=True,
        supports_arrays=True,
        thread_safe=False,
    ),
}


@dataclass
class SmartPointerAllocation:
    """Tracks a smart pointer allocation."""

    name: str
    pointer_type: SmartPointerType
    element_type: str
    line_number: int
    is_array: bool = False
    custom_deleter: Optional[str] = None
    allocator: Optional[str] = None
    reference_count: int = 1  # For shared_ptr tracking


class SmartPointerManager:
    """Manages smart pointer allocations and generates appropriate STC code.

    Provides:
    - Smart pointer type resolution
    - Memory-safe operations
    - Reference cycle detection
    - Integration with STC containers
    """

    def __init__(self) -> None:
        # Track all smart pointer allocations
        self.allocations: dict[str, SmartPointerAllocation] = {}

        # Track reference relationships for cycle detection
        self.reference_graph: dict[str, set[str]] = {}

        # Track shared_ptr reference counts
        self.shared_references: dict[str, int] = {}

        # Track weak_ptr relationships
        self.weak_references: dict[str, set[str]] = {}

        # Generated type definitions
        self.generated_types: set[str] = set()

    def register_smart_pointer(
        self,
        name: str,
        pointer_type: SmartPointerType,
        element_type: str,
        line_number: int = 0,
        is_array: bool = False,
        custom_deleter: Optional[str] = None,
        allocator: Optional[str] = None,
    ) -> SmartPointerAllocation:
        """Register a new smart pointer allocation."""
        allocation = SmartPointerAllocation(
            name=name,
            pointer_type=pointer_type,
            element_type=element_type,
            line_number=line_number,
            is_array=is_array,
            custom_deleter=custom_deleter,
            allocator=allocator,
        )

        self.allocations[name] = allocation

        # Initialize reference tracking
        self.reference_graph[name] = set()

        if pointer_type == SmartPointerType.SHARED:
            self.shared_references[name] = 1

        return allocation

    def generate_smart_pointer_type_def(self, allocation: SmartPointerAllocation) -> tuple[str, str]:
        """Generate STC smart pointer type definition."""
        spec = SMART_POINTER_SPECS[allocation.pointer_type]

        # Generate unique type name
        type_name = self._generate_type_name(allocation)

        if type_name in self.generated_types:
            return "", f"#include <{spec.header_file}>"

        self.generated_types.add(type_name)

        # Generate type definition based on pointer type
        if allocation.pointer_type == SmartPointerType.UNIQUE:
            type_def = self._generate_unique_ptr_def(allocation, type_name)
        elif allocation.pointer_type == SmartPointerType.SHARED:
            type_def = self._generate_shared_ptr_def(allocation, type_name)
        elif allocation.pointer_type == SmartPointerType.WEAK:
            type_def = self._generate_weak_ptr_def(allocation, type_name)
        else:  # SCOPED
            type_def = self._generate_scoped_ptr_def(allocation, type_name)

        include = f"#include <{spec.header_file}>"

        return type_def, include

    def generate_smart_pointer_operation(
        self, operation: str, pointer_name: str, args: Optional[list[str]] = None
    ) -> Optional[str]:
        """Generate smart pointer operation code."""
        if pointer_name not in self.allocations:
            return None

        allocation = self.allocations[pointer_name]
        type_name = self._generate_type_name(allocation)
        args = args or []

        # Map operations to STC smart pointer functions
        operation_mappings = {
            # Common operations
            "reset": "reset",
            "release": "release",
            "get": "get",
            "operator*": "deref",
            "operator->": "arrow",
            "operator bool": "has_value",
            # unique_ptr specific
            "move": "move",
            # shared_ptr specific
            "use_count": "use_count",
            "unique": "unique",
            # weak_ptr specific
            "expired": "expired",
            "lock": "lock",
        }

        stc_operation = operation_mappings.get(operation, operation)

        if args:
            args_str = ", ".join(args)
            return f"{type_name}_{stc_operation}(&{pointer_name}, {args_str})"
        else:
            return f"{type_name}_{stc_operation}(&{pointer_name})"

    def generate_make_smart_pointer(
        self, pointer_type: SmartPointerType, element_type: str, args: Optional[list[str]] = None
    ) -> str:
        """Generate make_unique, make_shared, etc. calls."""
        args = args or []
        args_str = ", ".join(args) if args else ""

        if pointer_type == SmartPointerType.UNIQUE:
            return f"make_unique_{element_type}({args_str})"
        elif pointer_type == SmartPointerType.SHARED:
            return f"make_shared_{element_type}({args_str})"
        else:
            # Direct construction for other types
            type_name = f"{pointer_type.value}_{element_type}"
            return f"{type_name}_create({args_str})"

    def track_assignment(self, target: str, source: str) -> None:
        """Track smart pointer assignments for reference counting."""
        if source in self.allocations and target in self.allocations:
            source_alloc = self.allocations[source]
            target_alloc = self.allocations[target]

            # Update reference graph
            self.reference_graph[target].add(source)

            # Handle shared_ptr reference counting
            if source_alloc.pointer_type == SmartPointerType.SHARED:
                if target_alloc.pointer_type == SmartPointerType.SHARED:
                    self.shared_references[source] += 1
                elif target_alloc.pointer_type == SmartPointerType.WEAK:
                    if source not in self.weak_references:
                        self.weak_references[source] = set()
                    self.weak_references[source].add(target)

    def detect_reference_cycles(self) -> list[list[str]]:
        """Detect reference cycles in smart pointer graph."""
        cycles = []
        visited = set()
        rec_stack = set()

        def dfs(node: str, path: list[str]) -> bool:
            if node in rec_stack:
                # Found cycle
                cycle_start = path.index(node)
                cycles.append(path[cycle_start:] + [node])
                return True

            if node in visited:
                return False

            visited.add(node)
            rec_stack.add(node)
            path.append(node)

            for neighbor in self.reference_graph.get(node, set()):
                if dfs(neighbor, path.copy()):
                    return True

            rec_stack.remove(node)
            return False

        # Check all nodes
        for node in self.reference_graph:
            if node not in visited:
                dfs(node, [])

        return cycles

    def generate_cleanup_code(self, pointer_name: str) -> list[str]:
        """Generate cleanup code for smart pointer."""
        if pointer_name not in self.allocations:
            return []

        allocation = self.allocations[pointer_name]
        type_name = self._generate_type_name(allocation)

        cleanup_code = []

        # Generate appropriate cleanup based on pointer type
        if allocation.pointer_type == SmartPointerType.UNIQUE:
            cleanup_code.append(f"{type_name}_reset(&{pointer_name}, NULL);")
        elif allocation.pointer_type == SmartPointerType.SHARED:
            cleanup_code.append(f"{type_name}_drop(&{pointer_name});")
        elif allocation.pointer_type == SmartPointerType.WEAK:
            cleanup_code.append(f"{type_name}_reset(&{pointer_name});")
        else:  # SCOPED
            cleanup_code.append(f"{type_name}_drop(&{pointer_name});")

        return cleanup_code

    def generate_move_semantics(self, source: str, target: str) -> list[str]:
        """Generate move semantics for smart pointers."""
        if source not in self.allocations or target not in self.allocations:
            return []

        source_alloc = self.allocations[source]
        target_alloc = self.allocations[target]

        # Only unique_ptr supports move semantics
        if source_alloc.pointer_type == SmartPointerType.UNIQUE:
            self._generate_type_name(source_alloc)
            target_type = self._generate_type_name(target_alloc)

            return [f"{target_type}_move(&{target}, &{source});", f"// {source} is now empty after move"]

        return []

    def _generate_type_name(self, allocation: SmartPointerAllocation) -> str:
        """Generate unique type name for smart pointer."""
        base_name = f"{allocation.pointer_type.value}_{allocation.element_type}"

        if allocation.is_array:
            base_name += "_array"

        if allocation.custom_deleter:
            base_name += f"_del_{allocation.custom_deleter}"

        if allocation.allocator:
            base_name += f"_alloc_{allocation.allocator}"

        return base_name

    def _generate_unique_ptr_def(self, allocation: SmartPointerAllocation, type_name: str) -> str:
        """Generate unique_ptr type definition."""
        element_type = allocation.element_type

        if allocation.custom_deleter:
            return f"#define T {type_name}, {element_type}, {allocation.custom_deleter}"
        else:
            return f"#define T {type_name}, {element_type}"

    def _generate_shared_ptr_def(self, allocation: SmartPointerAllocation, type_name: str) -> str:
        """Generate shared_ptr type definition."""
        element_type = allocation.element_type

        if allocation.custom_deleter:
            return f"#define T {type_name}, {element_type}, {allocation.custom_deleter}"
        else:
            return f"#define T {type_name}, {element_type}"

    def _generate_weak_ptr_def(self, allocation: SmartPointerAllocation, type_name: str) -> str:
        """Generate weak_ptr type definition."""
        element_type = allocation.element_type
        return f"#define T {type_name}, {element_type}"

    def _generate_scoped_ptr_def(self, allocation: SmartPointerAllocation, type_name: str) -> str:
        """Generate scoped_ptr type definition."""
        element_type = allocation.element_type

        if allocation.custom_deleter:
            return f"#define T {type_name}, {element_type}, {allocation.custom_deleter}"
        else:
            return f"#define T {type_name}, {element_type}"


__all__ = [
    "SmartPointerType",
    "SmartPointerSpec",
    "SmartPointerAllocation",
    "SmartPointerManager",
    "SMART_POINTER_SPECS",
]
