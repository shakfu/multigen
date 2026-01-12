"""Memory Allocator System for STC Integration.

This module provides advanced memory allocation strategies including arena allocators,
pool allocators, and stack allocators for high-performance memory management.

Features:
- Arena allocators: Linear allocation with bulk deallocation
- Pool allocators: Fixed-size block allocation
- Stack allocators: LIFO allocation with automatic cleanup
- Free list allocators: General purpose with fragmentation handling
- Integration with STC containers and smart pointers
"""

from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional


class AllocatorType(Enum):
    """Types of memory allocators supported."""

    ARENA = "arena"
    POOL = "pool"
    STACK = "stack"
    FREE_LIST = "free_list"
    SYSTEM = "system"


@dataclass
class AllocatorSpec:
    """Specification for a memory allocator type."""

    stc_name: str  # STC implementation name
    header_file: str  # Header to include
    allocator_type: AllocatorType
    description: str
    supports_alignment: bool = True
    supports_custom_size: bool = True
    thread_safe: bool = False
    suitable_for_containers: bool = True


# Memory allocator specifications
ALLOCATOR_SPECS = {
    AllocatorType.ARENA: AllocatorSpec(
        stc_name="arena_alloc",
        header_file="stc/arena_alloc.h",
        allocator_type=AllocatorType.ARENA,
        description="Linear allocator with bulk deallocation",
        supports_alignment=True,
        supports_custom_size=True,
        thread_safe=False,
        suitable_for_containers=True,
    ),
    AllocatorType.POOL: AllocatorSpec(
        stc_name="pool_alloc",
        header_file="stc/pool_alloc.h",
        allocator_type=AllocatorType.POOL,
        description="Fixed-size block allocator with O(1) allocation/deallocation",
        supports_alignment=True,
        supports_custom_size=False,  # Fixed size
        thread_safe=True,
        suitable_for_containers=True,
    ),
    AllocatorType.STACK: AllocatorSpec(
        stc_name="stack_alloc",
        header_file="stc/stack_alloc.h",
        allocator_type=AllocatorType.STACK,
        description="LIFO allocator with automatic scope-based cleanup",
        supports_alignment=True,
        supports_custom_size=True,
        thread_safe=False,
        suitable_for_containers=False,  # Scope-limited
    ),
    AllocatorType.FREE_LIST: AllocatorSpec(
        stc_name="free_list_alloc",
        header_file="stc/free_list_alloc.h",
        allocator_type=AllocatorType.FREE_LIST,
        description="General purpose allocator with fragmentation handling",
        supports_alignment=True,
        supports_custom_size=True,
        thread_safe=True,
        suitable_for_containers=True,
    ),
    AllocatorType.SYSTEM: AllocatorSpec(
        stc_name="system_alloc",
        header_file="stdlib.h",
        allocator_type=AllocatorType.SYSTEM,
        description="Standard system allocator (malloc/free)",
        supports_alignment=False,
        supports_custom_size=True,
        thread_safe=True,
        suitable_for_containers=True,
    ),
}


@dataclass
class AllocatorInstance:
    """Represents an allocator instance."""

    name: str
    allocator_type: AllocatorType
    block_size: Optional[int] = None
    pool_size: Optional[int] = None
    alignment: int = 8
    thread_safe: bool = False
    line_number: int = 0


@dataclass
class AllocationInfo:
    """Tracks an allocation made with a specific allocator."""

    variable_name: str
    allocator_name: str
    size: Optional[int]
    element_type: str
    is_array: bool = False
    line_number: int = 0


class MemoryAllocatorManager:
    """Manages memory allocators and their integration with STC containers.

    Provides:
    - Allocator instance management
    - Allocation tracking
    - Performance optimization
    - Memory usage analysis
    """

    def __init__(self) -> None:
        # Track allocator instances
        self.allocators: dict[str, AllocatorInstance] = {}

        # Track allocations per allocator
        self.allocations: dict[str, list[AllocationInfo]] = {}

        # Track container-allocator bindings
        self.container_allocators: dict[str, str] = {}

        # Performance metrics
        self.allocation_stats: dict[str, dict[str, int]] = {}

        # Generated type definitions
        self.generated_types: set[str] = set()

    def register_allocator(
        self,
        name: str,
        allocator_type: AllocatorType,
        block_size: Optional[int] = None,
        pool_size: Optional[int] = None,
        alignment: int = 8,
        thread_safe: bool = False,
        line_number: int = 0,
    ) -> AllocatorInstance:
        """Register a new allocator instance."""
        instance = AllocatorInstance(
            name=name,
            allocator_type=allocator_type,
            block_size=block_size,
            pool_size=pool_size,
            alignment=alignment,
            thread_safe=thread_safe,
            line_number=line_number,
        )

        self.allocators[name] = instance
        self.allocations[name] = []
        self.allocation_stats[name] = {
            "total_allocations": 0,
            "total_size": 0,
            "peak_usage": 0,
            "fragmentation_events": 0,
        }

        return instance

    def generate_allocator_setup(self, instance: AllocatorInstance) -> tuple[str, str]:
        """Generate allocator initialization code."""
        spec = ALLOCATOR_SPECS[instance.allocator_type]
        type_name = f"{spec.stc_name}_{instance.name}"

        if type_name in self.generated_types:
            return "", f"#include <{spec.header_file}>"

        self.generated_types.add(type_name)

        # Generate type definition
        type_def = self._generate_allocator_type_def(instance, type_name)

        # Generate initialization code
        init_code = self._generate_allocator_init_code(instance, type_name)

        include = f"#include <{spec.header_file}>"

        return f"{type_def}\n{init_code}", include

    def bind_container_to_allocator(self, container_name: str, allocator_name: str) -> None:
        """Bind a container to use a specific allocator."""
        if allocator_name in self.allocators:
            self.container_allocators[container_name] = allocator_name

    def generate_container_with_allocator(
        self, container_name: str, container_type: str, element_type: str
    ) -> tuple[str, str]:
        """Generate container definition with custom allocator."""
        if container_name not in self.container_allocators:
            return "", ""

        allocator_name = self.container_allocators[container_name]
        if allocator_name not in self.allocators:
            return "", ""

        allocator_instance = self.allocators[allocator_name]
        spec = ALLOCATOR_SPECS[allocator_instance.allocator_type]

        # Generate container type with allocator
        if allocator_instance.allocator_type == AllocatorType.SYSTEM:
            # Standard container without custom allocator
            type_def = f"#define T {container_name}_{container_type}, {element_type}"
        else:
            # Container with custom allocator
            allocator_type_name = f"{spec.stc_name}_{allocator_name}"
            type_def = f"#define T {container_name}_{container_type}, {element_type}, {allocator_type_name}"

        include = f"#include <{spec.header_file}>"
        return type_def, include

    def track_allocation(
        self,
        variable_name: str,
        allocator_name: str,
        size: Optional[int],
        element_type: str,
        is_array: bool = False,
        line_number: int = 0,
    ) -> None:
        """Track an allocation made with a specific allocator."""
        if allocator_name not in self.allocators:
            return

        allocation_info = AllocationInfo(
            variable_name=variable_name,
            allocator_name=allocator_name,
            size=size,
            element_type=element_type,
            is_array=is_array,
            line_number=line_number,
        )

        self.allocations[allocator_name].append(allocation_info)

        # Update statistics
        stats = self.allocation_stats[allocator_name]
        stats["total_allocations"] += 1
        if size:
            stats["total_size"] += size

    def generate_allocation_code(self, allocator_name: str, variable_name: str, size: str, element_type: str) -> str:
        """Generate allocation code using specific allocator."""
        if allocator_name not in self.allocators:
            return f"{element_type}* {variable_name} = malloc({size});"

        instance = self.allocators[allocator_name]
        spec = ALLOCATOR_SPECS[instance.allocator_type]
        type_name = f"{spec.stc_name}_{allocator_name}"

        if instance.allocator_type == AllocatorType.ARENA:
            return f"{element_type}* {variable_name} = {type_name}_alloc(&{allocator_name}, {size});"
        elif instance.allocator_type == AllocatorType.POOL:
            return f"{element_type}* {variable_name} = {type_name}_alloc(&{allocator_name});"
        elif instance.allocator_type == AllocatorType.STACK:
            return f"{element_type}* {variable_name} = {type_name}_alloc(&{allocator_name}, {size});"
        elif instance.allocator_type == AllocatorType.FREE_LIST:
            return f"{element_type}* {variable_name} = {type_name}_alloc(&{allocator_name}, {size});"
        else:  # SYSTEM
            return f"{element_type}* {variable_name} = malloc({size});"

    def generate_deallocation_code(self, allocator_name: str, variable_name: str) -> str:
        """Generate deallocation code for specific allocator."""
        if allocator_name not in self.allocators:
            return f"free({variable_name});"

        instance = self.allocators[allocator_name]
        spec = ALLOCATOR_SPECS[instance.allocator_type]
        type_name = f"{spec.stc_name}_{allocator_name}"

        if instance.allocator_type == AllocatorType.ARENA:
            return f"// {variable_name} freed with arena reset"
        elif instance.allocator_type == AllocatorType.POOL:
            return f"{type_name}_free(&{allocator_name}, {variable_name});"
        elif instance.allocator_type == AllocatorType.STACK:
            return f"// {variable_name} freed with stack pop"
        elif instance.allocator_type == AllocatorType.FREE_LIST:
            return f"{type_name}_free(&{allocator_name}, {variable_name});"
        else:  # SYSTEM
            return f"free({variable_name});"

    def generate_allocator_cleanup(self, allocator_name: str) -> list[str]:
        """Generate cleanup code for allocator."""
        if allocator_name not in self.allocators:
            return []

        instance = self.allocators[allocator_name]
        spec = ALLOCATOR_SPECS[instance.allocator_type]
        type_name = f"{spec.stc_name}_{allocator_name}"

        cleanup_code = []

        if instance.allocator_type == AllocatorType.ARENA:
            cleanup_code.append(f"{type_name}_clear(&{allocator_name});")
            cleanup_code.append(f"{type_name}_drop(&{allocator_name});")
        elif instance.allocator_type == AllocatorType.POOL:
            cleanup_code.append(f"{type_name}_clear(&{allocator_name});")
            cleanup_code.append(f"{type_name}_drop(&{allocator_name});")
        elif instance.allocator_type == AllocatorType.STACK:
            cleanup_code.append(f"{type_name}_clear(&{allocator_name});")
            cleanup_code.append(f"{type_name}_drop(&{allocator_name});")
        elif instance.allocator_type == AllocatorType.FREE_LIST:
            cleanup_code.append(f"{type_name}_clear(&{allocator_name});")
            cleanup_code.append(f"{type_name}_drop(&{allocator_name});")

        return cleanup_code

    def analyze_allocation_patterns(self) -> dict[str, Any]:
        """Analyze allocation patterns for optimization suggestions."""
        analysis: dict[str, Any] = {"allocators": {}, "recommendations": [], "total_allocations": 0, "total_memory": 0}

        for allocator_name, allocations in self.allocations.items():
            if not allocations:
                continue

            instance = self.allocators[allocator_name]
            stats = self.allocation_stats[allocator_name]

            allocator_analysis: dict[str, Any] = {
                "type": instance.allocator_type.value,
                "allocation_count": len(allocations),
                "total_size": stats["total_size"],
                "average_size": stats["total_size"] / len(allocations) if allocations else 0,
                "size_distribution": self._analyze_size_distribution(allocations),
                "fragmentation_risk": self._assess_fragmentation_risk(allocations, instance),
            }

            analysis["allocators"][allocator_name] = allocator_analysis
            analysis["total_allocations"] += len(allocations)
            total_size: int = stats["total_size"]
            analysis["total_memory"] += total_size

        # Generate recommendations
        analysis["recommendations"] = self._generate_optimization_recommendations(analysis)

        return analysis

    def _generate_allocator_type_def(self, instance: AllocatorInstance, type_name: str) -> str:
        """Generate allocator type definition."""
        if instance.allocator_type == AllocatorType.ARENA:
            if instance.block_size:
                return f"#define T {type_name}, {instance.block_size}"
            else:
                return f"#define T {type_name}"
        elif instance.allocator_type == AllocatorType.POOL:
            if instance.block_size and instance.pool_size:
                return f"#define T {type_name}, {instance.block_size}, {instance.pool_size}"
            else:
                return f"#define T {type_name}"
        elif instance.allocator_type == AllocatorType.STACK:
            if instance.block_size:
                return f"#define T {type_name}, {instance.block_size}"
            else:
                return f"#define T {type_name}"
        elif instance.allocator_type == AllocatorType.FREE_LIST:
            return f"#define T {type_name}"
        else:  # SYSTEM
            return ""

    def _generate_allocator_init_code(self, instance: AllocatorInstance, type_name: str) -> str:
        """Generate allocator initialization code."""
        if instance.allocator_type == AllocatorType.SYSTEM:
            return ""

        init_params = []
        if instance.block_size:
            init_params.append(str(instance.block_size))
        if instance.pool_size:
            init_params.append(str(instance.pool_size))

        params_str = ", ".join(init_params) if init_params else ""
        return f"{type_name} {instance.name} = {type_name}_init({params_str});"

    def _analyze_size_distribution(self, allocations: list[AllocationInfo]) -> dict[str, int]:
        """Analyze size distribution of allocations."""
        distribution = {
            "small": 0,  # < 64 bytes
            "medium": 0,  # 64-1024 bytes
            "large": 0,  # > 1024 bytes
            "unknown": 0,  # No size info
        }

        for allocation in allocations:
            if allocation.size is None:
                distribution["unknown"] += 1
            elif allocation.size < 64:
                distribution["small"] += 1
            elif allocation.size <= 1024:
                distribution["medium"] += 1
            else:
                distribution["large"] += 1

        return distribution

    def _assess_fragmentation_risk(self, allocations: list[AllocationInfo], instance: AllocatorInstance) -> str:
        """Assess fragmentation risk for allocator."""
        if instance.allocator_type in [AllocatorType.ARENA, AllocatorType.STACK]:
            return "low"  # Linear allocators have low fragmentation

        if instance.allocator_type == AllocatorType.POOL:
            return "none"  # Fixed-size blocks eliminate fragmentation

        # For free list allocators, assess based on allocation patterns
        if len(allocations) > 100:
            return "high"
        elif len(allocations) > 20:
            return "medium"
        else:
            return "low"

    def _generate_optimization_recommendations(self, analysis: dict[str, Any]) -> list[str]:
        """Generate optimization recommendations based on analysis."""
        recommendations = []

        for allocator_name, allocator_data in analysis["allocators"].items():
            allocator_type = allocator_data["type"]
            allocation_count = allocator_data["allocation_count"]
            fragmentation_risk = allocator_data["fragmentation_risk"]

            if allocator_type == "system" and allocation_count > 50:
                recommendations.append(
                    f"Consider using a custom allocator for {allocator_name} with {allocation_count} allocations"
                )

            if fragmentation_risk == "high":
                recommendations.append(
                    f"High fragmentation risk detected for {allocator_name}. "
                    f"Consider using a pool allocator for fixed-size allocations."
                )

            if allocator_data["size_distribution"]["small"] > 80:
                recommendations.append(
                    f"Many small allocations detected for {allocator_name}. A pool allocator would be more efficient."
                )

        return recommendations


__all__ = [
    "AllocatorType",
    "AllocatorSpec",
    "AllocatorInstance",
    "AllocationInfo",
    "MemoryAllocatorManager",
    "ALLOCATOR_SPECS",
]
