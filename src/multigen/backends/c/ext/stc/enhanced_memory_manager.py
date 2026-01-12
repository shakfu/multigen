"""Enhanced Memory Management System.

This module integrates smart pointers, custom allocators, and STC containers
into a comprehensive memory management system with advanced safety guarantees.

Features:
- Smart pointer integration with containers
- Custom allocator support
- Memory safety analysis
- Reference cycle detection
- Performance optimization
- RAII semantics
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from .allocators import AllocatorInstance, AllocatorType, MemoryAllocatorManager
from .memory_manager import MemoryError, MemoryScope, STCMemoryManager
from .smart_pointers import SmartPointerManager, SmartPointerType


class ResourceType(Enum):
    """Types of managed resources."""

    CONTAINER = "container"
    SMART_POINTER = "smart_pointer"
    RAW_ALLOCATION = "raw_allocation"
    FILE_HANDLE = "file_handle"
    NETWORK_SOCKET = "network_socket"


@dataclass
class ResourceAllocation:
    """Unified resource allocation tracking."""

    name: str
    resource_type: ResourceType
    data_type: str
    allocator: Optional[str] = None
    scope: MemoryScope = MemoryScope.BLOCK
    line_number: int = 0
    requires_cleanup: bool = True
    is_parameter: bool = False
    is_return_value: bool = False
    is_moved: bool = False
    reference_count: int = 1
    dependencies: set[str] = field(default_factory=set)  # Resources this depends on


class EnhancedMemoryManager:
    """Enhanced memory management system integrating smart pointers, allocators,
    and STC containers with comprehensive safety analysis.

    Provides:
    - Unified resource management
    - Smart pointer integration
    - Custom allocator support
    - Reference cycle detection
    - Memory leak prevention
    - Performance optimization
    """

    def __init__(self) -> None:
        # Component managers
        self.stc_manager = STCMemoryManager()
        self.smart_pointer_manager = SmartPointerManager()
        self.allocator_manager = MemoryAllocatorManager()

        # Unified resource tracking
        self.resources: dict[str, ResourceAllocation] = {}

        # Resource dependency graph
        self.dependency_graph: dict[str, set[str]] = {}

        # Scope management
        self.scope_stack: list[dict[str, ResourceAllocation]] = [{}]

        # Error tracking
        self.safety_errors: list[MemoryError] = []

        # Performance metrics
        self.performance_metrics: dict[str, Any] = {
            "total_allocations": 0,
            "smart_pointer_usage": 0,
            "custom_allocator_usage": 0,
            "cycles_detected": 0,
            "leaks_prevented": 0,
        }

    def register_container_with_allocator(
        self,
        name: str,
        container_type: str,
        element_type: str,
        allocator_name: Optional[str] = None,
        scope: MemoryScope = MemoryScope.BLOCK,
        line_number: int = 0,
    ) -> ResourceAllocation:
        """Register a container with optional custom allocator."""
        # Register with STC manager
        self.stc_manager.register_container(name, container_type, scope, line_number)

        # Create unified resource allocation
        resource = ResourceAllocation(
            name=name,
            resource_type=ResourceType.CONTAINER,
            data_type=f"{container_type}<{element_type}>",
            allocator=allocator_name,
            scope=scope,
            line_number=line_number,
        )

        self.resources[name] = resource
        self._add_to_current_scope(resource)

        # Bind to allocator if specified
        if allocator_name:
            self.allocator_manager.bind_container_to_allocator(name, allocator_name)
            self.performance_metrics["custom_allocator_usage"] += 1

        self.performance_metrics["total_allocations"] += 1
        return resource

    def register_smart_pointer(
        self,
        name: str,
        pointer_type: SmartPointerType,
        element_type: str,
        allocator_name: Optional[str] = None,
        is_array: bool = False,
        custom_deleter: Optional[str] = None,
        scope: MemoryScope = MemoryScope.BLOCK,
        line_number: int = 0,
    ) -> ResourceAllocation:
        """Register a smart pointer with optional custom allocator."""
        # Register with smart pointer manager
        self.smart_pointer_manager.register_smart_pointer(
            name, pointer_type, element_type, line_number, is_array, custom_deleter, allocator_name
        )

        # Create unified resource allocation
        resource = ResourceAllocation(
            name=name,
            resource_type=ResourceType.SMART_POINTER,
            data_type=f"{pointer_type.value}<{element_type}>",
            allocator=allocator_name,
            scope=scope,
            line_number=line_number,
        )

        self.resources[name] = resource
        self._add_to_current_scope(resource)

        self.performance_metrics["smart_pointer_usage"] += 1
        self.performance_metrics["total_allocations"] += 1
        return resource

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
        """Register a custom allocator."""
        return self.allocator_manager.register_allocator(
            name, allocator_type, block_size, pool_size, alignment, thread_safe, line_number
        )

    def track_resource_dependency(self, dependent: str, dependency: str) -> None:
        """Track dependency between resources."""
        if dependent in self.resources and dependency in self.resources:
            self.resources[dependent].dependencies.add(dependency)

            # Update dependency graph
            if dependent not in self.dependency_graph:
                self.dependency_graph[dependent] = set()
            self.dependency_graph[dependent].add(dependency)

            # Track smart pointer assignments
            if (
                self.resources[dependent].resource_type == ResourceType.SMART_POINTER
                and self.resources[dependency].resource_type == ResourceType.SMART_POINTER
            ):
                self.smart_pointer_manager.track_assignment(dependent, dependency)

    def detect_memory_issues(self) -> list[MemoryError]:
        """Comprehensive memory safety analysis."""
        self.safety_errors = []

        # Detect reference cycles
        cycles = self._detect_dependency_cycles()
        for cycle in cycles:
            self._add_error("reference_cycle", f"Reference cycle detected: {' -> '.join(cycle)}", 0, "warning")
            self.performance_metrics["cycles_detected"] += 1

        # Detect potential leaks
        potential_leaks = self._detect_potential_leaks()
        for leak in potential_leaks:
            self._add_error(
                "potential_leak",
                f"Resource {leak} may leak without proper cleanup",
                self.resources[leak].line_number,
                "warning",
            )

        # Detect use-after-move violations
        moved_violations = self._detect_use_after_move()
        for violation in moved_violations:
            self._add_error("use_after_move", f"Potential use of {violation} after move", 0, "error")

        # Detect double-free risks
        double_free_risks = self._detect_double_free_risks()
        for risk in double_free_risks:
            self._add_error("double_free_risk", f"Potential double-free of {risk}", 0, "error")

        return self.safety_errors

    def generate_initialization_code(self) -> tuple[list[str], list[str]]:
        """Generate initialization code for all allocators and smart pointers."""
        includes = []
        init_code = []

        # Generate allocator initialization
        for _allocator_name, allocator_instance in self.allocator_manager.allocators.items():
            init, include = self.allocator_manager.generate_allocator_setup(allocator_instance)
            if include:
                includes.append(include)
            if init:
                init_code.append(init)

        # Generate smart pointer type definitions
        for _pointer_name, pointer_alloc in self.smart_pointer_manager.allocations.items():
            type_def, include = self.smart_pointer_manager.generate_smart_pointer_type_def(pointer_alloc)
            if include:
                includes.append(include)
            if type_def:
                init_code.append(type_def)

        # Generate container-allocator bindings
        for container_name, _allocator_name in self.allocator_manager.container_allocators.items():
            if container_name in self.resources:
                resource = self.resources[container_name]
                if resource.resource_type == ResourceType.CONTAINER:
                    element_type = self._extract_element_type(resource.data_type)
                    container_type = self._extract_container_type(resource.data_type)
                    type_def, include = self.allocator_manager.generate_container_with_allocator(
                        container_name, container_type, element_type
                    )
                    if include:
                        includes.append(include)
                    if type_def:
                        init_code.append(type_def)

        # Remove duplicates while preserving order
        unique_includes = list(dict.fromkeys(includes))
        unique_init = list(dict.fromkeys(init_code))

        return unique_includes, unique_init

    def generate_cleanup_code(self, scope: MemoryScope = MemoryScope.BLOCK) -> list[str]:
        """Generate comprehensive cleanup code for current scope."""
        cleanup_code = []

        # Get resources in current scope
        current_scope = self.scope_stack[-1] if self.scope_stack else {}

        # Sort resources by dependency order (dependencies first)
        sorted_resources = self._sort_by_dependencies(list(current_scope.keys()))

        # Generate cleanup in reverse dependency order
        for resource_name in reversed(sorted_resources):
            if resource_name not in self.resources:
                continue

            resource = self.resources[resource_name]

            if not self._needs_cleanup(resource):
                continue

            if resource.resource_type == ResourceType.CONTAINER:
                stc_cleanup = self.stc_manager._generate_cleanup_statement(self.stc_manager.allocations[resource_name])
                cleanup_code.append(stc_cleanup)

            elif resource.resource_type == ResourceType.SMART_POINTER:
                pointer_cleanup = self.smart_pointer_manager.generate_cleanup_code(resource_name)
                cleanup_code.extend(pointer_cleanup)

            elif resource.resource_type == ResourceType.RAW_ALLOCATION:
                if resource.allocator:
                    alloc_cleanup = self.allocator_manager.generate_deallocation_code(resource.allocator, resource_name)
                    cleanup_code.append(alloc_cleanup)

        # Generate allocator cleanup
        for allocator_name in self.allocator_manager.allocators:
            allocator_cleanup = self.allocator_manager.generate_allocator_cleanup(allocator_name)
            cleanup_code.extend(allocator_cleanup)

        return cleanup_code

    def generate_move_semantics(self, source: str, target: str) -> list[str]:
        """Generate move semantics code."""
        if source not in self.resources or target not in self.resources:
            return []

        source_resource = self.resources[source]
        self.resources[target]

        if source_resource.resource_type == ResourceType.SMART_POINTER:
            move_code = self.smart_pointer_manager.generate_move_semantics(source, target)
            if move_code:
                # Mark source as moved
                source_resource.is_moved = True
                source_resource.requires_cleanup = False
                return move_code

        return []

    def analyze_performance(self) -> dict[str, Any]:
        """Analyze memory management performance and provide recommendations."""
        analysis = {
            "metrics": self.performance_metrics.copy(),
            "allocator_analysis": self.allocator_manager.analyze_allocation_patterns(),
            "smart_pointer_analysis": self._analyze_smart_pointer_usage(),
            "optimization_recommendations": [],
        }

        # Generate optimization recommendations
        recommendations = []

        # Smart pointer recommendations
        if self.performance_metrics["smart_pointer_usage"] == 0:
            recommendations.append("Consider using smart pointers for automatic memory management")

        # Allocator recommendations
        if self.performance_metrics["custom_allocator_usage"] == 0:
            recommendations.append("Consider using custom allocators for performance-critical code")

        # Cycle recommendations
        if self.performance_metrics["cycles_detected"] > 0:
            recommendations.append(
                f"Detected {self.performance_metrics['cycles_detected']} reference cycles. "
                "Consider using weak_ptr to break cycles."
            )

        # Add allocator-specific recommendations
        allocator_analysis = analysis.get("allocator_analysis", {})
        if isinstance(allocator_analysis, dict):
            allocator_recommendations = allocator_analysis.get("recommendations", [])
            if isinstance(allocator_recommendations, list):
                recommendations.extend(allocator_recommendations)

        analysis["optimization_recommendations"] = recommendations

        return analysis

    def enter_scope(self, scope_type: MemoryScope = MemoryScope.BLOCK) -> None:
        """Enter a new scope."""
        self.scope_stack.append({})
        self.stc_manager.enter_scope(scope_type)

    def exit_scope(self) -> list[str]:
        """Exit current scope and generate cleanup."""
        cleanup_code = self.generate_cleanup_code()
        if len(self.scope_stack) > 1:
            self.scope_stack.pop()
        return cleanup_code

    def _add_to_current_scope(self, resource: ResourceAllocation) -> None:
        """Add resource to current scope."""
        if self.scope_stack:
            self.scope_stack[-1][resource.name] = resource

    def _detect_dependency_cycles(self) -> list[list[str]]:
        """Detect cycles in the dependency graph."""
        cycles = []
        visited = set()
        rec_stack = set()

        def dfs(node: str, path: list[str]) -> bool:
            if node in rec_stack:
                cycle_start = path.index(node)
                cycles.append(path[cycle_start:] + [node])
                return True

            if node in visited:
                return False

            visited.add(node)
            rec_stack.add(node)
            path.append(node)

            for neighbor in self.dependency_graph.get(node, set()):
                if dfs(neighbor, path.copy()):
                    return True

            rec_stack.remove(node)
            return False

        for node in self.dependency_graph:
            if node not in visited:
                dfs(node, [])

        return cycles

    def _detect_potential_leaks(self) -> list[str]:
        """Detect resources that may leak."""
        potential_leaks = []

        for name, resource in self.resources.items():
            if (
                resource.requires_cleanup
                and not resource.is_return_value
                and not resource.is_moved
                and resource.resource_type != ResourceType.SMART_POINTER
            ):
                potential_leaks.append(name)

        return potential_leaks

    def _detect_use_after_move(self) -> list[str]:
        """Detect potential use-after-move violations."""
        violations = []

        for name, resource in self.resources.items():
            if resource.is_moved and resource.dependencies:
                violations.append(name)

        return violations

    def _detect_double_free_risks(self) -> list[str]:
        """Detect potential double-free risks."""
        risks = []

        # Check for resources with multiple cleanup paths
        cleanup_counts: dict[str, int] = {}
        for name, resource in self.resources.items():
            if resource.requires_cleanup:
                cleanup_counts[name] = cleanup_counts.get(name, 0) + 1

        for name, count in cleanup_counts.items():
            if count > 1:
                risks.append(name)

        return risks

    def _sort_by_dependencies(self, resource_names: list[str]) -> list[str]:
        """Sort resources by dependency order."""
        # Topological sort of dependency graph
        visited = set()
        result = []

        def dfs(node: str) -> None:
            if node in visited or node not in resource_names:
                return
            visited.add(node)

            resource = self.resources.get(node, ResourceAllocation("", ResourceType.CONTAINER, ""))
            for dependency in resource.dependencies:
                dfs(dependency)

            result.append(node)

        for name in resource_names:
            dfs(name)

        return result

    def _needs_cleanup(self, resource: ResourceAllocation) -> bool:
        """Check if resource needs cleanup."""
        return (
            resource.requires_cleanup
            and not resource.is_parameter
            and not resource.is_return_value
            and not resource.is_moved
        )

    def _analyze_smart_pointer_usage(self) -> dict[str, Any]:
        """Analyze smart pointer usage patterns."""
        smart_pointer_resources = [r for r in self.resources.values() if r.resource_type == ResourceType.SMART_POINTER]

        analysis = {
            "total_smart_pointers": len(smart_pointer_resources),
            "by_type": {},
            "cycles": len(self.smart_pointer_manager.detect_reference_cycles()),
            "shared_ptr_usage": 0,
            "unique_ptr_usage": 0,
            "weak_ptr_usage": 0,
        }

        for resource in smart_pointer_resources:
            pointer_type = resource.data_type.split("<")[0]
            by_type_dict = analysis["by_type"]
            if isinstance(by_type_dict, dict):
                by_type_dict[pointer_type] = by_type_dict.get(pointer_type, 0) + 1

            if "shared_ptr" in pointer_type and "shared_ptr_usage" in analysis:
                shared_ptr_count = analysis["shared_ptr_usage"]
                if isinstance(shared_ptr_count, int):
                    analysis["shared_ptr_usage"] = shared_ptr_count + 1
            elif "unique_ptr" in pointer_type and "unique_ptr_usage" in analysis:
                unique_ptr_count = analysis["unique_ptr_usage"]
                if isinstance(unique_ptr_count, int):
                    analysis["unique_ptr_usage"] = unique_ptr_count + 1
            elif "weak_ptr" in pointer_type and "weak_ptr_usage" in analysis:
                weak_ptr_count = analysis["weak_ptr_usage"]
                if isinstance(weak_ptr_count, int):
                    analysis["weak_ptr_usage"] = weak_ptr_count + 1

        return analysis

    def _extract_element_type(self, data_type: str) -> str:
        """Extract element type from data type string."""
        if "<" in data_type and ">" in data_type:
            return data_type.split("<")[1].split(">")[0].split(",")[0].strip()
        return data_type

    def _extract_container_type(self, data_type: str) -> str:
        """Extract container type from data type string."""
        if "<" in data_type:
            return data_type.split("<")[0]
        return data_type

    def _add_error(self, error_type: str, message: str, line_number: int, severity: str) -> None:
        """Add a memory safety error."""
        self.safety_errors.append(
            MemoryError(error_type=error_type, message=message, line_number=line_number, severity=severity)
        )


__all__ = ["EnhancedMemoryManager", "ResourceType", "ResourceAllocation"]
