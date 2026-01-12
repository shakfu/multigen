"""Memory Safety and Bounds Verification.

This module provides formal verification of memory safety properties,
including bounds checking, buffer overflow prevention, and pointer safety.
"""

import ast
import time
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union

try:
    import z3  # type: ignore[import-untyped]

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    z3 = None  # type: ignore[assignment]


from ..base import AnalysisContext
from .theorem_prover import ProofProperty, ProofResult, ProofStatus, PropertyType, TheoremProver


class MemorySafetyType(Enum):
    """Types of memory safety properties."""

    BUFFER_OVERFLOW = "buffer_overflow"
    BOUNDS_VIOLATION = "bounds_violation"
    NULL_POINTER_DEREFERENCE = "null_pointer_dereference"
    USE_AFTER_FREE = "use_after_free"
    DOUBLE_FREE = "double_free"
    UNINITIALIZED_READ = "uninitialized_read"
    STACK_OVERFLOW = "stack_overflow"
    HEAP_CORRUPTION = "heap_corruption"


@dataclass
class MemoryRegion:
    """Represents a memory region with bounds and properties."""

    name: str
    base_address: Union[str, int]
    size: Union[str, int]
    element_type: str
    is_heap: bool = False
    is_stack: bool = True
    is_null_terminated: bool = False
    initialization_status: str = "unknown"  # "initialized", "uninitialized", "unknown"


@dataclass
class MemoryAccess:
    """Represents a memory access operation."""

    region: MemoryRegion
    access_type: str  # "read", "write", "address"
    offset: Union[str, int]
    size: int
    line_number: int
    is_safe: Optional[bool] = None


@dataclass
class MemorySafetyProof:
    """Result of memory safety verification."""

    function_name: str
    safety_type: MemorySafetyType
    is_safe: bool
    proof_results: list[ProofResult]
    unsafe_accesses: list[MemoryAccess]
    recommendations: list[str]
    verification_time: float
    confidence: float  # 0.0 to 1.0

    @property
    def summary(self) -> str:
        """Human-readable summary of the proof."""
        status = "SAFE" if self.is_safe else "UNSAFE"
        return f"{self.function_name}: {self.safety_type.value} - {status} (confidence: {self.confidence:.2f})"


class BoundsProver:
    """Formal verification of memory bounds and safety properties."""

    def __init__(self, theorem_prover: Optional[TheoremProver] = None):
        """Initialize the bounds prover.

        Args:
            theorem_prover: Z3 theorem prover instance
        """
        self.theorem_prover = theorem_prover or TheoremProver()
        self.z3_available = Z3_AVAILABLE

        # Track memory regions and their properties
        self.memory_regions: dict[str, MemoryRegion] = {}
        self.memory_accesses: list[MemoryAccess] = []

    def verify_memory_safety(self, context: AnalysisContext) -> MemorySafetyProof:
        """Verify memory safety for a function.

        Args:
            context: Analysis context with AST and metadata

        Returns:
            MemorySafetyProof with verification results
        """
        start_time = time.time()

        # Extract memory operations from AST
        memory_extractor = MemoryOperationExtractor()
        memory_extractor.visit(context.ast_node)

        self.memory_regions = memory_extractor.memory_regions
        self.memory_accesses = memory_extractor.memory_accesses

        # Verify different types of memory safety
        all_proof_results = []
        unsafe_accesses = []

        # 1. Bounds checking verification
        bounds_results = self._verify_bounds_safety()
        all_proof_results.extend(bounds_results)

        # 2. Null pointer verification
        null_results = self._verify_null_pointer_safety()
        all_proof_results.extend(null_results)

        # 3. Buffer overflow verification
        overflow_results = self._verify_buffer_overflow_safety()
        all_proof_results.extend(overflow_results)

        # Determine overall safety status
        is_safe = all(result.is_verified for result in all_proof_results)

        # Find unsafe accesses
        for access in self.memory_accesses:
            if access.is_safe is False:
                unsafe_accesses.append(access)

        # Calculate confidence based on proof results
        confidence = self._calculate_confidence(all_proof_results)

        # Generate recommendations
        recommendations = self._generate_recommendations(unsafe_accesses, all_proof_results)

        verification_time = time.time() - start_time

        function_name = "unknown"
        if context.analysis_result and hasattr(context.analysis_result, "functions"):
            if context.analysis_result.functions:
                function_name = list(context.analysis_result.functions.keys())[0]
        elif hasattr(context.ast_node, "name"):
            function_name = context.ast_node.name

        return MemorySafetyProof(
            function_name=function_name,
            safety_type=MemorySafetyType.BOUNDS_VIOLATION,  # Primary type
            is_safe=is_safe,
            proof_results=all_proof_results,
            unsafe_accesses=unsafe_accesses,
            recommendations=recommendations,
            verification_time=verification_time,
            confidence=confidence,
        )

    def _verify_bounds_safety(self) -> list[ProofResult]:
        """Verify bounds checking for all memory accesses."""
        results = []

        for access in self.memory_accesses:
            if access.access_type in ["read", "write"]:
                # Create bounds checking property
                prop = self._create_bounds_property(access)
                result = self.theorem_prover.verify_property(prop)
                results.append(result)

                # Update access safety status
                access.is_safe = result.is_verified

        return results

    def _verify_null_pointer_safety(self) -> list[ProofResult]:
        """Verify null pointer dereference safety."""
        results = []

        for _region_name, region in self.memory_regions.items():
            if region.base_address == 0 or region.base_address == "null":
                # Create null pointer safety property
                prop = self._create_null_pointer_property(region)
                result = self.theorem_prover.verify_property(prop)
                results.append(result)

        return results

    def _verify_buffer_overflow_safety(self) -> list[ProofResult]:
        """Verify buffer overflow prevention."""
        results = []

        # Group accesses by memory region
        region_accesses: dict[str, list[MemoryAccess]] = {}
        for access in self.memory_accesses:
            region_name = access.region.name
            if region_name not in region_accesses:
                region_accesses[region_name] = []
            region_accesses[region_name].append(access)

        # Verify each region for buffer overflows
        for region_name, accesses in region_accesses.items():
            self.memory_regions[region_name]

            # Check for potential buffer overflows
            for access in accesses:
                if access.access_type == "write":
                    prop = self._create_buffer_overflow_property(access)
                    result = self.theorem_prover.verify_property(prop)
                    results.append(result)

        return results

    def _create_bounds_property(self, access: MemoryAccess) -> ProofProperty:
        """Create a bounds checking property for a memory access."""
        region = access.region

        if not self.z3_available:
            return ProofProperty(
                name=f"bounds_check_{region.name}",
                property_type=PropertyType.BOUNDS_CHECKING,
                description=f"Access to {region.name} at offset {access.offset} is within bounds",
                z3_formula="mock_formula",
            )

        # Create Z3 variables
        offset = z3.Int(f"offset_{region.name}")
        size = z3.Int(f"size_{region.name}")

        # Set concrete values if known
        if isinstance(access.offset, int):
            offset = z3.IntVal(access.offset)
        if isinstance(region.size, int):
            size = z3.IntVal(region.size)

        # Bounds property: 0 <= offset < size
        bounds_formula = z3.And(offset >= 0, offset < size)

        return ProofProperty(
            name=f"bounds_check_{region.name}_{access.line_number}",
            property_type=PropertyType.BOUNDS_CHECKING,
            description=f"Access to {region.name} at offset {access.offset} is within bounds",
            z3_formula=bounds_formula,
            context={"access": access, "region": region},
        )

    def _create_null_pointer_property(self, region: MemoryRegion) -> ProofProperty:
        """Create a null pointer safety property."""
        if not self.z3_available:
            return ProofProperty(
                name=f"null_check_{region.name}",
                property_type=PropertyType.NULL_POINTER_SAFETY,
                description=f"Region {region.name} is not null",
                z3_formula="mock_formula",
            )

        # Create Z3 variable for base address
        base_addr = z3.Int(f"base_{region.name}")

        # Null safety: base_addr != 0
        null_safety_formula = base_addr != 0

        return ProofProperty(
            name=f"null_check_{region.name}",
            property_type=PropertyType.NULL_POINTER_SAFETY,
            description=f"Region {region.name} is not null",
            z3_formula=null_safety_formula,
            context={"region": region},
        )

    def _create_buffer_overflow_property(self, access: MemoryAccess) -> ProofProperty:
        """Create a buffer overflow prevention property."""
        region = access.region

        if not self.z3_available:
            return ProofProperty(
                name=f"overflow_check_{region.name}",
                property_type=PropertyType.MEMORY_SAFETY,
                description=f"Write to {region.name} does not cause buffer overflow",
                z3_formula="mock_formula",
            )

        # Create Z3 variables
        offset = z3.Int(f"offset_{region.name}")
        write_size = z3.IntVal(access.size)
        buffer_size = z3.Int(f"size_{region.name}")

        # Set concrete values if known
        if isinstance(access.offset, int):
            offset = z3.IntVal(access.offset)
        if isinstance(region.size, int):
            buffer_size = z3.IntVal(region.size)

        # Overflow prevention: offset + write_size <= buffer_size
        overflow_formula = offset + write_size <= buffer_size

        return ProofProperty(
            name=f"overflow_check_{region.name}_{access.line_number}",
            property_type=PropertyType.MEMORY_SAFETY,
            description=f"Write to {region.name} does not cause buffer overflow",
            z3_formula=overflow_formula,
            context={"access": access, "region": region},
        )

    def _calculate_confidence(self, proof_results: list[ProofResult]) -> float:
        """Calculate confidence based on proof results."""
        if not proof_results:
            return 0.0

        proved_count = sum(1 for result in proof_results if result.status == ProofStatus.PROVED)
        unknown_count = sum(1 for result in proof_results if result.status == ProofStatus.UNKNOWN)
        total_count = len(proof_results)

        # Full confidence for proved properties, partial for unknown
        confidence = (proved_count + unknown_count * 0.5) / total_count
        return min(confidence, 1.0)

    def _generate_recommendations(
        self, unsafe_accesses: list[MemoryAccess], proof_results: list[ProofResult]
    ) -> list[str]:
        """Generate recommendations for improving memory safety."""
        recommendations = []

        if unsafe_accesses:
            recommendations.append("Add bounds checking before array/buffer accesses")
            recommendations.append("Consider using safer data structures (e.g., std::vector)")

        failed_proofs = [r for r in proof_results if r.status == ProofStatus.DISPROVED]
        if failed_proofs:
            recommendations.append("Review memory access patterns in failed verification points")

        unknown_proofs = [r for r in proof_results if r.status == ProofStatus.UNKNOWN]
        if unknown_proofs:
            recommendations.append("Add explicit bounds information to help verification")
            recommendations.append("Consider simplifying complex indexing expressions")

        if not recommendations:
            recommendations.append("Memory safety verification passed - code appears safe")

        return recommendations


class MemoryOperationExtractor(ast.NodeVisitor):
    """Extract memory operations and regions from Python AST."""

    def __init__(self) -> None:
        self.memory_regions: dict[str, MemoryRegion] = {}
        self.memory_accesses: list[MemoryAccess] = []
        self.current_function: Optional[str] = None

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Track function context."""
        self.current_function = node.name
        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign) -> None:
        """Extract memory region declarations."""
        if len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
            var_name = node.targets[0].id

            # Check for list/array initialization
            if isinstance(node.value, ast.List):
                size = len(node.value.elts)
                element_type = self._infer_element_type(node.value.elts)

                region = MemoryRegion(
                    name=var_name,
                    base_address=f"&{var_name}",
                    size=size,
                    element_type=element_type,
                    initialization_status="initialized",
                )
                self.memory_regions[var_name] = region

            # Check for explicit array allocation patterns
            elif isinstance(node.value, ast.Call):
                if isinstance(node.value.func, ast.Name) and node.value.func.id == "list":
                    # list(size) or similar
                    if node.value.args:
                        list_size = self._extract_size_info(node.value.args[0])
                        region = MemoryRegion(
                            name=var_name,
                            base_address=f"&{var_name}",
                            size=list_size,
                            element_type="unknown",
                            initialization_status="uninitialized",
                        )
                        self.memory_regions[var_name] = region

        self.generic_visit(node)

    def visit_Subscript(self, node: ast.Subscript) -> None:
        """Extract memory access operations."""
        if isinstance(node.value, ast.Name):
            array_name = node.value.id

            # Ensure we have a memory region for this array
            if array_name not in self.memory_regions:
                # Create unknown region
                self.memory_regions[array_name] = MemoryRegion(
                    name=array_name, base_address=f"&{array_name}", size="unknown", element_type="unknown"
                )

            region = self.memory_regions[array_name]

            # Extract index information
            offset = self._extract_offset_info(node.slice)

            # Determine access type from context
            access_type = "read"  # Default
            # Note: In a more complete implementation, we'd analyze the context
            # to determine if this is a read or write access

            access = MemoryAccess(
                region=region,
                access_type=access_type,
                offset=offset,
                size=1,  # Assume single element access
                line_number=node.lineno,
            )
            self.memory_accesses.append(access)

        self.generic_visit(node)

    def _infer_element_type(self, elements: list[ast.expr]) -> str:
        """Infer element type from list elements."""
        if not elements:
            return "unknown"

        first_elem = elements[0]
        if isinstance(first_elem, ast.Constant):
            if isinstance(first_elem.value, int):
                return "int"
            elif isinstance(first_elem.value, float):
                return "double"
            elif isinstance(first_elem.value, str):
                return "char*"
            elif isinstance(first_elem.value, bool):
                return "bool"

        return "unknown"

    def _extract_size_info(self, size_node: ast.expr) -> Union[str, int]:
        """Extract size information from AST node."""
        if isinstance(size_node, ast.Constant):
            if isinstance(size_node.value, (int, str)):
                return size_node.value
            else:
                return str(size_node.value) if size_node.value is not None else "unknown"
        elif isinstance(size_node, ast.Name):
            return size_node.id
        else:
            return "complex_expression"

    def _extract_offset_info(self, slice_node: ast.expr) -> Union[str, int]:
        """Extract offset information from slice node."""
        if isinstance(slice_node, ast.Constant):
            if isinstance(slice_node.value, (int, str)):
                return slice_node.value
            else:
                return str(slice_node.value) if slice_node.value is not None else "unknown"
        elif isinstance(slice_node, ast.Name):
            return slice_node.id
        else:
            return "complex_index"
