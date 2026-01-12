"""Memory Bounds Checking Analyzer.

This module provides memory safety analysis capabilities for Python code,
detecting potential buffer overflows, array access violations, and memory safety issues
that could occur when the code is converted to C.
"""

import ast
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from ..base import AnalysisContext, AnalysisLevel, AnalysisReport, BaseAnalyzer


class BoundsViolationType(Enum):
    """Types of bounds violations that can be detected."""

    ARRAY_INDEX_OUT_OF_BOUNDS = "array_index_out_of_bounds"
    NEGATIVE_INDEX = "negative_index"
    BUFFER_OVERFLOW = "buffer_overflow"
    UNINITIALIZED_ACCESS = "uninitialized_access"
    NULL_POINTER_DEREFERENCE = "null_pointer_dereference"
    DOUBLE_FREE = "double_free"
    USE_AFTER_FREE = "use_after_free"
    MEMORY_LEAK = "memory_leak"


class MemoryRegionType(Enum):
    """Types of memory regions."""

    STACK = "stack"
    HEAP = "heap"
    STATIC = "static"
    PARAMETER = "parameter"
    UNKNOWN = "unknown"


@dataclass
class MemoryRegion:
    """Represents a memory region with bounds information."""

    name: str
    region_type: MemoryRegionType
    size: Optional[int] = None  # Size in elements, None = unknown size
    element_type: str = "int"
    is_allocated: bool = True
    is_initialized: bool = False
    allocation_line: int = 0
    last_access_line: int = 0
    bounds: tuple[int, int] = (0, 0)  # (min_index, max_index)
    metadata: dict[str, Any] = field(default_factory=dict)

    def is_bounded(self) -> bool:
        """Check if this region has known bounds."""
        return self.size is not None

    def is_index_safe(self, index: int) -> bool:
        """Check if an index is safe for this region."""
        if not self.is_bounded():
            return False  # Unknown, assume unsafe
        assert self.size is not None
        return 0 <= index < self.size

    def is_range_safe(self, start: int, end: int) -> bool:
        """Check if a range access is safe for this region."""
        if not self.is_bounded():
            return False
        assert self.size is not None
        return 0 <= start <= end < self.size


@dataclass
class BoundsViolation:
    """Represents a detected bounds violation."""

    violation_type: BoundsViolationType
    variable_name: str
    line_number: int
    description: str
    severity: str = "error"  # error, warning, info
    confidence: float = 1.0  # 0.0 to 1.0
    suggested_fix: str = ""
    context: dict[str, Any] = field(default_factory=dict)


@dataclass
class BoundsCheckingReport(AnalysisReport):
    """Extended analysis report for bounds checking."""

    memory_regions: dict[str, MemoryRegion] = field(default_factory=dict)
    violations: list[BoundsViolation] = field(default_factory=list)
    safe_accesses: int = 0
    unsafe_accesses: int = 0
    unknown_accesses: int = 0
    memory_usage_estimate: int = 0  # Estimated memory usage in bytes


class BoundsChecker(BaseAnalyzer):
    """Memory bounds checking analyzer for Python code."""

    def __init__(self, analysis_level: AnalysisLevel = AnalysisLevel.BASIC):
        super().__init__("BoundsChecker", analysis_level)
        self._memory_regions: dict[str, MemoryRegion] = {}
        self._current_line = 0

    def analyze(self, context: AnalysisContext) -> BoundsCheckingReport:
        """Perform bounds checking analysis on the given context."""
        start_time = time.time()

        try:
            # Initialize analysis state
            self._memory_regions = {}
            self._current_line = 0

            # Analyze the AST
            violations: list[BoundsViolation] = []

            if isinstance(context.ast_node, ast.FunctionDef):
                violations.extend(self._analyze_function(context.ast_node))
            elif isinstance(context.ast_node, ast.Module):
                violations.extend(self._analyze_module(context.ast_node))
            else:
                # Cast to stmt for type safety
                stmt_node = context.ast_node if isinstance(context.ast_node, ast.stmt) else ast.Pass()
                violations.extend(self._analyze_statements([stmt_node]))

            # Calculate statistics
            safe_accesses = sum(1 for v in violations if v.severity == "info")
            unsafe_accesses = sum(1 for v in violations if v.severity == "error")
            unknown_accesses = sum(1 for v in violations if v.severity == "warning")

            execution_time = (time.time() - start_time) * 1000

            return BoundsCheckingReport(
                analyzer_name=self.name,
                success=True,
                confidence=0.85,  # High confidence for bounds checking
                findings=self._generate_findings(violations),
                warnings=[v.description for v in violations if v.severity == "warning"],
                errors=[v.description for v in violations if v.severity == "error"],
                metadata={
                    "memory_regions_count": len(self._memory_regions),
                    "analysis_level": self.analysis_level.value,
                },
                execution_time_ms=execution_time,
                memory_regions=self._memory_regions.copy(),
                violations=violations,
                safe_accesses=safe_accesses,
                unsafe_accesses=unsafe_accesses,
                unknown_accesses=unknown_accesses,
                memory_usage_estimate=self._estimate_memory_usage(),
            )

        except Exception as e:
            execution_time = (time.time() - start_time) * 1000
            return BoundsCheckingReport(
                analyzer_name=self.name,
                success=False,
                confidence=0.0,
                findings=[],
                warnings=[],
                errors=[f"Bounds checking analysis failed: {str(e)}"],
                metadata={},
                execution_time_ms=execution_time,
            )

    def _analyze_function(self, func_node: ast.FunctionDef) -> list[BoundsViolation]:
        """Analyze a function definition for bounds violations."""
        violations = []

        # Set up function parameters as memory regions
        for arg in func_node.args.args:
            # Assume parameters are initialized and safe initially
            param_region = MemoryRegion(
                name=arg.arg,
                region_type=MemoryRegionType.PARAMETER,
                is_initialized=True,
                allocation_line=func_node.lineno,
            )
            self._memory_regions[arg.arg] = param_region

        # Analyze function body
        violations.extend(self._analyze_statements(func_node.body))

        return violations

    def _analyze_module(self, module_node: ast.Module) -> list[BoundsViolation]:
        """Analyze a module for bounds violations."""
        return self._analyze_statements(module_node.body)

    def _analyze_statements(self, statements: list[ast.stmt]) -> list[BoundsViolation]:
        """Analyze a list of statements for bounds violations."""
        violations = []

        for stmt in statements:
            self._current_line = getattr(stmt, "lineno", self._current_line)
            violations.extend(self._analyze_statement(stmt))

        return violations

    def _analyze_statement(self, stmt: ast.stmt) -> list[BoundsViolation]:
        """Analyze a single statement for bounds violations."""
        violations = []

        if isinstance(stmt, ast.Assign):
            violations.extend(self._analyze_assignment(stmt))
        elif isinstance(stmt, ast.AugAssign):
            violations.extend(self._analyze_aug_assignment(stmt))
        elif isinstance(stmt, ast.For):
            violations.extend(self._analyze_for_loop(stmt))
        elif isinstance(stmt, ast.While):
            violations.extend(self._analyze_while_loop(stmt))
        elif isinstance(stmt, ast.If):
            violations.extend(self._analyze_if_statement(stmt))
        elif isinstance(stmt, ast.Expr):
            violations.extend(self._analyze_expression_statement(stmt))
        elif isinstance(stmt, ast.Return):
            violations.extend(self._analyze_return_statement(stmt))

        return violations

    def _analyze_assignment(self, assign_stmt: ast.Assign) -> list[BoundsViolation]:
        """Analyze an assignment statement."""
        violations = []

        # Analyze the right-hand side for any memory accesses
        violations.extend(self._analyze_expression(assign_stmt.value))

        # Handle assignment targets
        for target in assign_stmt.targets:
            if isinstance(target, ast.Name):
                # Simple variable assignment
                var_name = target.id
                violations.extend(self._handle_variable_assignment(var_name, assign_stmt.value))
            elif isinstance(target, ast.Subscript):
                # Array/list element assignment
                violations.extend(self._analyze_subscript_assignment(target, assign_stmt.value))

        return violations

    def _analyze_aug_assignment(self, aug_assign_stmt: ast.AugAssign) -> list[BoundsViolation]:
        """Analyze an augmented assignment statement."""
        violations = []

        # Check the target access first
        if isinstance(aug_assign_stmt.target, ast.Subscript):
            violations.extend(self._analyze_subscript_access(aug_assign_stmt.target))

        # Then analyze the value expression
        violations.extend(self._analyze_expression(aug_assign_stmt.value))

        return violations

    def _analyze_for_loop(self, for_stmt: ast.For) -> list[BoundsViolation]:
        """Analyze a for loop for bounds violations."""
        violations = []

        # Analyze the iterable
        violations.extend(self._analyze_expression(for_stmt.iter))

        # Set up loop variable as a memory region
        if isinstance(for_stmt.target, ast.Name):
            loop_var = for_stmt.target.id
            # Assume loop variable is properly bounded by the iterable
            loop_region = MemoryRegion(
                name=loop_var, region_type=MemoryRegionType.STACK, is_initialized=True, allocation_line=for_stmt.lineno
            )
            self._memory_regions[loop_var] = loop_region

        # Analyze loop body
        violations.extend(self._analyze_statements(for_stmt.body))

        # Analyze else clause if present
        if for_stmt.orelse:
            violations.extend(self._analyze_statements(for_stmt.orelse))

        return violations

    def _analyze_while_loop(self, while_stmt: ast.While) -> list[BoundsViolation]:
        """Analyze a while loop for bounds violations."""
        violations = []

        # Analyze the condition
        violations.extend(self._analyze_expression(while_stmt.test))

        # Analyze loop body
        violations.extend(self._analyze_statements(while_stmt.body))

        # Analyze else clause if present
        if while_stmt.orelse:
            violations.extend(self._analyze_statements(while_stmt.orelse))

        return violations

    def _analyze_if_statement(self, if_stmt: ast.If) -> list[BoundsViolation]:
        """Analyze an if statement for bounds violations."""
        violations = []

        # Analyze the condition
        violations.extend(self._analyze_expression(if_stmt.test))

        # Analyze if body
        violations.extend(self._analyze_statements(if_stmt.body))

        # Analyze else body if present
        if if_stmt.orelse:
            violations.extend(self._analyze_statements(if_stmt.orelse))

        return violations

    def _analyze_expression_statement(self, expr_stmt: ast.Expr) -> list[BoundsViolation]:
        """Analyze an expression statement."""
        return self._analyze_expression(expr_stmt.value)

    def _analyze_return_statement(self, return_stmt: ast.Return) -> list[BoundsViolation]:
        """Analyze a return statement."""
        if return_stmt.value:
            return self._analyze_expression(return_stmt.value)
        return []

    def _analyze_expression(self, expr: ast.expr) -> list[BoundsViolation]:
        """Analyze an expression for memory accesses and bounds violations."""
        violations = []

        if isinstance(expr, ast.Subscript):
            violations.extend(self._analyze_subscript_access(expr))
        elif isinstance(expr, ast.Call):
            violations.extend(self._analyze_function_call(expr))
        elif isinstance(expr, ast.BinOp):
            violations.extend(self._analyze_expression(expr.left))
            violations.extend(self._analyze_expression(expr.right))
        elif isinstance(expr, ast.UnaryOp):
            violations.extend(self._analyze_expression(expr.operand))
        elif isinstance(expr, ast.Compare):
            violations.extend(self._analyze_expression(expr.left))
            for comparator in expr.comparators:
                violations.extend(self._analyze_expression(comparator))
        elif isinstance(expr, ast.BoolOp):
            for value in expr.values:
                violations.extend(self._analyze_expression(value))
        elif isinstance(expr, ast.List):
            for elt in expr.elts:
                violations.extend(self._analyze_expression(elt))
        elif isinstance(expr, ast.Name):
            violations.extend(self._analyze_variable_access(expr))

        return violations

    def _analyze_subscript_access(self, subscript: ast.Subscript) -> list[BoundsViolation]:
        """Analyze a subscript access for bounds violations."""
        violations = []

        # Get the variable being subscripted
        if isinstance(subscript.value, ast.Name):
            var_name = subscript.value.id
            memory_region = self._memory_regions.get(var_name)

            if memory_region is None:
                # Unknown variable - potential issue
                violations.append(
                    BoundsViolation(
                        violation_type=BoundsViolationType.UNINITIALIZED_ACCESS,
                        variable_name=var_name,
                        line_number=self._current_line,
                        description=f"Access to undefined variable '{var_name}'",
                        severity="warning",
                        confidence=0.8,
                        suggested_fix=f"Initialize variable '{var_name}' before use",
                    )
                )
                return violations

            # Analyze the index
            if isinstance(subscript.slice, ast.Constant):
                # Constant index - can check bounds precisely
                index = subscript.slice.value
                if isinstance(index, int):
                    violations.extend(self._check_constant_index(var_name, index, memory_region))
            elif isinstance(subscript.slice, ast.UnaryOp) and isinstance(subscript.slice.op, ast.USub):
                # Negative index like arr[-5]
                if isinstance(subscript.slice.operand, ast.Constant):
                    operand_value = subscript.slice.operand.value
                    if isinstance(operand_value, (int, float)):
                        index = -int(operand_value)
                        violations.extend(self._check_constant_index(var_name, index, memory_region))
            elif isinstance(subscript.slice, ast.Name):
                # Variable index - need to track the index variable
                index_var = subscript.slice.id
                violations.extend(self._check_variable_index(var_name, index_var, memory_region))
            elif isinstance(subscript.slice, ast.Slice):
                # Slice access - check slice bounds
                violations.extend(self._check_slice_access(var_name, subscript.slice, memory_region))
            else:
                # Complex index expression
                violations.append(
                    BoundsViolation(
                        violation_type=BoundsViolationType.ARRAY_INDEX_OUT_OF_BOUNDS,
                        variable_name=var_name,
                        line_number=self._current_line,
                        description=f"Complex index expression for '{var_name}' - bounds cannot be verified",
                        severity="warning",
                        confidence=0.5,
                        suggested_fix="Use simple integer indices for better bounds checking",
                    )
                )

            # Update last access line
            memory_region.last_access_line = self._current_line

        return violations

    def _analyze_subscript_assignment(self, target: ast.Subscript, value: ast.expr) -> list[BoundsViolation]:
        """Analyze a subscript assignment."""
        violations = []

        # First check the subscript access itself
        violations.extend(self._analyze_subscript_access(target))

        # Then analyze the assigned value
        violations.extend(self._analyze_expression(value))

        return violations

    def _analyze_function_call(self, call: ast.Call) -> list[BoundsViolation]:
        """Analyze a function call for potential memory issues."""
        violations = []

        # Analyze all arguments
        for arg in call.args:
            violations.extend(self._analyze_expression(arg))

        for keyword in call.keywords:
            violations.extend(self._analyze_expression(keyword.value))

        # Check for specific dangerous functions
        if isinstance(call.func, ast.Name):
            func_name = call.func.id
            if func_name in ["malloc", "calloc", "realloc"]:
                violations.append(
                    BoundsViolation(
                        violation_type=BoundsViolationType.MEMORY_LEAK,
                        variable_name=func_name,
                        line_number=self._current_line,
                        description=f"Memory allocation with '{func_name}' - ensure proper deallocation",
                        severity="warning",
                        confidence=0.7,
                        suggested_fix="Ensure corresponding free() call",
                    )
                )
            elif func_name == "free":
                violations.append(
                    BoundsViolation(
                        violation_type=BoundsViolationType.DOUBLE_FREE,
                        variable_name=func_name,
                        line_number=self._current_line,
                        description="Memory deallocation - check for double free",
                        severity="warning",
                        confidence=0.6,
                        suggested_fix="Set pointer to NULL after free()",
                    )
                )

        return violations

    def _analyze_variable_access(self, name: ast.Name) -> list[BoundsViolation]:
        """Analyze a variable access."""
        violations = []

        var_name = name.id
        memory_region = self._memory_regions.get(var_name)

        if memory_region is None:
            # Variable not yet seen - might be uninitialized
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.UNINITIALIZED_ACCESS,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Potential use of uninitialized variable '{var_name}'",
                    severity="warning",
                    confidence=0.6,
                    suggested_fix=f"Initialize variable '{var_name}' before use",
                )
            )
        elif not memory_region.is_initialized:
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.UNINITIALIZED_ACCESS,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Use of uninitialized variable '{var_name}'",
                    severity="error",
                    confidence=0.9,
                    suggested_fix=f"Initialize variable '{var_name}' before use",
                )
            )

        return violations

    def _handle_variable_assignment(self, var_name: str, value_expr: ast.expr) -> list[BoundsViolation]:
        """Handle assignment to a variable."""
        violations: list[BoundsViolation] = []

        # Create or update memory region for the variable
        if isinstance(value_expr, ast.List):
            # List literal assignment
            size = len(value_expr.elts)
            memory_region = MemoryRegion(
                name=var_name,
                region_type=MemoryRegionType.STACK,
                size=size,
                is_initialized=True,
                allocation_line=self._current_line,
                bounds=(0, size - 1),
            )
            self._memory_regions[var_name] = memory_region
        elif isinstance(value_expr, ast.Constant):
            # Simple constant assignment
            memory_region = MemoryRegion(
                name=var_name,
                region_type=MemoryRegionType.STACK,
                is_initialized=True,
                allocation_line=self._current_line,
            )
            self._memory_regions[var_name] = memory_region
        else:
            # Other expressions - create basic region
            memory_region = MemoryRegion(
                name=var_name,
                region_type=MemoryRegionType.STACK,
                is_initialized=True,
                allocation_line=self._current_line,
            )
            self._memory_regions[var_name] = memory_region

        return violations

    def _check_constant_index(self, var_name: str, index: int, region: MemoryRegion) -> list[BoundsViolation]:
        """Check bounds for a constant index."""
        violations = []

        if index < 0:
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.NEGATIVE_INDEX,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Negative index {index} for variable '{var_name}'",
                    severity="error",
                    confidence=1.0,
                    suggested_fix="Use non-negative indices",
                )
            )
        elif region.is_bounded() and not region.is_index_safe(index):
            assert region.size is not None
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.ARRAY_INDEX_OUT_OF_BOUNDS,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Index {index} out of bounds for '{var_name}' (size: {region.size})",
                    severity="error",
                    confidence=1.0,
                    suggested_fix=f"Use index in range [0, {region.size - 1}]",
                )
            )

        return violations

    def _check_variable_index(self, var_name: str, index_var: str, region: MemoryRegion) -> list[BoundsViolation]:
        """Check bounds for a variable index."""
        violations = []

        index_region = self._memory_regions.get(index_var)
        if index_region is None:
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.UNINITIALIZED_ACCESS,
                    variable_name=index_var,
                    line_number=self._current_line,
                    description=f"Index variable '{index_var}' may be uninitialized",
                    severity="warning",
                    confidence=0.7,
                    suggested_fix=f"Initialize index variable '{index_var}'",
                )
            )

        # General warning about variable indices
        if region.is_bounded():
            assert region.size is not None
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.ARRAY_INDEX_OUT_OF_BOUNDS,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Variable index '{index_var}' for '{var_name}' - verify bounds at runtime",
                    severity="warning",
                    confidence=0.6,
                    suggested_fix=f"Add bounds check: if 0 <= {index_var} < {region.size}",
                )
            )
        else:
            # For unbounded regions (like parameters), always warn about variable indices
            violations.append(
                BoundsViolation(
                    violation_type=BoundsViolationType.ARRAY_INDEX_OUT_OF_BOUNDS,
                    variable_name=var_name,
                    line_number=self._current_line,
                    description=f"Variable index '{index_var}' for '{var_name}' with unknown bounds - verify safety",
                    severity="warning",
                    confidence=0.7,
                    suggested_fix=f"Add bounds check: if 0 <= {index_var} < len({var_name})",
                )
            )

        return violations

    def _check_slice_access(self, var_name: str, slice_obj: ast.Slice, region: MemoryRegion) -> list[BoundsViolation]:
        """Check bounds for slice access."""
        violations = []

        # For simplicity, we'll just warn about slice access
        violations.append(
            BoundsViolation(
                violation_type=BoundsViolationType.ARRAY_INDEX_OUT_OF_BOUNDS,
                variable_name=var_name,
                line_number=self._current_line,
                description=f"Slice access on '{var_name}' - verify bounds",
                severity="warning",
                confidence=0.5,
                suggested_fix="Verify slice bounds are within array limits",
            )
        )

        return violations

    def _estimate_memory_usage(self) -> int:
        """Estimate total memory usage in bytes."""
        total_bytes = 0

        for region in self._memory_regions.values():
            if region.is_bounded():
                assert region.size is not None
                # Estimate 4 bytes per integer element
                element_size = 4 if region.element_type == "int" else 8
                total_bytes += region.size * element_size

        return total_bytes

    def _generate_findings(self, violations: list[BoundsViolation]) -> list[str]:
        """Generate a list of analysis findings."""
        findings = []

        findings.append(f"Analyzed {len(self._memory_regions)} memory regions")

        error_count = sum(1 for v in violations if v.severity == "error")
        warning_count = sum(1 for v in violations if v.severity == "warning")

        if error_count > 0:
            findings.append(f"Found {error_count} potential memory safety errors")
        if warning_count > 0:
            findings.append(f"Found {warning_count} memory safety warnings")

        if error_count == 0 and warning_count == 0:
            findings.append("No obvious memory safety issues detected")

        return findings
