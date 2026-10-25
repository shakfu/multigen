"""Memory Safety and Bounds Verification.

This module provides formal verification of memory safety properties,
including bounds checking, buffer overflow prevention, and pointer safety.
"""

import ast
import time
from collections.abc import Sequence
from dataclasses import dataclass, field, replace
from enum import Enum
from typing import Any, Callable, Optional, Union

try:
    import z3  # type: ignore[import-untyped]

    Z3_AVAILABLE = True
except ImportError:
    Z3_AVAILABLE = False
    z3 = None


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
    # Z3 terms from MemoryOperationExtractor; None without Z3.
    path_condition: Any = None
    index_term: Any = None
    length_term: Any = None


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
    def is_proved(self) -> bool:
        """Report whether every property was proved, not merely left undisproved."""
        return all(result.status == ProofStatus.PROVED for result in self.proof_results)

    @property
    def summary(self) -> str:
        """Human-readable summary of the proof."""
        status = "UNSAFE" if not self.is_safe else "SAFE" if self.is_proved else "UNKNOWN"
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

        if self.z3_available:
            memory_extractor = MemoryOperationExtractor()
            memory_extractor.extract(context.ast_node)
            self.memory_regions = memory_extractor.memory_regions
            self.memory_accesses = memory_extractor.memory_accesses
        else:
            self._collect_accesses_without_z3(context.ast_node)

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

        # No counterexample was proved. Undecided accesses leave this True;
        # strict callers must also check is_proved.
        is_safe = not any(result.status == ProofStatus.DISPROVED for result in all_proof_results)

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

    def _collect_accesses_without_z3(self, node: ast.AST) -> None:
        """List subscript accesses so that each can be reported as unverified."""
        self.memory_regions, self.memory_accesses = {}, []
        for child in ast.walk(node):
            if isinstance(child, ast.Subscript) and not isinstance(child.slice, ast.Slice):
                name = ast.unparse(child.value)
                if name in _TYPE_CONSTRUCTORS:
                    continue
                region = self.memory_regions.setdefault(name, MemoryRegion(name, f"&{name}", "unknown", "unknown"))
                self.memory_accesses.append(MemoryAccess(region, "read", ast.unparse(child.slice), 1, child.lineno))

    def _verify_bounds_safety(self) -> list[ProofResult]:
        """Verify bounds checking for all memory accesses."""
        results = []

        for access in self.memory_accesses:
            if access.access_type in ["read", "write"]:
                result = self._decide(self._create_bounds_property(access), access)
                results.append(result)

                # Update access safety status
                if result.status == ProofStatus.DISPROVED:
                    access.is_safe = False
                elif result.status == ProofStatus.PROVED:
                    access.is_safe = True
                else:
                    access.is_safe = None

        return results

    def _decide(self, prop: ProofProperty, access: MemoryAccess) -> ProofResult:
        """Decide a safety formula on every path that reaches the access.

        PROVED means the formula holds whenever the access runs. DISPROVED
        means it fails whenever the access runs; a counterexample that only
        some inputs reach is UNKNOWN, since the caller may exclude them.

        Args:
            prop: Property whose z3_formula is the safety formula at the access
            access: The access, carrying its path condition

        Returns:
            The ProofResult, with z3_formula replaced by the path-guarded formula
        """
        if not self.z3_available or access.path_condition is None:
            return self.theorem_prover.verify_property(prop)
        safe = prop.z3_formula
        prop.z3_formula = z3.Implies(access.path_condition, safe)
        holds = self.theorem_prover.verify_property(prop)
        if holds.status != ProofStatus.DISPROVED:
            return holds
        fails = self.theorem_prover.verify_property(
            replace(prop, z3_formula=z3.Implies(access.path_condition, z3.Not(safe)))
        )
        if fails.status == ProofStatus.PROVED:
            return holds
        return ProofResult(
            proof_property=prop,
            status=ProofStatus.UNKNOWN,
            proof_time=holds.proof_time + fails.proof_time,
            counterexample=holds.counterexample,
            error_message=(f"{access.region.name}[{access.offset}]: index is not statically known to be within bounds"),
        )

    def _verify_null_pointer_safety(self) -> list[ProofResult]:
        """Verify null pointer dereference safety."""
        results = []

        for _region_name, region in self.memory_regions.items():
            # Only a literal null is a provable null dereference; a symbolic
            # base address carries no information either way.
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

        for accesses in region_accesses.values():
            for access in accesses:
                if access.access_type == "write":
                    results.append(self._decide(self._create_buffer_overflow_property(access), access))

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

        offset, size = access.index_term, access.length_term
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

        overflow_formula = access.index_term + access.size <= access.length_term

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
            recommendations.append(
                f"{len(unknown_proofs)} memory accesses could not be decided statically "
                "and were neither proved safe nor unsafe"
            )
            recommendations.append("Add explicit bounds information to help verification")
            recommendations.append("Consider simplifying complex indexing expressions")

        if not recommendations:
            recommendations.append("Memory safety verification passed - code appears safe")

        return recommendations


# Subscripts on these names are type expressions, never memory accesses.
_TYPE_CONSTRUCTORS = frozenset(
    {"list", "dict", "set", "tuple", "frozenset", "type", "Optional", "Union", "Callable", "Sequence", "Iterable"}
)

# Builtins that neither mutate nor retain their arguments.
_PURE_BUILTINS = frozenset(
    {
        "abs", "all", "any", "bool", "chr", "divmod", "enumerate", "float", "format", "frozenset", "hash", "id",
        "int", "isinstance", "iter", "len", "list", "max", "min", "next", "ord", "pow", "print", "range", "repr",
        "reversed", "round", "set", "sorted", "str", "sum", "tuple", "type", "zip",
    }
)  # fmt: skip

# List methods by effect on length. Any other method on a list is assumed to change it arbitrarily.
_NON_MUTATING_METHODS = frozenset({"copy", "count", "index", "sort", "reverse"})
_GROWING_METHODS = frozenset({"append", "extend", "insert"})
# List methods that store an argument in the list, aliasing it.
_RETAINING_METHODS = frozenset({"append", "insert"})

_ANNOTATION_KINDS = {
    "int": "int",
    "bool": "bool",
    "list": "list",
    "List": "list",
    "str": "str",
    "tuple": "tuple",
    "Tuple": "tuple",
    **dict.fromkeys(["dict", "Dict", "set", "Set", "frozenset", "defaultdict", "Mapping"], "dict"),
}

# A value during extraction: ("int", term), ("bool", term), ("cell", cell_id),
# ("new", kind, length term or None), or None when nothing is known.
_Value = Optional[tuple[Any, ...]]


@dataclass
class _State:
    """Abstract program state on one path: known values, object lengths, and path facts."""

    facts: list[Any] = field(default_factory=list)
    ints: dict[str, Any] = field(default_factory=dict)
    names: dict[str, int] = field(default_factory=dict)  # name -> cell id
    lengths: dict[int, Any] = field(default_factory=dict)  # cell id -> length term, None when opaque

    def copy(self) -> "_State":
        return _State(list(self.facts), dict(self.ints), dict(self.names), dict(self.lengths))

    def assuming(self, *conditions: Any) -> "_State":
        state = self.copy()
        state.facts.extend(conditions)
        return state


class MemoryOperationExtractor:
    """Extract subscript accesses with the path condition, index and length that hold at each.

    The state over-approximates every execution: a value the extractor cannot
    track becomes a fresh unconstrained symbol, loops havoc what their bodies
    change, and a list that escapes into a call or container becomes opaque
    (each read of its length is fresh). Proofs and disproofs over the model
    therefore hold for the program. Requires Z3.
    """

    def __init__(self) -> None:
        self.memory_regions: dict[str, MemoryRegion] = {}
        self.memory_accesses: list[MemoryAccess] = []
        self.current_function: Optional[str] = None
        # Facts true of every execution, such as lengths being non-negative.
        self._axioms: list[Any] = []
        self._kinds: dict[int, str] = {}
        self._param_cells: set[int] = set()
        self._globals: set[str] = set()
        # Names a nested function or lambda reads; it may mutate their objects when called.
        self._captured: set[str] = set()
        self._symbols = 0

    def extract(self, node: ast.AST) -> None:
        """Extract accesses from a function, or from each function and the top level of a module."""
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            self._extract_function(node)
        elif isinstance(node, ast.Module):
            self._captured = self._captured_names(node)
            self._exec_block(node.body, _State())
            for child in ast.walk(node):
                if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    self._extract_function(child)
        for access in self.memory_accesses:
            access.path_condition = z3.And(access.path_condition, *self._axioms)

    def _extract_function(self, node: Union[ast.FunctionDef, ast.AsyncFunctionDef]) -> None:
        self.current_function = node.name
        self._param_cells = set()
        self._captured = self._captured_names(node)
        self._globals = {
            name for child in ast.walk(node) if isinstance(child, (ast.Global, ast.Nonlocal)) for name in child.names
        }
        state = _State()
        args = node.args
        for arg in [*args.posonlyargs, *args.args, *args.kwonlyargs]:
            self._bind_parameter(arg, state)
        if args.vararg:
            self._param_cells.add(self._bind_new(state, args.vararg.arg, "tuple", None))
        if args.kwarg:
            self._bind_new(state, args.kwarg.arg, "dict", None)
        self._exec_block(node.body, state)

    @staticmethod
    def _captured_names(node: ast.AST) -> set[str]:
        return {
            name.id
            for child in ast.walk(node)
            if child is not node and isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda))
            for name in ast.walk(child)
            if isinstance(name, ast.Name)
        }

    def _bind_parameter(self, arg: ast.arg, state: _State) -> None:
        kind, length = self._annotation_kind(arg.annotation)
        if kind in ("int", "bool"):
            term = z3.Int(arg.arg)
            state.ints[arg.arg] = term
            if kind == "bool":
                self._axioms.append(z3.And(term >= 0, term <= 1))
        elif kind is not None:
            if kind in ("list", "str", "tuple") and length is None:
                length = z3.Int(f"len({arg.arg})")
                self._axioms.append(length >= 0)
            self._param_cells.add(self._bind_new(state, arg.arg, kind, length))

    def _annotation_kind(self, annotation: Optional[ast.expr]) -> tuple[Optional[str], Any]:
        """Return the kind an annotation declares, and a tuple's fixed length if it has one."""
        base = annotation.value if isinstance(annotation, ast.Subscript) else annotation
        name = base.id if isinstance(base, ast.Name) else base.attr if isinstance(base, ast.Attribute) else None
        kind = _ANNOTATION_KINDS.get(name or "")
        if kind == "tuple" and isinstance(annotation, ast.Subscript):
            elements = annotation.slice.elts if isinstance(annotation.slice, ast.Tuple) else [annotation.slice]
            if not any(isinstance(e, ast.Constant) and e.value is Ellipsis for e in elements):
                return kind, z3.IntVal(len(elements))
        return kind, None

    # Symbols and cells

    def _fresh(self, hint: str = "v") -> Any:
        self._symbols += 1
        return z3.Int(f"{hint}!{self._symbols}")

    def _fresh_bool(self) -> Any:
        self._symbols += 1
        return z3.Bool(f"cond!{self._symbols}")

    def _fresh_length(self) -> Any:
        length = self._fresh("len")
        self._axioms.append(length >= 0)
        return length

    def _new_cell(self, state: _State, kind: str, length: Any) -> int:
        cell = len(self._kinds)
        self._kinds[cell] = kind
        if kind != "dict" and length is None:
            length = self._fresh_length()
        state.lengths[cell] = length
        return cell

    def _bind_new(self, state: _State, name: str, kind: str, length: Any) -> int:
        self._unbind(state, name)
        cell = self._new_cell(state, kind, length)
        state.names[name] = cell
        if name in self._captured:
            state.lengths[cell] = None
        return cell

    def _unbind(self, state: _State, name: str) -> None:
        state.ints.pop(name, None)
        state.names.pop(name, None)

    def _length_of(self, state: _State, cell: int) -> Any:
        length = state.lengths.get(cell)
        return self._fresh_length() if length is None else length

    def _set_length(self, state: _State, cell: int, length: Any) -> None:
        """Change a cell's length; mutating a parameter may also change any other parameter it aliases."""
        if state.lengths.get(cell) is not None:
            state.lengths[cell] = length
        self._havoc_param_aliases(state, cell)

    def _havoc_param_aliases(self, state: _State, cell: int) -> None:
        if cell in self._param_cells:
            for other in self._param_cells - {cell}:
                if self._kinds[other] == "list" and state.lengths.get(other) is not None:
                    state.lengths[other] = self._fresh_length()

    # Statements

    def _exec_block(self, block: list[ast.stmt], state: Optional[_State]) -> Optional[_State]:
        """Execute statements; return the fall-through state, or None if no path falls through."""
        for statement in block:
            if state is None:
                break
            state = self._exec(statement, state)
        return state

    def _exec(self, stmt: ast.stmt, state: _State) -> Optional[_State]:
        if isinstance(stmt, ast.If):
            header: list[ast.AST] = [stmt.test]
        elif isinstance(stmt, ast.For):
            header = [stmt.iter]
        elif isinstance(stmt, ast.While):
            header = [stmt.test]
        else:
            header = [stmt]
        state = state.copy()
        for cell in self._escaped_cells(header, state):
            state.lengths[cell] = None
        for name in self._walrus_targets(header):
            self._unbind(state, name)
        # A mutation inside an expression can happen between two reads, so each read is fresh.
        mutated = {
            cell
            for cell in self._nested_mutations(header, state, top=stmt if isinstance(stmt, ast.Expr) else None)
            if state.lengths.get(cell) is not None
        }
        for cell in mutated:
            state.lengths[cell] = None

        result = self._exec_statement(stmt, state)

        if result is not None:
            for cell in mutated:
                if cell in result.lengths:
                    result.lengths[cell] = self._fresh_length()
                self._havoc_param_aliases(result, cell)
        return result

    def _exec_statement(self, stmt: ast.stmt, state: _State) -> Optional[_State]:
        if isinstance(stmt, ast.If):
            condition = self._bool(stmt.test, state, [])
            then = self._exec_block(stmt.body, state.assuming(condition))
            orelse = self._exec_block(stmt.orelse, state.assuming(z3.Not(condition)))
            return self._join(state, then, orelse)
        if isinstance(stmt, ast.While):
            return self._exec_while(stmt, state)
        if isinstance(stmt, ast.For):
            return self._exec_for(stmt, state)
        if isinstance(stmt, ast.Assign):
            value = self._value(stmt.value, state, [])
            for target in stmt.targets:
                self._assign(target, stmt.value, value, state)
            return state
        if isinstance(stmt, ast.AnnAssign):
            if stmt.value is None:
                if isinstance(stmt.target, ast.Name):
                    self._unbind(state, stmt.target.id)
                return state
            value = self._value(stmt.value, state, [])
            self._assign(stmt.target, stmt.value, self._coerce(value, stmt.annotation), state)
            return state
        if isinstance(stmt, ast.AugAssign):
            self._exec_aug_assign(stmt, state)
            return state
        if isinstance(stmt, ast.Expr):
            if not self._exec_method_call(stmt.value, state):
                self._value(stmt.value, state, [])
            return state
        if isinstance(stmt, (ast.Return, ast.Raise)):
            for child in ast.iter_child_nodes(stmt):
                self._value(child, state, [])
            return None
        if isinstance(stmt, (ast.Break, ast.Continue)):
            return None
        if isinstance(stmt, ast.Assert):
            # Asserts are not assumed: generated code may be built with them disabled.
            self._bool(stmt.test, state, [])
            return state
        if isinstance(stmt, ast.Delete):
            for target in stmt.targets:
                self._exec_delete(target, state)
            return state
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            # Nested definitions are analyzed on their own; here they only bind a name.
            self._unbind(state, stmt.name)
            return state
        if isinstance(stmt, (ast.Import, ast.ImportFrom)):
            for alias in stmt.names:
                self._unbind(state, (alias.asname or alias.name).split(".")[0])
            return state
        if isinstance(stmt, (ast.Pass, ast.Global, ast.Nonlocal)):
            return state
        return self._exec_generic(stmt, state)

    def _exec_generic(self, stmt: ast.stmt, state: _State) -> _State:
        """Over-approximate an unmodeled statement: havoc what it changes, and check every access in it."""
        havocked = self._havoc(state, [stmt])

        def visit(node: ast.AST) -> None:
            for _name, field_value in ast.iter_fields(node):
                values = field_value if isinstance(field_value, list) else [field_value]
                if values and all(isinstance(v, ast.stmt) for v in values):
                    self._exec_block(values, havocked.copy())
                    continue
                for value in values:
                    if isinstance(value, ast.expr):
                        self._value(value, havocked, [])
                    elif isinstance(value, ast.AST):
                        visit(value)

        visit(stmt)
        return havocked

    def _exec_while(self, stmt: ast.While, state: _State) -> Optional[_State]:
        loop = self._havoc(state, [stmt.test, *stmt.body])
        condition = self._bool(stmt.test, loop, [])
        self._exec_block(stmt.body, loop.assuming(condition))
        return self._loop_exit(stmt, loop.assuming(z3.Not(condition)), loop)

    def _exec_for(self, stmt: ast.For, state: _State) -> Optional[_State]:
        # The iterable is evaluated once, before the first iteration.
        self._value(stmt.iter, state, [])
        bounds = self._iteration_bounds(stmt, state)
        loop = self._havoc(state, [stmt.target, *stmt.body])
        body = loop.copy()
        index = stmt.target
        if isinstance(index, ast.Tuple) and len(index.elts) == 2 and self._is_call(stmt.iter, "enumerate"):
            index = index.elts[0]
        if bounds is not None and isinstance(index, ast.Name):
            term = self._fresh(index.id)
            body.ints[index.id] = term
            body.facts.append(bounds(term))
        return self._loop_exit(stmt, loop, loop, body)

    def _loop_exit(
        self, stmt: Union[ast.For, ast.While], normal: _State, loop: _State, body: Optional[_State] = None
    ) -> Optional[_State]:
        """Run a for body (while bodies run in the caller), then the else block, joining exits by break."""
        if body is not None:
            self._exec_block(stmt.body, body)
        after = self._exec_block(stmt.orelse, normal)
        return self._join(loop, after, loop) if self._has_break(stmt.body) else after

    def _iteration_bounds(self, stmt: ast.For, state: _State) -> Optional[Callable[[Any], Any]]:
        """Return the constraint on the loop index of a range() or enumerate() loop, or None."""
        iterable = stmt.iter
        if not isinstance(iterable, ast.Call):
            return None
        if self._is_call(iterable, "range") and isinstance(stmt.target, ast.Name):
            args = [self._int(arg, state, [], record=False) for arg in iterable.args]
            if len(args) == 1:
                return lambda i: z3.And(i >= 0, i < args[0])
            if len(args) == 2:
                return lambda i: z3.And(i >= args[0], i < args[1])
            if len(args) == 3 and z3.is_int_value(z3.simplify(args[2])):
                step = z3.simplify(args[2]).as_long()
                if step > 0:
                    return lambda i: z3.And(i >= args[0], i < args[1])
                if step < 0:
                    return lambda i: z3.And(i <= args[0], i > args[1])
            return None
        if self._is_call(iterable, "enumerate") and iterable.args:
            sequence = self._value(iterable.args[0], state, [], record=False)
            start = self._int(iterable.args[1], state, [], record=False) if len(iterable.args) > 1 else z3.IntVal(0)
            # A body that changes the sequence's length changes how many items enumerate yields.
            if sequence and sequence[0] == "cell" and sequence[1] not in self._mutated_cells(stmt.body, state):
                length = self._length_of(state, sequence[1])
                return lambda i: z3.And(i >= start, i < start + length)
        return None

    def _join(self, base: _State, left: Optional[_State], right: Optional[_State]) -> Optional[_State]:
        """Merge two branch states derived from base into one whose facts admit either."""
        if left is None or right is None:
            return right if left is None else left
        joined = base.copy()
        left_facts, right_facts = left.facts[len(base.facts) :], right.facts[len(base.facts) :]
        joined.ints = {}
        for name in left.ints.keys() & right.ints.keys():
            if left.ints[name].eq(right.ints[name]):
                joined.ints[name] = left.ints[name]
            else:
                term = joined.ints[name] = self._fresh(name)
                left_facts.append(term == left.ints[name])
                right_facts.append(term == right.ints[name])
        joined.names = {name: cell for name, cell in left.names.items() if right.names.get(name) == cell}
        # A name bound to different objects on the two paths could alias either.
        unsure = {cell for name, cell in left.names.items() if right.names.get(name) != cell}
        unsure |= {cell for name, cell in right.names.items() if left.names.get(name) != cell}
        joined.lengths = {}
        for cell in left.lengths.keys() | right.lengths.keys():
            a, b = left.lengths.get(cell), right.lengths.get(cell)
            if cell in unsure or a is None or b is None:
                joined.lengths[cell] = None
            elif a.eq(b):
                joined.lengths[cell] = a
            else:
                term = joined.lengths[cell] = self._fresh("len")
                left_facts.append(term == a)
                right_facts.append(term == b)
        joined.facts.append(z3.Or(z3.And(*left_facts), z3.And(*right_facts)))
        return joined

    def _havoc(self, state: _State, nodes: Sequence[ast.AST]) -> _State:
        """Return a state that holds before and after any number of executions of nodes."""
        state = state.copy()
        for cell in self._escaped_cells(nodes, state):
            state.lengths[cell] = None
        stores = [
            n for node in nodes for n in ast.walk(node) if isinstance(n, ast.Name) and not isinstance(n.ctx, ast.Load)
        ]
        for node in nodes:
            for child in ast.walk(node):
                # x = y inside a loop may alias y to an object x is later mutated through.
                if isinstance(child, (ast.Assign, ast.AnnAssign)) and isinstance(child.value, ast.Name):
                    for name in [child.value.id, *(t.id for t in self._targets(child) if isinstance(t, ast.Name))]:
                        if name in state.names:
                            state.lengths[state.names[name]] = None
        for cell, growing in self._mutated_cells(nodes, state).items():
            old = state.lengths.get(cell)
            if old is not None:
                new = self._fresh_length()
                if growing:
                    self._axioms.append(new >= old)
                self._set_length(state, cell, new)
        monotone = self._monotone_names(nodes)
        for name in {n.id for n in stores}:
            if name in state.ints:
                old, new = state.ints[name], self._fresh(name)
                state.ints[name] = new
                if monotone.get(name) == 1:
                    state.facts.append(new >= old)
                elif monotone.get(name) == -1:
                    state.facts.append(new <= old)
            elif name in state.names:
                self._bind_new(state, name, self._kinds[state.names[name]], None)
            else:
                self._unbind(state, name)
        return state

    def _monotone_names(self, nodes: Sequence[ast.AST]) -> dict[str, int]:
        """Map names only ever changed by += or -= a non-negative constant to +1 or -1."""
        steps: dict[str, set[int]] = {}
        stepped: set[int] = set()
        for node in nodes:
            for child in ast.walk(node):
                if (
                    isinstance(child, ast.AugAssign)
                    and isinstance(child.target, ast.Name)
                    and isinstance(child.op, (ast.Add, ast.Sub))
                    and isinstance(child.value, ast.Constant)
                    and type(child.value.value) is int
                    and child.value.value >= 0
                ):
                    steps.setdefault(child.target.id, set()).add(1 if isinstance(child.op, ast.Add) else -1)
                    stepped.add(id(child.target))
        for node in nodes:
            for child in ast.walk(node):
                if isinstance(child, ast.Name) and not isinstance(child.ctx, ast.Load) and id(child) not in stepped:
                    steps.pop(child.id, None)
        return {name: next(iter(signs)) for name, signs in steps.items() if len(signs) == 1}

    def _mutated_cells(self, nodes: Sequence[ast.AST], state: _State) -> dict[int, bool]:
        """Map each list cell some node may resize to whether it can only grow."""
        mutated: dict[int, bool] = {}

        def note(name: str, growing: bool) -> None:
            cell = state.names.get(name)
            if cell is not None and self._kinds[cell] == "list":
                mutated[cell] = mutated.get(cell, True) and growing

        for node in nodes:
            for child in ast.walk(node):
                if isinstance(child, ast.Call) and isinstance(child.func, ast.Attribute):
                    receiver = child.func.value
                    if isinstance(receiver, ast.Name) and child.func.attr not in _NON_MUTATING_METHODS:
                        note(receiver.id, child.func.attr in _GROWING_METHODS)
                elif isinstance(child, ast.AugAssign) and isinstance(child.target, ast.Name):
                    note(child.target.id, isinstance(child.op, ast.Add))
                elif isinstance(child, ast.Delete):
                    for target in child.targets:
                        if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
                            note(target.value.id, False)
        return mutated

    def _nested_mutations(self, nodes: Sequence[ast.AST], state: _State, top: Optional[ast.Expr]) -> set[int]:
        """Return cells resized by a method call other than a top-level statement call."""
        skip = top.value if top is not None else None
        calls = [n for node in nodes for n in ast.walk(node) if isinstance(n, ast.Call) and n is not skip]
        return set(self._mutated_cells(calls, state)) if calls else set()

    def _escaped_cells(self, nodes: Sequence[ast.AST], state: _State) -> set[int]:
        """Return cells some node stores where later code could alias or mutate them unseen."""
        escaped: set[int] = set()

        def note(expr: Optional[ast.AST]) -> None:
            if isinstance(expr, ast.Starred):
                expr = expr.value
            if isinstance(expr, ast.Name) and expr.id in state.names:
                escaped.add(state.names[expr.id])

        for node in nodes:
            for child in ast.walk(node):
                if isinstance(child, ast.Call):
                    func = child.func
                    if isinstance(func, ast.Name) and func.id in _PURE_BUILTINS:
                        continue
                    if (
                        isinstance(func, ast.Attribute)
                        and isinstance(func.value, ast.Name)
                        and func.value.id in state.names
                        and func.attr not in _RETAINING_METHODS
                    ):
                        continue
                    for arg in [*child.args, *(keyword.value for keyword in child.keywords)]:
                        note(arg)
                elif isinstance(child, (ast.List, ast.Tuple)) and isinstance(child.ctx, ast.Load):
                    for element in child.elts:
                        note(element)
                elif isinstance(child, ast.Set):
                    for element in child.elts:
                        note(element)
                elif isinstance(child, ast.Dict):
                    for entry in [*child.keys, *child.values]:
                        note(entry)
                elif isinstance(child, (ast.ListComp, ast.SetComp, ast.GeneratorExp)):
                    note(child.elt)
                elif isinstance(child, ast.DictComp):
                    note(child.key)
                    note(child.value)
                elif isinstance(child, (ast.Yield, ast.YieldFrom, ast.NamedExpr, ast.Await)):
                    note(child.value)
                elif isinstance(child, ast.IfExp):
                    note(child.body)
                    note(child.orelse)
                elif isinstance(child, ast.BoolOp):
                    for value in child.values:
                        note(value)
                elif isinstance(child, (ast.Lambda, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                    for name in ast.walk(child):
                        note(name)
                elif isinstance(child, (ast.Assign, ast.AnnAssign)):
                    if not all(isinstance(target, ast.Name) for target in self._targets(child)):
                        note(child.value)
                elif isinstance(child, ast.withitem):
                    note(child.context_expr)
        return escaped

    @staticmethod
    def _walrus_targets(nodes: Sequence[ast.AST]) -> set[str]:
        return {n.target.id for node in nodes for n in ast.walk(node) if isinstance(n, ast.NamedExpr)}

    @staticmethod
    def _targets(stmt: Union[ast.Assign, ast.AnnAssign]) -> list[ast.expr]:
        return stmt.targets if isinstance(stmt, ast.Assign) else [stmt.target]

    @staticmethod
    def _has_break(body: list[ast.stmt]) -> bool:
        """Report whether a loop body breaks out of this loop (not a nested one)."""

        def scan(node: ast.AST) -> bool:
            if isinstance(node, ast.Break):
                return True
            if isinstance(
                node, (ast.For, ast.While, ast.AsyncFor, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
            ):
                return any(scan(child) for child in node.orelse) if hasattr(node, "orelse") else False
            return any(scan(child) for child in ast.iter_child_nodes(node))

        return any(scan(statement) for statement in body)

    @staticmethod
    def _is_call(node: ast.expr, name: str) -> bool:
        return isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == name

    def _assign(self, target: ast.expr, source: ast.expr, value: _Value, state: _State) -> None:
        if isinstance(target, ast.Name):
            self._bind_value(state, target.id, value)
        elif isinstance(target, (ast.Tuple, ast.List)):
            elements = source.elts if isinstance(source, (ast.Tuple, ast.List)) else None
            if elements is not None and len(elements) == len(target.elts) and value is not None:
                # The right side is evaluated in full before any target is bound.
                values = [self._value(element, state, [], record=False) for element in elements]
                for element_target, element_source, element_value in zip(target.elts, elements, values):
                    self._assign(element_target, element_source, element_value, state)
            else:
                for name in self._target_names(target):
                    self._unbind(state, name)
                for element in target.elts:
                    if not isinstance(element, (ast.Name, ast.Tuple, ast.List, ast.Starred)):
                        self._value(element, state, [])
        else:
            self._value(target, state, [])

    @staticmethod
    def _target_names(target: ast.expr) -> list[str]:
        return [n.id for n in ast.walk(target) if isinstance(n, ast.Name)]

    def _bind_value(self, state: _State, name: str, value: _Value) -> None:
        if name in self._globals:
            return
        if value is None:
            self._unbind(state, name)
        elif value[0] == "int":
            self._unbind(state, name)
            state.ints[name] = value[1]
        elif value[0] == "bool":
            self._unbind(state, name)
            state.ints[name] = z3.If(value[1], z3.IntVal(1), z3.IntVal(0))
        elif value[0] == "cell":
            cell = value[1]
            self._unbind(state, name)
            state.names[name] = cell
            if name in self._captured:
                state.lengths[cell] = None
        else:
            self._bind_new(state, name, value[1], value[2])

    def _coerce(self, value: _Value, annotation: ast.expr) -> _Value:
        """Apply an annotation to a value the extractor could not classify."""
        kind, length = self._annotation_kind(annotation)
        if value is not None or kind is None:
            return value
        if kind in ("int", "bool"):
            return ("int", self._fresh())
        return ("new", kind, length)

    def _exec_aug_assign(self, stmt: ast.AugAssign, state: _State) -> None:
        target = stmt.target
        if not isinstance(target, ast.Name):
            self._value(stmt.value, state, [])
            self._value(target, state, [])
            return
        operand = self._value(stmt.value, state, [])
        if target.id in state.ints:
            current = ("int", state.ints[target.id])
            self._bind_value(state, target.id, self._binary(stmt.op, current, operand, state))
        elif target.id in state.names:
            cell = state.names[target.id]
            extra = self._length_of_value(operand, state)
            if isinstance(stmt.op, ast.Add) and extra is not None:
                new = self._length_of(state, cell) + extra
            else:
                new = self._fresh_length()
            if self._kinds[cell] == "list":
                self._set_length(state, cell, new)  # in place
            else:
                self._bind_new(state, target.id, self._kinds[cell], new)
        else:
            self._unbind(state, target.id)

    def _exec_method_call(self, expr: ast.expr, state: _State) -> bool:
        """Apply a statement-level list method call; return False if expr is not one."""
        if not (
            isinstance(expr, ast.Call)
            and isinstance(expr.func, ast.Attribute)
            and isinstance(expr.func.value, ast.Name)
            and expr.func.value.id in state.names
            and self._kinds[state.names[expr.func.value.id]] == "list"
        ):
            return False
        cell = state.names[expr.func.value.id]
        method = expr.func.attr
        args = [self._value(arg, state, []) for arg in expr.args]
        for keyword in expr.keywords:
            self._value(keyword.value, state, [])
        length = state.lengths.get(cell)
        if length is None:
            self._set_length(state, cell, None)
            return True
        if method in ("append", "insert"):
            self._set_length(state, cell, length + 1)
        elif method in ("pop", "remove"):
            # Execution continues only if the list was non-empty.
            state.facts.append(length >= 1)
            self._set_length(state, cell, length - 1)
        elif method == "clear":
            self._set_length(state, cell, z3.IntVal(0))
        elif method == "extend" and len(args) == 1 and self._length_of_value(args[0], state) is not None:
            self._set_length(state, cell, length + self._length_of_value(args[0], state))
        elif method not in _NON_MUTATING_METHODS:
            self._set_length(state, cell, self._fresh_length())
        return True

    def _exec_delete(self, target: ast.expr, state: _State) -> None:
        if isinstance(target, ast.Name):
            self._unbind(state, target.id)
            return
        self._value(target, state, [])
        if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
            cell = state.names.get(target.value.id)
            if cell is not None and self._kinds[cell] == "list" and state.lengths.get(cell) is not None:
                if isinstance(target.slice, ast.Slice):
                    self._set_length(state, cell, self._fresh_length())
                else:
                    self._set_length(state, cell, state.lengths[cell] - 1)

    # Expressions

    def _int(self, expr: ast.expr, state: _State, assume: list[Any], record: bool = True) -> Any:
        value = self._value(expr, state, assume, record)
        if value is not None and value[0] == "int":
            return value[1]
        if value is not None and value[0] == "bool":
            return z3.If(value[1], z3.IntVal(1), z3.IntVal(0))
        return self._fresh()

    def _bool(self, expr: ast.expr, state: _State, assume: list[Any]) -> Any:
        return self._truth(self._value(expr, state, assume), state)

    def _truth(self, value: _Value, state: _State) -> Any:
        if value is not None and value[0] == "bool":
            return value[1]
        if value is not None and value[0] == "int":
            return value[1] != 0
        length = self._length_of_value(value, state)
        return length > 0 if length is not None else self._fresh_bool()

    def _kind_of(self, value: _Value) -> Optional[str]:
        if value is None:
            return None
        if value[0] == "cell":
            return self._kinds[value[1]]
        kind: str = value[1] if value[0] == "new" else value[0]
        return kind

    def _length_of_value(self, value: _Value, state: _State) -> Any:
        if value is not None and value[0] == "cell" and self._kinds[value[1]] != "dict":
            return self._length_of(state, value[1])
        if value is not None and value[0] == "new" and value[1] != "dict":
            return value[2] if value[2] is not None else self._fresh_length()
        return None

    def _value(self, expr: ast.AST, state: _State, assume: list[Any], record: bool = True) -> _Value:
        """Evaluate expr abstractly, recording each subscript access under the facts that guard it."""
        if isinstance(expr, ast.Constant):
            if isinstance(expr.value, bool):
                return ("bool", z3.BoolVal(expr.value))
            if isinstance(expr.value, int):
                return ("int", z3.IntVal(expr.value))
            if isinstance(expr.value, (str, bytes)):
                return ("new", "str", z3.IntVal(len(expr.value)))
            return None
        if isinstance(expr, ast.Name):
            if expr.id in self._globals:
                return None
            if expr.id in state.ints:
                return ("int", state.ints[expr.id])
            if expr.id in state.names:
                return ("cell", state.names[expr.id])
            return None
        if isinstance(expr, (ast.List, ast.Tuple)) and isinstance(expr.ctx, ast.Load):
            starred = False
            for element in expr.elts:
                starred |= isinstance(element, ast.Starred)
                self._value(element, state, assume, record)
            kind = "list" if isinstance(expr, ast.List) else "tuple"
            return ("new", kind, None if starred else z3.IntVal(len(expr.elts)))
        if isinstance(expr, (ast.Dict, ast.Set)):
            for child in ast.iter_child_nodes(expr):
                self._value(child, state, assume, record)
            return ("new", "dict", None)
        if isinstance(expr, ast.BinOp):
            left = self._value(expr.left, state, assume, record)
            right = self._value(expr.right, state, assume, record)
            return self._binary(expr.op, left, right, state)
        if isinstance(expr, ast.UnaryOp):
            operand = self._value(expr.operand, state, assume, record)
            if isinstance(expr.op, ast.Not):
                return ("bool", z3.Not(self._truth(operand, state)))
            if operand is not None and operand[0] in ("int", "bool") and isinstance(expr.op, (ast.USub, ast.UAdd)):
                term = operand[1] if operand[0] == "int" else z3.If(operand[1], z3.IntVal(1), z3.IntVal(0))
                return ("int", -term if isinstance(expr.op, ast.USub) else term)
            return ("int", self._fresh()) if operand is not None and operand[0] in ("int", "bool") else None
        if isinstance(expr, ast.BoolOp):
            # Each operand runs only if the ones before it did not decide the result.
            guards: list[Any] = []
            for item in expr.values:
                truth = self._truth(self._value(item, state, assume + guards, record), state)
                guards.append(truth if isinstance(expr.op, ast.And) else z3.Not(truth))
            return ("bool", z3.And(*guards) if isinstance(expr.op, ast.And) else z3.Not(z3.And(*guards)))
        if isinstance(expr, ast.Compare):
            return self._compare(expr, state, assume, record)
        if isinstance(expr, ast.IfExp):
            condition = self._bool(expr.test, state, assume)
            body = self._value(expr.body, state, assume + [condition], record)
            orelse = self._value(expr.orelse, state, assume + [z3.Not(condition)], record)
            if body is not None and orelse is not None and body[0] == orelse[0] and body[0] in ("int", "bool"):
                return (body[0], z3.If(condition, body[1], orelse[1]))
            return None
        if isinstance(expr, ast.Subscript):
            return self._subscript(expr, state, assume, record)
        if isinstance(expr, ast.Call):
            return self._call(expr, state, assume, record)
        if isinstance(expr, (ast.ListComp, ast.SetComp, ast.GeneratorExp, ast.DictComp)):
            return self._comprehension(expr, state, assume, record)
        if isinstance(expr, ast.Lambda):
            # The body runs later, in an unknown state.
            self._value(expr.body, _State(), [], record)
            return None
        if isinstance(expr, ast.NamedExpr):
            return self._value(expr.value, state, assume, record)
        if isinstance(expr, ast.JoinedStr):
            for child in ast.iter_child_nodes(expr):
                self._value(child, state, assume, record)
            return ("new", "str", None)
        for child in ast.iter_child_nodes(expr):
            self._value(child, state, assume, record)
        return None

    def _binary(self, op: ast.operator, left: _Value, right: _Value, state: _State) -> _Value:
        if left is not None and right is not None and {left[0], right[0]} <= {"int", "bool"}:
            a = left[1] if left[0] == "int" else z3.If(left[1], z3.IntVal(1), z3.IntVal(0))
            b = right[1] if right[0] == "int" else z3.If(right[1], z3.IntVal(1), z3.IntVal(0))
            if isinstance(op, ast.Add):
                return ("int", a + b)
            if isinstance(op, ast.Sub):
                return ("int", a - b)
            if isinstance(op, ast.Mult):
                return ("int", a * b)
            divisor = z3.simplify(b)
            # Z3 and Python agree on // and % only for a positive divisor.
            if isinstance(op, (ast.FloorDiv, ast.Mod)) and z3.is_int_value(divisor) and divisor.as_long() > 0:
                return ("int", a / b if isinstance(op, ast.FloorDiv) else a % b)
            if isinstance(op, ast.Div):
                return None
            return ("int", self._fresh())
        sequence, count = (left, right) if self._kind_of(left) in ("list", "str", "tuple") else (right, left)
        kind = self._kind_of(sequence)
        if kind not in ("list", "str", "tuple"):
            # An int combined with an unknown operand is an int, or a TypeError.
            return ("int", self._fresh()) if "int" in (self._kind_of(left), self._kind_of(right)) else None
        length = self._length_of_value(sequence, state)
        if isinstance(op, ast.Mult) and count is not None and count[0] == "int":
            return ("new", kind, z3.If(count[1] > 0, length * count[1], z3.IntVal(0)))
        if isinstance(op, ast.Add) and sequence is left:
            other = self._length_of_value(right, state)
            return ("new", kind, length + other if other is not None else None)
        return ("new", kind, None)

    def _compare(self, expr: ast.Compare, state: _State, assume: list[Any], record: bool) -> _Value:
        operators: dict[type[ast.cmpop], Callable[[Any, Any], Any]] = {
            ast.Lt: lambda a, b: a < b,
            ast.LtE: lambda a, b: a <= b,
            ast.Gt: lambda a, b: a > b,
            ast.GtE: lambda a, b: a >= b,
            ast.Eq: lambda a, b: a == b,
            ast.NotEq: lambda a, b: a != b,
        }
        # A chain stops at its first false comparison; later operands run only if the earlier ones held.
        guards: list[Any] = []
        left = self._value(expr.left, state, assume, record)
        for op, comparator in zip(expr.ops, expr.comparators):
            right = self._value(comparator, state, assume + guards, record)
            ints = [v for v in (left, right) if v is not None and v[0] == "int"]
            apply = operators.get(type(op))
            guards.append(apply(ints[0][1], ints[1][1]) if apply and len(ints) == 2 else self._fresh_bool())
            left = right
        return ("bool", z3.And(*guards))

    def _call(self, expr: ast.Call, state: _State, assume: list[Any], record: bool) -> _Value:
        self._value(expr.func, state, assume, record)
        args = [self._value(arg, state, assume, record) for arg in expr.args]
        for keyword in expr.keywords:
            self._value(keyword.value, state, assume, record)
        name = expr.func.id if isinstance(expr.func, ast.Name) and expr.func.id not in self._globals else None
        if isinstance(expr.func, ast.Attribute) and expr.func.attr == "copy" and not expr.args:
            name, args = "list", [self._value(expr.func.value, state, assume, record=False)]
        if name == "len" and len(args) == 1:
            length = self._length_of_value(args[0], state)
            return ("int", length if length is not None else self._fresh_length())
        if name in ("list", "sorted", "tuple") and len(args) == 1:
            kind = self._kind_of(args[0])
            if kind == "dict" and isinstance(expr.func, ast.Attribute):
                return ("new", "dict", None)
            length = self._length_of_value(args[0], state) if kind in ("list", "tuple", "str") else None
            return ("new", "tuple" if name == "tuple" else "list", length)
        if name == "abs" and len(args) == 1 and args[0] is not None and args[0][0] == "int":
            return ("int", z3.If(args[0][1] >= 0, args[0][1], -args[0][1]))
        terms = [arg[1] for arg in args if arg is not None and arg[0] == "int"]
        if name in ("min", "max") and len(args) >= 2 and len(terms) == len(args):
            result = terms[0]
            for term in terms[1:]:
                result = z3.If(term < result if name == "min" else term > result, term, result)
            return ("int", result)
        if name in ("int", "ord", "sum", "round"):
            return ("int", self._fresh())
        if name == "str":
            return ("new", "str", None)
        return None

    def _subscript(self, expr: ast.Subscript, state: _State, assume: list[Any], record: bool) -> _Value:
        base = expr.value
        if isinstance(base, ast.Name) and base.id in _TYPE_CONSTRUCTORS:
            return None
        sequence = self._value(base, state, assume, record)
        if isinstance(expr.slice, ast.Slice):
            # Slices never raise; they clamp to the sequence.
            for part in (expr.slice.lower, expr.slice.upper, expr.slice.step):
                if part is not None:
                    self._value(part, state, assume, record)
            kind = self._kind_of(sequence)
            return ("new", kind, None) if kind in ("list", "str", "tuple") else None
        index = self._int(expr.slice, state, assume, record)
        kind = self._kind_of(sequence)
        if kind == "dict":
            # A missing key raises KeyError; it is not an out-of-bounds access.
            return None
        if record:
            length = self._length_of_value(sequence, state)
            self._record(expr, index, length if length is not None else self._fresh_length(), state, assume)
        return ("new", "str", z3.IntVal(1)) if kind == "str" else None

    def _comprehension(
        self,
        expr: Union[ast.ListComp, ast.SetComp, ast.GeneratorExp, ast.DictComp],
        state: _State,
        assume: list[Any],
        record: bool,
    ) -> _Value:
        scope = state.copy()
        guards = list(assume)
        exact: Any = None
        for position, generator in enumerate(expr.generators):
            loop = ast.For(target=generator.target, iter=generator.iter, body=[], orelse=[])
            self._value(generator.iter, scope, guards, record)
            bounds = self._iteration_bounds(loop, scope)
            for name in self._target_names(generator.target):
                self._unbind(scope, name)
            index = generator.target
            if isinstance(index, ast.Tuple) and len(index.elts) == 2 and self._is_call(generator.iter, "enumerate"):
                index = index.elts[0]
            if bounds is not None and isinstance(index, ast.Name):
                term = self._fresh(index.id)
                scope.ints[index.id] = term
                guards.append(bounds(term))
                if position == 0 and len(expr.generators) == 1 and not generator.ifs:
                    exact = self._range_length(generator.iter, state)
            for condition in generator.ifs:
                guards.append(self._bool(condition, scope, guards))
        if isinstance(expr, ast.DictComp):
            self._value(expr.key, scope, guards, record)
            self._value(expr.value, scope, guards, record)
            return ("new", "dict", None)
        self._value(expr.elt, scope, guards, record)
        if isinstance(expr, ast.ListComp):
            return ("new", "list", exact)
        return ("new", "dict", None) if isinstance(expr, ast.SetComp) else None

    def _range_length(self, iterable: ast.expr, state: _State) -> Any:
        """Return the length of range(n) or range(a, b); None for other iterables."""
        if (
            not isinstance(iterable, ast.Call)
            or not self._is_call(iterable, "range")
            or len(iterable.args) not in (1, 2)
        ):
            return None
        bounds = [self._int(arg, state, [], record=False) for arg in iterable.args]
        low, high = (z3.IntVal(0), bounds[0]) if len(bounds) == 1 else bounds
        return z3.If(high > low, high - low, z3.IntVal(0))

    def _record(self, node: ast.Subscript, index: Any, length: Any, state: _State, assume: list[Any]) -> None:
        name = ast.unparse(node.value)
        simple_length = z3.simplify(length)
        simple_index = z3.simplify(index)
        region = MemoryRegion(
            name=name,
            base_address=f"&{name}",
            size=simple_length.as_long() if z3.is_int_value(simple_length) else "unknown",
            element_type="unknown",
        )
        self.memory_regions[name] = region
        self.memory_accesses.append(
            MemoryAccess(
                region=region,
                access_type="read" if isinstance(node.ctx, ast.Load) else "write",
                offset=simple_index.as_long() if z3.is_int_value(simple_index) else ast.unparse(node.slice),
                size=1,
                line_number=node.lineno,
                path_condition=z3.And(*state.facts, *assume),
                index_term=index,
                length_term=length,
            )
        )
