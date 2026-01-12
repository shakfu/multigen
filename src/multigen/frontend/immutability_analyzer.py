"""Immutability analysis for Python code.

Analyzes Python functions to determine parameter mutability characteristics.
This is a backend-agnostic analysis - backends interpret results according to
their own semantics (e.g., Rust uses &T vs &mut T, C++ uses const& vs &).
"""

import ast
from enum import Enum


class MutabilityClass(Enum):
    """Classification of parameter mutability."""

    IMMUTABLE = "immutable"  # Type guarantees immutability (tuple, frozenset, Final)
    READ_ONLY = "read_only"  # Only read in function body, never modified
    MUTABLE = "mutable"  # Modified in function body
    UNKNOWN = "unknown"  # Cannot determine


class ImmutabilityAnalyzer:
    """Analyzes Python code to determine parameter mutability."""

    # Immutable built-in types
    IMMUTABLE_TYPES = {"tuple", "frozenset", "str", "bytes", "int", "float", "bool", "None"}

    # Read-only abstract base classes
    READONLY_ABC_TYPES = {"Sequence", "Mapping", "Set", "Collection", "Iterable"}

    # Mutable abstract base classes
    MUTABLE_ABC_TYPES = {"MutableSequence", "MutableMapping", "MutableSet"}

    # Methods that indicate mutation
    MUTATING_METHODS = {
        # List methods
        "append",
        "extend",
        "insert",
        "remove",
        "pop",
        "clear",
        "sort",
        "reverse",
        # Dict methods
        "update",
        "setdefault",
        "popitem",
        # Set methods
        "add",
        "discard",
        "intersection_update",
        "difference_update",
        "symmetric_difference_update",
    }

    def __init__(self) -> None:
        """Initialize the analyzer."""
        self.mutability_info: dict[str, dict[str, MutabilityClass]] = {}

    def analyze_function(self, func: ast.FunctionDef) -> dict[str, MutabilityClass]:
        """Analyze a function to determine parameter mutability.

        Args:
            func: Function definition AST node

        Returns:
            Dictionary mapping parameter names to mutability classifications
        """
        result = {}

        for arg in func.args.args:
            # Skip 'self' parameter
            if arg.arg == "self":
                continue

            mutability = self._analyze_parameter(arg, func)
            result[arg.arg] = mutability

        return result

    def _analyze_parameter(self, arg: ast.arg, func: ast.FunctionDef) -> MutabilityClass:
        """Analyze a single parameter to determine its mutability.

        Args:
            arg: Parameter AST node
            func: Function containing the parameter

        Returns:
            Mutability classification
        """
        # 1. Check type annotation for explicit immutability signals
        if arg.annotation:
            annotation_mutability = self._check_annotation_mutability(arg.annotation)
            if annotation_mutability == MutabilityClass.IMMUTABLE:
                return MutabilityClass.IMMUTABLE

        # 2. Check naming convention (ALL_CAPS suggests constant/immutable)
        if arg.arg.isupper() and "_" in arg.arg:
            return MutabilityClass.IMMUTABLE

        # 3. Analyze usage in function body
        usage_mutability = self._analyze_parameter_usage(arg.arg, func)

        # If annotation says mutable but usage is read-only, prefer usage analysis
        if arg.annotation:
            annotation_mutability = self._check_annotation_mutability(arg.annotation)
            if annotation_mutability == MutabilityClass.IMMUTABLE:
                return MutabilityClass.IMMUTABLE
            # If annotation suggests mutable type but usage is read-only, downgrade to read-only
            if annotation_mutability != MutabilityClass.IMMUTABLE and usage_mutability == MutabilityClass.READ_ONLY:
                return MutabilityClass.READ_ONLY

        return usage_mutability

    def _check_annotation_mutability(self, annotation: ast.expr) -> MutabilityClass:
        """Check if type annotation indicates immutability.

        Args:
            annotation: Type annotation AST node

        Returns:
            Mutability classification based on annotation
        """
        # Handle simple names: tuple, frozenset, str, etc.
        if isinstance(annotation, ast.Name):
            if annotation.id in self.IMMUTABLE_TYPES:
                return MutabilityClass.IMMUTABLE
            if annotation.id in self.READONLY_ABC_TYPES:
                return MutabilityClass.READ_ONLY
            if annotation.id in self.MUTABLE_ABC_TYPES:
                return MutabilityClass.MUTABLE

        # Handle subscripted types: tuple[int], Sequence[str], etc.
        if isinstance(annotation, ast.Subscript):
            if isinstance(annotation.value, ast.Name):
                if annotation.value.id in self.IMMUTABLE_TYPES:
                    return MutabilityClass.IMMUTABLE
                if annotation.value.id in self.READONLY_ABC_TYPES:
                    return MutabilityClass.READ_ONLY
                if annotation.value.id in self.MUTABLE_ABC_TYPES:
                    return MutabilityClass.MUTABLE

        # Handle attribute access: typing.Final, collections.abc.Sequence
        if isinstance(annotation, ast.Attribute):
            if annotation.attr == "Final":
                return MutabilityClass.IMMUTABLE
            if annotation.attr in self.READONLY_ABC_TYPES:
                return MutabilityClass.READ_ONLY
            if annotation.attr in self.MUTABLE_ABC_TYPES:
                return MutabilityClass.MUTABLE

        # Handle subscripted attributes: typing.Final[int]
        if isinstance(annotation, ast.Subscript):
            if isinstance(annotation.value, ast.Attribute) and annotation.value.attr == "Final":
                return MutabilityClass.IMMUTABLE

        return MutabilityClass.UNKNOWN

    def _analyze_parameter_usage(self, param_name: str, func: ast.FunctionDef) -> MutabilityClass:
        """Analyze how a parameter is used in the function body.

        Args:
            param_name: Name of parameter to analyze
            func: Function definition

        Returns:
            Mutability classification based on usage
        """
        has_reads = False
        has_mutations = False

        for node in ast.walk(func):
            # Check for mutating method calls
            if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call):
                if isinstance(node.value.func, ast.Attribute):
                    if isinstance(node.value.func.value, ast.Name) and node.value.func.value.id == param_name:
                        if node.value.func.attr in self.MUTATING_METHODS:
                            has_mutations = True

            # Check for subscript assignment: param[i] = value
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Subscript):
                        # Check if it's a direct subscript
                        if isinstance(target.value, ast.Name) and target.value.id == param_name:
                            has_mutations = True
                        # Check if it's a nested subscript: param[i][j] = value
                        elif isinstance(target.value, ast.Subscript):
                            if isinstance(target.value.value, ast.Name) and target.value.value.id == param_name:
                                has_mutations = True

            # Check for augmented assignment: param[i] += value (implies mutation)
            if isinstance(node, ast.AugAssign):
                if isinstance(node.target, ast.Subscript):
                    if isinstance(node.target.value, ast.Name) and node.target.value.id == param_name:
                        has_mutations = True

            # Check for reads (subscript access, iteration, etc.)
            if isinstance(node, ast.Subscript):
                if isinstance(node.value, ast.Name) and node.value.id == param_name:
                    has_reads = True
                # Nested subscript reads: param[i][j]
                elif isinstance(node.value, ast.Subscript):
                    if isinstance(node.value.value, ast.Name) and node.value.value.id == param_name:
                        has_reads = True

            # Check for use in for loop (iteration)
            if isinstance(node, ast.For):
                if isinstance(node.iter, ast.Name) and node.iter.id == param_name:
                    has_reads = True

        # Determine mutability based on usage patterns
        if has_mutations:
            return MutabilityClass.MUTABLE
        elif has_reads:
            return MutabilityClass.READ_ONLY
        else:
            # Parameter not used in function body
            return MutabilityClass.UNKNOWN

    def analyze_module(self, tree: ast.Module) -> dict[str, dict[str, MutabilityClass]]:
        """Analyze all functions in a module.

        Args:
            tree: Module AST

        Returns:
            Dictionary mapping function names to parameter mutability info
        """
        result = {}

        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                result[node.name] = self.analyze_function(node)

        self.mutability_info = result
        return result

    def get_parameter_mutability(self, func_name: str, param_name: str) -> MutabilityClass:
        """Get mutability classification for a specific parameter.

        Args:
            func_name: Function name
            param_name: Parameter name

        Returns:
            Mutability classification or UNKNOWN if not found
        """
        if func_name in self.mutability_info:
            return self.mutability_info[func_name].get(param_name, MutabilityClass.UNKNOWN)
        return MutabilityClass.UNKNOWN
