"""Python Subset Validation Framework.

This module defines and validates the "Static Python Subset" - the subset
of Python features that can be reliably converted to C code while maintaining
performance and correctness guarantees.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Callable, Optional

from ..common import log


class SubsetTier(Enum):
    """Tiers of Python subset support."""

    TIER_1_FUNDAMENTAL = 1  # Core features - production ready
    TIER_2_STRUCTURED = 2  # Structured data - feasible
    TIER_3_ADVANCED = 3  # Advanced patterns - research required
    TIER_4_UNSUPPORTED = 4  # Fundamental limitations


class FeatureStatus(Enum):
    """Status of feature support in the subset."""

    FULLY_SUPPORTED = "fully_supported"
    PARTIALLY_SUPPORTED = "partially_supported"
    EXPERIMENTAL = "experimental"
    PLANNED = "planned"
    NOT_SUPPORTED = "not_supported"


@dataclass
class FeatureRule:
    """Rule defining a feature's support in the Static Python Subset."""

    name: str
    tier: SubsetTier
    status: FeatureStatus
    description: str
    ast_nodes: list[type] = field(default_factory=list)
    validator: Optional[Callable] = None
    constraints: list[str] = field(default_factory=list)
    examples: dict[str, str] = field(default_factory=dict)
    c_mapping: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of subset validation."""

    is_valid: bool
    tier: SubsetTier
    violations: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    supported_features: list[str] = field(default_factory=list)
    unsupported_features: list[str] = field(default_factory=list)
    conversion_strategy: Optional[str] = None


class StaticPythonSubsetValidator:
    """Validator for the Static Python Subset."""

    def __init__(self) -> None:
        self.log = log.config(self.__class__.__name__)
        self.feature_rules = self._initialize_feature_rules()
        self.validation_cache: dict[str, ValidationResult] = {}
        self.last_validation_error: Optional[str] = None  # Store detailed error from validators

    def validate_code(self, source_code: str) -> ValidationResult:
        """Validate that code conforms to the Static Python Subset."""
        try:
            tree = ast.parse(source_code)
            return self._validate_ast(tree)
        except SyntaxError as e:
            return ValidationResult(
                is_valid=False, tier=SubsetTier.TIER_4_UNSUPPORTED, violations=[f"Syntax error: {e}"]
            )

    def validate_file(self, file_path: str) -> ValidationResult:
        """Validate a Python file."""
        with open(file_path, encoding="utf-8") as f:
            return self.validate_code(f.read())

    def get_feature_support(self, feature_name: str) -> Optional[FeatureRule]:
        """Get support information for a specific feature."""
        return self.feature_rules.get(feature_name)

    def list_supported_features(self, tier: Optional[SubsetTier] = None) -> list[FeatureRule]:
        """List all supported features, optionally filtered by tier."""
        rules = list(self.feature_rules.values())
        if tier:
            rules = [r for r in rules if r.tier == tier]
        return [r for r in rules if r.status != FeatureStatus.NOT_SUPPORTED]

    def _validate_ast(self, tree: ast.AST) -> ValidationResult:
        """Validate an AST against the subset rules."""
        result = ValidationResult(is_valid=True, tier=SubsetTier.TIER_1_FUNDAMENTAL)
        max_tier = SubsetTier.TIER_1_FUNDAMENTAL

        # Check each node against our rules
        for node in ast.walk(tree):
            node_result = self._validate_node(node)

            # Merge results
            if not node_result.is_valid:
                result.is_valid = False

            result.violations.extend(node_result.violations)
            result.warnings.extend(node_result.warnings)
            result.supported_features.extend(node_result.supported_features)
            result.unsupported_features.extend(node_result.unsupported_features)

            # Track highest tier used
            if node_result.tier.value > max_tier.value:
                max_tier = node_result.tier

        result.tier = max_tier

        # Remove duplicates
        result.supported_features = list(set(result.supported_features))
        result.unsupported_features = list(set(result.unsupported_features))

        # Determine conversion strategy
        result.conversion_strategy = self._determine_conversion_strategy(result)

        return result

    def _validate_node(self, node: ast.AST) -> ValidationResult:
        """Validate a single AST node."""
        node_type = type(node)
        result = ValidationResult(is_valid=True, tier=SubsetTier.TIER_1_FUNDAMENTAL)

        # Check against feature rules
        for _rule_name, rule in self.feature_rules.items():
            if node_type in rule.ast_nodes:
                # First run custom validator if present (for rules that need specific checking)
                validator_passed = True
                if rule.validator:
                    validator_passed = rule.validator(node)

                # Apply rule status based on validator result
                if rule.status == FeatureStatus.NOT_SUPPORTED:
                    # For NOT_SUPPORTED rules with validators, only fail if validator says so
                    if rule.validator:
                        if not validator_passed:
                            result.is_valid = False
                            result.tier = SubsetTier.TIER_4_UNSUPPORTED
                            result.violations.append(f"Unsupported feature: {rule.name}")
                            result.unsupported_features.append(rule.name)
                        else:
                            # Validator passed, treat as supported feature
                            result.supported_features.append(rule.name)
                            if rule.tier.value > result.tier.value:
                                result.tier = rule.tier
                    else:
                        # No validator, unconditionally unsupported
                        result.is_valid = False
                        result.tier = SubsetTier.TIER_4_UNSUPPORTED
                        result.violations.append(f"Unsupported feature: {rule.name}")
                        result.unsupported_features.append(rule.name)
                elif rule.status == FeatureStatus.EXPERIMENTAL:
                    if validator_passed:
                        result.warnings.append(f"Experimental feature: {rule.name}")
                        result.supported_features.append(rule.name)
                        if rule.tier.value > result.tier.value:
                            result.tier = rule.tier
                    else:
                        result.is_valid = False
                        result.violations.append(f"Validation failed for {rule.name}")
                else:
                    # FULLY_SUPPORTED, PARTIALLY_SUPPORTED, or PLANNED
                    if validator_passed:
                        result.supported_features.append(rule.name)
                        if rule.tier.value > result.tier.value:
                            result.tier = rule.tier
                    else:
                        result.is_valid = False
                        if rule.name == "Function Calls":
                            result.violations.append(
                                "Dynamic code execution functions (eval, exec, compile) not supported"
                            )
                        else:
                            # Use detailed error if available
                            if self.last_validation_error:
                                result.violations.append(self.last_validation_error)
                                self.last_validation_error = None
                            else:
                                result.violations.append(f"Validation failed for {rule.name}")

        return result

    def _initialize_feature_rules(self) -> dict[str, FeatureRule]:
        """Initialize the feature rules for the Static Python Subset."""
        rules = {}

        # Tier 1: Fundamental Support (Production Ready)

        rules["basic_types"] = FeatureRule(
            name="Basic Types",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Basic Python types: int, float, bool, str",
            ast_nodes=[ast.Constant],
            c_mapping="Direct mapping to C types",
            examples={
                "valid": "x: int = 42\ny: float = 3.14\nz: bool = True\ns: str = 'hello'",
                "invalid": "x = 42  # Missing type annotation",
            },
        )

        rules["function_definitions"] = FeatureRule(
            name="Function Definitions",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Type-annotated function definitions",
            ast_nodes=[ast.FunctionDef],
            validator=self._validate_function_def,
            constraints=["Must have type annotations", "No decorators except allowed ones"],
            c_mapping="C function declarations",
            examples={
                "valid": "def add(x: int, y: int) -> int:\n    return x + y",
                "invalid": "def add(x, y):  # Missing type annotations\n    return x + y",
            },
        )

        rules["variable_declarations"] = FeatureRule(
            name="Variable Declarations",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Annotated variable declarations",
            ast_nodes=[ast.AnnAssign],
            c_mapping="C variable declarations",
            examples={"valid": "result: int = x + y", "invalid": "result = x + y  # Missing type annotation"},
        )

        rules["control_flow"] = FeatureRule(
            name="Control Flow",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Basic control flow: if/else, while, for with range/containers, assert statements",
            ast_nodes=[ast.If, ast.While, ast.For, ast.Assert],
            validator=self._validate_control_flow,
            c_mapping="Direct mapping to C control structures and assert() function",
        )

        rules["arithmetic_operations"] = FeatureRule(
            name="Arithmetic Operations",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Basic arithmetic and comparison operations",
            ast_nodes=[ast.BinOp, ast.UnaryOp, ast.Compare],
            c_mapping="Direct mapping to C operators",
        )

        rules["f_strings"] = FeatureRule(
            name="F-Strings",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="F-string literals for string formatting",
            ast_nodes=[ast.JoinedStr, ast.FormattedValue],
            validator=self._validate_f_string,
            c_mapping="String concatenation with type conversion (std::to_string, sprintf, etc.)",
            examples={
                "valid": 'f"Result: {x}"\nf"Count: {len(items)} items"',
                "invalid": 'f"Value: {x:.2f}"  # Format specs not yet supported',
            },
            constraints=["No format specifications in Phase 1", "Expressions must be type-inferrable"],
        )

        # Tier 2: Structured Data (Feasible)

        rules["enums"] = FeatureRule(
            name="Enumerations",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PLANNED,
            description="Python enums mapped to C enums",
            ast_nodes=[ast.ClassDef],
            validator=self._validate_enum,
            c_mapping="C enum declarations",
            examples={
                "valid": "from enum import Enum\nclass Status(Enum):\n    IDLE = 0\n    RUNNING = 1",
                "invalid": "class Status(Enum):\n    IDLE = 'idle'  # Non-integer values",
            },
        )

        rules["dataclasses"] = FeatureRule(
            name="Data Classes",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Dataclasses mapped to C structs",
            ast_nodes=[ast.ClassDef],
            validator=self._validate_dataclass,
            c_mapping="C struct definitions with constructor functions",
        )

        rules["tuples"] = FeatureRule(
            name="Tuples",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="Fixed-size tuples as anonymous structs",
            ast_nodes=[ast.Tuple],
            c_mapping="Anonymous C structs",
        )

        rules["namedtuples"] = FeatureRule(
            name="Named Tuples",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="NamedTuple classes mapped to C structs",
            ast_nodes=[ast.ClassDef],
            validator=self._validate_namedtuple,
            c_mapping="C struct definitions with field access",
        )

        rules["lists"] = FeatureRule(
            name="Lists",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="Lists as arrays with size tracking",
            ast_nodes=[ast.List],
            validator=self._validate_list,
            constraints=["Fixed size or bounded growth", "Homogeneous element types"],
            c_mapping="C arrays with size metadata",
        )

        rules["union_types"] = FeatureRule(
            name="Union Types",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.EXPERIMENTAL,
            description="Union types as tagged unions",
            ast_nodes=[ast.Subscript],  # Union[int, str]
            validator=self._validate_union_type,
            c_mapping="Tagged unions in C",
        )

        # Tier 3: Advanced Patterns (Research Required)

        # Match statement only available in Python 3.10+
        if hasattr(ast, "Match"):
            rules["pattern_matching"] = FeatureRule(
                name="Pattern Matching",
                tier=SubsetTier.TIER_3_ADVANCED,
                status=FeatureStatus.PLANNED,
                description="Python 3.10+ match statements",
                ast_nodes=[ast.Match],  # type: ignore[attr-defined]
                c_mapping="Switch statements with guards",
            )

        rules["generators"] = FeatureRule(
            name="Generator Functions",
            tier=SubsetTier.TIER_3_ADVANCED,
            status=FeatureStatus.EXPERIMENTAL,
            description="Generators as state machines",
            ast_nodes=[ast.Yield],
            c_mapping="C state machine implementations",
        )

        rules["generics"] = FeatureRule(
            name="Generic Types",
            tier=SubsetTier.TIER_3_ADVANCED,
            status=FeatureStatus.EXPERIMENTAL,
            description="Generic types via monomorphization",
            ast_nodes=[ast.Subscript],  # List[T], Dict[K, V]
            c_mapping="Template instantiation/monomorphization",
        )

        # Tier 4: Unsupported Features

        rules["metaclasses"] = FeatureRule(
            name="Metaclasses",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Metaclasses require runtime introspection",
            ast_nodes=[ast.ClassDef],
            validator=self._validate_no_metaclasses,
        )

        rules["duck_typing"] = FeatureRule(
            name="Duck Typing",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Duck typing requires runtime type checks",
            ast_nodes=[],  # Hard to detect statically
        )

        rules["function_calls"] = FeatureRule(
            name="Function Calls",
            tier=SubsetTier.TIER_1_FUNDAMENTAL,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Static function calls (no eval/exec)",
            ast_nodes=[ast.Call],
            validator=self._validate_no_dynamic_execution,
            c_mapping="Direct function calls",
        )

        rules["comprehensions"] = FeatureRule(
            name="Comprehensions",
            tier=SubsetTier.TIER_3_ADVANCED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="List, dict, and set comprehensions converted to C loops with STC containers",
            ast_nodes=[ast.ListComp, ast.DictComp, ast.SetComp],
        )

        rules["lambda_functions"] = FeatureRule(
            name="Lambda Functions",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Lambda functions require function pointer support",
            ast_nodes=[ast.Lambda],
        )

        rules["exceptions"] = FeatureRule(
            name="Exception Handling",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Exception handling requires runtime stack unwinding",
            ast_nodes=[ast.Try, ast.Raise, ast.ExceptHandler],
        )

        return rules

    # Validator methods for specific features

    def _validate_function_def(self, node: ast.FunctionDef) -> bool:
        """Validate function definition constraints."""
        # Check return type annotation
        if not node.returns:
            self.last_validation_error = (
                f"Function '{node.name}' at line {node.lineno} is missing return type annotation"
            )
            return False

        # Must have type annotations for all parameters
        for arg in node.args.args:
            if not arg.annotation:
                self.last_validation_error = (
                    f"Function '{node.name}' at line {node.lineno}: parameter '{arg.arg}' is missing type annotation"
                )
                return False

        # No complex decorators
        allowed_decorators = {"staticmethod", "classmethod"}
        for decorator in node.decorator_list:
            if isinstance(decorator, ast.Name):
                if decorator.id not in allowed_decorators:
                    self.last_validation_error = (
                        f"Function '{node.name}' at line {node.lineno}: decorator '@{decorator.id}' is not allowed"
                    )
                    return False

        return True

    def _validate_control_flow(self, node: ast.stmt) -> bool:
        """Validate control flow constraints."""
        if isinstance(node, ast.For):
            # For loops can use range() or iterate over containers
            if isinstance(node.iter, ast.Call):
                # Allow range() calls and method calls that return iterables
                if isinstance(node.iter.func, ast.Name):
                    # range() calls
                    return node.iter.func.id == "range"
                elif isinstance(node.iter.func, ast.Attribute):
                    # Method calls like dict.items(), dict.values(), dict.keys()
                    return True
                return False
            elif isinstance(node.iter, ast.Name):
                # Container iteration: for item in container
                return True
            elif isinstance(node.iter, ast.Attribute):
                # Attribute access (shouldn't normally be iterable, but allowed)
                return True
            return False
        elif isinstance(node, ast.Assert):
            # Assert statements are allowed - they map to C assert()
            # The test expression should be a valid boolean expression
            return True

        return True

    def _validate_enum(self, node: ast.ClassDef) -> bool:
        """Validate enum constraints."""
        # Check if it's actually an enum
        for base in node.bases:
            if isinstance(base, ast.Name) and base.id == "Enum":
                # Validate enum members have integer values
                for stmt in node.body:
                    if isinstance(stmt, ast.Assign):
                        if len(stmt.targets) == 1 and isinstance(stmt.value, ast.Constant):
                            if not isinstance(stmt.value.value, int):
                                return False
                return True
        return True  # Not an enum, let other validators handle it

    def _validate_dataclass(self, node: ast.ClassDef) -> bool:
        """Validate dataclass constraints."""
        # Check for @dataclass decorator
        has_dataclass_decorator = False
        for decorator in node.decorator_list:
            if isinstance(decorator, ast.Name) and decorator.id == "dataclass":
                has_dataclass_decorator = True
                break
            elif isinstance(decorator, ast.Call) and isinstance(decorator.func, ast.Name):
                if decorator.func.id == "dataclass":
                    has_dataclass_decorator = True
                    break

        if not has_dataclass_decorator:
            return True  # Not a dataclass

        # Validate all fields have type annotations
        for stmt in node.body:
            if isinstance(stmt, ast.AnnAssign):
                # Type-annotated field - validate type is supported
                if not self._is_supported_type_annotation(stmt.annotation):
                    return False
            elif isinstance(stmt, ast.Assign):
                # Regular assignment without type annotation - not allowed
                return False
            elif isinstance(stmt, ast.FunctionDef):
                if stmt.name.startswith("__"):
                    continue  # Magic methods are OK
                # Regular methods should be simple
                if not self._validate_function_def(stmt):
                    return False
            elif isinstance(stmt, ast.Pass):
                continue  # Pass statements are OK
            else:
                return False  # Other statements not allowed

        return True

    def _validate_namedtuple(self, node: ast.ClassDef) -> bool:
        """Validate namedtuple constraints."""
        # Check if it inherits from NamedTuple
        is_namedtuple = False
        for base in node.bases:
            if isinstance(base, ast.Name) and base.id == "NamedTuple":
                is_namedtuple = True
                break
            elif isinstance(base, ast.Attribute):
                if isinstance(base.value, ast.Name) and base.value.id == "typing" and base.attr == "NamedTuple":
                    is_namedtuple = True
                    break

        if not is_namedtuple:
            return True  # Not a namedtuple

        # Validate all fields have type annotations and no methods
        for stmt in node.body:
            if isinstance(stmt, ast.AnnAssign):
                # Type-annotated field - validate type is supported
                if not self._is_supported_type_annotation(stmt.annotation):
                    return False
                # NamedTuple fields should not have default values in class body
                if stmt.value is not None:
                    return False
            elif isinstance(stmt, ast.Pass):
                continue  # Pass statements are OK
            elif isinstance(stmt, ast.FunctionDef):
                # Methods not allowed in NamedTuple
                return False
            else:
                return False  # Other statements not allowed

        return True

    def _is_supported_type_annotation(self, node: ast.expr) -> bool:
        """Check if a type annotation is supported for struct fields."""
        if isinstance(node, ast.Name):
            # Basic types
            return node.id in {"int", "float", "str", "bool"}
        elif isinstance(node, ast.Subscript):
            # Generic types like List[int], Dict[str, int]
            if isinstance(node.value, ast.Name):
                container_type = node.value.id
                # Allow basic container types
                return container_type in {"list", "List", "dict", "Dict", "set", "Set"}
        elif isinstance(node, ast.Attribute):
            # Qualified names like typing.List
            if isinstance(node.value, ast.Name) and node.value.id == "typing":
                return node.attr in {"List", "Dict", "Set", "Optional"}
        return False

    def _validate_list(self, node: ast.List) -> bool:
        """Validate list constraints."""
        # All elements should be the same type
        if not node.elts:
            return True  # Empty list is OK

        first_type = type(node.elts[0])
        return all(type(elt) == first_type for elt in node.elts)

    def _validate_union_type(self, node: ast.Subscript) -> bool:
        """Validate union type constraints."""
        if isinstance(node.value, ast.Name) and node.value.id == "Union":
            # Union types should have reasonable number of alternatives
            if isinstance(node.slice, ast.Tuple):
                return len(node.slice.elts) <= 4  # Arbitrary limit
        return True

    def _validate_f_string(self, node: ast.AST) -> bool:
        """Validate f-string constraints."""
        if isinstance(node, ast.JoinedStr):
            # Check each formatted value in the f-string
            for value in node.values:
                if isinstance(value, ast.FormattedValue):
                    # Phase 1: Don't support format specifications
                    if value.format_spec is not None:
                        self.last_validation_error = (
                            f"F-string format specifications (e.g., ':.2f') are not yet supported "
                            f"at line {node.lineno if hasattr(node, 'lineno') else '?'}"
                        )
                        return False
                    # Phase 1: Don't support conversion flags (!r, !s, !a)
                    if value.conversion != -1:
                        self.last_validation_error = (
                            f"F-string conversion flags (!r, !s, !a) are not yet supported "
                            f"at line {node.lineno if hasattr(node, 'lineno') else '?'}"
                        )
                        return False
        return True

    def _validate_no_metaclasses(self, node: ast.ClassDef) -> bool:
        """Validate no metaclasses are used."""
        for keyword in node.keywords:
            if keyword.arg == "metaclass":
                return False
        return True

    def _validate_no_dynamic_execution(self, node: ast.Call) -> bool:
        """Validate no dynamic code execution."""
        if isinstance(node.func, ast.Name):
            forbidden_functions = {"eval", "exec", "compile", "__import__"}
            return node.func.id not in forbidden_functions
        return True

    def _determine_conversion_strategy(self, result: ValidationResult) -> str:
        """Determine the appropriate conversion strategy."""
        if not result.is_valid:
            return "not_convertible"

        if result.tier == SubsetTier.TIER_1_FUNDAMENTAL:
            return "direct_conversion"
        elif result.tier == SubsetTier.TIER_2_STRUCTURED:
            return "structured_conversion_with_preprocessing"
        elif result.tier == SubsetTier.TIER_3_ADVANCED:
            return "advanced_conversion_with_intelligence_layer"
        else:
            return "not_convertible"

    def generate_subset_report(self) -> str:
        """Generate a comprehensive report of the Static Python Subset."""
        report_lines = [
            "# Static Python Subset - Feature Support Report",
            "",
            "This report describes the features supported in the Static Python Subset",
            "for Python-to-C conversion.",
            "",
        ]

        for tier in SubsetTier:
            tier_rules = [r for r in self.feature_rules.values() if r.tier == tier]
            if not tier_rules:
                continue

            report_lines.extend([f"## {tier.name.replace('_', ' ').title()}", ""])

            for rule in tier_rules:
                status_icon = {
                    FeatureStatus.FULLY_SUPPORTED: "✅",
                    FeatureStatus.PARTIALLY_SUPPORTED: "🟡",
                    FeatureStatus.EXPERIMENTAL: "🧪",
                    FeatureStatus.PLANNED: "📋",
                    FeatureStatus.NOT_SUPPORTED: "❌",
                }[rule.status]

                report_lines.extend(
                    [
                        f"### {status_icon} {rule.name}",
                        f"**Status:** {rule.status.value.replace('_', ' ').title()}",
                        f"**Description:** {rule.description}",
                        "",
                    ]
                )

                if rule.c_mapping:
                    report_lines.extend([f"**C Mapping:** {rule.c_mapping}", ""])

                if rule.constraints:
                    report_lines.extend(
                        ["**Constraints:**", *[f"- {constraint}" for constraint in rule.constraints], ""]
                    )

                if rule.examples:
                    for example_type, example_code in rule.examples.items():
                        report_lines.extend(
                            [f"**{example_type.title()} Example:**", "```python", example_code, "```", ""]
                        )

        return "\n".join(report_lines)
