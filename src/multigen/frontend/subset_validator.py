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
from .ast_analyzer import receiver_arg_ids
from .diagnostics import Diagnostic, DiagnosticSeverity, RuleId, SourceSpan


class SubsetTier(Enum):
    """Tiers of Python subset support."""

    TIER_1_FUNDAMENTAL = 1  # Core features
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
    # Registry key, assigned at construction. Backend profiles reject by key.
    key: str = ""
    ast_nodes: list[type] = field(default_factory=list)
    validator: Optional[Callable[..., bool]] = None
    # Decides whether this rule governs a node, as opposed to whether the node
    # satisfies it. Several rules claim ast.ClassDef; only one describes any
    # given class. A rule with no matcher governs every node of its types.
    matcher: Optional[Callable[..., bool]] = None
    # Like matcher, for rules that depend on where the node sits.
    context_matcher: Optional[Callable[["ast.AST", "ValidationContext"], bool]] = None
    # Alternative to `validator` for rules with specific messages: returns None
    # when the node is acceptable, otherwise the diagnostic describing why not.
    diagnose: Optional[Callable[..., Optional["Diagnostic"]]] = None
    # Order in which competing rules for one node type are consulted.
    priority: int = 0
    # Set for rules that describe a runtime construct, so they stay silent when
    # the same node type appears inside a type annotation.
    skip_in_annotations: bool = False
    constraints: list[str] = field(default_factory=list)
    examples: dict[str, str] = field(default_factory=dict)
    c_mapping: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of subset validation.

    `diagnostics` is the store; `violations` and `warnings` are string views of
    it, so the two can never disagree.
    """

    is_valid: bool
    tier: SubsetTier
    diagnostics: list[Diagnostic] = field(default_factory=list)
    supported_features: list[str] = field(default_factory=list)
    unsupported_features: list[str] = field(default_factory=list)
    # (rule key, where it was used). A profile that rejects a feature needs a
    # position to report, and an accepted feature produces no diagnostic.
    feature_uses: list[tuple[str, Optional[SourceSpan]]] = field(default_factory=list)
    conversion_strategy: Optional[str] = None

    @property
    def violations(self) -> list[str]:
        """Error messages, in the order they were reported."""
        return [d.message for d in self.diagnostics if d.is_error]

    @property
    def warnings(self) -> list[str]:
        """Warning messages, in the order they were reported."""
        return [d.message for d in self.diagnostics if not d.is_error]

    def errors(self) -> list[Diagnostic]:
        """Diagnostics that invalidate the source."""
        return [d for d in self.diagnostics if d.is_error]


def _ast_types(*names: str) -> frozenset[type]:
    """Resolve AST node names, tolerating types absent on older Pythons."""
    return frozenset(node for node in (getattr(ast, name, None) for name in names) if isinstance(node, type))


# Node types that carry no policy of their own: syntax scaffolding, operators,
# and statements whose meaning is already governed by an enclosing rule. The
# validator is an allowlist, so any node reaching it that is neither here nor
# claimed by a feature rule is rejected. That is deliberate: a construct nobody
# has classified must not reach a backend on the strength of being unrecognised.
STRUCTURAL_AST_NODES: frozenset[type] = _ast_types(
    # Module scaffolding and simple statements
    "Module",
    "Expr",
    "Assign",
    "AugAssign",
    "Return",
    "Pass",
    "Break",
    "Continue",
    "Import",
    "ImportFrom",
    # Expressions whose element types are checked by their own rules
    "Name",
    "Attribute",
    "Dict",
    "Set",
    "IfExp",
    "BoolOp",
    "Slice",
    # Syntax scaffolding
    "Load",
    "Store",
    "alias",
    "arg",
    "arguments",
    "keyword",
    "comprehension",
    "withitem",
    "match_case",
    # Binary operators
    "Add",
    "Sub",
    "Mult",
    "Div",
    "FloorDiv",
    "Mod",
    "Pow",
    "LShift",
    "RShift",
    "BitOr",
    "BitXor",
    "BitAnd",
    # Unary and boolean operators
    "Invert",
    "Not",
    "UAdd",
    "USub",
    "And",
    "Or",
    # Comparison operators
    "Eq",
    "NotEq",
    "Lt",
    "LtE",
    "Gt",
    "GtE",
    "Is",
    "IsNot",
    "In",
    "NotIn",
    # Match patterns; the enclosing Match statement carries the rule
    "MatchValue",
    "MatchSingleton",
    "MatchSequence",
    "MatchMapping",
    "MatchClass",
    "MatchStar",
    "MatchAs",
    "MatchOr",
)


def _annotation_node_ids(tree: ast.AST) -> frozenset[int]:
    """Identify every node that is part of a type annotation.

    A tuple written as a value and a tuple written inside `dict[str, int]` are
    the same ast.Tuple. The first is a runtime construct the backends mostly
    cannot build; the second is type syntax they handle routinely. Without
    knowing which is which, a rule about tuples has to be wrong about one of
    them.
    """
    annotations: list[ast.AST] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.AnnAssign):
            annotations.append(node.annotation)
        elif isinstance(node, ast.arg) and node.annotation is not None:
            annotations.append(node.annotation)
        elif isinstance(node, ast.FunctionDef) and node.returns is not None:
            annotations.append(node.returns)

    ids: set[int] = set()
    for annotation in annotations:
        for node in ast.walk(annotation):
            ids.add(id(node))
    return frozenset(ids)


def _items_target_ids(tree: ast.AST) -> frozenset[int]:
    """Identify `k, v` tuples that unpack `d.items()` in a for loop or comprehension.

    This is the one tuple form every backend translates: each maps it onto its
    own key/value iteration without building a tuple value.
    """
    ids: set[int] = set()
    for node in ast.walk(tree):
        if isinstance(node, (ast.For, ast.comprehension)):
            target, source = node.target, node.iter
            if (
                isinstance(target, ast.Tuple)
                and len(target.elts) == 2
                and all(isinstance(elt, ast.Name) for elt in target.elts)
                and isinstance(source, ast.Call)
                and isinstance(source.func, ast.Attribute)
                and source.func.attr == "items"
                and not source.args
                and not source.keywords
            ):
                ids.add(id(target))
    return frozenset(ids)


@dataclass(frozen=True)
class ValidationContext:
    """Where a node sits, for rules whose meaning depends on position."""

    annotation_nodes: frozenset[int] = frozenset()
    items_targets: frozenset[int] = frozenset()
    receivers: frozenset[int] = frozenset()

    def in_annotation(self, node: ast.AST) -> bool:
        """Whether this node is part of a type annotation."""
        return id(node) in self.annotation_nodes

    def is_items_target(self, node: ast.AST) -> bool:
        """Whether this node is the `k, v` target of a loop over `d.items()`."""
        return id(node) in self.items_targets


class StaticPythonSubsetValidator:
    """Validator for the Static Python Subset."""

    def __init__(self) -> None:
        self.log = log.config(self.__class__.__name__)
        self.feature_rules = self._initialize_feature_rules()
        for key, rule in self.feature_rules.items():
            rule.key = key

    def validate_code(self, source_code: str) -> ValidationResult:
        """Validate that code conforms to the Static Python Subset."""
        try:
            tree = ast.parse(source_code)
            return self._validate_ast(tree)
        except SyntaxError as e:
            span = SourceSpan(line=e.lineno, column=(e.offset or 1) - 1) if e.lineno else None
            return ValidationResult(
                is_valid=False,
                tier=SubsetTier.TIER_4_UNSUPPORTED,
                diagnostics=[
                    Diagnostic(
                        rule_id=RuleId.SYNTAX_ERROR,
                        severity=DiagnosticSeverity.ERROR,
                        message=f"Syntax error: {e}",
                        span=span,
                    )
                ],
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

    def validate_ast(self, tree: ast.AST) -> ValidationResult:
        """Validate an already-parsed AST, so callers can parse once."""
        return self._validate_ast(tree)

    def _validate_ast(self, tree: ast.AST) -> ValidationResult:
        """Validate an AST against the subset rules."""
        result = ValidationResult(is_valid=True, tier=SubsetTier.TIER_1_FUNDAMENTAL)
        max_tier = SubsetTier.TIER_1_FUNDAMENTAL

        context = ValidationContext(
            annotation_nodes=_annotation_node_ids(tree),
            items_targets=_items_target_ids(tree),
            receivers=receiver_arg_ids(tree),
        )

        # Check each node against our rules
        for node in ast.walk(tree):
            node_result = self._validate_node(node, context)

            # Merge results
            if not node_result.is_valid:
                result.is_valid = False

            result.diagnostics.extend(node_result.diagnostics)
            result.supported_features.extend(node_result.supported_features)
            result.unsupported_features.extend(node_result.unsupported_features)
            result.feature_uses.extend(node_result.feature_uses)

            # Track highest tier used
            if node_result.tier.value > max_tier.value:
                max_tier = node_result.tier

        result.tier = max_tier

        # ast.walk is breadth-first, so report in source order instead. The sort
        # is stable, so diagnostics at one position keep their discovery order.
        result.diagnostics.sort(key=lambda d: (0, d.span.line, d.span.column) if d.span else (1, 0, 0))

        # Remove duplicates
        result.supported_features = list(set(result.supported_features))
        result.unsupported_features = list(set(result.unsupported_features))

        # Determine conversion strategy
        result.conversion_strategy = self._determine_conversion_strategy(result)

        return result

    @staticmethod
    def _diagnostic(
        rule_id: str,
        message: str,
        node: ast.AST,
        *,
        severity: DiagnosticSeverity = DiagnosticSeverity.ERROR,
        feature: Optional[str] = None,
        remediation: Optional[str] = None,
        **evidence: str,
    ) -> Diagnostic:
        """Build a diagnostic positioned at `node`."""
        return Diagnostic(
            rule_id=rule_id,
            severity=severity,
            message=message,
            span=SourceSpan.from_node(node),
            node_type=type(node).__name__,
            feature=feature,
            remediation=remediation,
            evidence=evidence,
        )

    def _governing_rule(
        self, node: ast.AST, context: Optional["ValidationContext"] = None
    ) -> tuple[bool, Optional[FeatureRule]]:
        """Find the single rule that classifies this node.

        Returns whether any rule claims the node's type at all, and the one rule
        that governs this particular node. A claimed type with no governing rule
        is legal: a plain class is an ast.ClassDef that is neither an enum, a
        dataclass, a named tuple, nor metaclass-based, and its body is validated
        by the rules for the nodes it contains.
        """
        node_type = type(node)
        candidates = [rule for rule in self.feature_rules.values() if node_type in rule.ast_nodes]
        if not candidates:
            return False, None

        context = context or ValidationContext()
        for rule in sorted(candidates, key=lambda r: r.priority):
            if rule.matcher is not None and not rule.matcher(node):
                continue
            if rule.context_matcher is not None and not rule.context_matcher(node, context):
                continue
            return True, rule
        return True, None

    def _validate_node(self, node: ast.AST, context: Optional[ValidationContext] = None) -> ValidationResult:
        """Validate a single AST node against the one rule that governs it."""
        node_type = type(node)
        result = ValidationResult(is_valid=True, tier=SubsetTier.TIER_1_FUNDAMENTAL)
        context = context or ValidationContext()

        recognised, rule = self._governing_rule(node, context)

        if rule is not None and rule.skip_in_annotations and context.in_annotation(node):
            # Type syntax, not a runtime construct: this rule has nothing to say.
            return result

        if not recognised:
            if node_type not in STRUCTURAL_AST_NODES:
                # Unrecognised construct: reject rather than let it reach a
                # backend that will either fail late or silently drop it.
                result.is_valid = False
                result.tier = SubsetTier.TIER_4_UNSUPPORTED
                result.diagnostics.append(
                    self._diagnostic(
                        RuleId.UNRECOGNIZED_CONSTRUCT,
                        f"Unsupported Python construct: {node_type.__name__}",
                        node,
                        remediation="No rule classifies this construct, so no backend can be trusted with it.",
                    )
                )
                result.unsupported_features.append(node_type.__name__)
            return result

        if rule is None:
            return result

        def reject(diagnostic: Diagnostic) -> ValidationResult:
            result.is_valid = False
            result.tier = SubsetTier.TIER_4_UNSUPPORTED
            result.diagnostics.append(diagnostic)
            result.unsupported_features.append(rule.name)
            return result

        def accept() -> None:
            result.supported_features.append(rule.name)
            result.feature_uses.append((rule.key, SourceSpan.from_node(node)))
            if rule.tier.value > result.tier.value:
                result.tier = rule.tier

        if rule.status == FeatureStatus.NOT_SUPPORTED:
            return reject(
                self._diagnostic(
                    RuleId.UNSUPPORTED_FEATURE,
                    f"Unsupported feature: {rule.name}",
                    node,
                    feature=rule.name,
                    remediation=rule.description,
                )
            )

        if rule.status == FeatureStatus.PLANNED:
            # Planned means the backends have no implementation. Accepting it
            # here lets the construct reach a backend that drops it silently.
            return reject(
                self._diagnostic(
                    RuleId.FEATURE_NOT_IMPLEMENTED,
                    f"Feature not yet implemented: {rule.name}",
                    node,
                    feature=rule.name,
                    remediation="Avoid this construct until the backends implement it.",
                )
            )

        if rule.diagnose is not None:
            diagnostic = rule.diagnose(node, context)
            if diagnostic is not None:
                return reject(diagnostic)
        elif rule.validator is not None and not rule.validator(node):
            return reject(
                self._diagnostic(
                    RuleId.CONSTRAINT_VIOLATION,
                    f"Validation failed for {rule.name}",
                    node,
                    feature=rule.name,
                    remediation="; ".join(rule.constraints) or None,
                )
            )

        if rule.status == FeatureStatus.EXPERIMENTAL:
            result.diagnostics.append(
                self._diagnostic(
                    RuleId.EXPERIMENTAL_FEATURE,
                    f"Experimental feature: {rule.name}",
                    node,
                    severity=DiagnosticSeverity.WARNING,
                    feature=rule.name,
                )
            )

        accept()
        return result

    def _initialize_feature_rules(self) -> dict[str, FeatureRule]:
        """Initialize the feature rules for the Static Python Subset."""
        rules = {}

        # Tier 1: Fundamental Support

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
            diagnose=self._diagnose_function_def,
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
            diagnose=self._diagnose_f_string,
            c_mapping="String concatenation with type conversion (std::to_string, sprintf, etc.)",
            examples={
                "valid": 'f"Result: {x}"\nf"Pi: {pi:.2f}"\nf"Hex: {n:x}"',
                "invalid": 'f"Value: {x!r}"  # Conversion flags not supported',
            },
            constraints=["Format specs: .Nf, d, x, X, o, e, E, %", "No conversion flags (!r, !s, !a)"],
        )

        # Tier 2: Structured Data (Feasible)

        rules["enums"] = FeatureRule(
            name="Enumerations",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PLANNED,
            description="Python enums mapped to C enums",
            ast_nodes=[ast.ClassDef],
            matcher=self._matches_enum,
            priority=1,
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
            matcher=self._matches_dataclass,
            priority=3,
            validator=self._validate_dataclass,
            c_mapping="C struct definitions with constructor functions",
        )

        rules["dict_items_unpacking"] = FeatureRule(
            name="Dict Items Unpacking",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="`for k, v in d.items()` in loops and comprehensions; no tuple value is built",
            ast_nodes=[ast.Tuple],
            context_matcher=lambda node, context: context.is_items_target(node),
            priority=0,
            c_mapping="Iteration over the map's key/value entries",
        )

        rules["tuples"] = FeatureRule(
            name="Tuples",
            tier=SubsetTier.TIER_2_STRUCTURED,
            priority=1,
            # Audited across the backends: only TypeScript builds a tuple value.
            # C used to emit `tuple[int, int] p = /* Unsupported expression */`,
            # and the rest refuse. The annotation form, as in dict[str, int], is
            # handled everywhere, which is what skip_in_annotations preserves.
            status=FeatureStatus.NOT_SUPPORTED,
            description="Tuple values are not translatable; tuples in type annotations are",
            ast_nodes=[ast.Tuple],
            skip_in_annotations=True,
            c_mapping="Anonymous C structs",
        )

        rules["namedtuples"] = FeatureRule(
            name="Named Tuples",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="NamedTuple classes mapped to C structs",
            ast_nodes=[ast.ClassDef],
            matcher=self._matches_namedtuple,
            priority=2,
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
            matcher=self._matches_union_type,
            priority=0,
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
                ast_nodes=[ast.Match],
                c_mapping="Switch statements with guards",
            )

        rules["generators"] = FeatureRule(
            name="Generator Functions",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="Generator functions with yield/yield from (eager collection to list)",
            ast_nodes=[ast.Yield],
            validator=self._validate_yield,
            constraints=["No .send() or .throw()"],
            c_mapping="Function returning list/vector with accumulation pattern",
            examples={
                "valid": "def gen(n: int) -> int:\n    i: int = 0\n    while i < n:\n        yield i\n        i += 1",
                "invalid": "gen.send(42)  # .send() not supported",
            },
        )

        rules["yield_from"] = FeatureRule(
            name="Yield From",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="yield from for extending accumulator with iterable (eager collection)",
            ast_nodes=[ast.YieldFrom],
            diagnose=self._diagnose_yield_from,
            constraints=["Function calls, range(), and variables only", "No .send() or .throw()"],
            c_mapping="Extend accumulator with all elements from iterable",
            examples={
                "valid": "def gen(n: int) -> int:\n    yield from range(n)",
                "invalid": "def gen():\n    yield from (x for x in range(10))",
            },
        )

        rules["generator_expressions"] = FeatureRule(
            name="Generator Expressions",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.FULLY_SUPPORTED,
            description="Generator expressions normalized to list comprehensions (eager collection)",
            ast_nodes=[ast.GeneratorExp],
            c_mapping="Normalized to list comprehension, then standard comprehension handling",
            examples={
                "valid": "total: int = sum(x * x for x in range(10))",
                "invalid": "g = (x for x in range(10))  # Standalone genexpr as lazy iterator",
            },
        )

        rules["generics"] = FeatureRule(
            name="Generic Types",
            tier=SubsetTier.TIER_3_ADVANCED,
            # Audited against every backend: list[int] and dict[str, int] map to
            # vec_int, &Vec<i32>, number[] and []int respectively. Only the LLVM
            # backend refuses. "Experimental" was stale, and made the strict
            # profile reject most of the corpus over annotations that work.
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="Generic types via monomorphization (not supported by the LLVM backend)",
            ast_nodes=[ast.Subscript],  # List[T], Dict[K, V]
            priority=1,
            c_mapping="Template instantiation/monomorphization",
        )

        # Tier 4: Unsupported Features

        rules["negative_index"] = FeatureRule(
            name="Negative Indexing",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Index from the end with len(xs) - k: no backend translates xs[-k]",
            ast_nodes=[ast.Subscript],
            matcher=self._matches_negative_index,
            priority=0,
            skip_in_annotations=True,
        )

        rules["metaclasses"] = FeatureRule(
            name="Metaclasses",
            tier=SubsetTier.TIER_4_UNSUPPORTED,
            status=FeatureStatus.NOT_SUPPORTED,
            description="Metaclasses require runtime introspection",
            ast_nodes=[ast.ClassDef],
            matcher=self._matches_metaclass,
            priority=0,
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
            # No backend translates a second `for` clause: four refuse, three
            # emit code that does not build, and TypeScript fails at runtime.
            validator=self._validate_single_generator,
            constraints=["One `for` clause per comprehension"],
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
            tier=SubsetTier.TIER_2_STRUCTURED,
            # Measured by executing generated C against CPython: an explicit
            # `raise` is caught and `finally` runs, but no operation raises.
            # `10 // 0` inside a try is undefined behaviour in C, so the program
            # dies of SIGFPE where Python returns from the except clause.
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description=(
                "try/except/else/finally for explicitly raised exceptions. Operations do not raise: "
                "a ZeroDivisionError or IndexError crashes instead of reaching the handler"
            ),
            ast_nodes=[ast.Try, ast.Raise, ast.ExceptHandler],
            diagnose=self._diagnose_exception_handling,
            constraints=["No exception chaining (raise ... from ...)"],
            c_mapping="try/catch in C++, try/with in OCaml",
            examples={
                "valid": "try:\n    x = 1\nexcept ValueError:\n    x = 0\nelse:\n    print('ok')\nfinally:\n    cleanup()",
                "invalid": "raise ValueError('msg') from original_error",
            },
        )

        rules["context_managers"] = FeatureRule(
            name="Context Managers",
            tier=SubsetTier.TIER_2_STRUCTURED,
            status=FeatureStatus.PARTIALLY_SUPPORTED,
            description="Basic with statement for file I/O (single context manager)",
            ast_nodes=[ast.With],
            diagnose=self._diagnose_with_statement,
            constraints=["Single context manager only", "File operations only", "Requires 'as' binding"],
            c_mapping="RAII in C++, defer in Go, bracket in Haskell",
            examples={
                "valid": 'with open("file.txt", "r") as f:\n    content = f.read()',
                "invalid": 'with open("a.txt") as f1, open("b.txt") as f2:\n    pass',
            },
        )

        return rules

    # Validator methods for specific features

    def _diagnose_function_def(self, node: ast.FunctionDef, context: ValidationContext) -> Optional[Diagnostic]:
        """Validate function definition constraints."""
        if not node.returns:
            return self._diagnostic(
                RuleId.MISSING_RETURN_ANNOTATION,
                f"Function '{node.name}' at line {node.lineno} is missing return type annotation",
                node,
                feature="Function Definitions",
                remediation=f"Annotate the return type, for example `def {node.name}(...) -> int:`.",
                function=node.name,
            )

        for arg in node.args.args:
            if not arg.annotation and id(arg) not in context.receivers:
                return self._diagnostic(
                    RuleId.UNANNOTATED_PARAMETER,
                    f"Function '{node.name}' at line {node.lineno}: parameter '{arg.arg}' is missing type annotation",
                    arg,
                    feature="Function Definitions",
                    remediation=f"Annotate the parameter, for example `{arg.arg}: int`.",
                    function=node.name,
                    parameter=arg.arg,
                )

        allowed_decorators = {"staticmethod", "classmethod"}
        for decorator in node.decorator_list:
            if isinstance(decorator, ast.Name) and decorator.id not in allowed_decorators:
                return self._diagnostic(
                    RuleId.UNSUPPORTED_DECORATOR,
                    f"Function '{node.name}' at line {node.lineno}: decorator '@{decorator.id}' is not allowed",
                    decorator,
                    feature="Function Definitions",
                    remediation=f"Remove '@{decorator.id}'. Supported: {', '.join(sorted(allowed_decorators))}.",
                    function=node.name,
                    decorator=decorator.id,
                )

        return None

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
        """Validate enum constraints. The matcher has established this is an enum."""
        for stmt in node.body:
            if isinstance(stmt, ast.Assign):
                if len(stmt.targets) == 1 and isinstance(stmt.value, ast.Constant):
                    if not isinstance(stmt.value.value, int):
                        return False
        return True

    def _validate_dataclass(self, node: ast.ClassDef) -> bool:
        """Validate dataclass constraints. The matcher has established the decorator."""
        context = ValidationContext(receivers=receiver_arg_ids(node))
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
                if self._diagnose_function_def(stmt, context) is not None:
                    return False
            elif isinstance(stmt, ast.Pass):
                continue  # Pass statements are OK
            else:
                return False  # Other statements not allowed

        return True

    def _validate_namedtuple(self, node: ast.ClassDef) -> bool:
        """Validate namedtuple constraints. The matcher has established the base class."""
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

    def _matches_metaclass(self, node: ast.ClassDef) -> bool:
        """A class declaring `metaclass=` is governed by the metaclass rule."""
        return any(keyword.arg == "metaclass" for keyword in node.keywords)

    def _matches_enum(self, node: ast.ClassDef) -> bool:
        """A class inheriting from Enum is governed by the enum rule."""
        for base in node.bases:
            if isinstance(base, ast.Name) and base.id == "Enum":
                return True
            if isinstance(base, ast.Attribute) and base.attr == "Enum":
                return True
        return False

    def _matches_namedtuple(self, node: ast.ClassDef) -> bool:
        """A class inheriting from NamedTuple is governed by the namedtuple rule."""
        for base in node.bases:
            if isinstance(base, ast.Name) and base.id == "NamedTuple":
                return True
            if isinstance(base, ast.Attribute) and base.attr == "NamedTuple":
                return True
        return False

    def _matches_dataclass(self, node: ast.ClassDef) -> bool:
        """A class carrying @dataclass is governed by the dataclass rule."""
        for decorator in node.decorator_list:
            if isinstance(decorator, ast.Name) and decorator.id == "dataclass":
                return True
            if isinstance(decorator, ast.Call) and isinstance(decorator.func, ast.Name):
                if decorator.func.id == "dataclass":
                    return True
        return False

    def _matches_negative_index(self, node: ast.Subscript) -> bool:
        """Match `xs[-k]` for an integer constant k; every backend reads or writes out of range."""
        index = node.slice
        return (
            isinstance(index, ast.UnaryOp)
            and isinstance(index.op, ast.USub)
            and isinstance(index.operand, ast.Constant)
            and type(index.operand.value) is int
            and index.operand.value > 0
        )

    def _matches_union_type(self, node: ast.Subscript) -> bool:
        """Union[...] and Optional[...] subscripts, as opposed to generics."""
        target = node.value
        if isinstance(target, ast.Name):
            return target.id in ("Union", "Optional")
        if isinstance(target, ast.Attribute):
            return target.attr in ("Union", "Optional")
        return False

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

    def _validate_single_generator(self, node: ast.AST) -> bool:
        """Validate that a comprehension has exactly one `for` clause."""
        return len(getattr(node, "generators", [])) == 1

    def _validate_list(self, node: ast.List) -> bool:
        """Validate list constraints."""
        # All elements should be the same type
        if not node.elts:
            return True  # Empty list is OK

        def kind(elt: ast.expr) -> object:
            # Literals compare by value type: -7 is a UnaryOp node but an int.
            if isinstance(elt, ast.UnaryOp) and isinstance(elt.op, (ast.USub, ast.UAdd)):
                elt = elt.operand
            if isinstance(elt, ast.Constant):
                return type(elt.value)
            return type(elt)

        first = kind(node.elts[0])
        return all(kind(elt) is first for elt in node.elts)

    def _validate_union_type(self, node: ast.Subscript) -> bool:
        """Validate union type constraints. The matcher has established the form."""
        # Union types should have a reasonable number of alternatives.
        if isinstance(node.slice, ast.Tuple):
            return len(node.slice.elts) <= 4
        return True

    def _diagnose_f_string(self, node: ast.AST, context: ValidationContext) -> Optional[Diagnostic]:
        """Validate f-string constraints."""
        if isinstance(node, ast.JoinedStr):
            # Check each formatted value in the f-string
            for value in node.values:
                if isinstance(value, ast.FormattedValue):
                    # Conversion flags (!r, !s, !a) not supported
                    if value.conversion != -1:
                        return self._diagnostic(
                            RuleId.FSTRING_CONVERSION_FLAG,
                            "F-string conversion flags (!r, !s, !a) are not yet supported "
                            f"at line {node.lineno if hasattr(node, 'lineno') else '?'}",
                            value,
                            feature="F-Strings",
                            remediation="Format the value explicitly instead of using !r, !s or !a.",
                        )
                    # Validate format spec if present (only simple numeric specs supported)
                    if value.format_spec is not None and isinstance(value.format_spec, ast.JoinedStr):
                        spec_str = self._extract_format_spec_string(value.format_spec)
                        if spec_str is not None and not self._is_supported_format_spec(spec_str):
                            return self._diagnostic(
                                RuleId.FSTRING_FORMAT_SPEC,
                                f"F-string format spec ':{spec_str}' is not supported "
                                f"at line {node.lineno if hasattr(node, 'lineno') else '?'}. "
                                "Supported: numeric precision (.Nf), integer (d), hex (x/X), octal (o), "
                                "scientific (e/E), percentage (%)",
                                value,
                                feature="F-Strings",
                                remediation="Use a supported format spec: .Nf, d, x, X, o, e, E or %.",
                                format_spec=spec_str,
                            )
        return None

    def _extract_format_spec_string(self, format_spec: ast.JoinedStr) -> Optional[str]:
        """Extract a simple string from a format_spec JoinedStr node."""
        parts: list[str] = []
        for value in format_spec.values:
            if isinstance(value, ast.Constant) and isinstance(value.value, str):
                parts.append(value.value)
            else:
                return None  # Dynamic format spec, not supported
        return "".join(parts)

    def _is_supported_format_spec(self, spec: str) -> bool:
        """Check if a format specification string is supported."""
        import re

        # Supported format specs:
        # .Nf - float precision (e.g., .2f, .4f)
        # d   - integer
        # x/X - hex
        # o   - octal
        # b   - binary
        # e/E - scientific notation
        # %   - percentage
        # >N, <N, ^N - alignment with width (optional fill char)
        # 0Nd - zero-padded integer
        pattern = r"^[<>^]?\d*\.?\d*[dfxXobeE%]?$"
        return bool(re.match(pattern, spec)) and len(spec) > 0

    def _validate_no_dynamic_execution(self, node: ast.Call) -> bool:
        """Validate no dynamic code execution."""
        if isinstance(node.func, ast.Name):
            forbidden_functions = {"eval", "exec", "compile", "__import__"}
            return node.func.id not in forbidden_functions
        return True

    def _diagnose_exception_handling(self, node: ast.AST, context: ValidationContext) -> Optional[Diagnostic]:
        """Validate exception handling constraints.

        Supported: try/except with optional else and finally clauses. Rejects:
        - exception chaining (raise ... from ...)
        """
        if isinstance(node, ast.Try):
            return None

        elif isinstance(node, ast.Raise):
            # Check for exception chaining (raise ... from ...)
            if node.cause is not None:
                return self._diagnostic(
                    RuleId.EXCEPTION_CHAINING,
                    "Exception chaining (raise ... from ...) is not supported "
                    f"at line {node.lineno if hasattr(node, 'lineno') else '?'}",
                    node,
                    feature="Exception Handling",
                    remediation="Raise the exception without a `from` clause.",
                )

            return None

        # ExceptHandler nodes are always valid if we get here
        return None

    def _diagnose_with_statement(self, node: ast.AST, context: ValidationContext) -> Optional[Diagnostic]:
        """Validate with statement constraints.

        Only basic with statements are supported. Rejects:
        - Multiple context managers in a single with
        - Context managers without 'as' variable binding
        """
        if isinstance(node, ast.With):
            # Only allow single context manager
            if len(node.items) > 1:
                return self._diagnostic(
                    RuleId.MULTIPLE_CONTEXT_MANAGERS,
                    "Multiple context managers in single 'with' not supported "
                    f"at line {node.lineno if hasattr(node, 'lineno') else '?'}",
                    node,
                    feature="Context Managers",
                    remediation="Use one nested `with` statement per context manager.",
                )

            # Require variable binding
            item = node.items[0]
            if item.optional_vars is None:
                return self._diagnostic(
                    RuleId.CONTEXT_MANAGER_BINDING,
                    "Context manager requires 'as' variable binding "
                    f"at line {node.lineno if hasattr(node, 'lineno') else '?'}",
                    node,
                    feature="Context Managers",
                    remediation="Bind the context manager, for example `with open(p) as f:`.",
                )

            return None

        return None

    def _validate_yield(self, node: ast.AST) -> bool:
        """Validate yield statement constraints."""
        if isinstance(node, ast.Yield):
            return True
        return True

    def _diagnose_yield_from(self, node: ast.AST, context: ValidationContext) -> Optional[Diagnostic]:
        """Validate yield from statement constraints."""
        if isinstance(node, ast.YieldFrom):
            # Allow function calls, range(), and variable references
            value = node.value
            if isinstance(value, ast.Call):
                return None
            elif isinstance(value, ast.Name):
                return None
            else:
                return self._diagnostic(
                    RuleId.UNSUPPORTED_YIELD_FROM,
                    "yield from only supports function calls, range(), and variables "
                    f"at line {node.lineno if hasattr(node, 'lineno') else '?'}",
                    node,
                    feature="Yield From",
                    remediation="Assign the iterable to a variable, then `yield from` that variable.",
                )
        return None

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
