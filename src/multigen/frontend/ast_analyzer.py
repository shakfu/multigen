"""AST Analysis Framework for Static Python Code.

This module provides comprehensive AST analysis capabilities for the frontend layer,
focusing on static analysis of Python code that can be converted to C.
"""

import ast
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from ..common import log


class NodeType(Enum):
    """Types of AST nodes we can analyze."""

    FUNCTION_DEF = "function_def"
    CLASS_DEF = "class_def"
    VARIABLE_DEF = "variable_def"
    ASSIGNMENT = "assignment"
    FUNCTION_CALL = "function_call"
    RETURN = "return"
    IF_STMT = "if_stmt"
    WHILE_LOOP = "while_loop"
    FOR_LOOP = "for_loop"
    BINARY_OP = "binary_op"
    UNARY_OP = "unary_op"
    CONSTANT = "constant"
    NAME = "name"


class StaticComplexity(Enum):
    """Complexity levels for static analysis."""

    TRIVIAL = 1  # Simple constants, basic operations
    SIMPLE = 2  # Function calls, basic control flow
    MODERATE = 3  # Complex control flow, multiple variables
    COMPLEX = 4  # Advanced patterns, nested structures
    UNSUPPORTED = 5  # Dynamic features not convertible


@dataclass
class TypeInfo:
    """Information about a type in the static analysis."""

    name: str
    python_type: Optional[type] = None
    c_equivalent: Optional[str] = None
    is_mutable: bool = True
    is_nullable: bool = False
    constraints: list[str] = field(default_factory=list)

    def __post_init__(self) -> None:
        """Set up C equivalent mappings."""
        if not self.c_equivalent:
            self.c_equivalent = self._map_to_c_type()

    def _map_to_c_type(self) -> str:
        """Map Python type to C equivalent."""
        type_mapping = {
            "int": "int",
            "float": "double",
            "bool": "bool",
            "str": "char*",
            "NoneType": "void",
        }
        return type_mapping.get(self.name, "void*")


@dataclass
class VariableInfo:
    """Information about a variable in static analysis."""

    name: str
    type_info: Optional[TypeInfo]
    scope: str
    is_parameter: bool = False
    is_declared: bool = False
    first_assignment_line: Optional[int] = None
    usage_count: int = 0
    is_modified: bool = False


@dataclass
class FunctionInfo:
    """Information about a function in static analysis."""

    name: str
    parameters: list[VariableInfo] = field(default_factory=list)
    return_type: Optional[TypeInfo] = None
    local_variables: dict[str, VariableInfo] = field(default_factory=dict)
    complexity: StaticComplexity = StaticComplexity.SIMPLE
    calls_made: list[str] = field(default_factory=list)
    line_count: int = 0
    has_side_effects: bool = False


@dataclass
class AnalysisResult:
    """Result of AST analysis."""

    functions: dict[str, FunctionInfo] = field(default_factory=dict)
    global_variables: dict[str, VariableInfo] = field(default_factory=dict)
    imports: list[str] = field(default_factory=list)
    complexity: StaticComplexity = StaticComplexity.SIMPLE
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    convertible: bool = True
    conversion_confidence: float = 1.0


class ASTAnalyzer(ast.NodeVisitor):
    """Enhanced AST analyzer for static Python code analysis."""

    def __init__(self) -> None:
        self.log = log.config(self.__class__.__name__)
        self.result = AnalysisResult()
        self.current_function: Optional[str] = None
        self.current_scope = "global"
        self.type_hints: dict[str, TypeInfo] = {}
        self.node_types: dict[ast.AST, NodeType] = {}

    def analyze(self, source_code: str) -> AnalysisResult:
        """Analyze Python source code and return analysis results."""
        try:
            tree = ast.parse(source_code)
            self.visit(tree)
            self._finalize_analysis()
            return self.result
        except SyntaxError as e:
            self.result.errors.append(f"Syntax error: {e}")
            self.result.convertible = False
            self.result.conversion_confidence = 0.0
            return self.result

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Analyze function definitions."""
        self.node_types[node] = NodeType.FUNCTION_DEF

        func_info = FunctionInfo(name=node.name, line_count=node.end_lineno - node.lineno + 1 if node.end_lineno else 1)

        # Analyze return type annotation
        if node.returns:
            func_info.return_type = self._extract_type_info(node.returns)
        else:
            func_info.return_type = TypeInfo("void")

        # Analyze parameters
        for arg in node.args.args:
            if arg.annotation:
                type_info = self._extract_type_info(arg.annotation)
                param_info = VariableInfo(
                    name=arg.arg, type_info=type_info, scope=node.name, is_parameter=True, is_declared=True
                )
                func_info.parameters.append(param_info)
                func_info.local_variables[arg.arg] = param_info
            else:
                self.result.errors.append(f"Parameter '{arg.arg}' in function '{node.name}' lacks type annotation")
                self.result.convertible = False

        # Add function to result first
        self.result.functions[node.name] = func_info

        # Analyze function body
        old_function = self.current_function
        old_scope = self.current_scope
        self.current_function = node.name
        self.current_scope = node.name

        for stmt in node.body:
            self.visit(stmt)

        # Calculate complexity
        func_info.complexity = self._calculate_function_complexity(node)

        self.current_function = old_function
        self.current_scope = old_scope

    def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
        """Analyze annotated assignments (variable declarations)."""
        self.node_types[node] = NodeType.VARIABLE_DEF

        if isinstance(node.target, ast.Name):
            var_name = node.target.id
            type_info = self._extract_type_info(node.annotation)

            var_info = VariableInfo(
                name=var_name,
                type_info=type_info,
                scope=self.current_scope,
                is_declared=True,
                first_assignment_line=node.lineno,
            )

            if self.current_function:
                self.result.functions[self.current_function].local_variables[var_name] = var_info
            else:
                self.result.global_variables[var_name] = var_info

        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign) -> None:
        """Analyze regular assignments."""
        self.node_types[node] = NodeType.ASSIGNMENT

        for target in node.targets:
            if isinstance(target, ast.Name):
                var_name = target.id

                # Check if variable is declared
                var_info = self._get_variable_info(var_name)
                if not var_info:
                    # Create placeholder for type inference
                    # Local variables can be inferred, global variables still require annotations
                    if self.current_function:
                        # Create placeholder variable info for local variable
                        # Type will be inferred later by flow-sensitive analysis
                        var_info = VariableInfo(
                            name=var_name,
                            type_info=None,  # Will be inferred
                            is_parameter=False,
                            scope="local",
                        )
                        var_info.is_modified = True
                        var_info.usage_count = 1
                        self.result.functions[self.current_function].local_variables[var_name] = var_info
                    else:
                        # Global variables still require explicit annotation
                        self.result.errors.append(
                            f"Global variable '{var_name}' used without type annotation declaration"
                        )
                        self.result.convertible = False
                else:
                    var_info.is_modified = True
                    var_info.usage_count += 1

        self.generic_visit(node)

    def visit_Call(self, node: ast.Call) -> None:
        """Analyze function calls."""
        self.node_types[node] = NodeType.FUNCTION_CALL

        if isinstance(node.func, ast.Name):
            func_name = node.func.id
            if self.current_function:
                self.result.functions[self.current_function].calls_made.append(func_name)

        self.generic_visit(node)

    def visit_Return(self, node: ast.Return) -> None:
        """Analyze return statements."""
        self.node_types[node] = NodeType.RETURN
        self.generic_visit(node)

    def visit_If(self, node: ast.If) -> None:
        """Analyze if statements."""
        self.node_types[node] = NodeType.IF_STMT
        self.generic_visit(node)

    def visit_While(self, node: ast.While) -> None:
        """Analyze while loops."""
        self.node_types[node] = NodeType.WHILE_LOOP
        self.generic_visit(node)

    def visit_For(self, node: ast.For) -> None:
        """Analyze for loops."""
        self.node_types[node] = NodeType.FOR_LOOP

        # Check if it's a simple range-based loop
        if isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name):
            if node.iter.func.id != "range":
                self.result.warnings.append(
                    f"For loop at line {node.lineno} uses non-range iterator, may require special handling"
                )

        self.generic_visit(node)

    def visit_Name(self, node: ast.Name) -> None:
        """Analyze name references."""
        self.node_types[node] = NodeType.NAME

        if isinstance(node.ctx, ast.Load):
            # Variable usage
            var_info = self._get_variable_info(node.id)
            if var_info:
                var_info.usage_count += 1

        self.generic_visit(node)

    def _extract_type_info(self, annotation: ast.expr) -> TypeInfo:
        """Extract type information from type annotations."""
        if isinstance(annotation, ast.Name):
            return TypeInfo(annotation.id)
        elif isinstance(annotation, ast.Constant):
            return TypeInfo(str(annotation.value))
        elif isinstance(annotation, ast.Subscript):
            # Handle generic types like list[int]
            if isinstance(annotation.value, ast.Name):
                base_type = annotation.value.id
                if base_type == "list":
                    return TypeInfo("list", c_equivalent="*")
                elif base_type == "Optional":
                    inner_type = self._extract_type_info(annotation.slice)
                    inner_type.is_nullable = True
                    return inner_type

        # Fallback for complex annotations
        return TypeInfo("unknown")

    def _get_variable_info(self, var_name: str) -> Optional[VariableInfo]:
        """Get variable information from current scope."""
        if self.current_function:
            func_info = self.result.functions.get(self.current_function)
            if func_info and var_name in func_info.local_variables:
                return func_info.local_variables[var_name]

        return self.result.global_variables.get(var_name)

    def _calculate_function_complexity(self, node: ast.FunctionDef) -> StaticComplexity:
        """Calculate the complexity level of a function."""
        complexity_score: float = 0.0

        # Count control flow structures
        for child in ast.walk(node):
            if isinstance(child, (ast.If, ast.While, ast.For)):
                complexity_score += 1.0
            elif isinstance(child, ast.Call):
                complexity_score += 0.5
            elif isinstance(child, (ast.ListComp, ast.DictComp, ast.GeneratorExp)):
                complexity_score += 2.0  # Comprehensions are complex

        # Map score to complexity level
        if complexity_score <= 1:
            return StaticComplexity.TRIVIAL
        elif complexity_score <= 3:
            return StaticComplexity.SIMPLE
        elif complexity_score <= 6:
            return StaticComplexity.MODERATE
        elif complexity_score <= 10:
            return StaticComplexity.COMPLEX
        else:
            return StaticComplexity.UNSUPPORTED

    def _finalize_analysis(self) -> None:
        """Finalize the analysis with overall assessments."""
        # Calculate overall complexity
        max_complexity = StaticComplexity.TRIVIAL
        for func_info in self.result.functions.values():
            if func_info.complexity.value > max_complexity.value:
                max_complexity = func_info.complexity

        self.result.complexity = max_complexity

        # Calculate conversion confidence
        error_count = len(self.result.errors)
        warning_count = len(self.result.warnings)

        if error_count > 0:
            self.result.conversion_confidence = max(0.0, 1.0 - (error_count * 0.3))
        elif warning_count > 0:
            self.result.conversion_confidence = max(0.7, 1.0 - (warning_count * 0.1))

        # Determine if code is convertible
        if error_count > 0 or max_complexity == StaticComplexity.UNSUPPORTED:
            self.result.convertible = False


def analyze_python_code(source_code: str) -> AnalysisResult:
    """Convenience function to analyze Python code."""
    analyzer = ASTAnalyzer()
    return analyzer.analyze(source_code)


def analyze_python_file(file_path: str) -> AnalysisResult:
    """Analyze a Python file."""
    with open(file_path, encoding="utf-8") as f:
        source_code = f.read()
    return analyze_python_code(source_code)
