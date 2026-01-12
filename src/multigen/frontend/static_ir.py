"""Intermediate Representation for Static Python Code.

This module defines the Static Python IR - an intermediate representation
that bridges the gap between Python AST and C code generation, optimized
for the three-layer architecture.
"""

import ast
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Optional

from .ast_analyzer import StaticComplexity


class IRNodeType(Enum):
    """Types of IR nodes."""

    MODULE = "module"
    FUNCTION = "function"
    VARIABLE = "variable"
    EXPRESSION = "expression"
    STATEMENT = "statement"
    TYPE_DECLARATION = "type_declaration"
    CONTROL_FLOW = "control_flow"


class IRDataType(Enum):
    """IR data types with C mapping information."""

    VOID = ("void", "void")
    INT = ("int", "int")
    FLOAT = ("float", "double")  # Python float maps to C double
    BOOL = ("bool", "bool")
    STRING = ("str", "char*")
    LIST = ("list", "vec_int")  # Python list maps to vec_int (dynamic array)
    DICT = ("dict", "map")  # Python dict maps to hash map
    SET = ("set", "set")  # Python set maps to hash set
    POINTER = ("pointer", "*")
    ARRAY = ("array", "[]")
    STRUCT = ("struct", "struct")
    UNION = ("union", "union")
    FUNCTION_PTR = ("function_ptr", "(*)")

    def __init__(self, python_name: str, c_equivalent: str):
        self.python_name = python_name
        self.c_equivalent = c_equivalent


@dataclass
class IRLocation:
    """Source location information for IR nodes."""

    line: int
    column: int = 0
    end_line: Optional[int] = None
    end_column: Optional[int] = None
    filename: Optional[str] = None


@dataclass
class IRAnnotation:
    """Annotations for IR nodes providing metadata."""

    optimization_hints: list[str] = field(default_factory=list)
    performance_notes: list[str] = field(default_factory=list)
    conversion_notes: list[str] = field(default_factory=list)
    intelligence_layer_data: dict[str, Any] = field(default_factory=dict)


class IRNode(ABC):
    """Base class for all IR nodes."""

    def __init__(self, node_type: IRNodeType, location: Optional[IRLocation] = None):
        self.node_type = node_type
        self.location = location
        self.annotations = IRAnnotation()
        self.children: list[IRNode] = []
        self.parent: Optional[IRNode] = None

    def add_child(self, child: "IRNode") -> None:
        """Add a child node."""
        child.parent = self
        self.children.append(child)

    def remove_child(self, child: "IRNode") -> None:
        """Remove a child node."""
        if child in self.children:
            child.parent = None
            self.children.remove(child)

    def get_ancestors(self) -> list["IRNode"]:
        """Get all ancestor nodes."""
        ancestors = []
        current = self.parent
        while current:
            ancestors.append(current)
            current = current.parent
        return ancestors

    def find_children_by_type(self, node_type: IRNodeType) -> list["IRNode"]:
        """Find all children of a specific type."""
        return [child for child in self.children if child.node_type == node_type]

    @abstractmethod
    def to_dict(self) -> dict[str, Any]:
        """Convert node to dictionary representation."""
        pass

    @abstractmethod
    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for the visitor pattern."""
        pass


@dataclass
class IRType:
    """Type information in the IR."""

    base_type: IRDataType
    is_const: bool = False
    is_pointer: bool = False
    pointer_depth: int = 0
    array_dimensions: list[Optional[int]] = field(default_factory=list)
    struct_name: Optional[str] = None
    union_name: Optional[str] = None
    qualifiers: set[str] = field(default_factory=set)
    element_type: Optional["IRType"] = None  # For containers (LIST, DICT, SET)

    def is_numeric(self) -> bool:
        """Check if type is numeric."""
        return self.base_type in [IRDataType.INT, IRDataType.FLOAT]

    def is_aggregate(self) -> bool:
        """Check if type is an aggregate (struct/union/array)."""
        return self.base_type in [IRDataType.STRUCT, IRDataType.UNION] or bool(self.array_dimensions)

    def to_c_declaration(self, var_name: str = "") -> str:
        """Generate C type declaration."""
        base = self.base_type.c_equivalent

        if self.base_type == IRDataType.STRUCT and self.struct_name:
            base = f"struct {self.struct_name}"
        elif self.base_type == IRDataType.UNION and self.union_name:
            base = f"union {self.union_name}"

        # Add const qualifier
        if self.is_const:
            base = f"const {base}"

        # Add pointer stars
        stars = "*" * self.pointer_depth
        if self.is_pointer and not self.pointer_depth:
            stars = "*"

        # Add array dimensions
        arrays = "".join(f"[{dim if dim else ''}]" for dim in self.array_dimensions)

        return f"{base} {stars}{var_name}{arrays}".strip()


class IRModule(IRNode):
    """IR representation of a Python module."""

    def __init__(self, name: str, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.MODULE, location)
        self.name = name
        self.imports: list[str] = []
        self.functions: list[IRFunction] = []
        self.global_variables: list[IRVariable] = []
        self.type_declarations: list[IRTypeDeclaration] = []

    def add_function(self, function: "IRFunction") -> None:
        """Add a function to the module."""
        self.add_child(function)
        self.functions.append(function)

    def add_global_variable(self, variable: "IRVariable") -> None:
        """Add a global variable to the module."""
        self.add_child(variable)
        self.global_variables.append(variable)

    def to_dict(self) -> dict[str, Any]:
        """Serialize module to dictionary representation."""
        return {
            "type": "module",
            "name": self.name,
            "imports": self.imports,
            "functions": [f.to_dict() for f in self.functions],
            "global_variables": [v.to_dict() for v in self.global_variables],
            "type_declarations": [t.to_dict() for t in self.type_declarations],
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_module(self)


class IRFunction(IRNode):
    """IR representation of a function."""

    def __init__(self, name: str, return_type: IRType, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.FUNCTION, location)
        self.name = name
        self.return_type = return_type
        self.parameters: list[IRVariable] = []
        self.local_variables: list[IRVariable] = []
        self.body: list[IRStatement] = []
        self.is_static: bool = False
        self.is_inline: bool = False
        self.complexity: StaticComplexity = StaticComplexity.SIMPLE

    def add_parameter(self, param: "IRVariable") -> None:
        """Add a parameter to the function."""
        param.is_parameter = True
        self.add_child(param)
        self.parameters.append(param)

    def add_local_variable(self, var: "IRVariable") -> None:
        """Add a local variable to the function."""
        self.add_child(var)
        self.local_variables.append(var)

    def add_statement(self, stmt: "IRStatement") -> None:
        """Add a statement to the function body."""
        self.add_child(stmt)
        self.body.append(stmt)

    def get_signature(self) -> str:
        """Get C function signature."""
        param_list = ", ".join(param.ir_type.to_c_declaration(param.name) for param in self.parameters)
        if not param_list:
            param_list = "void"

        modifiers = []
        if self.is_static:
            modifiers.append("static")
        if self.is_inline:
            modifiers.append("inline")

        modifier_str = " ".join(modifiers)
        if modifier_str:
            modifier_str += " "

        return f"{modifier_str}{self.return_type.to_c_declaration(self.name)}({param_list})"

    def to_dict(self) -> dict[str, Any]:
        """Serialize function to dictionary representation."""
        return {
            "type": "function",
            "name": self.name,
            "return_type": str(self.return_type),
            "parameters": [p.to_dict() for p in self.parameters],
            "local_variables": [v.to_dict() for v in self.local_variables],
            "body": [s.to_dict() for s in self.body],
            "complexity": self.complexity.name,
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_function(self)


class IRVariable(IRNode):
    """IR representation of a variable."""

    def __init__(self, name: str, ir_type: IRType, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.VARIABLE, location)
        self.name = name
        self.ir_type = ir_type
        self.initial_value: Optional[IRExpression] = None
        self.is_parameter: bool = False
        self.is_global: bool = False
        self.is_static: bool = False
        self.scope: Optional[str] = None

    def to_dict(self) -> dict[str, Any]:
        """Serialize variable to dictionary representation."""
        return {
            "type": "variable",
            "name": self.name,
            "ir_type": str(self.ir_type),
            "is_parameter": self.is_parameter,
            "is_global": self.is_global,
            "scope": self.scope,
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_variable(self)


class IRStatement(IRNode):
    """Base class for IR statements."""

    def __init__(self, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.STATEMENT, location)


class IRExpression(IRNode):
    """Base class for IR expressions."""

    def __init__(self, result_type: IRType, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.EXPRESSION, location)
        self.result_type = result_type


class IRAssignment(IRStatement):
    """IR representation of an assignment statement."""

    def __init__(self, target: IRVariable, value: Optional[IRExpression], location: Optional[IRLocation] = None):
        super().__init__(location)
        self.target = target
        self.value = value
        self.add_child(target)
        if value is not None:
            self.add_child(value)

    def to_dict(self) -> dict[str, Any]:
        """Serialize assignment to dictionary representation."""
        return {
            "type": "assignment",
            "target": self.target.to_dict(),
            "value": self.value.to_dict() if self.value else None,
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_assignment(self)


class IRBinaryOperation(IRExpression):
    """IR representation of binary operations."""

    def __init__(
        self,
        left: IRExpression,
        operator: str,
        right: IRExpression,
        result_type: IRType,
        location: Optional[IRLocation] = None,
    ):
        super().__init__(result_type, location)
        self.left = left
        self.operator = operator
        self.right = right
        self.add_child(left)
        self.add_child(right)

    def to_dict(self) -> dict[str, Any]:
        """Serialize binary operation to dictionary representation."""
        return {
            "type": "binary_operation",
            "left": self.left.to_dict(),
            "operator": self.operator,
            "right": self.right.to_dict(),
            "result_type": str(self.result_type),
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_binary_operation(self)


class IRLiteral(IRExpression):
    """IR representation of literal values."""

    def __init__(self, value: Any, ir_type: IRType, location: Optional[IRLocation] = None):
        super().__init__(ir_type, location)
        self.value = value

    def to_dict(self) -> dict[str, Any]:
        """Serialize literal to dictionary representation."""
        return {"type": "literal", "value": str(self.value), "ir_type": str(self.result_type)}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_literal(self)


class IRComprehension(IRExpression):
    """IR representation of list/dict/set comprehensions.

    Stores the original AST node for backends to expand into loops.
    """

    def __init__(self, ast_node: ast.expr, ir_type: IRType, location: Optional[IRLocation] = None):
        super().__init__(ir_type, location)
        self.ast_node = ast_node  # ast.ListComp, ast.DictComp, or ast.SetComp

    def to_dict(self) -> dict[str, Any]:
        """Serialize comprehension to dictionary representation."""
        return {"type": "comprehension", "ast_type": type(self.ast_node).__name__, "ir_type": str(self.result_type)}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_comprehension(self)


class IRVariableReference(IRExpression):
    """IR representation of variable references."""

    def __init__(self, variable: IRVariable, location: Optional[IRLocation] = None):
        super().__init__(variable.ir_type, location)
        self.variable = variable

    def to_dict(self) -> dict[str, Any]:
        """Serialize variable reference to dictionary representation."""
        return {"type": "variable_reference", "variable": self.variable.name, "ir_type": str(self.result_type)}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_variable_reference(self)


class IRFunctionCall(IRExpression):
    """IR representation of function calls."""

    def __init__(
        self,
        function_name: str,
        arguments: list[IRExpression],
        return_type: IRType,
        location: Optional[IRLocation] = None,
    ):
        super().__init__(return_type, location)
        self.function_name = function_name
        self.arguments = arguments
        for arg in arguments:
            self.add_child(arg)

    def to_dict(self) -> dict[str, Any]:
        """Serialize function call to dictionary representation."""
        return {
            "type": "function_call",
            "function_name": self.function_name,
            "arguments": [arg.to_dict() for arg in self.arguments],
            "return_type": str(self.result_type),
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_function_call(self)


class IRTypeCast(IRExpression):
    """IR representation of type casts (int(), float(), etc.)."""

    def __init__(
        self,
        value: IRExpression,
        target_type: IRType,
        location: Optional[IRLocation] = None,
    ):
        super().__init__(target_type, location)
        self.value = value
        self.add_child(value)

    def to_dict(self) -> dict[str, Any]:
        """Serialize type cast to dictionary representation."""
        return {
            "type": "type_cast",
            "value": self.value.to_dict(),
            "target_type": str(self.result_type),
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_type_cast(self)


class IRReturn(IRStatement):
    """IR representation of return statements."""

    def __init__(self, value: Optional[IRExpression] = None, location: Optional[IRLocation] = None):
        super().__init__(location)
        self.value = value
        if value:
            self.add_child(value)

    def to_dict(self) -> dict[str, Any]:
        """Serialize return statement to dictionary representation."""
        return {"type": "return", "value": self.value.to_dict() if self.value else None}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_return(self)


class IRBreak(IRStatement):
    """IR representation of break statements."""

    def __init__(self, location: Optional[IRLocation] = None):
        super().__init__(location)

    def to_dict(self) -> dict[str, Any]:
        """Serialize break statement to dictionary representation."""
        return {"type": "break"}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_break(self)


class IRContinue(IRStatement):
    """IR representation of continue statements."""

    def __init__(self, location: Optional[IRLocation] = None):
        super().__init__(location)

    def to_dict(self) -> dict[str, Any]:
        """Serialize continue statement to dictionary representation."""
        return {"type": "continue"}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_continue(self)


class IRExpressionStatement(IRStatement):
    """IR representation of expression statements (e.g., void function calls)."""

    def __init__(self, expression: "IRExpression", location: Optional[IRLocation] = None):
        super().__init__(location)
        self.expression = expression
        self.add_child(expression)

    def to_dict(self) -> dict[str, Any]:
        """Serialize expression statement to dictionary representation."""
        return {"type": "expression_statement", "expression": self.expression.to_dict()}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_expression_statement(self)


class IRIf(IRStatement):
    """IR representation of if statements."""

    def __init__(
        self,
        condition: IRExpression,
        then_body: list[IRStatement],
        else_body: Optional[list[IRStatement]] = None,
        location: Optional[IRLocation] = None,
    ):
        super().__init__(location)
        self.condition = condition
        self.then_body = then_body
        self.else_body = else_body or []

        self.add_child(condition)
        for stmt in self.then_body:
            self.add_child(stmt)
        for stmt in self.else_body:
            self.add_child(stmt)

    def to_dict(self) -> dict[str, Any]:
        """Serialize if statement to dictionary representation."""
        return {
            "type": "if",
            "condition": self.condition.to_dict(),
            "then_body": [s.to_dict() for s in self.then_body],
            "else_body": [s.to_dict() for s in self.else_body],
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_if(self)


class IRWhile(IRStatement):
    """IR representation of while loops."""

    def __init__(self, condition: IRExpression, body: list[IRStatement], location: Optional[IRLocation] = None):
        super().__init__(location)
        self.condition = condition
        self.body = body

        self.add_child(condition)
        for stmt in body:
            self.add_child(stmt)

    def to_dict(self) -> dict[str, Any]:
        """Serialize while loop to dictionary representation."""
        return {"type": "while", "condition": self.condition.to_dict(), "body": [s.to_dict() for s in self.body]}

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_while(self)


class IRFor(IRStatement):
    """IR representation of for loops (range-based)."""

    def __init__(
        self,
        variable: IRVariable,
        start: IRExpression,
        end: IRExpression,
        step: Optional[IRExpression],
        body: list[IRStatement],
        location: Optional[IRLocation] = None,
    ):
        super().__init__(location)
        self.variable = variable
        self.start = start
        self.end = end
        self.step = step
        self.body = body

        self.add_child(variable)
        self.add_child(start)
        self.add_child(end)
        if step:
            self.add_child(step)
        for stmt in body:
            self.add_child(stmt)

    def to_dict(self) -> dict[str, Any]:
        """Serialize for loop to dictionary representation."""
        return {
            "type": "for",
            "variable": self.variable.to_dict(),
            "start": self.start.to_dict(),
            "end": self.end.to_dict(),
            "step": self.step.to_dict() if self.step else None,
            "body": [s.to_dict() for s in self.body],
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_for(self)


class IRTypeDeclaration(IRNode):
    """IR representation of type declarations (structs, unions, enums)."""

    def __init__(self, name: str, declaration_type: str, location: Optional[IRLocation] = None):
        super().__init__(IRNodeType.TYPE_DECLARATION, location)
        self.name = name
        self.declaration_type = declaration_type  # "struct", "union", "enum"
        self.fields: list[IRVariable] = []

    def add_field(self, field: IRVariable) -> None:
        """Add a field to the type declaration."""
        self.add_child(field)
        self.fields.append(field)

    def to_dict(self) -> dict[str, Any]:
        """Serialize type declaration to dictionary representation."""
        return {
            "type": "type_declaration",
            "name": self.name,
            "declaration_type": self.declaration_type,
            "fields": [f.to_dict() for f in self.fields],
        }

    def accept(self, visitor: "IRVisitor") -> Any:
        """Accept a visitor for traversal (visitor pattern)."""
        return visitor.visit_type_declaration(self)


class IRVisitor(ABC):
    """Visitor interface for IR nodes."""

    @abstractmethod
    def visit_module(self, node: IRModule) -> Any:
        """Visit a module node."""
        pass

    @abstractmethod
    def visit_function(self, node: IRFunction) -> Any:
        """Visit a function node."""
        pass

    @abstractmethod
    def visit_variable(self, node: IRVariable) -> Any:
        """Visit a variable node."""
        pass

    @abstractmethod
    def visit_assignment(self, node: IRAssignment) -> Any:
        """Visit an assignment node."""
        pass

    @abstractmethod
    def visit_binary_operation(self, node: IRBinaryOperation) -> Any:
        """Visit a binary operation node."""
        pass

    @abstractmethod
    def visit_literal(self, node: IRLiteral) -> Any:
        """Visit a literal node."""
        pass

    @abstractmethod
    def visit_comprehension(self, node: IRComprehension) -> Any:
        """Visit a comprehension node."""
        pass

    @abstractmethod
    def visit_variable_reference(self, node: IRVariableReference) -> Any:
        """Visit a variable reference node."""
        pass

    @abstractmethod
    def visit_function_call(self, node: IRFunctionCall) -> Any:
        """Visit a function call node."""
        pass

    @abstractmethod
    def visit_type_cast(self, node: "IRTypeCast") -> Any:
        """Visit a type cast node."""
        pass

    @abstractmethod
    def visit_return(self, node: IRReturn) -> Any:
        """Visit a return statement node."""
        pass

    @abstractmethod
    def visit_break(self, node: "IRBreak") -> Any:
        """Visit a break statement node."""
        pass

    @abstractmethod
    def visit_continue(self, node: "IRContinue") -> Any:
        """Visit a continue statement node."""
        pass

    @abstractmethod
    def visit_expression_statement(self, node: "IRExpressionStatement") -> Any:
        """Visit an expression statement node."""
        pass

    @abstractmethod
    def visit_if(self, node: IRIf) -> Any:
        """Visit an if statement node."""
        pass

    @abstractmethod
    def visit_while(self, node: IRWhile) -> Any:
        """Visit a while loop node."""
        pass

    @abstractmethod
    def visit_for(self, node: IRFor) -> Any:
        """Visit a for loop node."""
        pass

    @abstractmethod
    def visit_type_declaration(self, node: IRTypeDeclaration) -> Any:
        """Visit a type declaration node."""
        pass


class IRBuilder:
    """Builder for constructing IR from Python AST."""

    def __init__(self) -> None:
        self.current_module: Optional[IRModule] = None
        self.current_function: Optional[IRFunction] = None
        self.symbol_table: dict[str, IRVariable] = {}

    def build_from_ast(self, tree: ast.AST, module_name: str = "main") -> IRModule:
        """Build IR from Python AST using a two-pass approach.

        Pass 1: Create function declarations (signatures) so recursive calls can resolve types
        Pass 2: Build function bodies with full type information
        """
        self.current_module = IRModule(module_name)

        # Get module-level statements if tree is a Module
        module_nodes: list[ast.stmt] = []
        if isinstance(tree, ast.Module):
            module_nodes = tree.body

        # Process module-level global variables first
        for node in module_nodes:
            if isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
                # This is a global variable declaration
                var_type = self._extract_ir_type(node.annotation) if node.annotation else IRType(IRDataType.VOID)
                var = IRVariable(node.target.id, var_type, self._get_location(node))
                if node.value:
                    var.initial_value = self._build_expression(node.value)
                self.current_module.add_global_variable(var)
                # Add to symbol table for lookup
                self.symbol_table[node.target.id] = var

        # Pass 1: Collect function signatures
        function_nodes: list[ast.FunctionDef] = []
        for node in module_nodes:
            if isinstance(node, ast.FunctionDef):
                function_nodes.append(node)
                # Create function declaration with signature only
                return_type = self._extract_ir_type(node.returns) if node.returns else IRType(IRDataType.VOID)
                func_decl = IRFunction(node.name, return_type)
                # Add parameters
                for arg in node.args.args:
                    param_type = self._extract_ir_type(arg.annotation) if arg.annotation else IRType(IRDataType.VOID)
                    param = IRVariable(arg.arg, param_type)
                    func_decl.add_parameter(param)
                self.current_module.add_function(func_decl)

        # Pass 2: Build function bodies
        for i, node in enumerate(function_nodes):
            ir_func = self._build_function(node)
            # Replace the declaration with the full function
            self.current_module.functions[i] = ir_func

        return self.current_module

    def _build_function(self, node: ast.FunctionDef) -> IRFunction:
        """Build IR function from AST function definition."""
        # Extract return type
        return_type = self._extract_ir_type(node.returns) if node.returns else IRType(IRDataType.VOID)

        ir_func = IRFunction(node.name, return_type, self._get_location(node))
        self.current_function = ir_func

        # Add parameters
        for arg in node.args.args:
            param_type = self._extract_ir_type(arg.annotation) if arg.annotation else IRType(IRDataType.VOID)
            param = IRVariable(arg.arg, param_type, self._get_location(arg))
            ir_func.add_parameter(param)
            self.symbol_table[arg.arg] = param

        # Process function body
        for stmt in node.body:
            ir_stmt = self._build_statement(stmt)
            if ir_stmt:
                ir_func.add_statement(ir_stmt)

        return ir_func

    def _build_statement(self, node: ast.stmt) -> Optional[IRStatement]:
        """Build IR statement from AST statement."""
        if isinstance(node, ast.AnnAssign):
            return self._build_annotated_assignment(node)
        elif isinstance(node, ast.Assign):
            return self._build_assignment(node)
        elif isinstance(node, ast.AugAssign):
            return self._build_augmented_assignment(node)
        elif isinstance(node, ast.Return):
            return self._build_return(node)
        elif isinstance(node, ast.Break):
            return IRBreak(self._get_location(node))
        elif isinstance(node, ast.Continue):
            return IRContinue(self._get_location(node))
        elif isinstance(node, ast.Expr):
            # Expression statement (e.g., void function call)
            expr = self._build_expression(node.value)
            return IRExpressionStatement(expr, self._get_location(node))
        elif isinstance(node, ast.If):
            return self._build_if(node)
        elif isinstance(node, ast.While):
            return self._build_while(node)
        elif isinstance(node, ast.For):
            return self._build_for(node)

        return None

    def _build_annotated_assignment(self, node: ast.AnnAssign) -> IRStatement:
        """Build annotated assignment (variable declaration)."""
        if isinstance(node.target, ast.Name):
            var_name = node.target.id
            var_type = self._extract_ir_type(node.annotation)

            var = IRVariable(var_name, var_type, self._get_location(node))
            self.symbol_table[var_name] = var

            if self.current_function:
                self.current_function.add_local_variable(var)

            if node.value:
                value_expr = self._build_expression(node.value)

                # Type inference: propagate element type between variable and value
                # Case 1: list literal with no element type -> use variable's annotation
                if (
                    isinstance(value_expr, IRLiteral)
                    and value_expr.result_type.base_type == IRDataType.LIST
                    and (
                        not hasattr(value_expr.result_type, "element_type")
                        or value_expr.result_type.element_type is None
                    )
                ):
                    # Copy element type from variable annotation to literal
                    if hasattr(var_type, "element_type") and var_type.element_type:
                        value_expr.result_type.element_type = var_type.element_type

                # Case 1b: dict literal with no element type -> use variable's annotation
                elif (
                    isinstance(value_expr, IRLiteral)
                    and value_expr.result_type.base_type == IRDataType.DICT
                    and (
                        not hasattr(value_expr.result_type, "element_type")
                        or value_expr.result_type.element_type is None
                    )
                ):
                    # Copy element type from variable annotation to literal
                    if hasattr(var_type, "element_type") and var_type.element_type:
                        value_expr.result_type.element_type = var_type.element_type

                # Case 2: generic variable annotation (e.g., `list`) but value has specific type (e.g., `list[str]`)
                # Infer the variable's element type from the value
                elif (
                    var_type.base_type == IRDataType.LIST
                    and (not hasattr(var_type, "element_type") or var_type.element_type is None)
                    and hasattr(value_expr.result_type, "element_type")
                    and value_expr.result_type.element_type
                ):
                    # Propagate element type from value to variable
                    var_type.element_type = value_expr.result_type.element_type

                return IRAssignment(var, value_expr, self._get_location(node))
            else:
                # Create assignment with None value for declaration only
                return IRAssignment(var, None, self._get_location(node))

        # Fallback for complex targets - create a dummy assignment with placeholder
        # This should be handled properly by the caller
        from warnings import warn

        warn(f"Complex annotated assignment target not supported: {type(node.target)}", stacklevel=2)
        # Return a dummy assignment to satisfy type checker
        dummy_var = IRVariable("_unknown", IRType(IRDataType.VOID), self._get_location(node))
        return IRAssignment(dummy_var, None, self._get_location(node))

    def _build_assignment(self, node: ast.Assign) -> Optional[IRStatement]:
        """Build regular assignment."""
        if len(node.targets) == 1:
            target = node.targets[0]

            # Handle simple variable assignment
            if isinstance(target, ast.Name):
                target_name = target.id
                if target_name in self.symbol_table:
                    target_var = self.symbol_table[target_name]
                    value_expr = self._build_expression(node.value)

                    # Type inference: propagate element type from value to variable if needed
                    # If variable has generic LIST/DICT type and value has specific element_type, propagate it
                    if hasattr(value_expr, "result_type") and value_expr.result_type:
                        # For LIST
                        if (
                            target_var.ir_type.base_type == IRDataType.LIST
                            and value_expr.result_type.base_type == IRDataType.LIST
                            and (
                                not hasattr(target_var.ir_type, "element_type")
                                or target_var.ir_type.element_type is None
                            )
                            and hasattr(value_expr.result_type, "element_type")
                            and value_expr.result_type.element_type
                        ):
                            target_var.ir_type.element_type = value_expr.result_type.element_type

                        # For DICT
                        elif (
                            target_var.ir_type.base_type == IRDataType.DICT
                            and value_expr.result_type.base_type == IRDataType.DICT
                            and (
                                not hasattr(target_var.ir_type, "element_type")
                                or target_var.ir_type.element_type is None
                            )
                            and hasattr(value_expr.result_type, "element_type")
                            and value_expr.result_type.element_type
                        ):
                            target_var.ir_type.element_type = value_expr.result_type.element_type

                    return IRAssignment(target_var, value_expr, self._get_location(node))

            # Handle subscript assignment (arr[i] = val)
            elif isinstance(target, ast.Subscript):
                # Build as a function call to __setitem__(base, index, value)
                base = self._build_expression(target.value)
                index = self._build_expression(target.slice)
                value = self._build_expression(node.value)

                # Type inference for dict: infer key type from first assignment
                # If base is a dict variable reference with no element_type, infer from index type
                from .static_ir import IRVariableReference

                if (
                    isinstance(base, IRVariableReference)
                    and base.result_type.base_type == IRDataType.DICT
                    and (not hasattr(base.result_type, "element_type") or base.result_type.element_type is None)
                ):
                    # Infer dict key type from the index being used
                    if hasattr(index, "result_type") and index.result_type:
                        # Update both the variable reference's result_type AND the underlying variable's ir_type
                        base.result_type.element_type = index.result_type
                        base.variable.ir_type.element_type = index.result_type

                # Create synthetic function call for subscript assignment
                # This will be translated to vec_int_set or similar by backend
                return_type = IRType(IRDataType.VOID)
                setitem_call = IRFunctionCall(
                    "__setitem__", [base, index, value], return_type, self._get_location(node)
                )
                return IRExpressionStatement(setitem_call, self._get_location(node))

        return None

    def _build_augmented_assignment(self, node: ast.AugAssign) -> Optional[IRStatement]:
        """Build augmented assignment (+=, -=, etc.).

        Converts 'x += y' to 'x = x + y'.
        """
        if isinstance(node.target, ast.Name):
            target_name = node.target.id
            if target_name in self.symbol_table:
                target_var = self.symbol_table[target_name]

                # Create binary operation: target op value
                left = IRVariableReference(target_var, self._get_location(node))
                right = self._build_expression(node.value)
                operator = self._get_operator_string(node.op)

                # Infer result type from target variable
                result_type = target_var.ir_type

                binary_op = IRBinaryOperation(left, operator, right, result_type, self._get_location(node))
                return IRAssignment(target_var, binary_op, self._get_location(node))

        return None

    def _build_expression(self, node: ast.expr) -> IRExpression:
        """Build IR expression from AST expression."""
        if isinstance(node, ast.Constant):
            return self._build_literal(node)
        elif isinstance(node, ast.Name):
            return self._build_variable_reference(node)
        elif isinstance(node, ast.BinOp):
            return self._build_binary_operation(node)
        elif isinstance(node, ast.Compare):
            return self._build_comparison(node)
        elif isinstance(node, ast.BoolOp):
            return self._build_bool_operation(node)
        elif isinstance(node, ast.UnaryOp):
            return self._build_unary_operation(node)
        elif isinstance(node, ast.Call):
            return self._build_function_call(node)
        elif isinstance(node, ast.List):
            return self._build_list_literal(node)
        elif isinstance(node, ast.Dict):
            return self._build_dict_literal(node)
        elif isinstance(node, ast.ListComp):
            # List comprehension - store AST node for backend expansion
            return IRComprehension(node, IRType(IRDataType.LIST), self._get_location(node))
        elif isinstance(node, ast.DictComp):
            # Dict comprehension - store AST node for backend expansion
            return IRComprehension(node, IRType(IRDataType.DICT), self._get_location(node))
        elif isinstance(node, ast.SetComp):
            # Set comprehension - store AST node for backend expansion
            return IRComprehension(node, IRType(IRDataType.SET), self._get_location(node))
        elif isinstance(node, ast.Subscript):
            return self._build_subscript(node)

        # Fallback for unknown expressions
        return IRLiteral(None, IRType(IRDataType.VOID), self._get_location(node))

    def _build_literal(self, node: ast.Constant) -> IRLiteral:
        """Build literal from constant."""
        value = node.value
        if isinstance(value, bool):
            ir_type = IRType(IRDataType.BOOL)
        elif isinstance(value, int):
            ir_type = IRType(IRDataType.INT)
        elif isinstance(value, float):
            ir_type = IRType(IRDataType.FLOAT)
        elif isinstance(value, str):
            ir_type = IRType(IRDataType.STRING)
        else:
            ir_type = IRType(IRDataType.VOID)

        return IRLiteral(value, ir_type, self._get_location(node))

    def _build_variable_reference(self, node: ast.Name) -> IRVariableReference:
        """Build variable reference."""
        var = self.symbol_table.get(node.id)
        if not var:
            # Create a placeholder variable
            var = IRVariable(node.id, IRType(IRDataType.VOID))
            self.symbol_table[node.id] = var

        return IRVariableReference(var, self._get_location(node))

    def _build_binary_operation(self, node: ast.BinOp) -> IRBinaryOperation:
        """Build binary operation."""
        left = self._build_expression(node.left)
        right = self._build_expression(node.right)

        # Infer result type (simplified)
        result_type = left.result_type  # Simplified type inference

        operator = self._get_operator_string(node.op)

        return IRBinaryOperation(left, operator, right, result_type, self._get_location(node))

    def _build_comparison(self, node: ast.Compare) -> IRExpression:
        """Build comparison operation.

        Supports chained comparisons like a < b < c by converting to
        (a < b) and (b < c).
        """
        # Handle simple single comparison
        if len(node.ops) == 1:
            left = self._build_expression(node.left)
            right = self._build_expression(node.comparators[0])
            operator = self._get_comparison_operator_string(node.ops[0])
            result_type = IRType(IRDataType.BOOL)
            return IRBinaryOperation(left, operator, right, result_type, self._get_location(node))

        # Handle chained comparisons: a < b < c becomes (a < b) and (b < c)
        comparisons = []
        left_expr = self._build_expression(node.left)

        for i, (op, comparator) in enumerate(zip(node.ops, node.comparators)):
            right_expr = self._build_expression(comparator)
            operator = self._get_comparison_operator_string(op)
            result_type = IRType(IRDataType.BOOL)

            # Create comparison: left op right
            comparison = IRBinaryOperation(left_expr, operator, right_expr, result_type, self._get_location(node))
            comparisons.append(comparison)

            # For next iteration, left becomes current right
            left_expr = right_expr

        # Chain all comparisons with 'and'
        result = comparisons[0]
        for comparison in comparisons[1:]:
            result = IRBinaryOperation(result, "and", comparison, IRType(IRDataType.BOOL), self._get_location(node))

        return result

    def _build_bool_operation(self, node: ast.BoolOp) -> IRExpression:
        """Build boolean operation (and/or).

        Supports chaining like a and b and c by converting to nested
        binary operations: (a and b) and c.
        """
        if len(node.values) < 2:
            # Need at least 2 values
            return IRLiteral(None, IRType(IRDataType.VOID), self._get_location(node))  # type: ignore[return-value]

        # Determine the operator
        if isinstance(node.op, ast.And):
            operator = "and"
        elif isinstance(node.op, ast.Or):
            operator = "or"
        else:
            return IRLiteral(None, IRType(IRDataType.VOID), self._get_location(node))  # type: ignore[return-value]

        # Build first expression
        result = self._build_expression(node.values[0])

        # Chain with remaining values
        for value in node.values[1:]:
            right = self._build_expression(value)
            result = IRBinaryOperation(result, operator, right, IRType(IRDataType.BOOL), self._get_location(node))

        return result

    def _build_unary_operation(self, node: ast.UnaryOp) -> IRExpression:
        """Build unary operation (not, -, +, ~)."""
        operand = self._build_expression(node.operand)

        # For 'not' operation, we need special handling
        if isinstance(node.op, ast.Not):
            # Create a comparison: operand == False
            # This is equivalent to 'not operand'
            false_literal = IRLiteral(False, IRType(IRDataType.BOOL), self._get_location(node))
            return IRBinaryOperation(operand, "==", false_literal, IRType(IRDataType.BOOL), self._get_location(node))
        elif isinstance(node.op, ast.USub):
            # Unary minus: 0 - operand
            zero = IRLiteral(0, operand.result_type, self._get_location(node))
            return IRBinaryOperation(zero, "-", operand, operand.result_type, self._get_location(node))
        elif isinstance(node.op, ast.UAdd):
            # Unary plus: just return operand
            return operand
        elif isinstance(node.op, ast.Invert):
            # Bitwise not: ~operand (XOR with -1)
            neg_one = IRLiteral(-1, operand.result_type, self._get_location(node))
            return IRBinaryOperation(operand, "^", neg_one, operand.result_type, self._get_location(node))

        # Unknown unary operator
        return IRLiteral(None, IRType(IRDataType.VOID), self._get_location(node))

    def _build_function_call(self, node: ast.Call) -> IRExpression:
        """Build function call or type cast."""
        if isinstance(node.func, ast.Name):
            func_name = node.func.id

            # Check if this is a set() constructor with no args (empty set)
            if func_name == "set" and len(node.args) == 0:
                # set() creates an empty set - return a literal like empty dict/list
                ir_type = IRType(IRDataType.SET)
                return IRLiteral({}, ir_type, self._get_location(node))

            # Check if this is a type cast function
            if func_name in ("int", "float", "bool", "str") and len(node.args) == 1:
                value = self._build_expression(node.args[0])

                # Determine target type
                if func_name == "int":
                    target_type = IRType(IRDataType.INT)
                elif func_name == "float":
                    target_type = IRType(IRDataType.FLOAT)
                elif func_name == "bool":
                    target_type = IRType(IRDataType.BOOL)
                elif func_name == "str":
                    target_type = IRType(IRDataType.STRING)
                else:
                    target_type = IRType(IRDataType.VOID)

                return IRTypeCast(value, target_type, self._get_location(node))

            # Regular function call
            arguments = [self._build_expression(arg) for arg in node.args]

            # Look up function return type from module
            return_type = IRType(IRDataType.VOID)  # Default
            if self.current_module:
                for func in self.current_module.functions:
                    if func.name == func_name:
                        return_type = func.return_type
                        break

            return IRFunctionCall(func_name, arguments, return_type, self._get_location(node))
        elif isinstance(node.func, ast.Attribute):
            # Method call (e.g., list.append(), str.split())
            obj = self._build_expression(node.func.value)
            method_name = node.func.attr
            arguments = [obj] + [self._build_expression(arg) for arg in node.args]

            # Create synthetic function name for method
            func_name = f"__method_{method_name}__"

            # Determine return type based on method name
            if method_name == "values":
                # dict.values() returns a list of values
                return_type = IRType(IRDataType.LIST)
            elif method_name == "keys":
                # dict.keys() returns a list of keys
                return_type = IRType(IRDataType.LIST)
            elif method_name == "split":
                # str.split() returns a list of strings
                return_type = IRType(IRDataType.LIST, element_type=IRType(IRDataType.STRING))
            else:
                # Most methods return void (append, etc.)
                return_type = IRType(IRDataType.VOID)

            return IRFunctionCall(func_name, arguments, return_type, self._get_location(node))
        else:
            # Handle complex function calls (attribute access, etc.)
            func_name = "complex_call"  # Placeholder for complex calls
            arguments = [self._build_expression(arg) for arg in node.args]
            return_type = IRType(IRDataType.VOID)
            return IRFunctionCall(func_name, arguments, return_type, self._get_location(node))

    def _build_list_literal(self, node: ast.List) -> IRLiteral:
        """Build list literal from AST.

        For empty list [], creates an IR literal with LIST type.
        For non-empty lists, elements are stored and will be initialized by backend.
        """
        # Build list literal with elements
        ir_type = IRType(IRDataType.LIST)

        if len(node.elts) == 0:
            # Empty list
            return IRLiteral([], ir_type, self._get_location(node))
        else:
            # Non-empty list - store element expressions
            # Backend will generate initialization code (allocate + push each element)
            elements = [self._build_expression(elt) for elt in node.elts]
            return IRLiteral(elements, ir_type, self._get_location(node))

    def _build_dict_literal(self, node: ast.Dict) -> IRLiteral:
        """Build dict literal from AST.

        For empty dict {}, creates an IR literal with DICT type.
        For non-empty dicts, key-value pairs are stored and will be initialized by backend.
        """
        ir_type = IRType(IRDataType.DICT)

        if len(node.keys) == 0:
            # Empty dict
            return IRLiteral({}, ir_type, self._get_location(node))
        else:
            # Non-empty dict - store key-value pair expressions
            # Backend will generate initialization code (allocate + set each pair)
            # Store as list of (key_expr, value_expr) tuples
            # Note: keys can be None for dict unpacking (**dict), which we don't support yet
            pairs = [
                (self._build_expression(k), self._build_expression(v))
                for k, v in zip(node.keys, node.values)
                if k is not None
            ]
            return IRLiteral(pairs, ir_type, self._get_location(node))

    def _build_subscript(self, node: ast.Subscript) -> IRExpression:
        """Build subscript expression (array/list indexing).

        Creates a function call to a subscript operation.
        """
        # Get the base object being indexed
        base = self._build_expression(node.value)
        # Get the index
        index = self._build_expression(node.slice)

        # Create a synthetic function call for subscript operation
        # Backend will translate this to appropriate indexing code
        # Return type depends on container element type (for now assume INT)
        return_type = IRType(IRDataType.INT)
        return IRFunctionCall("__getitem__", [base, index], return_type, self._get_location(node))

    def _build_return(self, node: ast.Return) -> IRReturn:
        """Build return statement."""
        value = self._build_expression(node.value) if node.value else None

        # Type inference: update function return type from returned value
        # If function return type is generic (list/dict with no element_type) and
        # the returned value has a more specific type, propagate it
        if value and self.current_function and hasattr(value, "result_type"):
            func_ret_type = self.current_function.return_type
            val_type = value.result_type

            # For LIST: propagate element_type if function return type lacks it
            if (
                func_ret_type.base_type == IRDataType.LIST
                and (not hasattr(func_ret_type, "element_type") or func_ret_type.element_type is None)
                and hasattr(val_type, "element_type")
                and val_type.element_type
            ):
                func_ret_type.element_type = val_type.element_type

            # For DICT: propagate element_type (key type) if function return type lacks it
            elif (
                func_ret_type.base_type == IRDataType.DICT
                and (not hasattr(func_ret_type, "element_type") or func_ret_type.element_type is None)
                and hasattr(val_type, "element_type")
                and val_type.element_type
            ):
                func_ret_type.element_type = val_type.element_type

        return IRReturn(value, self._get_location(node))

    def _build_if(self, node: ast.If) -> IRIf:
        """Build if statement."""
        condition = self._build_expression(node.test)
        then_body_raw = [self._build_statement(stmt) for stmt in node.body]
        then_body: list[IRStatement] = [stmt for stmt in then_body_raw if stmt is not None]

        else_body: list[IRStatement] = []
        if node.orelse:
            else_body_raw = [self._build_statement(stmt) for stmt in node.orelse]
            else_body = [stmt for stmt in else_body_raw if stmt is not None]

        return IRIf(condition, then_body, else_body, self._get_location(node))

    def _build_while(self, node: ast.While) -> IRWhile:
        """Build while loop."""
        condition = self._build_expression(node.test)
        body_raw = [self._build_statement(stmt) for stmt in node.body]
        body: list[IRStatement] = [stmt for stmt in body_raw if stmt is not None]

        return IRWhile(condition, body, self._get_location(node))

    def _build_for(self, node: ast.For) -> Optional[IRFor]:
        """Build for loop - supports both range() and list iteration."""
        # Check if it's a range-based loop
        if isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name):
            if node.iter.func.id == "range":
                # Extract range parameters
                args = node.iter.args
                start: IRExpression
                end: IRExpression
                step: IRExpression

                if len(args) == 1:
                    start = IRLiteral(0, IRType(IRDataType.INT))
                    end = self._build_expression(args[0])
                    step = IRLiteral(1, IRType(IRDataType.INT))
                elif len(args) == 2:
                    start = self._build_expression(args[0])
                    end = self._build_expression(args[1])
                    step = IRLiteral(1, IRType(IRDataType.INT))
                elif len(args) == 3:
                    start = self._build_expression(args[0])
                    end = self._build_expression(args[1])
                    step = self._build_expression(args[2])
                else:
                    return None

                # Create loop variable
                if isinstance(node.target, ast.Name):
                    var_name = node.target.id
                    loop_var = IRVariable(var_name, IRType(IRDataType.INT), self._get_location(node.target))
                    self.symbol_table[var_name] = loop_var

                    body_raw = [self._build_statement(stmt) for stmt in node.body]
                    body: list[IRStatement] = [stmt for stmt in body_raw if stmt is not None]

                    return IRFor(loop_var, start, end, step, body, self._get_location(node))

        # Handle list/set iteration: for item in container:
        # For lists: Convert to: for __idx in range(len(list)): item = list[__idx]
        # For sets: Convert to: for __idx in range(len(set)): item = set_get_nth_element(set, __idx)
        if isinstance(node.target, ast.Name):
            item_name = node.target.id

            # Build expression for the iterable (list or set)
            iter_expr = self._build_expression(node.iter)
            if iter_expr is None:
                return None

            # Check if we're iterating over a set
            is_set_iteration = (
                hasattr(iter_expr.result_type, "base_type") and iter_expr.result_type.base_type == IRDataType.SET
            )

            # Create: len(container) call
            len_call = IRFunctionCall("len", [iter_expr], IRType(IRDataType.INT), self._get_location(node))

            # Create index variable: __idx_<item_name>
            idx_var_name = f"__idx_{item_name}"
            idx_var = IRVariable(idx_var_name, IRType(IRDataType.INT), self._get_location(node))
            self.symbol_table[idx_var_name] = idx_var

            # Infer item type from the container's element type
            item_type = IRType(IRDataType.INT)  # Default to INT for backward compatibility
            if hasattr(iter_expr.result_type, "element_type") and iter_expr.result_type.element_type:
                item_type = iter_expr.result_type.element_type

            # Create item variable with inferred type
            item_var = IRVariable(item_name, item_type, self._get_location(node.target))
            self.symbol_table[item_name] = item_var

            # Build body with item assignment prepended
            index_ref = IRVariableReference(idx_var, self._get_location(node))

            if is_set_iteration:
                # For sets: item = __set_get_nth__(set, __idx)
                get_nth_call = IRFunctionCall(
                    "__set_get_nth__", [iter_expr, index_ref], item_type, self._get_location(node)
                )
                item_assignment = IRAssignment(item_var, get_nth_call, self._get_location(node))
            else:
                # For lists: item = list[__idx]
                subscript_call = IRFunctionCall(
                    "__getitem__",
                    [iter_expr, index_ref],
                    item_type,  # Use inferred item type
                    self._get_location(node),
                )
                item_assignment = IRAssignment(item_var, subscript_call, self._get_location(node))

            # Build original body statements
            body_raw = [self._build_statement(stmt) for stmt in node.body]
            body_stmts: list[IRStatement] = [stmt for stmt in body_raw if stmt is not None]

            # Prepend item assignment to body
            body = [item_assignment] + body_stmts

            # Create for loop: for __idx in range(len(container))
            start = IRLiteral(0, IRType(IRDataType.INT))
            end = len_call
            step = IRLiteral(1, IRType(IRDataType.INT))

            return IRFor(idx_var, start, end, step, body, self._get_location(node))

        return None

    def _extract_ir_type(self, annotation: ast.expr) -> IRType:
        """Extract IR type from AST annotation."""
        if isinstance(annotation, ast.Name):
            type_mapping = {
                "int": IRDataType.INT,
                "float": IRDataType.FLOAT,
                "bool": IRDataType.BOOL,
                "str": IRDataType.STRING,
                "list": IRDataType.LIST,
                "dict": IRDataType.DICT,
                "set": IRDataType.SET,
                "void": IRDataType.VOID,
            }
            base_type = type_mapping.get(annotation.id, IRDataType.VOID)
            return IRType(base_type)
        elif isinstance(annotation, ast.Subscript):
            # Handle generic types like list[int], list[list[int]], dict[str, int], etc.
            if isinstance(annotation.value, ast.Name) and annotation.value.id == "list":
                element_type = self._extract_ir_type(annotation.slice)
                result = IRType(IRDataType.LIST)
                result.element_type = element_type
                return result
            elif isinstance(annotation.value, ast.Name) and annotation.value.id == "dict":
                # Handle dict[K, V] subscripts
                result = IRType(IRDataType.DICT)

                # Extract key type (element_type) from subscript
                # For dict[K, V], the slice is a Tuple with (K, V)
                if isinstance(annotation.slice, ast.Tuple) and len(annotation.slice.elts) >= 1:
                    # Get the key type (first element)
                    key_type = self._extract_ir_type(annotation.slice.elts[0])
                    result.element_type = key_type
                    # Note: value type is always int for now (map_*_int)

                return result

        return IRType(IRDataType.VOID)

    def _get_location(self, node: ast.AST) -> IRLocation:
        """Extract location information from AST node."""
        return IRLocation(
            line=getattr(node, "lineno", 0),
            column=getattr(node, "col_offset", 0),
            end_line=getattr(node, "end_lineno", None),
            end_column=getattr(node, "end_col_offset", None),
        )

    def _get_operator_string(self, op: ast.operator) -> str:
        """Convert AST operator to string."""
        operator_mapping = {
            ast.Add: "+",
            ast.Sub: "-",
            ast.Mult: "*",
            ast.Div: "/",
            ast.FloorDiv: "//",
            ast.Mod: "%",
            ast.Pow: "**",
            ast.LShift: "<<",
            ast.RShift: ">>",
            ast.BitOr: "|",
            ast.BitXor: "^",
            ast.BitAnd: "&",
        }
        return operator_mapping.get(type(op), "?")

    def _get_comparison_operator_string(self, op: ast.cmpop) -> str:
        """Convert AST comparison operator to string."""
        comparison_mapping = {
            ast.Lt: "<",
            ast.LtE: "<=",
            ast.Gt: ">",
            ast.GtE: ">=",
            ast.Eq: "==",
            ast.NotEq: "!=",
            ast.Is: "is",
            ast.IsNot: "is not",
            ast.In: "in",
            ast.NotIn: "not in",
        }
        return comparison_mapping.get(type(op), "?")


def build_ir_from_code(source_code: str, module_name: str = "main") -> IRModule:
    """Convenience function to build IR from Python source code."""
    tree = ast.parse(source_code)
    builder = IRBuilder()
    return builder.build_from_ast(tree, module_name)
