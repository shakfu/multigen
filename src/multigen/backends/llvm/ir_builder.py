"""LLVM-specific Static IR builder.

Extends the shared `IRBuilder` with `for k, v in d.items()` loops.
"""

import ast
from typing import Any, Optional, Union

from ...errors import UnsupportedFeatureError
from ...frontend.static_ir import (
    IRBuilder,
    IRDataType,
    IRExpression,
    IRFor,
    IRLocation,
    IRModule,
    IRStatement,
    IRType,
    IRVariable,
    IRVisitor,
)


def items_unpack_names(target: ast.expr, iterable: ast.expr) -> Optional[tuple[str, str, ast.expr]]:
    """Match `k, v` over a no-argument `d.items()` call.

    Returns:
        (key name, value name, dict expression), or None if the shape differs.
    """
    if (
        isinstance(target, ast.Tuple)
        and len(target.elts) == 2
        and isinstance(target.elts[0], ast.Name)
        and isinstance(target.elts[1], ast.Name)
        and isinstance(iterable, ast.Call)
        and isinstance(iterable.func, ast.Attribute)
        and iterable.func.attr == "items"
        and not iterable.args
        and not iterable.keywords
    ):
        return target.elts[0].id, target.elts[1].id, iterable.func.value
    return None


class IRDictItemsFor(IRStatement):
    """`for key, value in dict_expr.items(): body`, in dict insertion order."""

    def __init__(
        self,
        dict_expr: IRExpression,
        key_var: IRVariable,
        value_var: IRVariable,
        body: list[IRStatement],
        location: Optional[IRLocation] = None,
    ):
        super().__init__(location)
        self.dict_expr = dict_expr
        self.key_var = key_var
        self.value_var = value_var
        self.body = body
        for child in [dict_expr, key_var, value_var, *body]:
            self.add_child(child)

    def to_dict(self) -> dict[str, Any]:
        """Serialize the loop to a dictionary."""
        return {
            "type": "dict_items_for",
            "dict": self.dict_expr.to_dict(),
            "key": self.key_var.to_dict(),
            "value": self.value_var.to_dict(),
            "body": [s.to_dict() for s in self.body],
        }

    def accept(self, visitor: IRVisitor) -> Any:
        """Dispatch to `visit_dict_items_for`; only the LLVM converter has it."""
        visit = getattr(visitor, "visit_dict_items_for", None)
        if visit is None:
            raise UnsupportedFeatureError(f"{type(visitor).__name__} cannot visit dict .items() loops")
        return visit(self)


class LLVMIRBuilder(IRBuilder):
    """Static IR builder for the LLVM backend."""

    def _build_for(self, node: ast.For) -> Optional[Union[IRFor, IRDictItemsFor]]:  # type: ignore[override]
        """Build a for loop, adding `for k, v in d.items()`."""
        if node.orelse:
            raise UnsupportedFeatureError(f"LLVM backend does not support for/else (line {node.lineno})")

        match = items_unpack_names(node.target, node.iter)
        if match is None:
            if isinstance(node.target, ast.Name):
                iter_type = self._build_expression(node.iter).result_type
                if iter_type.base_type == IRDataType.DICT:
                    raise UnsupportedFeatureError(
                        f"LLVM backend does not support iterating a dict directly (line {node.lineno}); "
                        "use `for k, v in d.items()`"
                    )
            return super()._build_for(node)

        key_name, value_name, dict_node = match
        dict_expr = self._build_expression(dict_node)
        dict_type = dict_expr.result_type
        if dict_type.base_type != IRDataType.DICT or (
            dict_type.element_type is not None and dict_type.element_type.base_type != IRDataType.INT
        ):
            raise UnsupportedFeatureError(
                f"LLVM backend supports .items() loops only over dict[int, int] (line {node.lineno})"
            )

        location = self._get_location(node)
        key_var = IRVariable(key_name, IRType(IRDataType.INT), self._get_location(node.target))
        value_var = IRVariable(value_name, IRType(IRDataType.INT), self._get_location(node.target))
        self.symbol_table[key_name] = key_var
        self.symbol_table[value_name] = value_var

        body_raw = [self._build_statement(stmt) for stmt in node.body]
        body = [stmt for stmt in body_raw if stmt is not None]
        return IRDictItemsFor(dict_expr, key_var, value_var, body, location)


def build_llvm_ir_from_code(source_code: str, module_name: str = "main") -> IRModule:
    """Build Static IR for the LLVM backend from Python source."""
    return LLVMIRBuilder().build_from_ast(ast.parse(source_code), module_name)
