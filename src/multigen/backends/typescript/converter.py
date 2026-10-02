"""Python-to-TypeScript converter for MultiGen.

Structurally modeled on the Go converter, but emits idiomatic TypeScript:
native ``class`` blocks, template-literal f-strings, ``T[]`` / ``Map`` / ``Set``
containers, and array-method comprehensions. Python semantics that plain
transpilation gets wrong (floor division, modulo sign, strip/split, truthiness)
are routed through the ``mg`` runtime (``multigen_ts_runtime.ts``).

Python ``int`` is represented as TypeScript ``number`` (float64). This is
faithful for values below 2**53; large-integer benchmarks (fibonacci at high n,
matmul) diverge. See ``docs/dev/ts-plan.md`` trap #1 for the bigint tradeoff.
"""

import ast
import json
from typing import Any, Optional

from ..converter_utils import (
    extract_format_spec,
    get_augmented_assignment_operator,
    get_standard_binary_operator,
    get_standard_comparison_operator,
    normalize_ast,
)
from ..errors import TypeMappingError, UnsupportedFeatureError
from ..type_inference_strategies import InferenceContext


class MultiGenPythonToTypeScriptConverter:
    """Python-to-TypeScript converter with comprehensive language support."""

    def __init__(self) -> None:
        """Initialize the converter."""
        self.type_map = {
            "int": "number",
            "float": "number",
            "bool": "boolean",
            "str": "string",
            "list": "number[]",  # default element type for unsubscripted list
            "dict": "Map<number, number>",  # default key/value for unsubscripted dict
            "set": "Set<number>",  # default element for unsubscripted set
            "void": "void",
            "None": "void",
        }
        # Exception type mapping -> runtime error classes
        self.exception_map = {
            "Exception": "Error",
            "ValueError": "mg.ValueError",
            "TypeError": "mg.TypeError",
            "RuntimeError": "mg.RuntimeError",
            "IndexError": "mg.IndexError",
            "KeyError": "mg.KeyError",
            "ZeroDivisionError": "mg.ZeroDivisionError",
        }
        self.struct_info: dict[str, dict[str, Any]] = {}  # class definitions
        self.current_function: Optional[str] = None
        self.declared_vars: set[str] = set()
        self.function_return_types: dict[str, str] = {}
        self.variable_types: dict[str, str] = {}
        self.nested_vars: set[str] = set()
        self.append_map: dict[str, str] = {}
        self._is_generator: bool = False
        self._type_inference_engine: Optional[Any] = None

    # ------------------------------------------------------------------
    # Type helpers
    # ------------------------------------------------------------------
    @property
    def type_inference_engine(self) -> Any:
        """Lazily initialize and return the type inference engine."""
        if self._type_inference_engine is None:
            from .type_inference import create_typescript_type_inference_engine

            self._type_inference_engine = create_typescript_type_inference_engine(self)
        return self._type_inference_engine

    def _map_type(self, python_type: str) -> str:
        """Map a Python type name to a TypeScript type name."""
        return self.type_map.get(python_type, "any")

    @staticmethod
    def _is_array(t: str) -> bool:
        return t.endswith("[]")

    @staticmethod
    def _array_element(t: str) -> str:
        return t[:-2]

    @staticmethod
    def _is_map(t: str) -> bool:
        return t.startswith("Map<") and t.endswith(">")

    @staticmethod
    def _is_set(t: str) -> bool:
        return t.startswith("Set<") and t.endswith(">")

    @staticmethod
    def _set_element(t: str) -> str:
        return t[len("Set<") : -1]

    @staticmethod
    def _map_kv(t: str) -> tuple[str, str]:
        """Split ``Map<K, V>`` into (K, V), respecting nested generics."""
        inner = t[len("Map<") : -1]
        depth = 0
        for i, ch in enumerate(inner):
            if ch in "<[":
                depth += 1
            elif ch in ">]":
                depth -= 1
            elif ch == "," and depth == 0:
                return inner[:i].strip(), inner[i + 1 :].strip()
        return inner.strip(), "any"

    # ------------------------------------------------------------------
    # Module / top level
    # ------------------------------------------------------------------
    def convert_code(self, python_code: str) -> str:
        """Convert Python source to TypeScript."""
        try:
            tree = ast.parse(python_code)
            tree = normalize_ast(tree)
            return self._convert_module(tree)
        except Exception as e:
            raise TypeMappingError(f"Failed to convert Python code: {e}") from e

    def _convert_module(self, node: ast.Module) -> str:
        """Convert a Python module to a TypeScript module."""
        parts: list[str] = ['import * as mg from "./multigen_ts_runtime.ts";', ""]

        # Classes first
        for item in node.body:
            if isinstance(item, ast.ClassDef):
                parts.append(self._convert_class(item))
                parts.append("")

        # First pass: collect function return types
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "main":
                    self.function_return_types[item.name] = "void"
                elif item.returns:
                    mapped = self._map_type_annotation(item.returns)
                    self.function_return_types[item.name] = mapped or "void"
                else:
                    self.function_return_types[item.name] = "number"

        # Functions
        has_main = False
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "main":
                    has_main = True
                parts.append(self._convert_function(item))
                parts.append("")

        # Entry point: call main() if present
        if has_main:
            parts.append("main();")

        return "\n".join(parts).rstrip() + "\n"

    # ------------------------------------------------------------------
    # Classes
    # ------------------------------------------------------------------
    def _convert_class(self, node: ast.ClassDef) -> str:
        """Convert a Python class to a TypeScript class."""
        class_name = node.name

        init_method: Optional[ast.FunctionDef] = None
        other_methods: list[ast.FunctionDef] = []
        for item in node.body:
            if isinstance(item, ast.FunctionDef):
                if item.name == "__init__":
                    init_method = item
                else:
                    other_methods.append(item)

        lines = [f"class {class_name} {{"]

        # Field declarations from __init__
        if init_method:
            for stmt in init_method.body:
                if isinstance(stmt, ast.Assign):
                    for target in stmt.targets:
                        if self._is_self_attr(target):
                            field_type = self._infer_type_from_value(stmt.value)
                            lines.append(f"    {target.attr}: {field_type};")  # type: ignore[attr-defined]
                elif (
                    isinstance(stmt, ast.AnnAssign)
                    and isinstance(stmt.target, ast.Attribute)
                    and self._is_self_attr(stmt.target)
                ):
                    field_type = self._map_type_annotation(stmt.annotation)
                    lines.append(f"    {stmt.target.attr}: {field_type};")

        self.struct_info[class_name] = {"fields": self._extract_struct_fields(init_method) if init_method else []}

        # Constructor
        if init_method:
            lines.append("")
            lines.append(self._convert_constructor(init_method))

        # Methods
        for method in other_methods:
            lines.append("")
            lines.append(self._convert_method(method))

        lines.append("}")
        return "\n".join(lines)

    @staticmethod
    def _is_self_attr(node: ast.expr) -> bool:
        return isinstance(node, ast.Attribute) and isinstance(node.value, ast.Name) and node.value.id == "self"

    def _convert_constructor(self, init_method: ast.FunctionDef) -> str:
        """Convert __init__ to a TypeScript constructor."""
        params = self._params(init_method, skip_self=True)
        self.current_function = "__init__"
        self.declared_vars = set(a.arg for a in init_method.args.args[1:])
        self.variable_types = {}
        self._seed_param_types(init_method, skip_self=True)
        body = self._convert_statements(init_method.body)
        self.current_function = None
        return f"    constructor({params}) {{\n{body}\n    }}"

    def _convert_method(self, method: ast.FunctionDef) -> str:
        """Convert a Python instance method to a TypeScript method."""
        params = self._params(method, skip_self=True)
        return_type = ""
        if method.returns:
            mapped = self._map_type_annotation(method.returns)
            return_type = f": {mapped}" if mapped else ": void"
        else:
            return_type = ": void"

        self.current_function = method.name
        self.declared_vars = set(a.arg for a in method.args.args[1:])
        self.variable_types = {}
        self._seed_param_types(method, skip_self=True)
        self._pre_infer_variable_types(method.body)
        body = self._convert_statements(method.body)
        self.current_function = None
        return f"    {method.name}({params}){return_type} {{\n{body}\n    }}"

    # ------------------------------------------------------------------
    # Functions
    # ------------------------------------------------------------------
    def _params(self, node: ast.FunctionDef, skip_self: bool) -> str:
        args = node.args.args[1:] if skip_self else node.args.args
        nested = self.nested_vars
        parts = []
        for arg in args:
            t = self._infer_parameter_type(arg, node)
            if arg.arg in nested and t == "number[]":
                t = "number[][]"
            parts.append(f"{arg.arg}: {t}")
        return ", ".join(parts)

    def _seed_param_types(self, node: ast.FunctionDef, skip_self: bool) -> None:
        args = node.args.args[1:] if skip_self else node.args.args
        for arg in args:
            t = self._infer_parameter_type(arg, node)
            if arg.arg in self.nested_vars and t == "number[]":
                t = "number[][]"
            self.variable_types[arg.arg] = t

    def _convert_function(self, node: ast.FunctionDef) -> str:
        """Convert a Python function to a TypeScript function."""
        self.nested_vars = self._analyze_nested_subscripts(node.body)
        self.append_map = self._analyze_append_operations(node.body)

        params = self._params(node, skip_self=False)

        is_generator = any(isinstance(n, (ast.Yield, ast.YieldFrom)) for n in ast.walk(node))

        # Return type
        return_type = "void"
        if node.name != "main":
            if node.returns:
                mapped = self._map_type_annotation(node.returns)
                return_type = mapped or "void"
            else:
                return_type = self._infer_return_type(node) or "void"

        gen_element_type = "number"
        if is_generator:
            gen_element_type = self._get_generator_element_type(node)
            return_type = f"{gen_element_type}[]"

        # Reset per-function state
        self.current_function = node.name
        self.declared_vars = set(a.arg for a in node.args.args)
        self.variable_types = {}
        self._is_generator = is_generator
        self._seed_param_types(node, skip_self=False)
        self._pre_infer_variable_types(node.body)

        if node.name != "main":
            self.function_return_types[node.name] = return_type

        gen_prefix = ""
        if is_generator:
            gen_prefix = f"    let __mgen_result: {gen_element_type}[] = [];\n"

        body = self._convert_statements(node.body)
        if is_generator:
            body = gen_prefix + body + "\n    return __mgen_result;"

        self.current_function = None
        self.nested_vars = set()
        self.append_map = {}

        return f"function {node.name}({params}): {return_type} {{\n{body}\n}}"

    # ------------------------------------------------------------------
    # AST analysis passes (language-agnostic, adapted type strings)
    # ------------------------------------------------------------------
    def _analyze_nested_subscripts(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect variables used with nested subscripts like a[i][j]."""
        nested_vars: set[str] = set()

        def check_expr(expr: ast.expr) -> None:
            if isinstance(expr, ast.Subscript):
                if isinstance(expr.value, ast.Subscript):
                    base = expr.value.value
                    if isinstance(base, ast.Name):
                        nested_vars.add(base.id)
                check_expr(expr.value)
                if not isinstance(expr.slice, ast.Slice):
                    check_expr(expr.slice)
            elif isinstance(expr, ast.BinOp):
                check_expr(expr.left)
                check_expr(expr.right)
            elif isinstance(expr, ast.Call):
                for arg in expr.args:
                    check_expr(arg)
            elif isinstance(expr, ast.Compare):
                check_expr(expr.left)
                for comp in expr.comparators:
                    check_expr(comp)

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    check_expr(target)
                check_expr(stmt.value)
            elif isinstance(stmt, ast.AnnAssign):
                if stmt.value:
                    check_expr(stmt.value)
            elif isinstance(stmt, ast.AugAssign):
                check_expr(stmt.target)
                check_expr(stmt.value)
            elif isinstance(stmt, ast.Expr):
                check_expr(stmt.value)
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
                for s in getattr(stmt, "orelse", []):
                    check_stmt(s)
            elif isinstance(stmt, ast.If):
                check_expr(stmt.test)
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)
            elif isinstance(stmt, ast.Return) and stmt.value:
                check_expr(stmt.value)

        for stmt in stmts:
            check_stmt(stmt)
        return nested_vars

    def _analyze_append_operations(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Detect `container.append(var)` to propagate nested list types."""
        append_map: dict[str, str] = {}

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call):
                call = stmt.value
                if isinstance(call.func, ast.Attribute) and call.func.attr == "append":
                    if isinstance(call.func.value, ast.Name) and call.args:
                        if isinstance(call.args[0], ast.Name):
                            append_map[call.func.value.id] = call.args[0].id
            elif isinstance(stmt, (ast.For, ast.While)):
                for s in stmt.body:
                    check_stmt(s)
            elif isinstance(stmt, ast.If):
                for s in stmt.body:
                    check_stmt(s)
                for s in stmt.orelse:
                    check_stmt(s)

        for stmt in stmts:
            check_stmt(stmt)
        return append_map

    def _analyze_map_key_types(self, stmts: list[ast.stmt]) -> set[str]:
        """Detect maps accessed with string keys."""
        string_keyed: set[str] = set()

        def check_expr(expr: ast.expr) -> None:
            if isinstance(expr, ast.Subscript) and isinstance(expr.value, ast.Name):
                name = expr.value.id
                if isinstance(expr.slice, ast.Constant) and isinstance(expr.slice.value, str):
                    string_keyed.add(name)
                elif isinstance(expr.slice, ast.Name) and self.variable_types.get(expr.slice.id) == "string":
                    string_keyed.add(name)
            for child in ast.iter_child_nodes(expr):
                if isinstance(child, ast.expr):
                    check_expr(child)

        def check_stmt(stmt: ast.stmt) -> None:
            for child in ast.iter_child_nodes(stmt):
                if isinstance(child, ast.expr):
                    check_expr(child)
                elif isinstance(child, ast.stmt):
                    check_stmt(child)

        for stmt in stmts:
            check_stmt(stmt)
        return string_keyed

    def _analyze_map_value_types(self, stmts: list[ast.stmt]) -> dict[str, str]:
        """Detect map value types from `map[key] = value` assignments."""
        map_value_types: dict[str, str] = {}

        def check_stmt(stmt: ast.stmt) -> None:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name):
                        name = target.value.id
                        if name not in map_value_types:
                            map_value_types[name] = self._infer_type_from_value(stmt.value)
            for child in ast.iter_child_nodes(stmt):
                if isinstance(child, ast.stmt):
                    check_stmt(child)

        for stmt in stmts:
            check_stmt(stmt)
        return map_value_types

    def _pre_infer_variable_types(self, stmts: list[ast.stmt]) -> None:
        """Pre-pass: infer all variable types before code generation."""

        def collect_types(stmts: list[ast.stmt]) -> None:
            for stmt in stmts:
                if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                    if stmt.target.id not in self.variable_types:
                        self.variable_types[stmt.target.id] = self._map_type_annotation(stmt.annotation)
                elif isinstance(stmt, ast.Assign):
                    for target in stmt.targets:
                        if isinstance(target, ast.Name) and target.id not in self.variable_types:
                            self.variable_types[target.id] = self._infer_type_from_value(stmt.value)
                elif isinstance(stmt, (ast.For, ast.While)):
                    collect_types(stmt.body)
                    collect_types(getattr(stmt, "orelse", []))
                elif isinstance(stmt, ast.If):
                    collect_types(stmt.body)
                    collect_types(stmt.orelse)

        collect_types(stmts)

        # Upgrade based on append operations (list-of-list)
        for container, appended in self.append_map.items():
            at = self.variable_types.get(appended, "")
            if at.endswith("[]") and container in self.variable_types:
                self.variable_types[container] = f"{at}[]"

        # Upgrade based on nested subscript usage
        for name in self.nested_vars:
            if self.variable_types.get(name) == "number[]":
                self.variable_types[name] = "number[][]"

        # String-keyed maps: Map<number, V> -> Map<string, V>
        for name in self._analyze_map_key_types(stmts):
            t = self.variable_types.get(name, "")
            if self._is_map(t):
                _, v = self._map_kv(t)
                self.variable_types[name] = f"Map<string, {v}>"

        # Map value types from subscript assignment
        for name, vtype in self._analyze_map_value_types(stmts).items():
            t = self.variable_types.get(name, "")
            if self._is_map(t) and vtype != "any":
                k, v = self._map_kv(t)
                if v == "number":
                    self.variable_types[name] = f"Map<{k}, {vtype}>"

    # ------------------------------------------------------------------
    # Statements
    # ------------------------------------------------------------------
    def _convert_statements(self, statements: list[ast.stmt]) -> str:
        return "\n".join(self._convert_statement(s) for s in statements)

    def _convert_statement(self, stmt: ast.stmt) -> str:
        if isinstance(stmt, ast.Return):
            return self._convert_return(stmt)
        elif isinstance(stmt, ast.Assign):
            return self._convert_assignment(stmt)
        elif isinstance(stmt, ast.AnnAssign):
            return self._convert_annotated_assignment(stmt)
        elif isinstance(stmt, ast.AugAssign):
            return self._convert_aug_assignment(stmt)
        elif isinstance(stmt, ast.If):
            return self._convert_if(stmt)
        elif isinstance(stmt, ast.While):
            return self._convert_while(stmt)
        elif isinstance(stmt, ast.For):
            return self._convert_for(stmt)
        elif isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.YieldFrom):
            return self._convert_yield_from(stmt.value)
        elif isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Yield):
            return self._convert_yield(stmt.value)
        elif isinstance(stmt, ast.Expr):
            return self._convert_expression_statement(stmt)
        elif isinstance(stmt, ast.Pass):
            return "    // pass"
        elif isinstance(stmt, ast.Assert):
            return self._convert_assert(stmt)
        elif isinstance(stmt, ast.Try):
            return self._convert_try(stmt)
        elif isinstance(stmt, ast.Raise):
            return self._convert_raise(stmt)
        elif isinstance(stmt, ast.With):
            return self._convert_with(stmt)
        elif isinstance(stmt, ast.Break):
            return "    break;"
        elif isinstance(stmt, ast.Continue):
            return "    continue;"
        else:
            raise UnsupportedFeatureError(f"Unsupported statement type: {type(stmt).__name__}")

    def _convert_yield(self, node: ast.Yield) -> str:
        value = self._convert_expression(node.value) if node.value is not None else "0"
        return f"    __mgen_result.push({value});"

    def _convert_yield_from(self, node: ast.YieldFrom) -> str:
        if isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Name):
            if node.value.func.id == "range":
                args = [self._convert_expression(a) for a in node.value.args]
                if len(args) == 1:
                    return f"    for (let __yf = 0; __yf < {args[0]}; __yf++) {{ __mgen_result.push(__yf); }}"
                elif len(args) == 2:
                    return f"    for (let __yf = {args[0]}; __yf < {args[1]}; __yf++) {{ __mgen_result.push(__yf); }}"
                elif len(args) == 3:
                    return (
                        f"    for (let __yf = {args[0]}; __yf < {args[1]}; __yf += {args[2]}) "
                        f"{{ __mgen_result.push(__yf); }}"
                    )
        expr = self._convert_expression(node.value)
        return f"    __mgen_result.push(...{expr});"

    def _get_generator_element_type(self, node: ast.FunctionDef) -> str:
        if node.returns:
            mapped = self._map_type_annotation(node.returns)
            if mapped in ("number", "boolean", "string"):
                return mapped
        return "number"

    def _convert_assert(self, stmt: ast.Assert) -> str:
        test = self._convert_expression(stmt.test)
        if stmt.msg and isinstance(stmt.msg, ast.Constant) and isinstance(stmt.msg.value, str):
            msg = json.dumps(stmt.msg.value)
        else:
            msg = '"assertion failed"'
        return f"    mg.assert_({test}, {msg});"

    def _convert_try(self, stmt: ast.Try) -> str:
        lines = ["    try {"]
        for s in stmt.body:
            for line in self._convert_statement(s).split("\n"):
                lines.append(f"    {line}")
        # Single catch block dispatching on instanceof
        lines.append("    } catch (__e) {")
        first = True
        has_bare = False
        for handler in stmt.handlers:
            if handler.type is None:
                has_bare = True
                indent = "        "
                if not first:
                    lines.append("        } else {")
                    indent = "            "
                if handler.name:
                    lines.append(f"{indent}const {handler.name} = __e;")
                for s in handler.body:
                    for line in self._convert_statement(s).split("\n"):
                        lines.append(f"    {line}" if first else f"        {line}")
                first = False
            else:
                exc_type = handler.type.id if isinstance(handler.type, ast.Name) else "Error"
                ts_exc = self.exception_map.get(exc_type, exc_type)
                cond = f"__e instanceof {ts_exc}"
                lines.append(f"        {'if' if first else '} else if'} ({cond}) {{")
                if handler.name:
                    lines.append(f"            const {handler.name} = __e;")
                for s in handler.body:
                    for line in self._convert_statement(s).split("\n"):
                        lines.append(f"        {line}")
                first = False
        if not first and not has_bare:
            lines.append("        } else {")
            lines.append("            throw __e;")
            lines.append("        }")
        lines.append("    }")
        if stmt.finalbody:
            # emit finally by wrapping: simplest is a trailing finally block
            lines[-1] = "    } finally {"
            for s in stmt.finalbody:
                for line in self._convert_statement(s).split("\n"):
                    lines.append(f"    {line}")
            lines.append("    }")
        return "\n".join(lines)

    def _convert_raise(self, stmt: ast.Raise) -> str:
        if stmt.exc is None:
            return "    throw __e;"
        if isinstance(stmt.exc, ast.Call) and isinstance(stmt.exc.func, ast.Name):
            exc_type = stmt.exc.func.id
            ts_exc = self.exception_map.get(exc_type, exc_type)
            if stmt.exc.args:
                msg = self._convert_expression(stmt.exc.args[0])
                return f"    throw new {ts_exc}({msg});"
            return f'    throw new {ts_exc}("");'
        return '    throw new Error("Unknown exception");'

    def _convert_with(self, stmt: ast.With) -> str:
        item = stmt.items[0]
        var_name = item.optional_vars.id if isinstance(item.optional_vars, ast.Name) else "f"
        ctx = item.context_expr
        lines = []
        if isinstance(ctx, ast.Call) and isinstance(ctx.func, ast.Name) and ctx.func.id == "open":
            filename = self._convert_expression(ctx.args[0])
            mode = "r"
            if len(ctx.args) > 1 and isinstance(ctx.args[1], ast.Constant) and isinstance(ctx.args[1].value, str):
                mode = ctx.args[1].value
            lines.append(f"    const {var_name} = mg.open({filename}, {json.dumps(mode)});")
            lines.append("    try {")
            for s in stmt.body:
                for line in self._convert_statement(s).split("\n"):
                    lines.append(f"    {line}")
            lines.append("    } finally {")
            lines.append(f"        {var_name}.close();")
            lines.append("    }")
        else:
            for s in stmt.body:
                lines.append(self._convert_statement(s))
        return "\n".join(lines)

    def _convert_return(self, stmt: ast.Return) -> str:
        if self.current_function == "main":
            return ""
        if stmt.value:
            return f"    return {self._convert_expression(stmt.value)};"
        return "    return;"

    def _convert_assignment(self, stmt: ast.Assign) -> str:
        value_expr = self._convert_expression(stmt.value)
        out = []
        for target in stmt.targets:
            if isinstance(target, ast.Name):
                if target.id in self.declared_vars:
                    out.append(f"    {target.id} = {value_expr};")
                else:
                    self.declared_vars.add(target.id)
                    t = self.variable_types.get(target.id)
                    if t and t != "any" and not isinstance(stmt.value, ast.Call):
                        out.append(f"    let {target.id}: {t} = {value_expr};")
                    else:
                        out.append(f"    let {target.id} = {value_expr};")
            elif isinstance(target, ast.Attribute):
                out.append(f"    {self._convert_attribute_target(target)} = {value_expr};")
            elif isinstance(target, ast.Subscript):
                out.append(self._convert_subscript_assign(target, value_expr))
            elif isinstance(target, ast.Tuple):
                names = ", ".join(e.id if isinstance(e, ast.Name) else self._convert_expression(e) for e in target.elts)
                out.append(f"    [{names}] = {value_expr};")
        return "\n".join(out)

    def _convert_subscript_assign(self, target: ast.Subscript, value_expr: str) -> str:
        container = self._convert_expression(target.value)
        ctype = self._infer_type_from_value(target.value)
        index = self._convert_expression(target.slice)
        if self._is_map(ctype):
            return f"    {container}.set({index}, {value_expr});"
        return f"    {container}[{index}] = {value_expr};"

    def _convert_attribute_target(self, target: ast.Attribute) -> str:
        if isinstance(target.value, ast.Name) and target.value.id == "self":
            return f"this.{target.attr}"
        return f"{self._convert_expression(target.value)}.{target.attr}"

    def _convert_annotated_assignment(self, stmt: ast.AnnAssign) -> str:
        if isinstance(stmt.target, ast.Name):
            var_type = self.variable_types.get(stmt.target.id) or self._map_type_annotation(stmt.annotation)
            self.declared_vars.add(stmt.target.id)
            self.variable_types[stmt.target.id] = var_type
            if stmt.value is not None:
                value_expr = self._convert_expression(stmt.value)
                return f"    let {stmt.target.id}: {var_type} = {value_expr};"
            return f"    let {stmt.target.id}: {var_type} = {self._get_default_value(var_type)};"
        elif isinstance(stmt.target, ast.Attribute):
            attr = self._convert_attribute_target(stmt.target)
            if stmt.value is not None:
                return f"    {attr} = {self._convert_expression(stmt.value)};"
            var_type = self._map_type_annotation(stmt.annotation)
            return f"    {attr} = {self._get_default_value(var_type)};"
        raise UnsupportedFeatureError(f"Unsupported annotated assignment: {ast.unparse(stmt)}")

    def _convert_aug_assignment(self, stmt: ast.AugAssign) -> str:
        value_expr = self._convert_expression(stmt.value)
        target = stmt.target
        if isinstance(target, ast.Name):
            tref = target.id
        elif isinstance(target, ast.Attribute):
            tref = self._convert_attribute_target(target)
        elif isinstance(target, ast.Subscript):
            return self._convert_aug_subscript(target, stmt.op, value_expr)
        else:
            raise UnsupportedFeatureError(f"Unsupported aug-assign target: {ast.unparse(target)}")

        if isinstance(stmt.op, ast.FloorDiv):
            return f"    {tref} = mg.floorDiv({tref}, {value_expr});"
        elif isinstance(stmt.op, ast.Mod):
            return f"    {tref} = mg.pyMod({tref}, {value_expr});"
        elif isinstance(stmt.op, ast.Pow):
            return f"    {tref} = {tref} ** {value_expr};"
        op = get_augmented_assignment_operator(stmt.op) or "/*UNKNOWN_OP*/"
        return f"    {tref} {op} {value_expr};"

    def _convert_aug_subscript(self, target: ast.Subscript, op: ast.operator, value_expr: str) -> str:
        container = self._convert_expression(target.value)
        ctype = self._infer_type_from_value(target.value)
        index = self._convert_expression(target.slice)
        cur = self._read_subscript(container, ctype, index)
        combined = self._binop_expr(cur, op, value_expr)
        if self._is_map(ctype):
            return f"    {container}.set({index}, {combined});"
        return f"    {container}[{index}] = {combined};"

    def _convert_if(self, stmt: ast.If) -> str:
        condition = self._convert_expression(stmt.test)
        then_body = self._convert_statements(stmt.body)
        if_part = f"    if ({condition}) {{\n{then_body}\n    }}"
        if stmt.orelse:
            if len(stmt.orelse) == 1 and isinstance(stmt.orelse[0], ast.If):
                else_body = self._convert_if(stmt.orelse[0]).strip()
                return f"{if_part} else {else_body}"
            else:
                else_body = self._convert_statements(stmt.orelse)
                return if_part + " else {\n" + else_body + "\n    }"
        return if_part

    def _convert_while(self, stmt: ast.While) -> str:
        condition = self._convert_expression(stmt.test)
        body = self._convert_statements(stmt.body)
        return f"    while ({condition}) {{\n{body}\n    }}"

    def _convert_for(self, stmt: ast.For) -> str:
        if isinstance(stmt.iter, ast.Call) and isinstance(stmt.iter.func, ast.Name) and stmt.iter.func.id == "range":
            range_args = [self._convert_expression(a) for a in stmt.iter.args]
            var = stmt.target.id if isinstance(stmt.target, ast.Name) else "i"
            body = self._convert_statements(stmt.body)
            if len(range_args) == 1:
                start, stop, step = "0", range_args[0], "1"
            elif len(range_args) == 2:
                start, stop, step = range_args[0], range_args[1], "1"
            elif len(range_args) == 3:
                start, stop, step = range_args
            else:
                raise UnsupportedFeatureError("range() expects one to three arguments")
            step_node = stmt.iter.args[2] if len(stmt.iter.args) == 3 else None
            if step_node is None or (isinstance(step_node, ast.Constant) and step_node.value == 1):
                header = f"let {var} = {start}; {var} < {stop}; {var}++"
                return f"    for ({header}) {{\n{body}\n    }}"
            if isinstance(step_node, ast.Constant) and isinstance(step_node.value, int) and step_node.value == 0:
                return f"    if ({step} === 0) {{ throw new Error('range() step cannot be zero'); }}\n"
            if isinstance(step_node, ast.Constant) and isinstance(step_node.value, int) and step_node.value > 0:
                header = f"let {var} = {start}; {var} < {stop}; {var} += {step}"
                return f"    for ({header}) {{\n{body}\n    }}"
            zero_check = f"    if ({step} === 0) {{ throw new Error('range() step cannot be zero'); }}\n"
            header = f"let {var} = {start}; ({step} > 0 ? {var} < {stop} : {var} > {stop}); {var} += {step}"
            return f"{zero_check}    for ({header}) {{\n{body}\n    }}"

        # Iteration over container(s)
        body = self._convert_statements(stmt.body)
        iter_expr, tuple_target = self._convert_iterable(stmt.iter)
        if isinstance(stmt.target, ast.Tuple):
            names = ", ".join(e.id if isinstance(e, ast.Name) else "_" for e in stmt.target.elts)
            return f"    for (const [{names}] of {iter_expr}) {{\n{body}\n    }}"
        var = stmt.target.id if isinstance(stmt.target, ast.Name) else "item"
        return f"    for (const {var} of {iter_expr}) {{\n{body}\n    }}"

    def _convert_iterable(self, expr: ast.expr) -> tuple[str, bool]:
        """Return (iterable_expr, is_tuple) for a for-loop iterable."""
        # dict.items()/keys()/values()
        if isinstance(expr, ast.Call) and isinstance(expr.func, ast.Attribute):
            obj = self._convert_expression(expr.func.value)
            if expr.func.attr == "items":
                return obj, True
            if expr.func.attr == "keys":
                return f"{obj}.keys()", False
            if expr.func.attr == "values":
                return f"{obj}.values()", False
        return self._convert_expression(expr), False

    def _convert_expression_statement(self, stmt: ast.Expr) -> str:
        if isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            return f"    // {stmt.value.value}"
        return f"    {self._convert_expression(stmt.value)};"

    # ------------------------------------------------------------------
    # Expressions
    # ------------------------------------------------------------------
    def _convert_expression(self, expr: ast.expr) -> str:
        if isinstance(expr, ast.Constant):
            return self._convert_constant(expr)
        elif isinstance(expr, ast.Name):
            return expr.id
        elif isinstance(expr, ast.BinOp):
            return self._convert_binop(expr)
        elif isinstance(expr, ast.BoolOp):
            return self._convert_boolop(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self._convert_unaryop(expr)
        elif isinstance(expr, ast.Compare):
            return self._convert_compare(expr)
        elif isinstance(expr, ast.Call):
            return self._convert_call(expr)
        elif isinstance(expr, ast.Attribute):
            return self._convert_attribute(expr)
        elif isinstance(expr, ast.List):
            return self._convert_list_literal(expr)
        elif isinstance(expr, ast.Tuple):
            return "[" + ", ".join(self._convert_expression(e) for e in expr.elts) + "]"
        elif isinstance(expr, ast.Dict):
            return self._convert_dict_literal(expr)
        elif isinstance(expr, ast.Set):
            return self._convert_set_literal(expr)
        elif isinstance(expr, ast.ListComp):
            return self._convert_list_comprehension(expr)
        elif isinstance(expr, ast.DictComp):
            return self._convert_dict_comprehension(expr)
        elif isinstance(expr, ast.SetComp):
            return self._convert_set_comprehension(expr)
        elif isinstance(expr, ast.Subscript):
            return self._convert_subscript(expr)
        elif isinstance(expr, ast.JoinedStr):
            return self._convert_f_string(expr)
        elif isinstance(expr, ast.IfExp):
            body = self._convert_expression(expr.body)
            test = self._convert_expression(expr.test)
            orelse = self._convert_expression(expr.orelse)
            return f"({test} ? {body} : {orelse})"
        else:
            raise UnsupportedFeatureError(f"Unsupported expression type: {type(expr).__name__}")

    def _convert_constant(self, expr: ast.Constant) -> str:
        if isinstance(expr.value, bool):
            return "true" if expr.value else "false"
        elif isinstance(expr.value, str):
            return json.dumps(expr.value)
        elif expr.value is None:
            return "null"
        elif isinstance(expr.value, float):
            if expr.value.is_integer():
                return str(int(expr.value))
            return repr(expr.value)
        else:
            return str(expr.value)

    def _binop_expr(self, left: str, op: ast.operator, right: str) -> str:
        if isinstance(op, ast.Pow):
            return f"({left} ** {right})"
        elif isinstance(op, ast.FloorDiv):
            return f"mg.floorDiv({left}, {right})"
        elif isinstance(op, ast.Mod):
            return f"mg.pyMod({left}, {right})"
        op_str = get_standard_binary_operator(op) or "/*UNKNOWN_OP*/"
        return f"({left} {op_str} {right})"

    def _convert_binop(self, expr: ast.BinOp) -> str:
        left = self._convert_expression(expr.left)
        right = self._convert_expression(expr.right)
        # str + str stays +, but Python `*` on strings/lists is unsupported here
        return self._binop_expr(left, expr.op, right)

    def _convert_boolop(self, expr: ast.BoolOp) -> str:
        op = "&&" if isinstance(expr.op, ast.And) else "||"
        parts = [self._convert_expression(v) for v in expr.values]
        return "(" + f" {op} ".join(parts) + ")"

    def _convert_unaryop(self, expr: ast.UnaryOp) -> str:
        operand = self._convert_expression(expr.operand)
        op_map = {ast.UAdd: "+", ast.USub: "-", ast.Not: "!", ast.Invert: "~"}
        op = op_map.get(type(expr.op), "/*UNKNOWN_OP*/")
        return f"({op}{operand})"

    def _convert_compare(self, expr: ast.Compare) -> str:
        result = self._convert_expression(expr.left)
        for op, comp in zip(expr.ops, expr.comparators):
            comp_expr = self._convert_expression(comp)
            if isinstance(op, (ast.In, ast.NotIn)):
                ctype = self._infer_type_from_value(comp)
                if self._is_map(ctype) or self._is_set(ctype):
                    membership = f"{comp_expr}.has({result})"
                else:
                    membership = f"{comp_expr}.includes({result})"
                result = f"(!{membership})" if isinstance(op, ast.NotIn) else membership
            elif isinstance(op, ast.Is):
                result = f"({result} === {comp_expr})"
            elif isinstance(op, ast.IsNot):
                result = f"({result} !== {comp_expr})"
            else:
                op_str = get_standard_comparison_operator(op) or "/*UNKNOWN_OP*/"
                result = f"({result} {op_str} {comp_expr})"
        return result

    def _convert_call(self, expr: ast.Call) -> str:
        if isinstance(expr.func, ast.Name):
            func_name = expr.func.id
            args = [self._convert_expression(a) for a in expr.args]

            # Empty container constructors
            if func_name == "list" and not args:
                return "[]"
            if func_name == "dict" and not args:
                return "new Map()"
            if func_name == "set" and not args:
                return "new Set()"
            if func_name == "list" and args:
                return f"Array.from({args[0]})"
            if func_name == "set" and args:
                return f"new Set({args[0]})"

            builtin = self._convert_builtin(func_name, expr, args)
            if builtin is not None:
                return builtin

            if func_name in self.struct_info:
                return f"new {func_name}({', '.join(args)})"
            return f"{func_name}({', '.join(args)})"

        elif isinstance(expr.func, ast.Attribute):
            return self._convert_method_call_expression(expr)
        return "/* Complex function call */"

    def _convert_builtin(self, func_name: str, expr: ast.Call, args: list[str]) -> Optional[str]:
        if func_name == "print":
            return f"mg.print({', '.join(args)})"
        if func_name == "len":
            arg_type = self._infer_type_from_value(expr.args[0])
            if self._is_map(arg_type) or self._is_set(arg_type):
                return f"{args[0]}.size"
            return f"{args[0]}.length"
        if func_name == "abs":
            return f"Math.abs({args[0]})"
        if func_name == "min":
            return f"mg.min({', '.join(args)})"
        if func_name == "max":
            return f"mg.max({', '.join(args)})"
        if func_name == "sum":
            return f"mg.sum({args[0]})"
        if func_name == "any":
            return f"mg.any({args[0]})"
        if func_name == "all":
            return f"mg.all({args[0]})"
        if func_name == "bool":
            return f"mg.toBool({args[0]})"
        if func_name == "int":
            return f"mg.toInt({args[0]})"
        if func_name == "float":
            return f"mg.toFloat({args[0]})"
        if func_name == "str":
            return f"mg.toStr({args[0]})"
        if func_name == "range":
            return f"mg.range({', '.join(args)})"
        if func_name == "sorted":
            return f"mg.sorted({args[0]})"
        if func_name == "enumerate":
            return f"mg.enumerate({args[0]})"
        if func_name == "zip":
            return f"mg.zip({', '.join(args)})"
        return None

    def _convert_method_call_expression(self, expr: ast.Call) -> str:
        assert isinstance(expr.func, ast.Attribute)
        obj_expr = self._convert_expression(expr.func.value)
        method_name = expr.func.attr
        args = [self._convert_expression(a) for a in expr.args]
        obj_type = self._infer_type_from_value(expr.func.value)

        # String methods
        if method_name == "upper":
            return f"{obj_expr}.toUpperCase()"
        if method_name == "lower":
            return f"{obj_expr}.toLowerCase()"
        if method_name == "strip":
            return f"mg.strip({obj_expr}, {args[0]})" if args else f"mg.strip({obj_expr})"
        if method_name == "split":
            return f"mg.split({obj_expr}, {args[0]})" if args else f"mg.split({obj_expr})"
        if method_name == "replace":
            return f"{obj_expr}.replaceAll({args[0]}, {args[1]})"
        if method_name == "find":
            return f"{obj_expr}.indexOf({args[0]})"
        if method_name == "join":
            return f"{args[0]}.join({obj_expr})"
        if method_name == "startswith":
            return f"{obj_expr}.startsWith({args[0]})"
        if method_name == "endswith":
            return f"{obj_expr}.endsWith({args[0]})"

        # List methods
        if method_name == "append":
            return f"{obj_expr}.push({args[0]})"
        if method_name == "pop" and not args:
            return f"{obj_expr}.pop()"

        # Set methods
        if method_name == "add":
            return f"{obj_expr}.add({args[0]})"
        if method_name == "discard" or method_name == "remove":
            if self._is_set(obj_type):
                return f"{obj_expr}.delete({args[0]})"

        # Dict methods
        if method_name == "keys":
            return f"[...{obj_expr}.keys()]"
        if method_name == "values":
            return f"[...{obj_expr}.values()]"
        if method_name == "items":
            return f"[...{obj_expr}.entries()]"
        if method_name == "get":
            if len(args) == 2:
                return f"({obj_expr}.get({args[0]}) ?? {args[1]})"
            return f"{obj_expr}.get({args[0]})"

        # self.method() -> this.method()
        args_str = ", ".join(args)
        return f"{obj_expr}.{method_name}({args_str})"

    def _convert_attribute(self, expr: ast.Attribute) -> str:
        if isinstance(expr.value, ast.Name) and expr.value.id == "self":
            return f"this.{expr.attr}"
        return f"{self._convert_expression(expr.value)}.{expr.attr}"

    def _convert_list_literal(self, expr: ast.List) -> str:
        return "[" + ", ".join(self._convert_expression(e) for e in expr.elts) + "]"

    def _convert_dict_literal(self, expr: ast.Dict) -> str:
        if not expr.keys:
            return "new Map()"
        pairs = []
        for key, value in zip(expr.keys, expr.values):
            if key is not None:
                pairs.append(f"[{self._convert_expression(key)}, {self._convert_expression(value)}]")
        return f"new Map([{', '.join(pairs)}])"

    def _convert_set_literal(self, expr: ast.Set) -> str:
        if not expr.elts:
            return "new Set()"
        elements = ", ".join(self._convert_expression(e) for e in expr.elts)
        return f"new Set([{elements}])"

    def _comp_source(self, iter_expr: ast.expr, ifs: list[ast.expr], var: str) -> str:
        """Build the source array expression with optional filter."""
        if isinstance(iter_expr, ast.Call) and isinstance(iter_expr.func, ast.Name) and iter_expr.func.id == "range":
            range_args = ", ".join(self._convert_expression(a) for a in iter_expr.args)
            source = f"mg.range({range_args})"
        else:
            source = self._convert_expression(iter_expr)
            # Set/Map are iterable but lack array methods (.filter/.map); spread first.
            itype = self._infer_type_from_value(iter_expr)
            if self._is_set(itype) or self._is_map(itype):
                source = f"[...{source}]"
        if ifs:
            cond = " && ".join(self._convert_expression(c) for c in ifs)
            source = f"{source}.filter(({var}) => {cond})"
        return source

    def _comp_binding(self, target: ast.expr) -> str:
        """Return the arrow-function parameter for a comprehension target: `x` or `[k, v]`."""
        if isinstance(target, ast.Name):
            return target.id
        if isinstance(target, ast.Tuple):
            names = [e.id for e in target.elts if isinstance(e, ast.Name)]
            if len(names) == len(target.elts) == 2:
                return f"[{names[0]}, {names[1]}]"
        raise UnsupportedFeatureError(f"Unsupported comprehension target: {ast.unparse(target)}")

    def _convert_list_comprehension(self, expr: ast.ListComp) -> str:
        gen = expr.generators[0]
        var = self._comp_binding(gen.target)
        source = self._comp_source(gen.iter, gen.ifs, var)
        element = self._convert_expression(expr.elt)
        return f"{source}.map(({var}) => {element})"

    def _convert_dict_comprehension(self, expr: ast.DictComp) -> str:
        gen = expr.generators[0]
        binding = self._comp_binding(gen.target)
        source = self._comp_source(gen.iter, gen.ifs, binding)
        key = self._convert_expression(expr.key)
        value = self._convert_expression(expr.value)
        return f"new Map({source}.map(({binding}) => [{key}, {value}]))"

    def _convert_set_comprehension(self, expr: ast.SetComp) -> str:
        gen = expr.generators[0]
        var = self._comp_binding(gen.target)
        source = self._comp_source(gen.iter, gen.ifs, var)
        element = self._convert_expression(expr.elt)
        return f"new Set({source}.map(({var}) => {element}))"

    def _read_subscript(self, container: str, ctype: str, index: str) -> str:
        if self._is_map(ctype):
            return f"{container}.get({index})!"
        return f"{container}[{index}]"

    def _convert_subscript(self, expr: ast.Subscript) -> str:
        if isinstance(expr.slice, ast.Slice):
            return self._convert_slice(expr)
        container = self._convert_expression(expr.value)
        ctype = self._infer_type_from_value(expr.value)
        index = self._convert_expression(expr.slice)
        # Python negative index a[-1] -> a.at(-1) for arrays/strings
        if isinstance(expr.slice, ast.UnaryOp) and isinstance(expr.slice.op, ast.USub):
            if not self._is_map(ctype):
                return f"{container}.at({index})"
        return self._read_subscript(container, ctype, index)

    def _convert_slice(self, expr: ast.Subscript) -> str:
        slice_obj = expr.slice
        assert isinstance(slice_obj, ast.Slice)
        obj = self._convert_expression(expr.value)
        start = self._convert_expression(slice_obj.lower) if slice_obj.lower else ""
        stop = self._convert_expression(slice_obj.upper) if slice_obj.upper else ""
        if start and stop:
            return f"{obj}.slice({start}, {stop})"
        elif start:
            return f"{obj}.slice({start})"
        elif stop:
            return f"{obj}.slice(0, {stop})"
        return f"{obj}.slice()"

    def _convert_f_string(self, expr: ast.JoinedStr) -> str:
        parts = ["`"]
        for value in expr.values:
            if isinstance(value, ast.Constant) and isinstance(value.value, str):
                parts.append(value.value.replace("\\", "\\\\").replace("`", "\\`").replace("$", "\\$"))
            elif isinstance(value, ast.FormattedValue):
                code = self._convert_expression(value.value)
                spec = extract_format_spec(value)
                if spec:
                    parts.append(f"${{mg.format({code}, {json.dumps(spec)})}}")
                else:
                    parts.append(f"${{mg.toStr({code})}}")
        parts.append("`")
        return "".join(parts)

    # ------------------------------------------------------------------
    # Type inference / mapping helpers
    # ------------------------------------------------------------------
    def _map_type_annotation(self, annotation: ast.expr) -> str:
        if isinstance(annotation, ast.Name):
            return self.type_map.get(annotation.id, "any")
        elif isinstance(annotation, ast.Subscript) and isinstance(annotation.value, ast.Name):
            container = annotation.value.id
            if container == "list":
                if isinstance(annotation.slice, ast.Name):
                    return f"{self.type_map.get(annotation.slice.id, annotation.slice.id)}[]"
                elif isinstance(annotation.slice, ast.Subscript):
                    return f"{self._map_type_annotation(annotation.slice)}[]"
                return "any[]"
            elif container == "dict":
                if isinstance(annotation.slice, ast.Tuple) and len(annotation.slice.elts) == 2:
                    k = self._map_type_annotation(annotation.slice.elts[0])
                    v = self._map_type_annotation(annotation.slice.elts[1])
                    return f"Map<{k}, {v}>"
                return "Map<any, any>"
            elif container == "set":
                if isinstance(annotation.slice, ast.Name):
                    return f"Set<{self.type_map.get(annotation.slice.id, annotation.slice.id)}>"
                return "Set<any>"
            elif container == "tuple":
                return "any[]"
            elif container in ("Optional",):
                inner = self._map_type_annotation(annotation.slice)
                return f"{inner} | null"
            return "any"
        elif isinstance(annotation, ast.Constant):
            if annotation.value is None:
                return "void"
            return str(annotation.value)
        return "any"

    def _infer_type_from_value(self, value: ast.expr) -> str:
        context = InferenceContext(type_mapper=self._map_type, variable_types=self.variable_types)
        result: str = self.type_inference_engine.infer_type(value, context)
        return result

    def _infer_loop_variable_type(self, generator: ast.comprehension) -> dict[str, str]:
        target = generator.target
        iter_expr = generator.iter
        loop_var_types: dict[str, str] = {}
        if isinstance(target, ast.Name):
            if (
                isinstance(iter_expr, ast.Call)
                and isinstance(iter_expr.func, ast.Name)
                and iter_expr.func.id == "range"
            ):
                loop_var_types[target.id] = "number"
            else:
                iter_type = self._infer_type_from_value(iter_expr)
                if self._is_array(iter_type):
                    loop_var_types[target.id] = self._array_element(iter_type)
                elif self._is_set(iter_type):
                    loop_var_types[target.id] = self._set_element(iter_type)
                else:
                    loop_var_types[target.id] = "any"
        return loop_var_types

    def _infer_comprehension_element_type(self, expr: ast.expr, loop_var_types: dict[str, str]) -> str:
        if isinstance(expr, ast.Constant):
            if isinstance(expr.value, bool):
                return "boolean"
            elif isinstance(expr.value, int):
                return "number"
            elif isinstance(expr.value, float):
                return "number"
            elif isinstance(expr.value, str):
                return "string"
        elif isinstance(expr, ast.Name):
            if expr.id in loop_var_types:
                return loop_var_types[expr.id]
            if expr.id in self.variable_types:
                return self.variable_types[expr.id]
            return "number"
        elif isinstance(expr, ast.BinOp):
            left = self._infer_comprehension_element_type(expr.left, loop_var_types)
            right = self._infer_comprehension_element_type(expr.right, loop_var_types)
            if left == right:
                return left
            return "number"
        elif isinstance(expr, ast.Call) and isinstance(expr.func, ast.Attribute):
            if expr.func.attr in ("upper", "lower", "strip", "replace"):
                return "string"
            return "number"
        return "number"

    def _infer_parameter_type(self, arg: ast.arg, func: ast.FunctionDef) -> str:
        if arg.annotation:
            return self._map_type_annotation(arg.annotation)
        return "any"

    def _infer_return_type(self, func: ast.FunctionDef) -> str:
        for stmt in func.body:
            if isinstance(stmt, ast.Return) and stmt.value:
                return "any"
        return "void"

    def _extract_struct_fields(self, init_method: ast.FunctionDef) -> list[str]:
        fields = []
        for stmt in init_method.body:
            if isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    if self._is_self_attr(target):
                        fields.append(target.attr)  # type: ignore[attr-defined]
            elif (
                isinstance(stmt, ast.AnnAssign)
                and isinstance(stmt.target, ast.Attribute)
                and self._is_self_attr(stmt.target)
            ):
                fields.append(stmt.target.attr)
        return fields

    def _get_default_value(self, ts_type: str) -> str:
        defaults = {
            "number": "0",
            "boolean": "false",
            "string": '""',
            "void": "undefined",
            "any": "null",
        }
        if ts_type in defaults:
            return defaults[ts_type]
        if self._is_array(ts_type):
            return "[]"
        if self._is_map(ts_type):
            return "new Map()"
        if self._is_set(ts_type):
            return "new Set()"
        return "null"
