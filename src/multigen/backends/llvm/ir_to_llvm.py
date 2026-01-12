"""Convert MultiGen Static IR to LLVM IR using llvmlite.

This module implements the IRVisitor pattern to traverse MultiGen's Static IR
and generate corresponding LLVM IR instructions.
"""

import ast
from typing import Optional, Union

from llvmlite import ir  # type: ignore[import-untyped]

from ...frontend.static_ir import (
    IRAssignment,
    IRBinaryOperation,
    IRBreak,
    IRComprehension,
    IRContinue,
    IRDataType,
    IRExpression,
    IRExpressionStatement,
    IRFor,
    IRFunction,
    IRFunctionCall,
    IRIf,
    IRLiteral,
    IRModule,
    IRReturn,
    IRType,
    IRTypeCast,
    IRTypeDeclaration,
    IRVariable,
    IRVariableReference,
    IRVisitor,
    IRWhile,
)
from .runtime_decls import LLVMRuntimeDeclarations


class IRToLLVMConverter(IRVisitor):
    """Convert MultiGen Static IR to LLVM IR using the visitor pattern."""

    def __init__(self) -> None:
        """Initialize the LLVM IR converter."""
        self.module: ir.Module = ir.Module(name="multigen_module")
        # Set target triple to empty string to use native target
        # llvmlite will use the host's target triple
        self.module.triple = ""
        self.builder: Optional[ir.IRBuilder] = None
        self.func_symtab: dict[str, ir.Function] = {}
        self.var_symtab: dict[str, ir.AllocaInstr] = {}
        self.global_symtab: dict[str, ir.GlobalVariable] = {}
        self.current_function: Optional[ir.Function] = None
        # Track current loop blocks for break/continue
        self.loop_exit_stack: list[ir.Block] = []
        self.loop_continue_stack: list[ir.Block] = []
        # Runtime declarations for C library
        self.runtime = LLVMRuntimeDeclarations(self.module)

    def visit_module(self, node: IRModule) -> ir.Module:
        """Convert IR module to LLVM module.

        Args:
            node: IR module to convert

        Returns:
            LLVM module with generated functions
        """
        # Declare runtime library functions first
        self.runtime.declare_all()

        # Generate type declarations first (for structs, etc.)
        for type_decl in node.type_declarations:
            type_decl.accept(self)

        # Generate global variables
        for global_var in node.global_variables:
            global_var.accept(self)

        # Generate functions
        for func in node.functions:
            func.accept(self)

        return self.module

    def visit_function(self, node: IRFunction) -> ir.Function:
        """Convert IR function to LLVM function.

        Args:
            node: IR function to convert

        Returns:
            LLVM function with generated body
        """
        # Map IRType → llvmlite type
        ret_type = self._convert_type(node.return_type)
        param_types = [self._convert_type(p.ir_type) for p in node.parameters]

        # Create LLVM function type and function
        func_type = ir.FunctionType(ret_type, param_types)
        func = ir.Function(self.module, func_type, node.name)

        # Store in symbol table
        self.func_symtab[node.name] = func
        self.current_function = func

        # Create entry block
        entry_block = func.append_basic_block(name="entry")
        self.builder = ir.IRBuilder(entry_block)

        # Clear variable symbol table for new function
        self.var_symtab = {}

        # Map parameters to LLVM function arguments
        for i, param in enumerate(node.parameters):
            # Allocate stack space for parameter (enables taking address)
            param_ptr = self.builder.alloca(func.args[i].type, name=param.name)
            self.builder.store(func.args[i], param_ptr)
            self.var_symtab[param.name] = param_ptr

        # Generate function body
        for stmt in node.body:
            stmt.accept(self)

        # Add implicit return if missing
        if not self.builder.block.is_terminated:
            if ret_type == ir.VoidType():
                self.builder.ret_void()
            else:
                # Return zero/null as default
                self.builder.ret(ir.Constant(ret_type, 0))

        return func

    def visit_variable(self, node: IRVariable) -> Union[ir.AllocaInstr, ir.GlobalVariable]:
        """Visit a variable node (used for declarations).

        Args:
            node: IR variable to convert

        Returns:
            LLVM alloca instruction for local variables or GlobalVariable for globals
        """
        var_type = self._convert_type(node.ir_type)

        # If builder is None, this is a global variable
        if self.builder is None:
            # Create global variable with initializer
            if node.initial_value is not None:
                initial_value = node.initial_value.accept(self)
            else:
                # Default initialization to zero
                if var_type == ir.IntType(64):
                    initial_value = ir.Constant(ir.IntType(64), 0)
                elif var_type == ir.DoubleType():
                    initial_value = ir.Constant(ir.DoubleType(), 0.0)
                elif var_type == ir.IntType(1):
                    initial_value = ir.Constant(ir.IntType(1), 0)
                else:
                    raise NotImplementedError(f"Default initialization for type {var_type} not implemented")

            global_var = ir.GlobalVariable(self.module, var_type, name=node.name)
            global_var.initializer = initial_value
            global_var.linkage = "internal"  # Make it module-private
            self.global_symtab[node.name] = global_var
            return global_var
        else:
            # Local variable - use alloca
            var_ptr = self.builder.alloca(var_type, name=node.name)
            self.var_symtab[node.name] = var_ptr
            return var_ptr

    def visit_assignment(self, node: IRAssignment) -> None:
        """Convert IR assignment to LLVM store instruction.

        Args:
            node: IR assignment to convert
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Generate value expression first (we might need its type)
        value = None
        if node.value:
            value = node.value.accept(self)

        # Check if it's a global variable first
        if node.target.name in self.global_symtab:
            var_ptr = self.global_symtab[node.target.name]
        elif node.target.name not in self.var_symtab:
            # Allocate new local variable using IR type
            # For lists, we store pointers in variables, so we alloca space for a pointer
            var_type = self._convert_type(node.target.ir_type)
            var_ptr = self.builder.alloca(var_type, name=node.target.name)
            self.var_symtab[node.target.name] = var_ptr
        else:
            var_ptr = self.var_symtab[node.target.name]

        # Store the value if present
        if value is not None:
            # Check for list dimensionality mismatch (1D vs 2D list literals only)
            value_type_str = str(value.type)
            var_type_str = str(var_ptr.type)

            # Detect actual dimension mismatch:
            # - value is vec_int* (1D) but variable expects vec_vec_int* (2D)
            # - value is vec_vec_int* (2D) but variable expects vec_int* (1D)
            value_is_1d = "vec_int" in value_type_str and "vec_vec_int" not in value_type_str
            value_is_2d = "vec_vec_int" in value_type_str
            var_is_1d = "vec_int" in var_type_str and "vec_vec_int" not in var_type_str
            var_is_2d = "vec_vec_int" in var_type_str

            is_dimension_mismatch = (value_is_1d and var_is_2d) or (value_is_2d and var_is_1d)

            if is_dimension_mismatch:
                # Don't store the mismatched value - it's an empty list with wrong dimension
                # The variable will remain uninitialized, which is fine - it will be initialized
                # on first use (e.g., append)
                pass
            else:
                # Types match - normal store
                self.builder.store(value, var_ptr)
        else:
            # No value provided - initialize pointer types to NULL
            # This handles cases like `result: dict = {}` where we defer the literal
            var_type_str = str(var_ptr.type)
            if "map_" in var_type_str or "vec_" in var_type_str or "set_" in var_type_str:
                # Initialize pointer to NULL (0)
                pointee_type = var_ptr.type.pointee
                null_value = ir.Constant(pointee_type, None)
                self.builder.store(null_value, var_ptr)

    def visit_binary_operation(self, node: IRBinaryOperation) -> ir.Instruction:
        """Convert IR binary operation to LLVM instruction.

        Args:
            node: IR binary operation to convert

        Returns:
            LLVM instruction representing the operation
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # For short-circuit operators (and, or), we need special handling
        # to avoid evaluating the right side when not necessary
        if node.result_type.base_type == IRDataType.BOOL and node.operator in ("and", "or"):
            return self._visit_short_circuit_boolean(node)

        # For all other operators, evaluate both sides immediately
        left = node.left.accept(self)
        right = node.right.accept(self)

        # String operations
        if node.left.result_type.base_type == IRDataType.STRING:
            if node.operator == "+":
                # String concatenation using C library functions
                return self._concat_strings(left, right)

        # Integer operations
        if node.result_type.base_type == IRDataType.INT:
            if node.operator == "+":
                return self.builder.add(left, right, name="add_tmp")
            elif node.operator == "-":
                return self.builder.sub(left, right, name="sub_tmp")
            elif node.operator == "*":
                return self.builder.mul(left, right, name="mul_tmp")
            elif node.operator == "/" or node.operator == "//":
                return self.builder.sdiv(left, right, name="div_tmp")
            elif node.operator == "%":
                # Python modulo uses floored division, C uses truncated division
                # To convert: if remainder and divisor have different signs, add divisor to remainder
                c_rem = self.builder.srem(left, right, name="c_rem")

                # Check if signs differ: (c_rem < 0) != (right < 0)
                zero = ir.Constant(ir.IntType(64), 0)
                rem_neg = self.builder.icmp_signed("<", c_rem, zero, name="rem_neg")
                divisor_neg = self.builder.icmp_signed("<", right, zero, name="divisor_neg")
                signs_differ = self.builder.xor(rem_neg, divisor_neg, name="signs_differ")

                # Check if remainder is non-zero
                rem_nonzero = self.builder.icmp_signed("!=", c_rem, zero, name="rem_nonzero")

                # Adjust if signs differ AND remainder is non-zero
                need_adjust = self.builder.and_(signs_differ, rem_nonzero, name="need_adjust")

                # result = need_adjust ? (c_rem + right) : c_rem
                adjusted = self.builder.add(c_rem, right, name="adjusted")
                result = self.builder.select(need_adjust, adjusted, c_rem, name="mod_tmp")
                return result
            elif node.operator == "<<":
                return self.builder.shl(left, right, name="shl_tmp")
            elif node.operator == ">>":
                return self.builder.ashr(left, right, name="shr_tmp")
            elif node.operator == "&":
                return self.builder.and_(left, right, name="and_tmp")
            elif node.operator == "|":
                return self.builder.or_(left, right, name="or_tmp")
            elif node.operator == "^":
                return self.builder.xor(left, right, name="xor_tmp")

        # Float operations
        elif node.result_type.base_type == IRDataType.FLOAT:
            if node.operator == "+":
                return self.builder.fadd(left, right, name="fadd_tmp")
            elif node.operator == "-":
                return self.builder.fsub(left, right, name="fsub_tmp")
            elif node.operator == "*":
                return self.builder.fmul(left, right, name="fmul_tmp")
            elif node.operator == "/":
                return self.builder.fdiv(left, right, name="fdiv_tmp")

        # Boolean operations (comparisons)
        elif node.result_type.base_type == IRDataType.BOOL:
            # Determine operand types to choose correct comparison instruction
            left_type = node.left.result_type.base_type

            if left_type == IRDataType.INT:
                # Integer comparisons (icmp)
                if node.operator == "<":
                    return self.builder.icmp_signed("<", left, right, name="cmp_tmp")
                elif node.operator == "<=":
                    return self.builder.icmp_signed("<=", left, right, name="cmp_tmp")
                elif node.operator == ">":
                    return self.builder.icmp_signed(">", left, right, name="cmp_tmp")
                elif node.operator == ">=":
                    return self.builder.icmp_signed(">=", left, right, name="cmp_tmp")
                elif node.operator == "==":
                    return self.builder.icmp_signed("==", left, right, name="cmp_tmp")
                elif node.operator == "!=":
                    return self.builder.icmp_signed("!=", left, right, name="cmp_tmp")
            elif left_type == IRDataType.FLOAT:
                # Float comparisons (fcmp)
                if node.operator == "<":
                    return self.builder.fcmp_ordered("<", left, right, name="fcmp_tmp")
                elif node.operator == "<=":
                    return self.builder.fcmp_ordered("<=", left, right, name="fcmp_tmp")
                elif node.operator == ">":
                    return self.builder.fcmp_ordered(">", left, right, name="fcmp_tmp")
                elif node.operator == ">=":
                    return self.builder.fcmp_ordered(">=", left, right, name="fcmp_tmp")
                elif node.operator == "==":
                    return self.builder.fcmp_ordered("==", left, right, name="fcmp_tmp")
                elif node.operator == "!=":
                    return self.builder.fcmp_ordered("!=", left, right, name="fcmp_tmp")
            elif left_type == IRDataType.BOOL:
                # Boolean comparisons (and/or handled separately via _visit_short_circuit_boolean)
                if node.operator == "==":
                    return self.builder.icmp_signed("==", left, right, name="cmp_tmp")
                elif node.operator == "!=":
                    return self.builder.icmp_signed("!=", left, right, name="cmp_tmp")

            # Handle "in" operator for dict membership testing
            # Example: "key" in dict -> map_str_int_contains(dict, key)
            if node.operator == "in":
                # right operand should be the container (dict)
                right_type = node.right.result_type.base_type
                if right_type == IRDataType.DICT:
                    # Determine dict type to select appropriate contains function
                    if (
                        node.right.result_type.element_type
                        and node.right.result_type.element_type.base_type == IRDataType.STRING
                    ):
                        map_contains_func = self.runtime.get_function("map_str_int_contains")
                    else:
                        map_contains_func = self.runtime.get_function("map_int_int_contains")
                    result = self.builder.call(map_contains_func, [right, left], name="contains_result")
                    # Convert i32 result to i1 (bool)
                    zero = ir.Constant(ir.IntType(32), 0)
                    return self.builder.icmp_signed("!=", result, zero, name="contains_bool")

        raise NotImplementedError(
            f"Binary operator '{node.operator}' not implemented for type {node.result_type.base_type}"
        )

    def _visit_short_circuit_boolean(self, node: IRBinaryOperation) -> ir.Instruction:
        """Handle short-circuit evaluation for 'and' and 'or' operators.

        Args:
            node: IR binary operation with 'and' or 'or' operator

        Returns:
            LLVM instruction representing the short-circuit boolean result
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Evaluate left side first
        left = node.left.accept(self)
        left_end_block = self.builder.block  # Block where left evaluation ended

        if node.operator == "and":
            # Short-circuit AND: if left is false, result is false without evaluating right
            eval_right_block = self.current_function.append_basic_block("and.eval_right")
            merge_block = self.current_function.append_basic_block("and.merge")

            # Branch: if left is true, evaluate right; otherwise skip to merge
            self.builder.cbranch(left, eval_right_block, merge_block)

            # Evaluate right side (only if left was true)
            self.builder.position_at_end(eval_right_block)
            right = node.right.accept(self)
            eval_right_end_block = self.builder.block  # May have changed during right evaluation
            self.builder.branch(merge_block)

            # Merge block: use phi to select result
            self.builder.position_at_end(merge_block)
            phi = self.builder.phi(ir.IntType(1), name="and_tmp")
            phi.add_incoming(ir.Constant(ir.IntType(1), 0), left_end_block)  # Left was false
            phi.add_incoming(right, eval_right_end_block)  # Right result
            return phi

        elif node.operator == "or":
            # Short-circuit OR: if left is true, result is true without evaluating right
            eval_right_block = self.current_function.append_basic_block("or.eval_right")
            merge_block = self.current_function.append_basic_block("or.merge")

            # Branch: if left is false, evaluate right; otherwise skip to merge
            self.builder.cbranch(left, merge_block, eval_right_block)

            # Evaluate right side (only if left was false)
            self.builder.position_at_end(eval_right_block)
            right = node.right.accept(self)
            eval_right_end_block = self.builder.block  # May have changed during right evaluation
            self.builder.branch(merge_block)

            # Merge block: use phi to select result
            self.builder.position_at_end(merge_block)
            phi = self.builder.phi(ir.IntType(1), name="or_tmp")
            phi.add_incoming(ir.Constant(ir.IntType(1), 1), left_end_block)  # Left was true
            phi.add_incoming(right, eval_right_end_block)  # Right result
            return phi

        else:
            raise RuntimeError(f"Unexpected operator in short-circuit: {node.operator}")

    def visit_literal(self, node: IRLiteral) -> Union[ir.Constant, ir.CallInstr]:
        """Convert IR literal to LLVM constant or initialization call.

        Args:
            node: IR literal to convert

        Returns:
            LLVM constant value or call instruction for complex types
        """
        llvm_type = self._convert_type(node.result_type)

        if node.result_type.base_type == IRDataType.LIST:
            # List literal - allocate and initialize
            if self.builder is None:
                raise RuntimeError("Builder not initialized - must be inside a function")

            # Determine element type from IR type annotation
            elem_type = None
            if hasattr(node.result_type, "element_type") and node.result_type.element_type:
                elem_type = node.result_type.element_type.base_type
            elif isinstance(node.value, list) and len(node.value) > 0:
                # Fallback: infer from first element
                first_elem = node.value[0]
                if hasattr(first_elem, "result_type"):
                    elem_type = first_elem.result_type.base_type

            # Select appropriate vec_* type based on element type
            is_2d_list = elem_type == IRDataType.LIST

            if elem_type == IRDataType.LIST:
                # 2D list: vec_vec_int
                vec_type = self.runtime.get_vec_vec_int_type()
                vec_init_ptr_func = self.runtime.get_function("vec_vec_int_init_ptr")
                vec_push_func = self.runtime.get_function("vec_vec_int_push")
            elif elem_type == IRDataType.STRING:
                # String list: vec_str
                vec_type = self.runtime.get_vec_str_type()
                vec_init_ptr_func = self.runtime.get_function("vec_str_init_ptr")
                vec_push_func = self.runtime.get_function("vec_str_push")
            else:
                # Default to integer list: vec_int
                vec_type = self.runtime.get_vec_int_type()
                vec_init_ptr_func = self.runtime.get_function("vec_int_init_ptr")
                vec_push_func = self.runtime.get_function("vec_int_push")

            # Allocate space for the vec struct on heap (not stack!)
            # Calculate size of struct using GEP null trick
            i64 = ir.IntType(64)
            i8_ptr = ir.IntType(8).as_pointer()
            null_ptr = ir.Constant(vec_type.as_pointer(), None)
            size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="size_gep")
            struct_size = self.builder.ptrtoint(size_gep, i64, name="struct_size")

            # Get malloc function and allocate memory
            malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
            raw_ptr = self.builder.call(malloc_func, [struct_size], name="list_malloc")

            # Cast i8* to struct pointer
            vec_ptr = self.builder.bitcast(raw_ptr, vec_type.as_pointer(), name="list_tmp")

            # Initialize it by calling vec_init_ptr() which takes a pointer
            self.builder.call(vec_init_ptr_func, [vec_ptr], name="")

            # If list has elements, push them
            if isinstance(node.value, list) and len(node.value) > 0:
                for element_expr in node.value:
                    # Visit the element expression to get its LLVM value
                    element_val = element_expr.accept(self)

                    if is_2d_list:
                        # For 2D lists, element_val is a pointer to vec_int
                        # vec_vec_int_push now takes vec_int by pointer (not by value)
                        self.builder.call(vec_push_func, [vec_ptr, element_val], name="")
                    else:
                        # For 1D lists, element_val is an i64
                        self.builder.call(vec_push_func, [vec_ptr, element_val], name="")

            # Return the pointer
            return vec_ptr

        elif node.result_type.base_type == IRDataType.DICT:
            # Dict literal - allocate and initialize
            if self.builder is None:
                raise RuntimeError("Builder not initialized - must be inside a function")

            # Check element_type to determine key type (default to int)
            if node.result_type.element_type and node.result_type.element_type.base_type == IRDataType.STRING:
                map_type = self.runtime.get_map_str_int_type()
                map_init_ptr_func = self.runtime.get_function("map_str_int_init_ptr")
            else:
                map_type = self.runtime.get_map_int_int_type()
                map_init_ptr_func = self.runtime.get_function("map_int_int_init_ptr")

            # Allocate space for the map struct on heap
            i64 = ir.IntType(64)
            i8_ptr = ir.IntType(8).as_pointer()
            null_ptr = ir.Constant(map_type.as_pointer(), None)
            size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="size_gep")
            struct_size = self.builder.ptrtoint(size_gep, i64, name="struct_size")

            # Get malloc function and allocate memory
            malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
            raw_ptr = self.builder.call(malloc_func, [struct_size], name="dict_malloc")

            # Cast i8* to struct pointer
            map_ptr = self.builder.bitcast(raw_ptr, map_type.as_pointer(), name="dict_tmp")

            # Initialize it by calling map_init_ptr()
            self.builder.call(map_init_ptr_func, [map_ptr], name="")

            # TODO: If dict has initial key-value pairs, set them here
            # For now, we only support empty dict literals: {}

            # Return the pointer
            return map_ptr

        elif node.result_type.base_type == IRDataType.SET:
            # Set literal - allocate and initialize (typically empty set from set())
            if self.builder is None:
                raise RuntimeError("Builder not initialized - must be inside a function")

            # Get set_int type and init function
            set_type = self.runtime.get_set_int_type()
            set_init_ptr_func = self.runtime.get_function("set_int_init_ptr")

            # Allocate space for the set struct on heap
            i64 = ir.IntType(64)
            i8_ptr = ir.IntType(8).as_pointer()
            null_ptr = ir.Constant(set_type.as_pointer(), None)
            size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="size_gep")
            struct_size = self.builder.ptrtoint(size_gep, i64, name="struct_size")

            # Get malloc function and allocate memory
            malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
            raw_ptr = self.builder.call(malloc_func, [struct_size], name="set_malloc")

            # Cast i8* to struct pointer
            set_ptr = self.builder.bitcast(raw_ptr, set_type.as_pointer(), name="set_tmp")

            # Initialize it by calling set_init_ptr()
            self.builder.call(set_init_ptr_func, [set_ptr], name="")

            # Return the pointer
            return set_ptr

        elif node.result_type.base_type == IRDataType.INT:
            return ir.Constant(llvm_type, int(node.value))
        elif node.result_type.base_type == IRDataType.FLOAT:
            return ir.Constant(llvm_type, float(node.value))
        elif node.result_type.base_type == IRDataType.BOOL:
            return ir.Constant(llvm_type, 1 if node.value else 0)
        elif node.result_type.base_type == IRDataType.STRING:
            # String literals are stored as global constants
            # Create a null-terminated string
            str_value = str(node.value)
            str_bytes = (str_value + "\0").encode("utf-8")
            str_const = ir.Constant(ir.ArrayType(ir.IntType(8), len(str_bytes)), bytearray(str_bytes))

            # Create global variable for the string
            str_global = ir.GlobalVariable(self.module, str_const.type, name=f"str_{len(self.module.globals)}")
            str_global.linkage = "internal"
            str_global.global_constant = True
            str_global.initializer = str_const

            # Return pointer to the string (i8*)
            if self.builder is not None:
                return self.builder.gep(str_global, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), 0)])
            else:
                # During global variable initialization, return the global itself
                return str_global
        elif node.result_type.base_type == IRDataType.VOID:
            # VOID literals shouldn't exist - this is likely a bug in IR generation
            # Return null pointer as workaround
            return ir.Constant(ir.IntType(8).as_pointer(), None)

        raise NotImplementedError(f"Literal type {node.result_type.base_type} not implemented (value={node.value})")

    def visit_comprehension(self, node: IRComprehension) -> ir.Value:
        """Convert IR comprehension to LLVM loop with append/insert operations.

        Handles comprehensions like:
        - List: [expr for var in iterable if condition]
        - Dict: {key: value for var in iterable if condition}
        - Set: {expr for var in iterable if condition}

        Generates:
        1. Allocate result container
        2. Generate for loop
        3. Add conditional if present
        4. Append/insert expression to result
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        ast_node = node.ast_node

        # Determine comprehension type
        if isinstance(ast_node, ast.ListComp):
            return self._visit_list_comprehension(node, ast_node)
        elif isinstance(ast_node, ast.DictComp):
            return self._visit_dict_comprehension(node, ast_node)
        elif isinstance(ast_node, ast.SetComp):
            return self._visit_set_comprehension(node, ast_node)
        else:
            raise NotImplementedError(f"Unsupported comprehension type: {type(ast_node).__name__}")

    def _visit_list_comprehension(self, node: IRComprehension, ast_node: ast.ListComp) -> ir.Value:
        """Handle list comprehension: [expr for var in iterable if condition]."""
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized")

        # Allocate result list on heap
        vec_int_type = self.runtime.get_vec_int_type()
        vec_int_init_ptr_func = self.runtime.get_function("vec_int_init_ptr")
        vec_int_push_func = self.runtime.get_function("vec_int_push")

        # Calculate size and malloc the struct
        i64 = ir.IntType(64)
        i8_ptr = ir.IntType(8).as_pointer()
        null_ptr = ir.Constant(vec_int_type.as_pointer(), None)
        size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="size_gep")
        struct_size = self.builder.ptrtoint(size_gep, i64, name="struct_size")
        malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
        raw_ptr = self.builder.call(malloc_func, [struct_size], name="comp_malloc")
        result_ptr = self.builder.bitcast(raw_ptr, vec_int_type.as_pointer(), name="comp_result")

        self.builder.call(vec_int_init_ptr_func, [result_ptr], name="")

        # Process the comprehension (only single generator supported for now)
        if len(ast_node.generators) != 1:
            raise NotImplementedError("Only single generator in comprehensions supported")

        generator = ast_node.generators[0]

        # Determine iteration type: range() or list iteration
        is_range_iter = (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Name)
            and generator.iter.func.id == "range"
        )

        if is_range_iter:
            # Handle range() iteration
            # At this point we know generator.iter is ast.Call due to is_range_iter check
            assert isinstance(generator.iter, ast.Call)  # For mypy
            range_args = generator.iter.args
            if len(range_args) == 1:
                start_val = ir.Constant(ir.IntType(64), 0)
                end_expr = self._convert_ast_expr(range_args[0])
                step_val = ir.Constant(ir.IntType(64), 1)
            elif len(range_args) == 2:
                start_expr = self._convert_ast_expr(range_args[0])
                end_expr = self._convert_ast_expr(range_args[1])
                start_val = start_expr
                step_val = ir.Constant(ir.IntType(64), 1)
            elif len(range_args) == 3:
                start_expr = self._convert_ast_expr(range_args[0])
                end_expr = self._convert_ast_expr(range_args[1])
                step_expr = self._convert_ast_expr(range_args[2])
                start_val = start_expr
                step_val = step_expr
            else:
                raise ValueError("Invalid range() arguments")

            # Create loop variable for range iteration
            loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
            loop_var = self.builder.alloca(ir.IntType(64), name=loop_var_name)
            self.builder.store(start_val, loop_var)

            # Create loop blocks
            loop_cond_block = self.current_function.append_basic_block(name="loop_cond")
            loop_body_block = self.current_function.append_basic_block(name="loop_body")
            loop_end_block = self.current_function.append_basic_block(name="loop_end")

            # Branch to loop condition
            self.builder.branch(loop_cond_block)

            # Loop condition: i < end
            self.builder.position_at_end(loop_cond_block)
            loop_var_val = self.builder.load(loop_var, name=f"{loop_var_name}_val")
            cond = self.builder.icmp_signed("<", loop_var_val, end_expr, name="loop_cond")
            self.builder.cbranch(cond, loop_body_block, loop_end_block)

            # Loop body
            self.builder.position_at_end(loop_body_block)

            # Store loop variable in symbol table
            old_var = self.var_symtab.get(loop_var_name)
            self.var_symtab[loop_var_name] = loop_var

        else:
            # Handle list iteration: for x in some_list
            # Get the list being iterated over
            iter_expr = self._convert_ast_expr(generator.iter)

            # Get list size
            vec_int_size_func = self.runtime.get_function("vec_int_size")
            list_size = self.builder.call(vec_int_size_func, [iter_expr], name="list_size")

            # Create index variable
            idx_var = self.builder.alloca(ir.IntType(64), name="idx")
            self.builder.store(ir.Constant(ir.IntType(64), 0), idx_var)

            # Create element variable for loop target
            loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
            elem_var = self.builder.alloca(ir.IntType(64), name=loop_var_name)

            # Create loop blocks
            loop_cond_block = self.current_function.append_basic_block(name="loop_cond")
            loop_body_block = self.current_function.append_basic_block(name="loop_body")
            loop_end_block = self.current_function.append_basic_block(name="loop_end")

            # Branch to loop condition
            self.builder.branch(loop_cond_block)

            # Loop condition: idx < size
            self.builder.position_at_end(loop_cond_block)
            idx_val = self.builder.load(idx_var, name="idx_val")
            cond = self.builder.icmp_signed("<", idx_val, list_size, name="loop_cond")
            self.builder.cbranch(cond, loop_body_block, loop_end_block)

            # Loop body
            self.builder.position_at_end(loop_body_block)

            # Get element at index: elem = list[idx]
            vec_int_at_func = self.runtime.get_function("vec_int_at")
            elem_val = self.builder.call(vec_int_at_func, [iter_expr, idx_val], name="elem")
            self.builder.store(elem_val, elem_var)

            # Store element variable in symbol table
            old_var = self.var_symtab.get(loop_var_name)
            self.var_symtab[loop_var_name] = elem_var

            # Store current index and list size for increment later
            start_val = None  # Not used for list iteration
            step_val = ir.Constant(ir.IntType(64), 1)
            loop_var = idx_var  # Use idx_var for increment
            loop_var_val = idx_val

        # Handle optional condition (common for both range and list iteration)
        if generator.ifs:
            if_cond_expr = self._convert_ast_expr(generator.ifs[0])
            if_then_block = self.current_function.append_basic_block(name="if_then")
            if_merge_block = self.current_function.append_basic_block(name="if_merge")

            self.builder.cbranch(if_cond_expr, if_then_block, if_merge_block)

            self.builder.position_at_end(if_then_block)
            # Evaluate and append expression
            expr_val = self._convert_ast_expr(ast_node.elt)
            self.builder.call(vec_int_push_func, [result_ptr, expr_val], name="")
            self.builder.branch(if_merge_block)

            self.builder.position_at_end(if_merge_block)
        else:
            # No condition - just append
            expr_val = self._convert_ast_expr(ast_node.elt)
            self.builder.call(vec_int_push_func, [result_ptr, expr_val], name="")

        # Increment loop variable
        incremented = self.builder.add(loop_var_val, step_val, name="inc")
        self.builder.store(incremented, loop_var)
        self.builder.branch(loop_cond_block)

        # Restore symbol table
        loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
        if old_var is not None:
            self.var_symtab[loop_var_name] = old_var
        else:
            self.var_symtab.pop(loop_var_name, None)

        # Continue after loop
        self.builder.position_at_end(loop_end_block)

        return result_ptr

    def _infer_dict_key_type(self, key_expr: ast.expr) -> str:
        """Infer the type of a dict key expression.

        Returns:
            "int" for integer keys, "str" for string keys
        """
        # Integer keys: constants, variables, arithmetic operations
        if isinstance(key_expr, ast.Constant):
            if isinstance(key_expr.value, int):
                return "int"
            elif isinstance(key_expr.value, str):
                return "str"
        elif isinstance(key_expr, (ast.Name, ast.BinOp, ast.UnaryOp)):
            # Assume Name and arithmetic ops are integers
            return "int"
        elif isinstance(key_expr, ast.Call):
            # String operations like str()
            if isinstance(key_expr.func, ast.Name) and key_expr.func.id == "str":
                return "str"
            return "int"

        # Default to string for safety
        return "str"

    def _generate_dict_values_vec(self, dict_ptr: ir.Value) -> ir.Value:
        """Generate a vec_int containing all values from a dict.

        This is used to implement dict.values() which returns an iterable.

        Args:
            dict_ptr: Pointer to the dict (map_int_int*)

        Returns:
            Pointer to vec_int containing all values
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized")

        # Allocate vec_int on heap
        vec_type = self.runtime.get_vec_int_type()
        i64 = ir.IntType(64)
        i8_ptr = ir.IntType(8).as_pointer()

        # Malloc the vec_int struct
        null_ptr = ir.Constant(vec_type.as_pointer(), None)
        size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="vec_size_gep")
        struct_size = self.builder.ptrtoint(size_gep, i64, name="vec_struct_size")
        malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
        raw_ptr = self.builder.call(malloc_func, [struct_size], name="values_vec_malloc")
        vec_ptr = self.builder.bitcast(raw_ptr, vec_type.as_pointer(), name="values_vec")

        # Initialize the vec
        vec_init_func = self.runtime.get_function("vec_int_init_ptr")
        self.builder.call(vec_init_func, [vec_ptr], name="")

        # Get dict runtime functions (assume int-keyed for now)
        capacity_func = self.runtime.get_function("map_int_int_capacity")
        is_occupied_func = self.runtime.get_function("map_int_int_entry_is_occupied")
        entry_value_func = self.runtime.get_function("map_int_int_entry_value")
        vec_push_func = self.runtime.get_function("vec_int_push")

        # Get dict capacity
        capacity = self.builder.call(capacity_func, [dict_ptr], name="dict_capacity")

        # Create loop to iterate through all entries
        loop_var = self.builder.alloca(i64, name="values_iter_idx")
        self.builder.store(ir.Constant(i64, 0), loop_var)

        loop_cond_block = self.current_function.append_basic_block(name="values_loop_cond")
        loop_body_block = self.current_function.append_basic_block(name="values_loop_body")
        entry_check_block = self.current_function.append_basic_block(name="values_entry_check")
        loop_inc_block = self.current_function.append_basic_block(name="values_loop_inc")
        loop_end_block = self.current_function.append_basic_block(name="values_loop_end")

        # Branch to condition
        self.builder.branch(loop_cond_block)

        # Loop condition: idx < capacity
        self.builder.position_at_end(loop_cond_block)
        idx_val = self.builder.load(loop_var, name="idx_val")
        cond = self.builder.icmp_signed("<", idx_val, capacity, name="values_cond")
        self.builder.cbranch(cond, loop_body_block, loop_end_block)

        # Loop body: check if occupied
        self.builder.position_at_end(loop_body_block)
        is_occupied = self.builder.call(is_occupied_func, [dict_ptr, idx_val], name="is_occupied")
        zero_i32 = ir.Constant(ir.IntType(32), 0)
        occupied_cond = self.builder.icmp_signed("!=", is_occupied, zero_i32, name="occupied_cond")
        self.builder.cbranch(occupied_cond, entry_check_block, loop_inc_block)

        # Entry is occupied - extract value and push to vec
        self.builder.position_at_end(entry_check_block)
        entry_value = self.builder.call(entry_value_func, [dict_ptr, idx_val], name="entry_value")
        self.builder.call(vec_push_func, [vec_ptr, entry_value], name="")
        self.builder.branch(loop_inc_block)

        # Increment index
        self.builder.position_at_end(loop_inc_block)
        incremented = self.builder.add(idx_val, ir.Constant(i64, 1), name="idx_inc")
        self.builder.store(incremented, loop_var)
        self.builder.branch(loop_cond_block)

        # After loop, return the vec pointer
        self.builder.position_at_end(loop_end_block)

        return vec_ptr

    def _visit_dict_comprehension_items(
        self,
        ast_node: ast.DictComp,
        generator: ast.comprehension,
        result_ptr: ir.Value,
        map_set_func: ir.Function,
        key_type: str,
    ) -> ir.Value:
        """Handle dict comprehension with .items() iteration.

        Example: {k: v for k, v in source_dict.items() if condition}
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized")

        # Get the source dict being iterated over
        # generator.iter is ast.Call to .items()
        # generator.iter.func is ast.Attribute with .items
        # generator.iter.func.value is the dict variable
        assert isinstance(generator.iter, ast.Call)
        assert isinstance(generator.iter.func, ast.Attribute)
        source_dict_ast = generator.iter.func.value
        source_dict_ptr = self._convert_ast_expr(source_dict_ast)

        # Determine source dict type (map_int_int or map_str_int)
        # For now, assume both source and result use same type based on key_type
        if key_type == "int":
            capacity_func = self.runtime.get_function("map_int_int_capacity")
            is_occupied_func = self.runtime.get_function("map_int_int_entry_is_occupied")
            entry_key_func = self.runtime.get_function("map_int_int_entry_key")
            entry_value_func = self.runtime.get_function("map_int_int_entry_value")
        else:
            raise NotImplementedError("String-keyed dict .items() iteration not yet implemented")

        # Get capacity of source dict
        capacity = self.builder.call(capacity_func, [source_dict_ptr], name="source_capacity")

        # Create loop variable for iterating through capacity
        i64 = ir.IntType(64)
        loop_var = self.builder.alloca(i64, name="items_iter_idx")
        self.builder.store(ir.Constant(i64, 0), loop_var)

        # Create loop blocks
        loop_cond_block = self.current_function.append_basic_block(name="items_loop_cond")
        loop_body_block = self.current_function.append_basic_block(name="items_loop_body")
        entry_check_block = self.current_function.append_basic_block(name="items_entry_check")
        loop_increment_block = self.current_function.append_basic_block(name="items_loop_inc")
        loop_end_block = self.current_function.append_basic_block(name="items_loop_end")

        # Branch to loop condition
        self.builder.branch(loop_cond_block)

        # Loop condition: idx < capacity
        self.builder.position_at_end(loop_cond_block)
        idx_val = self.builder.load(loop_var, name="idx_val")
        cond = self.builder.icmp_signed("<", idx_val, capacity, name="items_cond")
        self.builder.cbranch(cond, loop_body_block, loop_end_block)

        # Loop body: check if entry is occupied
        self.builder.position_at_end(loop_body_block)
        is_occupied = self.builder.call(is_occupied_func, [source_dict_ptr, idx_val], name="is_occupied")
        zero_i32 = ir.Constant(ir.IntType(32), 0)
        occupied_cond = self.builder.icmp_signed("!=", is_occupied, zero_i32, name="occupied_cond")
        self.builder.cbranch(occupied_cond, entry_check_block, loop_increment_block)

        # Entry is occupied - extract key and value
        self.builder.position_at_end(entry_check_block)
        entry_key = self.builder.call(entry_key_func, [source_dict_ptr, idx_val], name="entry_key")
        entry_value = self.builder.call(entry_value_func, [source_dict_ptr, idx_val], name="entry_value")

        # Handle tuple unpacking for (k, v)
        # generator.target should be ast.Tuple with two elements
        if isinstance(generator.target, ast.Tuple) and len(generator.target.elts) == 2:
            key_var_name = generator.target.elts[0].id if isinstance(generator.target.elts[0], ast.Name) else "k"
            val_var_name = generator.target.elts[1].id if isinstance(generator.target.elts[1], ast.Name) else "v"

            # Allocate and store key and value in symbol table
            key_alloca = self.builder.alloca(i64, name=key_var_name)
            val_alloca = self.builder.alloca(i64, name=val_var_name)
            self.builder.store(entry_key, key_alloca)
            self.builder.store(entry_value, val_alloca)

            old_key_var = self.var_symtab.get(key_var_name)
            old_val_var = self.var_symtab.get(val_var_name)
            self.var_symtab[key_var_name] = key_alloca
            self.var_symtab[val_var_name] = val_alloca
        else:
            raise NotImplementedError("Dict .items() requires tuple unpacking: for k, v in ...")

        # Handle optional filter condition
        if generator.ifs:
            filter_cond_expr = self._convert_ast_expr(generator.ifs[0])
            filter_then_block = self.current_function.append_basic_block(name="items_filter_then")

            self.builder.cbranch(filter_cond_expr, filter_then_block, loop_increment_block)
            self.builder.position_at_end(filter_then_block)

            # Evaluate key and value expressions, then insert
            result_key = self._convert_ast_expr(ast_node.key)
            result_value = self._convert_ast_expr(ast_node.value)
            self.builder.call(map_set_func, [result_ptr, result_key, result_value], name="")
            self.builder.branch(loop_increment_block)
        else:
            # No filter - just insert
            result_key = self._convert_ast_expr(ast_node.key)
            result_value = self._convert_ast_expr(ast_node.value)
            self.builder.call(map_set_func, [result_ptr, result_key, result_value], name="")
            self.builder.branch(loop_increment_block)

        # Increment loop variable
        self.builder.position_at_end(loop_increment_block)
        # Restore symbol table for next iteration
        if isinstance(generator.target, ast.Tuple):
            if old_key_var is not None:
                self.var_symtab[key_var_name] = old_key_var
            else:
                self.var_symtab.pop(key_var_name, None)

            if old_val_var is not None:
                self.var_symtab[val_var_name] = old_val_var
            else:
                self.var_symtab.pop(val_var_name, None)

        # Actually increment and loop back
        incremented = self.builder.add(idx_val, ir.Constant(i64, 1), name="idx_inc")
        self.builder.store(incremented, loop_var)
        self.builder.branch(loop_cond_block)

        # Continue after loop
        self.builder.position_at_end(loop_end_block)

        return result_ptr

    def _visit_dict_comprehension(self, node: IRComprehension, ast_node: ast.DictComp) -> ir.Value:
        """Handle dict comprehension: {key_expr: value_expr for var in iterable if condition}."""
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized")

        # Detect key type to select appropriate map runtime
        key_type = self._infer_dict_key_type(ast_node.key)

        if key_type == "int":
            # Use map_int_int for integer keys
            map_type = self.runtime.get_map_int_int_type()
            map_init_ptr_func = self.runtime.get_function("map_int_int_init_ptr")
            map_set_func = self.runtime.get_function("map_int_int_set")
        else:
            # Use map_str_int for string keys
            map_type = self.runtime.get_map_str_int_type()
            map_init_ptr_func = self.runtime.get_function("map_str_int_init_ptr")
            map_set_func = self.runtime.get_function("map_str_int_set")

        # Calculate size and malloc the struct
        i64 = ir.IntType(64)
        i8_ptr = ir.IntType(8).as_pointer()
        null_ptr = ir.Constant(map_type.as_pointer(), None)
        size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="size_gep")
        struct_size = self.builder.ptrtoint(size_gep, i64, name="struct_size")
        malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
        raw_ptr = self.builder.call(malloc_func, [struct_size], name="dict_comp_malloc")
        result_ptr = self.builder.bitcast(raw_ptr, map_type.as_pointer(), name="dict_comp_result")

        self.builder.call(map_init_ptr_func, [result_ptr], name="")

        # Process the comprehension (only single generator supported for now)
        if len(ast_node.generators) != 1:
            raise NotImplementedError("Only single generator in dict comprehensions supported")

        generator = ast_node.generators[0]

        # Determine iteration type: range() or .items()
        is_range_iter = (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Name)
            and generator.iter.func.id == "range"
        )

        is_items_iter = (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Attribute)
            and generator.iter.func.attr == "items"
        )

        if is_items_iter:
            # Handle .items() iteration: {k: v for k, v in dict.items()}
            return self._visit_dict_comprehension_items(ast_node, generator, result_ptr, map_set_func, key_type)
        elif not is_range_iter:
            raise NotImplementedError("Dict comprehensions only support range() and .items() iteration")

        # Handle range() iteration
        assert isinstance(generator.iter, ast.Call)
        range_args = generator.iter.args
        if len(range_args) == 1:
            start_val = ir.Constant(ir.IntType(64), 0)
            end_expr = self._convert_ast_expr(range_args[0])
            step_val = ir.Constant(ir.IntType(64), 1)
        elif len(range_args) == 2:
            start_expr = self._convert_ast_expr(range_args[0])
            end_expr = self._convert_ast_expr(range_args[1])
            start_val = start_expr
            step_val = ir.Constant(ir.IntType(64), 1)
        elif len(range_args) == 3:
            start_expr = self._convert_ast_expr(range_args[0])
            end_expr = self._convert_ast_expr(range_args[1])
            step_expr = self._convert_ast_expr(range_args[2])
            start_val = start_expr
            step_val = step_expr
        else:
            raise ValueError("Invalid range() arguments")

        # Create loop variable
        loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
        loop_var = self.builder.alloca(ir.IntType(64), name=loop_var_name)
        self.builder.store(start_val, loop_var)

        # Create loop blocks
        loop_cond_block = self.current_function.append_basic_block(name="dict_loop_cond")
        loop_body_block = self.current_function.append_basic_block(name="dict_loop_body")
        loop_end_block = self.current_function.append_basic_block(name="dict_loop_end")

        # Branch to loop condition
        self.builder.branch(loop_cond_block)

        # Loop condition: i < end
        self.builder.position_at_end(loop_cond_block)
        loop_var_val = self.builder.load(loop_var, name=f"{loop_var_name}_val")
        cond = self.builder.icmp_signed("<", loop_var_val, end_expr, name="dict_loop_cond")
        self.builder.cbranch(cond, loop_body_block, loop_end_block)

        # Loop body
        self.builder.position_at_end(loop_body_block)

        # Store loop variable in symbol table
        old_var = self.var_symtab.get(loop_var_name)
        self.var_symtab[loop_var_name] = loop_var

        # Handle optional condition
        if generator.ifs:
            if_cond_expr = self._convert_ast_expr(generator.ifs[0])
            if_then_block = self.current_function.append_basic_block(name="dict_if_then")
            if_merge_block = self.current_function.append_basic_block(name="dict_if_merge")

            self.builder.cbranch(if_cond_expr, if_then_block, if_merge_block)

            self.builder.position_at_end(if_then_block)
            # Evaluate key and value, then insert
            key_val = self._convert_ast_expr(ast_node.key)
            value_val = self._convert_ast_expr(ast_node.value)
            self.builder.call(map_set_func, [result_ptr, key_val, value_val], name="")
            self.builder.branch(if_merge_block)

            self.builder.position_at_end(if_merge_block)
        else:
            # No condition - just insert
            key_val = self._convert_ast_expr(ast_node.key)
            value_val = self._convert_ast_expr(ast_node.value)
            self.builder.call(map_set_func, [result_ptr, key_val, value_val], name="")

        # Increment loop variable
        incremented = self.builder.add(loop_var_val, step_val, name="dict_inc")
        self.builder.store(incremented, loop_var)
        self.builder.branch(loop_cond_block)

        # Restore symbol table
        if old_var is not None:
            self.var_symtab[loop_var_name] = old_var
        else:
            self.var_symtab.pop(loop_var_name, None)

        # Continue after loop
        self.builder.position_at_end(loop_end_block)

        return result_ptr

    def _visit_set_comprehension(self, node: IRComprehension, ast_node: ast.SetComp) -> ir.Value:
        """Handle set comprehension: {expr for var in iterable if condition}."""
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized")

        # Allocate result set on heap (like list comprehensions)
        set_int_type = self.runtime.get_set_int_type()
        set_int_init_ptr_func = self.runtime.get_function("set_int_init_ptr")
        set_int_insert_func = self.runtime.get_function("set_int_insert")

        # Calculate size and malloc the struct on heap
        i64 = ir.IntType(64)
        i8_ptr = ir.IntType(8).as_pointer()
        null_ptr = ir.Constant(set_int_type.as_pointer(), None)
        size_gep = self.builder.gep(null_ptr, [ir.Constant(ir.IntType(32), 1)], name="set_size_gep")
        struct_size = self.builder.ptrtoint(size_gep, i64, name="set_struct_size")
        malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
        raw_ptr = self.builder.call(malloc_func, [struct_size], name="set_malloc")
        result_set = self.builder.bitcast(raw_ptr, set_int_type.as_pointer(), name="comp_set")

        # Initialize the set via pointer (avoids ABI issues with large struct returns)
        self.builder.call(set_int_init_ptr_func, [result_set], name="")

        # Process the comprehension (only single generator supported for now)
        if len(ast_node.generators) != 1:
            raise NotImplementedError("Only single generator in set comprehensions supported")

        generator = ast_node.generators[0]

        # Determine iteration type: range() or set iteration
        is_range_iter = (
            isinstance(generator.iter, ast.Call)
            and isinstance(generator.iter.func, ast.Name)
            and generator.iter.func.id == "range"
        )

        if is_range_iter:
            # Handle range() iteration
            assert isinstance(generator.iter, ast.Call)  # For mypy
            range_args = generator.iter.args
            if len(range_args) == 1:
                start_val = ir.Constant(ir.IntType(64), 0)
                end_expr = self._convert_ast_expr(range_args[0])
                step_val = ir.Constant(ir.IntType(64), 1)
            elif len(range_args) == 2:
                start_expr = self._convert_ast_expr(range_args[0])
                end_expr = self._convert_ast_expr(range_args[1])
                start_val = start_expr
                step_val = ir.Constant(ir.IntType(64), 1)
            elif len(range_args) == 3:
                start_expr = self._convert_ast_expr(range_args[0])
                end_expr = self._convert_ast_expr(range_args[1])
                step_expr = self._convert_ast_expr(range_args[2])
                start_val = start_expr
                step_val = step_expr
            else:
                raise ValueError("Invalid range() arguments")

            # Create loop variable for range iteration
            loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
            loop_var = self.builder.alloca(ir.IntType(64), name=loop_var_name)
            self.builder.store(start_val, loop_var)

            # Create loop blocks
            loop_cond_block = self.current_function.append_basic_block(name="set_loop_cond")
            loop_body_block = self.current_function.append_basic_block(name="set_loop_body")
            loop_end_block = self.current_function.append_basic_block(name="set_loop_end")

            # Branch to loop condition
            self.builder.branch(loop_cond_block)

            # Loop condition: i < end
            self.builder.position_at_end(loop_cond_block)
            loop_var_val = self.builder.load(loop_var, name=f"{loop_var_name}_val")
            cond = self.builder.icmp_signed("<", loop_var_val, end_expr, name="set_loop_cond")
            self.builder.cbranch(cond, loop_body_block, loop_end_block)

            # Loop body
            self.builder.position_at_end(loop_body_block)

            # Store loop variable in symbol table
            old_var = self.var_symtab.get(loop_var_name)
            self.var_symtab[loop_var_name] = loop_var

        else:
            # Handle set/list iteration: for x in some_set
            iter_expr = self._convert_ast_expr(generator.iter)

            # Detect if we're iterating over a set or list based on LLVM type
            is_set_iter = False
            if hasattr(iter_expr, "type") and isinstance(iter_expr.type, ir.PointerType):
                pointee_type_str = str(iter_expr.type.pointee)
                is_set_iter = "set_int" in pointee_type_str

            # Get size and element access functions based on type
            if is_set_iter:
                size_func = self.runtime.get_function("set_int_size")
                get_nth_func = self.runtime.get_function("set_int_get_nth_element")
            else:
                size_func = self.runtime.get_function("vec_int_size")
                get_nth_func = self.runtime.get_function("vec_int_at")

            iter_size = self.builder.call(size_func, [iter_expr], name="iter_size")

            # Create index variable
            idx_var = self.builder.alloca(ir.IntType(64), name="idx")
            self.builder.store(ir.Constant(ir.IntType(64), 0), idx_var)

            # Create element variable for loop target
            loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
            elem_var = self.builder.alloca(ir.IntType(64), name=loop_var_name)

            # Create loop blocks
            loop_cond_block = self.current_function.append_basic_block(name="set_loop_cond")
            loop_body_block = self.current_function.append_basic_block(name="set_loop_body")
            loop_end_block = self.current_function.append_basic_block(name="set_loop_end")

            # Branch to loop condition
            self.builder.branch(loop_cond_block)

            # Loop condition: idx < size
            self.builder.position_at_end(loop_cond_block)
            idx_val = self.builder.load(idx_var, name="idx_val")
            cond = self.builder.icmp_signed("<", idx_val, iter_size, name="set_loop_cond")
            self.builder.cbranch(cond, loop_body_block, loop_end_block)

            # Loop body
            self.builder.position_at_end(loop_body_block)

            # Get element at index: elem = iter[idx] or set_get_nth_element(iter, idx)
            elem_val = self.builder.call(get_nth_func, [iter_expr, idx_val], name="elem")
            self.builder.store(elem_val, elem_var)

            # Store element variable in symbol table
            old_var = self.var_symtab.get(loop_var_name)
            self.var_symtab[loop_var_name] = elem_var

            # Store for increment
            step_val = ir.Constant(ir.IntType(64), 1)
            loop_var = idx_var
            loop_var_val = idx_val

        # Handle optional condition (common for both range and list iteration)
        if generator.ifs:
            if_cond_expr = self._convert_ast_expr(generator.ifs[0])
            if_then_block = self.current_function.append_basic_block(name="set_if_then")
            if_merge_block = self.current_function.append_basic_block(name="set_if_merge")

            self.builder.cbranch(if_cond_expr, if_then_block, if_merge_block)

            self.builder.position_at_end(if_then_block)
            # Evaluate and insert expression
            expr_val = self._convert_ast_expr(ast_node.elt)
            self.builder.call(set_int_insert_func, [result_set, expr_val], name="")
            self.builder.branch(if_merge_block)

            self.builder.position_at_end(if_merge_block)
        else:
            # No condition - just insert
            expr_val = self._convert_ast_expr(ast_node.elt)
            self.builder.call(set_int_insert_func, [result_set, expr_val], name="")

        # Increment loop variable
        incremented = self.builder.add(loop_var_val, step_val, name="inc")
        self.builder.store(incremented, loop_var)
        self.builder.branch(loop_cond_block)

        # Restore symbol table
        loop_var_name = generator.target.id if isinstance(generator.target, ast.Name) else "loop_var"
        if old_var is not None:
            self.var_symtab[loop_var_name] = old_var
        else:
            self.var_symtab.pop(loop_var_name, None)

        # Continue after loop
        self.builder.position_at_end(loop_end_block)

        return result_set

    def _convert_ast_expr(self, ast_expr: ast.expr) -> ir.Value:
        """Helper to convert AST expression to LLVM value."""
        if self.builder is None:
            raise RuntimeError("Builder not initialized")

        if isinstance(ast_expr, ast.Constant):
            # Handle constant values - must be int for our use case
            if not isinstance(ast_expr.value, int):
                raise ValueError(f"Expected int constant, got {type(ast_expr.value)}")
            return ir.Constant(ir.IntType(64), ast_expr.value)
        elif isinstance(ast_expr, ast.Name):
            var_ptr = self.var_symtab[ast_expr.id]
            return self.builder.load(var_ptr, name=ast_expr.id)
        elif isinstance(ast_expr, ast.BinOp):
            left = self._convert_ast_expr(ast_expr.left)
            right = self._convert_ast_expr(ast_expr.right)
            if isinstance(ast_expr.op, ast.Add):
                return self.builder.add(left, right, name="add_tmp")
            elif isinstance(ast_expr.op, ast.Sub):
                return self.builder.sub(left, right, name="sub_tmp")
            elif isinstance(ast_expr.op, ast.Mult):
                return self.builder.mul(left, right, name="mul_tmp")
            elif isinstance(ast_expr.op, ast.Mod):
                return self.builder.srem(left, right, name="mod_tmp")
            else:
                raise NotImplementedError(f"Binary op {type(ast_expr.op).__name__} not implemented")
        elif isinstance(ast_expr, ast.Compare):
            left = self._convert_ast_expr(ast_expr.left)
            right = self._convert_ast_expr(ast_expr.comparators[0])
            op = ast_expr.ops[0]
            if isinstance(op, ast.Lt):
                return self.builder.icmp_signed("<", left, right, name="cmp_tmp")
            elif isinstance(op, ast.Gt):
                return self.builder.icmp_signed(">", left, right, name="cmp_tmp")
            elif isinstance(op, ast.Eq):
                return self.builder.icmp_signed("==", left, right, name="cmp_tmp")
            elif isinstance(op, ast.LtE):
                return self.builder.icmp_signed("<=", left, right, name="cmp_tmp")
            elif isinstance(op, ast.GtE):
                return self.builder.icmp_signed(">=", left, right, name="cmp_tmp")
            else:
                raise NotImplementedError(f"Compare op {type(op).__name__} not implemented")
        else:
            raise NotImplementedError(f"AST expression {type(ast_expr).__name__} not implemented in comprehensions")

    def visit_variable_reference(self, node: IRVariableReference) -> ir.LoadInstr:
        """Convert IR variable reference to LLVM load instruction.

        Args:
            node: IR variable reference to convert

        Returns:
            LLVM load instruction
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Check global variables first, then local
        var_ptr = self.global_symtab.get(node.variable.name)
        if var_ptr is None:
            var_ptr = self.var_symtab.get(node.variable.name)
        if var_ptr is None:
            raise RuntimeError(f"Variable '{node.variable.name}' not found in symbol table")

        return self.builder.load(var_ptr, name=node.variable.name)

    def _get_or_create_c_function(
        self, name: str, ret_type: ir.Type, arg_types: list[ir.Type], var_arg: bool = False
    ) -> ir.Function:
        """Get or create a C library function declaration.

        Args:
            name: Name of the C function
            ret_type: Return type
            arg_types: List of argument types
            var_arg: Whether function has variable arguments

        Returns:
            LLVM function declaration
        """
        if name in self.func_symtab:
            return self.func_symtab[name]

        func_ty = ir.FunctionType(ret_type, arg_types, var_arg=var_arg)
        func = ir.Function(self.module, func_ty, name=name)
        self.func_symtab[name] = func
        return func

    def _create_string_constant(self, str_value: str) -> ir.Value:
        """Create a string constant.

        Args:
            str_value: String value to create

        Returns:
            Pointer to the string constant (i8*)
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized")

        str_bytes = (str_value + "\0").encode("utf-8")
        str_const = ir.Constant(ir.ArrayType(ir.IntType(8), len(str_bytes)), bytearray(str_bytes))

        # Create global variable for the string
        str_global = ir.GlobalVariable(self.module, str_const.type, name=f"str_{len(self.module.globals)}")
        str_global.linkage = "internal"
        str_global.global_constant = True
        str_global.initializer = str_const

        # Return pointer to the string (i8*)
        return self.builder.gep(str_global, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), 0)])

    def _concat_strings(self, left: ir.Value, right: ir.Value) -> ir.Value:
        """Concatenate two strings using C library functions.

        Args:
            left: First string (i8*)
            right: Second string (i8*)

        Returns:
            Concatenated string (i8*)
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized")

        # Declare C library functions if not already declared
        i8_ptr = ir.IntType(8).as_pointer()
        i64 = ir.IntType(64)

        strlen_func = self._get_or_create_c_function("strlen", i64, [i8_ptr])
        malloc_func = self._get_or_create_c_function("malloc", i8_ptr, [i64])
        strcpy_func = self._get_or_create_c_function("strcpy", i8_ptr, [i8_ptr, i8_ptr])
        strcat_func = self._get_or_create_c_function("strcat", i8_ptr, [i8_ptr, i8_ptr])

        # Get lengths of both strings
        left_len = self.builder.call(strlen_func, [left], name="left_len")
        right_len = self.builder.call(strlen_func, [right], name="right_len")

        # Calculate total length (left_len + right_len + 1 for null terminator)
        total_len = self.builder.add(left_len, right_len, name="total_len")
        total_len_plus_null = self.builder.add(total_len, ir.Constant(i64, 1), name="total_len_plus_null")

        # Allocate memory for result
        result_ptr = self.builder.call(malloc_func, [total_len_plus_null], name="result_ptr")

        # Copy first string
        self.builder.call(strcpy_func, [result_ptr, left], name="strcpy_tmp")

        # Concatenate second string
        self.builder.call(strcat_func, [result_ptr, right], name="strcat_tmp")

        return result_ptr

    def _get_or_create_builtin(self, name: str, arg_types: list[ir.Type]) -> ir.Function:
        """Get or create a builtin function declaration.

        Args:
            name: Name of the builtin function
            arg_types: List of argument types

        Returns:
            LLVM function declaration for the builtin
        """
        # Check if already declared
        if name in self.func_symtab:
            return self.func_symtab[name]

        # Create builtin function declarations
        if name == "print":
            # print() uses printf internally
            # For simplicity, we'll handle integer printing first
            # Signature: int printf(i8*, ...)
            printf_ty = ir.FunctionType(ir.IntType(32), [ir.IntType(8).as_pointer()], var_arg=True)
            printf_func = ir.Function(self.module, printf_ty, name="printf")
            self.func_symtab["printf"] = printf_func
            return printf_func
        else:
            raise NotImplementedError(f"Builtin function '{name}' not implemented")

    def visit_function_call(self, node: IRFunctionCall) -> ir.CallInstr:
        """Convert IR function call to LLVM call instruction.

        Args:
            node: IR function call to convert

        Returns:
            LLVM call instruction
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Handle method calls (from IR builder)
        if node.function_name == "__method_append__":
            # list.append(value) -> vec_int_push, vec_str_push, or vec_vec_int_push
            if len(node.arguments) != 2:
                raise RuntimeError("append() requires exactly 2 arguments (list and value)")

            list_ptr = node.arguments[0].accept(self)  # Already a pointer
            value = node.arguments[1].accept(self)

            # Determine list type based on LLVM types
            from llvmlite import ir as llvm_ir

            list_ptr_type = list_ptr.type

            # Check the pointee type to determine which vec_* type we have
            if isinstance(list_ptr_type, llvm_ir.PointerType):
                pointee_type_str = str(list_ptr_type.pointee)

                if "vec_vec_int" in pointee_type_str:
                    # 2D list: vec_vec_int_push(list_ptr, vec_int_ptr)
                    vec_push_func = self.runtime.get_function("vec_vec_int_push")
                    self.builder.call(vec_push_func, [list_ptr, value], name="")
                elif "vec_str" in pointee_type_str:
                    # String list: vec_str_push(list_ptr, char* value)
                    vec_push_func = self.runtime.get_function("vec_str_push")
                    self.builder.call(vec_push_func, [list_ptr, value], name="")
                else:
                    # Integer list (default): vec_int_push(list_ptr, int_value)
                    vec_push_func = self.runtime.get_function("vec_int_push")
                    self.builder.call(vec_push_func, [list_ptr, value], name="")
            else:
                # Fallback to vec_int
                vec_push_func = self.runtime.get_function("vec_int_push")
                self.builder.call(vec_push_func, [list_ptr, value], name="")

            # Return the pointer (unchanged, since append mutates in place)
            return list_ptr

        elif node.function_name == "__getitem__":
            # list[index] or dict[key] -> vec_*_at or map_*_get
            if len(node.arguments) != 2:
                raise RuntimeError("__getitem__() requires exactly 2 arguments (container and index/key)")

            container_ptr = node.arguments[0].accept(self)  # Already a pointer
            key_or_index = node.arguments[1].accept(self)

            # Determine container type based on LLVM type
            from llvmlite import ir as llvm_ir

            container_type = container_ptr.type

            # Check the pointee type to determine which container we have
            if isinstance(container_type, llvm_ir.PointerType):
                pointee_type_str = str(container_type.pointee)

                if "map_str_int" in pointee_type_str:
                    # Dict: map_str_int_get(dict_ptr, key) returns i64
                    map_get_func = self.runtime.get_function("map_str_int_get")
                    return self.builder.call(map_get_func, [container_ptr, key_or_index], name="dict_get")
                elif "map_int_int" in pointee_type_str:
                    # Dict: map_int_int_get(dict_ptr, key) returns i64
                    map_get_func = self.runtime.get_function("map_int_int_get")
                    return self.builder.call(map_get_func, [container_ptr, key_or_index], name="dict_get")
                elif "vec_vec_int" in pointee_type_str:
                    # 2D list: vec_vec_int_at(list_ptr, index) returns vec_int*
                    vec_at_func = self.runtime.get_function("vec_vec_int_at")
                    return self.builder.call(vec_at_func, [container_ptr, key_or_index], name="list_at")
                elif "vec_str" in pointee_type_str:
                    # String list: vec_str_at(list_ptr, index) returns char*
                    vec_at_func = self.runtime.get_function("vec_str_at")
                    return self.builder.call(vec_at_func, [container_ptr, key_or_index], name="list_at")
                else:
                    # Integer list (default): vec_int_at(list_ptr, index) returns i64
                    vec_at_func = self.runtime.get_function("vec_int_at")
                    return self.builder.call(vec_at_func, [container_ptr, key_or_index], name="list_at")
            else:
                # Fallback to vec_int
                vec_at_func = self.runtime.get_function("vec_int_at")
                return self.builder.call(vec_at_func, [container_ptr, key_or_index], name="list_at")

        elif node.function_name == "__set_get_nth__":
            # set_get_nth_element(set, index) -> element
            if len(node.arguments) != 2:
                raise RuntimeError("__set_get_nth__() requires exactly 2 arguments (set and index)")

            set_ptr = node.arguments[0].accept(self)  # Already a pointer
            index = node.arguments[1].accept(self)

            # Call set_int_get_nth_element(set_ptr, index) returns i64
            get_nth_func = self.runtime.get_function("set_int_get_nth_element")
            return self.builder.call(get_nth_func, [set_ptr, index], name="set_get_nth")

        elif node.function_name == "__setitem__":
            # list[index] = value or dict[key] = value -> vec_*_set or map_*_set
            if len(node.arguments) != 3:
                raise RuntimeError("__setitem__() requires exactly 3 arguments (container, index/key, value)")

            container_ptr = node.arguments[0].accept(self)  # Already a pointer
            key_or_index = node.arguments[1].accept(self)
            value = node.arguments[2].accept(self)

            # Determine container type based on LLVM type
            from llvmlite import ir as llvm_ir

            container_type = container_ptr.type

            # Check the pointee type to determine which container we have
            if isinstance(container_type, llvm_ir.PointerType):
                pointee_type_str = str(container_type.pointee)

                if "map_str_int" in pointee_type_str:
                    # Dict: map_str_int_set(dict_ptr, key, value)
                    map_set_func = self.runtime.get_function("map_str_int_set")
                    return self.builder.call(map_set_func, [container_ptr, key_or_index, value], name="")
                elif "map_int_int" in pointee_type_str:
                    # Dict: map_int_int_set(dict_ptr, key, value)
                    map_set_func = self.runtime.get_function("map_int_int_set")
                    return self.builder.call(map_set_func, [container_ptr, key_or_index, value], name="")
                elif "vec_str" in pointee_type_str:
                    # String list: vec_str_set(list_ptr, index, char* value)
                    vec_set_func = self.runtime.get_function("vec_str_set")
                    return self.builder.call(vec_set_func, [container_ptr, key_or_index, value], name="")
                else:
                    # Integer list (default): vec_int_set(list_ptr, index, i64 value)
                    vec_set_func = self.runtime.get_function("vec_int_set")
                    return self.builder.call(vec_set_func, [container_ptr, key_or_index, value], name="")
            else:
                # Fallback to vec_int
                vec_set_func = self.runtime.get_function("vec_int_set")
                return self.builder.call(vec_set_func, [container_ptr, key_or_index, value], name="")

        elif node.function_name == "__contains__":
            # key in dict -> map_str_int_contains(dict_ptr, key)
            if len(node.arguments) != 2:
                raise RuntimeError("__contains__() requires exactly 2 arguments (container and key)")

            container_ptr = node.arguments[0].accept(self)  # Container pointer
            key = node.arguments[1].accept(self)  # Key to check

            # Determine container type
            from llvmlite import ir as llvm_ir

            container_type = container_ptr.type

            if isinstance(container_type, llvm_ir.PointerType):
                pointee_type_str = str(container_type.pointee)

                if "map_str_int" in pointee_type_str:
                    # Dict: map_str_int_contains(dict_ptr, key) returns i32
                    map_contains_func = self.runtime.get_function("map_str_int_contains")
                    result = self.builder.call(map_contains_func, [container_ptr, key], name="contains_result")
                    # Convert i32 result to i1 (bool) by comparing with 0
                    return self.builder.icmp_signed("!=", result, ir.Constant(ir.IntType(32), 0), name="contains_bool")
                else:
                    # TODO: Add list contains support if needed
                    raise NotImplementedError(f"__contains__ not implemented for type {pointee_type_str}")
            else:
                raise NotImplementedError("__contains__ requires a pointer type")

        elif node.function_name == "__method_split__":
            # str.split(delimiter) -> multigen_str_split(str, delimiter)
            # Returns multigen_string_array_t* which we bitcast to vec_str*
            if len(node.arguments) < 1 or len(node.arguments) > 2:
                raise RuntimeError("split() requires 1 or 2 arguments (string and optional delimiter)")

            str_ptr = node.arguments[0].accept(self)  # char* string

            # Get delimiter (default to empty string for whitespace splitting)
            if len(node.arguments) == 2:
                delim_ptr = node.arguments[1].accept(self)
            else:
                # Empty string means split on whitespace
                empty_str = self._create_string_constant("")
                delim_ptr = empty_str

            # Call multigen_str_split (returns multigen_string_array_t*)
            split_func = self.runtime.get_function("multigen_str_split")
            string_array_result = self.builder.call(split_func, [str_ptr, delim_ptr], name="split_result")

            # Bitcast multigen_string_array_t* to vec_str* (same layout: char**, size_t, size_t)
            vec_str_ptr_type = self.runtime.get_vec_str_type().as_pointer()
            vec_str_result = self.builder.bitcast(string_array_result, vec_str_ptr_type, name="split_as_vec_str")
            return vec_str_result

        elif node.function_name == "__method_lower__":
            # str.lower() -> multigen_str_lower(str)
            if len(node.arguments) != 1:
                raise RuntimeError("lower() requires exactly 1 argument (string)")

            str_ptr = node.arguments[0].accept(self)
            lower_func = self.runtime.get_function("multigen_str_lower")
            return self.builder.call(lower_func, [str_ptr], name="lower_result")

        elif node.function_name == "__method_strip__":
            # str.strip() -> multigen_str_strip(str)
            if len(node.arguments) != 1:
                raise RuntimeError("strip() requires exactly 1 argument (string)")

            str_ptr = node.arguments[0].accept(self)
            strip_func = self.runtime.get_function("multigen_str_strip")
            return self.builder.call(strip_func, [str_ptr], name="strip_result")

        elif node.function_name == "__method_upper__":
            # str.upper() -> multigen_str_upper(str)
            if len(node.arguments) != 1:
                raise RuntimeError("upper() requires exactly 1 argument (string)")

            str_ptr = node.arguments[0].accept(self)
            upper_func = self.runtime.get_function("multigen_str_upper")
            return self.builder.call(upper_func, [str_ptr], name="upper_result")

        elif node.function_name == "__method_replace__":
            # str.replace(old, new) -> multigen_str_replace(str, old, new)
            if len(node.arguments) != 3:
                raise RuntimeError("replace() requires exactly 3 arguments (string, old, new)")

            str_ptr = node.arguments[0].accept(self)
            old_ptr = node.arguments[1].accept(self)
            new_ptr = node.arguments[2].accept(self)
            replace_func = self.runtime.get_function("multigen_str_replace")
            return self.builder.call(replace_func, [str_ptr, old_ptr, new_ptr], name="replace_result")

        elif node.function_name == "__method_startswith__":
            # str.startswith(prefix) -> multigen_str_startswith(str, prefix) returns i32
            if len(node.arguments) != 2:
                raise RuntimeError("startswith() requires exactly 2 arguments (string, prefix)")

            str_ptr = node.arguments[0].accept(self)
            prefix_ptr = node.arguments[1].accept(self)
            startswith_func = self.runtime.get_function("multigen_str_startswith")
            result = self.builder.call(startswith_func, [str_ptr, prefix_ptr], name="startswith_result")
            # Convert i32 result to i1 (bool) by comparing with 0
            return self.builder.icmp_signed("!=", result, ir.Constant(ir.IntType(32), 0), name="startswith_bool")

        elif node.function_name == "__method_endswith__":
            # str.endswith(suffix) -> multigen_str_endswith(str, suffix) returns i32
            if len(node.arguments) != 2:
                raise RuntimeError("endswith() requires exactly 2 arguments (string, suffix)")

            str_ptr = node.arguments[0].accept(self)
            suffix_ptr = node.arguments[1].accept(self)
            endswith_func = self.runtime.get_function("multigen_str_endswith")
            result = self.builder.call(endswith_func, [str_ptr, suffix_ptr], name="endswith_result")
            # Convert i32 result to i1 (bool) by comparing with 0
            return self.builder.icmp_signed("!=", result, ir.Constant(ir.IntType(32), 0), name="endswith_bool")

        elif node.function_name == "__method_join__":
            # separator.join(list) -> multigen_str_join(separator, list)
            # Note: list should be vec_str* which has same layout as multigen_string_array_t*
            if len(node.arguments) != 2:
                raise RuntimeError("join() requires exactly 2 arguments (separator, list)")

            sep_ptr = node.arguments[0].accept(self)
            list_ptr = node.arguments[1].accept(self)

            # Bitcast vec_str* to multigen_string_array_t* (same layout: char**, size_t, size_t)
            string_array_ptr_type = self.runtime.get_string_array_type().as_pointer()
            string_array_ptr = self.builder.bitcast(list_ptr, string_array_ptr_type, name="list_as_string_array")

            join_func = self.runtime.get_function("multigen_str_join")
            return self.builder.call(join_func, [sep_ptr, string_array_ptr], name="join_result")

        elif node.function_name == "__method_values__":
            # dict.values() -> create and return vec_int with all values
            if len(node.arguments) != 1:
                raise RuntimeError("values() requires exactly 1 argument (dict)")

            dict_ptr = node.arguments[0].accept(self)
            return self._generate_dict_values_vec(dict_ptr)

        elif node.function_name == "__method_items__":
            # dict.items() - not directly callable, should only be used in for loops
            raise NotImplementedError("dict.items() can only be used in for loops or comprehensions")

        # Handle builtin functions
        elif node.function_name == "len":
            # len() function - use strlen for strings, vec_*_size for lists, map_*_size for dicts
            if len(node.arguments) != 1:
                raise NotImplementedError("len() requires exactly one argument")

            arg = node.arguments[0]
            llvm_arg = arg.accept(self)

            if arg.result_type.base_type == IRDataType.STRING:
                # Use C strlen function
                i8_ptr = ir.IntType(8).as_pointer()
                i64 = ir.IntType(64)
                strlen_func = self._get_or_create_c_function("strlen", i64, [i8_ptr])
                return self.builder.call(strlen_func, [llvm_arg], name="len_tmp")
            elif arg.result_type.base_type == IRDataType.DICT:
                # Use map_*_size function from runtime
                # llvm_arg is already a pointer
                from llvmlite import ir as llvm_ir

                if isinstance(llvm_arg.type, llvm_ir.PointerType):
                    pointee_type_str = str(llvm_arg.type.pointee)

                    if "map_str_int" in pointee_type_str:
                        map_size_func = self.runtime.get_function("map_str_int_size")
                        return self.builder.call(map_size_func, [llvm_arg], name="len_tmp")
                    elif "map_int_int" in pointee_type_str:
                        map_size_func = self.runtime.get_function("map_int_int_size")
                        return self.builder.call(map_size_func, [llvm_arg], name="len_tmp")
                    else:
                        raise NotImplementedError(f"len() for dict type {pointee_type_str} not implemented")
                else:
                    raise NotImplementedError("len() requires a pointer type for dicts")
            elif arg.result_type.base_type == IRDataType.LIST:
                # Use vec_*_size function from runtime based on element type
                # llvm_arg is already a pointer
                from llvmlite import ir as llvm_ir

                # Check the pointee type to determine which vec_* type we have
                if isinstance(llvm_arg.type, llvm_ir.PointerType):
                    pointee_type_str = str(llvm_arg.type.pointee)

                    if "vec_vec_int" in pointee_type_str:
                        vec_size_func = self.runtime.get_function("vec_vec_int_size")
                    elif "vec_str" in pointee_type_str:
                        vec_size_func = self.runtime.get_function("vec_str_size")
                    else:
                        vec_size_func = self.runtime.get_function("vec_int_size")
                else:
                    # Fallback to vec_int
                    vec_size_func = self.runtime.get_function("vec_int_size")

                return self.builder.call(vec_size_func, [llvm_arg], name="len_tmp")
            elif arg.result_type.base_type == IRDataType.SET:
                # Use set_int_size function from runtime
                # llvm_arg is already a pointer
                set_size_func = self.runtime.get_function("set_int_size")
                return self.builder.call(set_size_func, [llvm_arg], name="len_tmp")
            else:
                raise NotImplementedError(f"len() for type {arg.result_type.base_type} not implemented")

        elif node.function_name == "print":
            # Get or create printf declaration
            arg_types = [arg.result_type for arg in node.arguments]
            printf_func = self._get_or_create_builtin("print", arg_types)

            # Create format string based on argument type
            if len(node.arguments) == 1:
                arg = node.arguments[0]
                if arg.result_type.base_type == IRDataType.INT:
                    fmt_bytes = b"%lld\n\x00"  # %lld\n\0 as bytes
                elif arg.result_type.base_type == IRDataType.FLOAT:
                    fmt_bytes = b"%f\n\x00"  # %f\n\0 as bytes
                elif arg.result_type.base_type == IRDataType.BOOL:
                    fmt_bytes = b"%d\n\x00"  # %d\n\0 as bytes
                elif arg.result_type.base_type == IRDataType.STRING:
                    fmt_bytes = b"%s\n\x00"  # %s\n\0 as bytes
                else:
                    raise NotImplementedError(f"Print for type {arg.result_type.base_type} not implemented")

                # Create global string constant for format
                fmt_const = ir.Constant(ir.ArrayType(ir.IntType(8), len(fmt_bytes)), bytearray(fmt_bytes))
                fmt_global = ir.GlobalVariable(self.module, fmt_const.type, name=f"fmt_{len(self.module.globals)}")
                fmt_global.linkage = "internal"
                fmt_global.global_constant = True
                fmt_global.initializer = fmt_const

                # Get pointer to the format string
                fmt_ptr = self.builder.gep(fmt_global, [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), 0)])

                # Evaluate argument and call printf
                llvm_arg = arg.accept(self)
                return self.builder.call(printf_func, [fmt_ptr, llvm_arg], name="print_tmp")
            else:
                raise NotImplementedError("Print with multiple arguments not implemented")

        # Regular function call
        func = self.func_symtab.get(node.function_name)
        if func is None:
            raise RuntimeError(f"Function '{node.function_name}' not found in symbol table")

        args = [arg.accept(self) for arg in node.arguments]
        return self.builder.call(func, args, name="call_tmp")

    def visit_type_cast(self, node: IRTypeCast) -> ir.Instruction:
        """Convert IR type cast to LLVM cast instruction.

        Args:
            node: IR type cast to convert

        Returns:
            LLVM cast instruction
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        value = node.value.accept(self)
        source_type = node.value.result_type.base_type
        target_type = node.result_type.base_type

        # INT to FLOAT
        if source_type == IRDataType.INT and target_type == IRDataType.FLOAT:
            llvm_target = self._convert_type(node.result_type)
            return self.builder.sitofp(value, llvm_target, name="cast_tmp")

        # FLOAT to INT
        elif source_type == IRDataType.FLOAT and target_type == IRDataType.INT:
            llvm_target = self._convert_type(node.result_type)
            return self.builder.fptosi(value, llvm_target, name="cast_tmp")

        # INT to BOOL
        elif source_type == IRDataType.INT and target_type == IRDataType.BOOL:
            zero = ir.Constant(ir.IntType(64), 0)
            return self.builder.icmp_signed("!=", value, zero, name="cast_tmp")

        # FLOAT to BOOL
        elif source_type == IRDataType.FLOAT and target_type == IRDataType.BOOL:
            zero = ir.Constant(ir.DoubleType(), 0.0)
            return self.builder.fcmp_ordered("!=", value, zero, name="cast_tmp")

        # BOOL to INT
        elif source_type == IRDataType.BOOL and target_type == IRDataType.INT:
            llvm_target = self._convert_type(node.result_type)
            return self.builder.zext(value, llvm_target, name="cast_tmp")

        # Same type - no cast needed
        elif source_type == target_type:
            return value

        # Unsupported cast
        else:
            raise NotImplementedError(f"Type cast from {source_type} to {target_type} not implemented")

    def visit_return(self, node: IRReturn) -> None:
        """Convert IR return statement to LLVM ret instruction.

        Args:
            node: IR return statement to convert
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        if node.value:
            ret_val = node.value.accept(self)
            self.builder.ret(ret_val)
        else:
            self.builder.ret_void()

    def visit_break(self, node: IRBreak) -> None:
        """Convert IR break statement to LLVM branch to loop exit.

        Args:
            node: IR break statement to convert
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        if not self.loop_exit_stack:
            raise RuntimeError("Break statement outside of loop")

        # Branch to the current loop's exit block
        self.builder.branch(self.loop_exit_stack[-1])

    def visit_continue(self, node: IRContinue) -> None:
        """Convert IR continue statement to LLVM branch to loop condition.

        Args:
            node: IR continue statement to convert
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        if not self.loop_continue_stack:
            raise RuntimeError("Continue statement outside of loop")

        # Branch to the current loop's condition block
        self.builder.branch(self.loop_continue_stack[-1])

    def visit_expression_statement(self, node: IRExpressionStatement) -> None:
        """Convert IR expression statement to LLVM.

        Args:
            node: IR expression statement to convert
        """
        if self.builder is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Evaluate the expression (e.g., void function call)
        # The expression's side effects (like function calls) will be executed
        node.expression.accept(self)

    def visit_if(self, node: IRIf) -> None:
        """Convert IR if statement to LLVM basic blocks with branches.

        Args:
            node: IR if statement to convert
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Evaluate condition
        cond = node.condition.accept(self)

        # Create basic blocks
        then_block = self.current_function.append_basic_block("if.then")
        else_block = self.current_function.append_basic_block("if.else")
        merge_block = self.current_function.append_basic_block("if.merge")

        # Branch on condition
        self.builder.cbranch(cond, then_block, else_block)

        # Generate then block
        self.builder.position_at_end(then_block)
        for stmt in node.then_body:
            stmt.accept(self)
        if not self.builder.block.is_terminated:
            self.builder.branch(merge_block)

        # Generate else block
        self.builder.position_at_end(else_block)
        for stmt in node.else_body:
            stmt.accept(self)
        if not self.builder.block.is_terminated:
            self.builder.branch(merge_block)

        # Continue at merge point
        self.builder.position_at_end(merge_block)

    def visit_while(self, node: IRWhile) -> None:
        """Convert IR while loop to LLVM loop blocks.

        Args:
            node: IR while loop to convert
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Create basic blocks
        cond_block = self.current_function.append_basic_block("while.cond")
        body_block = self.current_function.append_basic_block("while.body")
        exit_block = self.current_function.append_basic_block("while.exit")

        # Track loop blocks for break/continue
        self.loop_exit_stack.append(exit_block)
        self.loop_continue_stack.append(cond_block)

        # Jump to condition check
        self.builder.branch(cond_block)

        # Generate condition block
        self.builder.position_at_end(cond_block)
        cond = node.condition.accept(self)
        self.builder.cbranch(cond, body_block, exit_block)

        # Generate body block
        self.builder.position_at_end(body_block)
        for stmt in node.body:
            stmt.accept(self)
        if not self.builder.block.is_terminated:
            self.builder.branch(cond_block)  # Loop back

        # Pop loop blocks from stack
        self.loop_exit_stack.pop()
        self.loop_continue_stack.pop()

        # Continue after loop
        self.builder.position_at_end(exit_block)

    def visit_for(self, node: IRFor) -> None:
        """Convert IR for loop (range-based) to LLVM loop blocks.

        Args:
            node: IR for loop to convert
        """
        if self.builder is None or self.current_function is None:
            raise RuntimeError("Builder not initialized - must be inside a function")

        # Allocate loop variable
        loop_var_type = self._convert_type(node.variable.ir_type)
        loop_var_ptr = self.builder.alloca(loop_var_type, name=node.variable.name)
        self.var_symtab[node.variable.name] = loop_var_ptr

        # Initialize loop variable with start value
        start_val = node.start.accept(self)
        self.builder.store(start_val, loop_var_ptr)

        # Create basic blocks
        cond_block = self.current_function.append_basic_block("for.cond")
        body_block = self.current_function.append_basic_block("for.body")
        inc_block = self.current_function.append_basic_block("for.inc")
        exit_block = self.current_function.append_basic_block("for.exit")

        # Track loop blocks for break/continue
        self.loop_exit_stack.append(exit_block)
        self.loop_continue_stack.append(inc_block)  # continue jumps to increment

        # Jump to condition
        self.builder.branch(cond_block)

        # Condition: loop_var < end (or > end for negative step)
        self.builder.position_at_end(cond_block)
        loop_var_val = self.builder.load(loop_var_ptr)
        end_val = node.end.accept(self)

        # Determine comparison operator based on step value
        # For negative steps, use >, for positive steps use <
        from ...frontend.static_ir import IRBinaryOperation, IRLiteral

        def is_negative_step(step: Optional[IRExpression]) -> bool:
            """Check if step is a negative constant."""
            if step is None:
                return False
            if isinstance(step, IRLiteral):
                return isinstance(step.value, int) and step.value < 0
            # Handle negative literals encoded as 0 - N
            if isinstance(step, IRBinaryOperation) and step.operator == "-":
                if isinstance(step.left, IRLiteral) and step.left.value == 0:
                    if isinstance(step.right, IRLiteral) and isinstance(step.right.value, int):
                        return step.right.value > 0
            return False

        comparison_op = ">" if is_negative_step(node.step) else "<"
        cond = self.builder.icmp_signed(comparison_op, loop_var_val, end_val, name="for.cond")
        self.builder.cbranch(cond, body_block, exit_block)

        # Body
        self.builder.position_at_end(body_block)
        for stmt in node.body:
            stmt.accept(self)
        if not self.builder.block.is_terminated:
            self.builder.branch(inc_block)

        # Increment
        self.builder.position_at_end(inc_block)
        loop_var_val = self.builder.load(loop_var_ptr)
        if node.step:
            step_val = node.step.accept(self)
        else:
            step_val = ir.Constant(loop_var_type, 1)
        next_val = self.builder.add(loop_var_val, step_val, name="for.inc")
        self.builder.store(next_val, loop_var_ptr)
        self.builder.branch(cond_block)

        # Pop loop blocks from stack
        self.loop_exit_stack.pop()
        self.loop_continue_stack.pop()

        # Exit
        self.builder.position_at_end(exit_block)

    def visit_type_declaration(self, node: IRTypeDeclaration) -> None:
        """Visit a type declaration node (structs, unions, enums).

        Args:
            node: IR type declaration to convert
        """
        # Type declarations will be implemented when needed for complex types
        # For now, we focus on basic types
        pass

    def _convert_type(self, ir_type: IRType) -> ir.Type:
        """Map IRType to llvmlite type.

        Args:
            ir_type: MultiGen IR type

        Returns:
            Corresponding llvmlite type
        """
        # Handle list types (LIST<T>)
        if ir_type.base_type == IRDataType.LIST:
            # Lists are represented as pointers to vec_* structs
            # Check element type to determine which vec_* type to use
            if hasattr(ir_type, "element_type") and ir_type.element_type:
                elem_type = ir_type.element_type.base_type

                if elem_type == IRDataType.LIST:
                    # 2D list: list[list[int]] -> vec_vec_int*
                    return self.runtime.get_vec_vec_int_type().as_pointer()
                elif elem_type == IRDataType.STRING:
                    # String list: list[str] -> vec_str*
                    return self.runtime.get_vec_str_type().as_pointer()
                elif elem_type == IRDataType.INT:
                    # Integer list: list[int] -> vec_int*
                    return self.runtime.get_vec_int_type().as_pointer()
                else:
                    # Default to vec_int for other types
                    return self.runtime.get_vec_int_type().as_pointer()
            else:
                # No element type info, default to vec_int
                return self.runtime.get_vec_int_type().as_pointer()

        # Handle dict types (DICT<K, V>)
        if ir_type.base_type == IRDataType.DICT:
            # Dicts are represented as pointers to map_* structs
            # Check element_type to determine key type
            if ir_type.element_type and ir_type.element_type.base_type == IRDataType.STRING:
                # String keys -> map_str_int*
                return self.runtime.get_map_str_int_type().as_pointer()
            else:
                # Default to int keys -> map_int_int*
                # This handles both dict[int, int] and generic dict
                return self.runtime.get_map_int_int_type().as_pointer()

        # Handle set types (SET<T>)
        if ir_type.base_type == IRDataType.SET:
            # Sets are represented as pointers to set_* structs
            # For now, only support int sets -> set_int*
            return self.runtime.get_set_int_type().as_pointer()

        # Base type mapping
        type_mapping = {
            IRDataType.VOID: ir.VoidType(),
            IRDataType.INT: ir.IntType(64),  # 64-bit integer
            IRDataType.FLOAT: ir.DoubleType(),  # double precision
            IRDataType.BOOL: ir.IntType(1),  # i1
            IRDataType.STRING: ir.IntType(8).as_pointer(),  # char*
        }

        base = type_mapping.get(ir_type.base_type, ir.VoidType())

        # Handle pointers
        if ir_type.is_pointer or ir_type.pointer_depth > 0:
            depth = ir_type.pointer_depth or 1
            for _ in range(depth):
                base = base.as_pointer()

        # Handle arrays
        if ir_type.array_dimensions:
            # Build array types from innermost to outermost
            for dim in reversed(ir_type.array_dimensions):
                if dim:
                    base = ir.ArrayType(base, dim)
                else:
                    # Unknown dimension, use pointer
                    base = base.as_pointer()

        return base
