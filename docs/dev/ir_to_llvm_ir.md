# Using static_ir.py to Target LLVM IR

**Analysis Date:** October 2025
**Status:** Planning / Proof of Concept Phase
**Estimated Effort:** 2-3 months to feature parity

## Executive Summary

Using MultiGen's existing Static Python IR (`static_ir.py`) to target LLVM IR is **strategically sound** and architecturally aligned with the project. Recommendation: Implement as a **7th backend** alongside existing C/C++/Rust/Go/Haskell/OCaml backends, rather than replacing them.

**Key Benefits:**

- [x] Leverages existing well-designed IR infrastructure
- [x] Industry-standard LLVM optimization pipeline
- [x] Multiple target architectures from single implementation
- [x] Potential for JIT compilation
- [x] Future-proof architecture

**Key Challenges:**

- [X] Large dependency (~100MB+ LLVM)
- [X] Learning curve for LLVM IR generation
- [X] Python-LLVM impedance mismatch (GC, strings)
- [X] 2-3 month development timeline

## Current State Assessment

### What We Have

**Static Python IR** (`src/multigen/frontend/static_ir.py` - 922 lines)

- Well-designed visitor pattern architecture
- Type system mapping (Python → C types via `IRDataType`)
- Comprehensive IR nodes:
  - `IRModule`, `IRFunction`, `IRVariable`
  - `IRStatement`, `IRExpression`
  - `IRAssignment`, `IRBinaryOperation`, `IRLiteral`
  - `IRIf`, `IRWhile`, `IRFor`
  - `IRReturn`, `IRFunctionCall`
- Location tracking for debugging
- Serialization via `to_dict()`
- `IRBuilder` for AST → IR conversion

**Current Usage:** Defined but **not actively used** in pipeline. Current flow:

```text
Python AST → Backend-specific code generation (6 backends)
```

**Proposed Flow:**

```text
Python AST → Static IR → LLVM IR → Native binary/WebAssembly/etc.
```

### Existing Backends Performance

For reference, current backend status (v0.1.52):

- **C++**: 7/7 benchmarks (100%) - 422ms compile, 236ms execute, 36KB binary
- **C**: 7/7 benchmarks (100%) - 658ms compile, 238ms execute, 82KB binary
- **Rust**: 7/7 benchmarks (100%) - Production-ready
- **Go**: 7/7 benchmarks (100%) - 63ms compile, 42ms execute, 2365KB binary
- **OCaml**: 7/7 benchmarks (100%) - 209ms compile, 167ms execute, 771KB binary
- **Haskell**: 6/7 benchmarks (86%) - Functionally complete

## Strategic Analysis

### Why This Makes Sense

#### 1. Architectural Alignment

MultiGen's Static IR was designed to bridge Python AST and code generation. LLVM IR serves the same purpose but with industrial-strength optimization and multiple backend targets.

```text
Current:  Python AST → [Static IR (unused)] → 6 separate backends
Proposed: Python AST → Static IR → LLVM IR → Multiple targets
```

#### 2. Multiple Targets from One Implementation

```text
Python AST → Static IR → LLVM IR → {
    - x86_64 native binary
    - ARM native binary
    - WebAssembly
    - RISC-V
    - Your existing C/C++/Rust backends (as alternative path)
}
```

#### 3. LLVM Optimization Advantage

- Industry-proven optimization passes (inlining, dead code elimination, constant folding)
- Often produces code faster than hand-written C
- Continuous improvement from LLVM community

#### 4. Reduced Long-term Maintenance

- One IR → many targets (vs maintaining 6+ separate backends)
- LLVM handles architecture-specific optimizations
- Focus effort on Python → IR quality, not code generation details

### Why This Could Be Challenging

#### 1. LLVM Dependency Size

- LLVM libraries: ~100MB+
- May not be suitable for minimal deployments
- Solution: Keep existing backends for lightweight use cases

#### 2. Python-LLVM Impedance Mismatch

- **Garbage Collection**: Python has GC, LLVM doesn't (need runtime support)
- **Strings**: Python strings are complex objects, LLVM has char arrays
- **Dynamic Features**: Python's dynamism doesn't map cleanly to static LLVM
- Solution: Runtime library for Python semantics (similar to current C runtime)

#### 3. Learning Curve

- LLVM IR is verbose and low-level
- Requires understanding SSA form, phi nodes, basic blocks
- Debugging LLVM IR is harder than C code
- Solution: Start small, iterate, comprehensive tests

#### 4. Development Timeline

- Estimated 2-3 months to reach feature parity with C backend
- Additional time for optimization tuning
- Ongoing maintenance as LLVM evolves

## Technical Architecture

### LLVM Backend Structure

```text
src/multigen/backends/llvm/
├── __init__.py
├── backend.py              # LLVMBackend(LanguageBackend)
├── ir_to_llvm.py          # IRModule → llvmlite IR conversion
├── emitter.py             # LLVM IR emission and formatting
├── builder.py             # Compilation, linking, execution
├── type_mapper.py         # IRType → llvmlite type conversion
├── containers.py          # Container implementations (via runtime)
└── runtime/               # C runtime for Python semantics
    ├── multigen_llvm_runtime.c
    ├── multigen_llvm_string.c
    └── multigen_llvm_containers.c
```

### Key Components

#### 1. IRToLLVMConverter (Visitor Pattern)

```python
# src/multigen/backends/llvm/ir_to_llvm.py
from llvmlite import ir
from ...frontend.static_ir import IRVisitor, IRModule, IRFunction, IRDataType

class IRToLLVMConverter(IRVisitor):
    """Convert MultiGen Static IR to LLVM IR using llvmlite."""

    def __init__(self):
        self.module = ir.Module(name="multigen_module")
        self.builder = None
        self.func_symtab = {}
        self.var_symtab = {}

    def visit_module(self, node: IRModule):
        """Convert IR module to LLVM module."""
        # Generate type declarations first
        for type_decl in node.type_declarations:
            type_decl.accept(self)

        # Generate functions
        for func in node.functions:
            func.accept(self)

        return self.module

    def visit_function(self, node: IRFunction):
        """Convert IR function to LLVM function."""
        # Map IRType → llvmlite type
        ret_type = self._convert_type(node.return_type)
        param_types = [self._convert_type(p.ir_type) for p in node.parameters]

        # Create LLVM function
        func_type = ir.FunctionType(ret_type, param_types)
        func = ir.Function(self.module, func_type, node.name)

        # Store in symbol table
        self.func_symtab[node.name] = func

        # Create entry block
        block = func.append_basic_block(name="entry")
        self.builder = ir.IRBuilder(block)

        # Map parameters to LLVM values
        for i, param in enumerate(node.parameters):
            self.var_symtab[param.name] = func.args[i]

        # Generate function body
        for stmt in node.body:
            stmt.accept(self)

        return func

    def visit_assignment(self, node: IRAssignment):
        """Convert IR assignment to LLVM store."""
        if node.value:
            value = node.value.accept(self)
            # Allocate stack space for variable
            var_ptr = self.builder.alloca(value.type, name=node.target.name)
            self.builder.store(value, var_ptr)
            self.var_symtab[node.target.name] = var_ptr

    def visit_binary_operation(self, node: IRBinaryOperation):
        """Convert IR binary op to LLVM instruction."""
        left = node.left.accept(self)
        right = node.right.accept(self)

        if node.operator == "+":
            if node.result_type.base_type == IRDataType.INT:
                return self.builder.add(left, right, name="add_tmp")
            elif node.result_type.base_type == IRDataType.FLOAT:
                return self.builder.fadd(left, right, name="fadd_tmp")
        elif node.operator == "-":
            if node.result_type.base_type == IRDataType.INT:
                return self.builder.sub(left, right, name="sub_tmp")
            elif node.result_type.base_type == IRDataType.FLOAT:
                return self.builder.fsub(left, right, name="fsub_tmp")
        # ... other operators

    def visit_literal(self, node: IRLiteral):
        """Convert IR literal to LLVM constant."""
        llvm_type = self._convert_type(node.result_type)
        if node.result_type.base_type == IRDataType.INT:
            return ir.Constant(llvm_type, node.value)
        elif node.result_type.base_type == IRDataType.FLOAT:
            return ir.Constant(llvm_type, float(node.value))
        # ... other types

    def visit_variable_reference(self, node: IRVariableReference):
        """Convert IR variable reference to LLVM load."""
        var_ptr = self.var_symtab.get(node.variable.name)
        if var_ptr:
            return self.builder.load(var_ptr, name=node.variable.name)

    def visit_return(self, node: IRReturn):
        """Convert IR return to LLVM ret."""
        if node.value:
            ret_val = node.value.accept(self)
            self.builder.ret(ret_val)
        else:
            self.builder.ret_void()

    def visit_if(self, node: IRIf):
        """Convert IR if to LLVM basic blocks with phi nodes."""
        # Evaluate condition
        cond = node.condition.accept(self)

        # Create basic blocks
        then_block = self.builder.append_basic_block("if.then")
        else_block = self.builder.append_basic_block("if.else")
        merge_block = self.builder.append_basic_block("if.merge")

        # Branch on condition
        self.builder.cbranch(cond, then_block, else_block)

        # Generate then block
        self.builder.position_at_end(then_block)
        for stmt in node.then_body:
            stmt.accept(self)
        self.builder.branch(merge_block)

        # Generate else block
        self.builder.position_at_end(else_block)
        for stmt in node.else_body:
            stmt.accept(self)
        self.builder.branch(merge_block)

        # Continue at merge point
        self.builder.position_at_end(merge_block)

    def visit_while(self, node: IRWhile):
        """Convert IR while to LLVM loop blocks."""
        # Create basic blocks
        cond_block = self.builder.append_basic_block("while.cond")
        body_block = self.builder.append_basic_block("while.body")
        exit_block = self.builder.append_basic_block("while.exit")

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
        self.builder.branch(cond_block)  # Loop back

        # Continue after loop
        self.builder.position_at_end(exit_block)

    def visit_for(self, node: IRFor):
        """Convert IR for (range-based) to LLVM loop."""
        # Allocate loop variable
        loop_var_type = self._convert_type(node.variable.ir_type)
        loop_var_ptr = self.builder.alloca(loop_var_type, name=node.variable.name)

        # Initialize loop variable
        start_val = node.start.accept(self)
        self.builder.store(start_val, loop_var_ptr)
        self.var_symtab[node.variable.name] = loop_var_ptr

        # Create basic blocks
        cond_block = self.builder.append_basic_block("for.cond")
        body_block = self.builder.append_basic_block("for.body")
        inc_block = self.builder.append_basic_block("for.inc")
        exit_block = self.builder.append_basic_block("for.exit")

        # Jump to condition
        self.builder.branch(cond_block)

        # Condition: loop_var < end
        self.builder.position_at_end(cond_block)
        loop_var_val = self.builder.load(loop_var_ptr)
        end_val = node.end.accept(self)
        cond = self.builder.icmp_signed("<", loop_var_val, end_val)
        self.builder.cbranch(cond, body_block, exit_block)

        # Body
        self.builder.position_at_end(body_block)
        for stmt in node.body:
            stmt.accept(self)
        self.builder.branch(inc_block)

        # Increment
        self.builder.position_at_end(inc_block)
        loop_var_val = self.builder.load(loop_var_ptr)
        step_val = node.step.accept(self) if node.step else ir.Constant(loop_var_type, 1)
        next_val = self.builder.add(loop_var_val, step_val)
        self.builder.store(next_val, loop_var_ptr)
        self.builder.branch(cond_block)

        # Exit
        self.builder.position_at_end(exit_block)

    def visit_function_call(self, node: IRFunctionCall):
        """Convert IR function call to LLVM call."""
        func = self.func_symtab.get(node.function_name)
        args = [arg.accept(self) for arg in node.arguments]
        return self.builder.call(func, args, name="call_tmp")

    def _convert_type(self, ir_type):
        """Map IRType to llvmlite type."""
        mapping = {
            IRDataType.VOID: ir.VoidType(),
            IRDataType.INT: ir.IntType(64),
            IRDataType.FLOAT: ir.DoubleType(),
            IRDataType.BOOL: ir.IntType(1),
            IRDataType.STRING: ir.IntType(8).as_pointer(),  # char*
        }

        base = mapping.get(ir_type.base_type, ir.VoidType())

        # Handle pointers
        if ir_type.is_pointer or ir_type.pointer_depth > 0:
            depth = ir_type.pointer_depth or 1
            for _ in range(depth):
                base = base.as_pointer()

        return base
```

#### 2. LLVM Backend Class

```python
# src/multigen/backends/llvm/backend.py
from pathlib import Path
from ..base import LanguageBackend
from .ir_to_llvm import IRToLLVMConverter
from .builder import LLVMBuilder

class LLVMBackend(LanguageBackend):
    """LLVM IR backend for MultiGen."""

    def __init__(self):
        self.name = "llvm"
        self.file_extension = ".ll"  # LLVM IR text format
        self.builder = LLVMBuilder()

    def generate_code(self, ir_module, output_path: Path) -> str:
        """Generate LLVM IR from Static IR."""
        converter = IRToLLVMConverter()
        llvm_module = converter.visit_module(ir_module)

        # Return LLVM IR as string
        return str(llvm_module)

    def compile(self, source_file: str, output_dir: str) -> bool:
        """Compile LLVM IR to native binary."""
        return self.builder.compile_direct(source_file, output_dir)
```

#### 3. Type Mapping

```python
# Type Mapping: IRDataType → LLVM Types
IRDataType.VOID      → ir.VoidType()
IRDataType.INT       → ir.IntType(64)       # 64-bit integer
IRDataType.FLOAT     → ir.DoubleType()      # double precision
IRDataType.BOOL      → ir.IntType(1)        # i1
IRDataType.STRING    → ir.IntType(8).as_pointer()  # char*
IRDataType.POINTER   → <base_type>.as_pointer()
IRDataType.ARRAY     → ir.ArrayType(<elem_type>, <size>)
```

## Implementation Roadmap

### Phase 1: Proof of Concept (Weeks 1-3)

**Week 1: Setup & Infrastructure**

- [x] Install llvmlite: `uv add llvmlite`
- [ ] Create `src/multigen/backends/llvm/` directory structure
- [ ] Implement basic `IRToLLVMConverter` skeleton
- [ ] Implement type mapping (`IRDataType` → llvmlite types)
- [ ] Test: Convert simple IR to LLVM IR text

**Week 2: Basic Functions**

- [ ] Implement `visit_function()` - function generation
- [ ] Implement `visit_literal()` - constants
- [ ] Implement `visit_binary_operation()` - arithmetic
- [ ] Implement `visit_return()` - return statements
- [ ] Test case: `def add(x: int, y: int) -> int: return x + y`
- [ ] Verify: Can generate LLVM IR that compiles

**Week 3: Control Flow**

- [ ] Implement `visit_if()` - conditional branches with phi nodes
- [ ] Implement `visit_while()` - loop basic blocks
- [ ] Implement `visit_for()` - range-based loops
- [ ] Test case: Fibonacci (recursive and iterative)
- [ ] Verify: Control flow works correctly

**Milestone 1:** Simple functions with arithmetic and control flow compile and execute correctly.

### Phase 2: Language Features (Weeks 4-8)

**Week 4-5: Variables & Assignments**

- [ ] Implement `visit_assignment()` - alloca + store
- [ ] Implement `visit_variable_reference()` - load
- [ ] Handle local variables properly
- [ ] Test: Functions with local variables
- [ ] Benchmark: Fibonacci iterative (should pass)

**Week 6-7: Function Calls**

- [ ] Implement `visit_function_call()` - call instruction
- [ ] Handle parameter passing
- [ ] Support recursive calls
- [ ] Test: Quicksort (recursive)
- [ ] Benchmark: Should pass quicksort test

**Week 8: Integration**

- [ ] Add LLVM backend to pipeline
- [ ] Support `multigen --target llvm convert file.py`
- [ ] Create `LLVMBuilder` for native compilation
- [ ] Test: All 7 benchmarks (expect 2-3 to pass)

**Milestone 2:** Basic language features work, 2-3 benchmarks passing.

### Phase 3: Advanced Features (Weeks 9-12)

**Week 9-10: Strings**

- [ ] Design string representation (char* + length? or struct?)
- [ ] Implement string operations via runtime library
- [ ] Support string concatenation, comparison
- [ ] Test: String manipulation functions
- [ ] Benchmark: Wordcount (should pass with runtime support)

**Week 11-12: Containers**

- [ ] Design container representation (pointers to runtime structs)
- [ ] Implement list operations via runtime
- [ ] Implement dict operations via runtime
- [ ] Implement set operations via runtime
- [ ] Test: Container manipulation
- [ ] Benchmarks: list_ops, dict_ops, set_ops (should pass)

**Milestone 3:** All 7 benchmarks passing, feature parity with C backend.

### Phase 4: Optimization & Polish (Ongoing)

**Optimization Passes**

- [ ] Enable LLVM optimization levels (-O1, -O2, -O3)
- [ ] Measure performance vs C/C++/Rust backends
- [ ] Tune optimization flags for best results
- [ ] Document performance characteristics

**Quality & Testing**

- [ ] Add comprehensive unit tests
- [ ] Add integration tests
- [ ] Performance regression tests
- [ ] Documentation and examples

**Advanced Features (Future)**

- [ ] JIT compilation support (execute without file output)
- [ ] WebAssembly target
- [ ] Debug info generation (DWARF)
- [ ] Better error messages for LLVM failures

## Usage Examples

### Command Line

```bash
# Generate LLVM IR (text format)
multigen --target llvm convert fibonacci.py
# Output: fibonacci.ll

# Compile to native binary
multigen --target llvm build fibonacci.py
# Output: fibonacci (executable)

# View LLVM IR
multigen --target llvm convert fibonacci.py --emit-ir
cat fibonacci.ll

# With optimization
multigen --target llvm build fibonacci.py --opt-level 3
```

### Example Output

**Python Input:**

```python
def add(x: int, y: int) -> int:
    result: int = x + y
    return result
```

**Generated LLVM IR:**

```llvm
; ModuleID = "multigen_module"
target triple = "unknown-unknown-unknown"

define i64 @add(i64 %x, i64 %y) {
entry:
  %result = alloca i64
  %add_tmp = add i64 %x, %y
  store i64 %add_tmp, i64* %result
  %result_val = load i64, i64* %result
  ret i64 %result_val
}
```

**With Optimization (-O2):**

```llvm
define i64 @add(i64 %x, i64 %y) {
entry:
  %add_tmp = add i64 %x, %y
  ret i64 %add_tmp
}
```

## Performance Expectations

### Compilation Time

- **LLVM IR generation:** ~10-50ms per module
- **LLVM optimization:** ~50-200ms (depends on -O level)
- **Native compilation:** ~100-500ms
- **Total:** ~200-750ms (competitive with C++ at ~422ms)

### Execution Performance

- **-O0 (no optimization):** Similar to C -O0
- **-O2 (default):** Within 5-10% of C -O2
- **-O3 (aggressive):** Often matches or beats hand-written C

### Binary Size

- **Without optimization:** Larger than C (debug info)
- **With optimization + strip:** Similar to C (30-100KB range)

## Comparison: LLVM vs Existing Backends

| Feature | LLVM | C | C++ | Rust | Go | Haskell | OCaml |
|---------|------|---|-----|------|----|---------| ------|
| **Compile Speed** | Medium | Fast | Medium | Slow | Very Fast | Medium | Fast |
| **Runtime Speed** | Excellent | Excellent | Excellent | Excellent | Good | Good | Good |
| **Binary Size** | Medium | Small | Small | Medium | Large | Very Large | Medium |
| **Portability** | Excellent | Good | Good | Good | Excellent | Good | Good |
| **Debug Info** | Excellent | Good | Good | Excellent | Good | Good | Good |
| **Optimization** | Excellent | Good | Good | Excellent | Good | Good | Good |
| **Maintenance** | Low* | Medium | Medium | Medium | Medium | High | High |

\* Once working, LLVM handles architecture-specific concerns

## Decision Matrix

### When to Use LLVM Backend

**Use LLVM when:**

- [x] Need multiple target architectures (x86, ARM, RISC-V)
- [x] Want maximum optimization potential
- [x] Need WebAssembly output
- [x] Want JIT compilation capability
- [x] Long-term maintainability is priority
- [x] Binary size < 500KB is acceptable

**Use C/C++ backends when:**

- [x] Need minimal binary size (<50KB)
- [x] Need very fast compilation (<100ms)
- [x] Need specific C library integration
- [x] Need to inspect/modify generated code
- [x] LLVM dependency is too large

**Use Rust backend when:**

- [x] Need memory safety guarantees in output
- [x] Want Rust ecosystem integration
- [x] Ownership semantics are important

## Risk Assessment

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| LLVM version compatibility | Medium | High | Pin LLVM version, test upgrades |
| Performance worse than C | Low | Medium | Extensive optimization tuning |
| Container implementation complex | High | Medium | Reuse C runtime libraries |
| Development takes >3 months | Medium | Low | Phased rollout, keep existing backends |
| llvmlite bugs/limitations | Low | Medium | Fallback to LLVM C API if needed |

### Project Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Diverts from core mission | Low | Medium | Keep as optional 7th backend |
| Dependency bloat | High | Low | Document deployment options |
| Community doesn't adopt | Medium | Low | Existing backends remain primary |
| Maintenance burden | Medium | Medium | Comprehensive tests, good docs |

## Resources & References

### LLVM Learning Resources

1. **llvmlite Documentation**
   - Official docs: <https://llvmlite.readthedocs.io/>
   - IR Builder guide: <https://llvmlite.readthedocs.io/en/latest/user-guide/ir/ir-builder.html>
   - Type system: <https://llvmlite.readthedocs.io/en/latest/user-guide/ir/types.html>
   - Examples: <https://llvmlite.readthedocs.io/en/latest/user-guide/ir/examples.html>

2. **PyKaleidoscope Tutorial**
   - GitHub: <https://github.com/eliben/pykaleidoscope>
   - Complete working example of Python → LLVM IR
   - Updated for llvmlite 0.45.0 (2025)

3. **LLVM Language Reference**
   - Official LLVM IR reference: <https://llvm.org/docs/LangRef.html>
   - SSA form explained: <https://llvm.org/docs/tutorial/>

4. **llvmlite on PyPI**
   - Package page: <https://pypi.org/project/llvmlite/>
   - Version 0.45.0+ requires LLVM 20.x
   - Supports Python 3.10-3.13

### MultiGen Relevant Files

- `src/multigen/frontend/static_ir.py` - IR definition (922 lines)
- `src/multigen/backends/base.py` - LanguageBackend interface
- `src/multigen/backends/c/runtime/` - Reusable runtime for LLVM
- `tests/benchmarks/` - 7 benchmark tests to pass

### Similar Projects

- **Numba**: Python JIT compiler using LLVM (for NumPy)
- **Nuitka**: Python → C → binary compiler
- **Cython**: Python-like language → C
- **Shed Skin**: Python → C++ compiler
- **PyPy**: Python interpreter with JIT (not LLVM, but instructive)

## Success Criteria

### Minimum Viable Product (MVP)

- [ ] Compiles simple arithmetic functions
- [ ] Supports int, float types
- [ ] Supports if/else, while, for loops
- [ ] Generates working LLVM IR
- [ ] Can produce native binaries
- [ ] Passes 2-3 benchmark tests

### Feature Parity with C Backend

- [ ] All 7 benchmarks passing
- [ ] String support
- [ ] Container support (list, dict, set)
- [ ] File I/O support
- [ ] Module imports
- [ ] Comparable performance to C backend

### Production Ready

- [ ] Comprehensive test coverage (>90%)
- [ ] Documentation complete
- [ ] Performance within 10% of C
- [ ] Stable across LLVM versions
- [ ] User feedback positive

## Next Steps

### Immediate (This Week)

1. Install llvmlite: `uv add llvmlite`
2. Create directory structure: `src/multigen/backends/llvm/`
3. Implement basic `IRToLLVMConverter` skeleton
4. Write first test: Convert IR for `add(x, y)` function

### Short Term (This Month)

1. Complete Phase 1 (Proof of Concept)
2. Get simple functions working end-to-end
3. Pass first benchmark (fibonacci)
4. Document progress and learnings

### Medium Term (3 Months)

1. Complete Phase 2 (Language Features)
2. Pass 5-7 benchmarks
3. Achieve feature parity with C backend
4. Write comprehensive documentation

### Long Term (6+ Months)

1. Optimize performance
2. Add advanced features (JIT, WebAssembly)
3. Gather community feedback
4. Consider making LLVM default backend

## Conclusion

Using `static_ir.py` to target LLVM IR is a **strategically sound decision** that aligns with MultiGen's architecture and goals. The existing Static IR infrastructure provides an excellent foundation, and LLVM offers industrial-strength optimization with multiple target architectures.

**Recommendation:** Proceed with **Phase 1 (Proof of Concept)** as a 7th backend alongside existing C/C++/Rust/Go/Haskell/OCaml backends. This approach:

- Minimizes risk (existing backends remain)
- Enables learning and iteration
- Provides future-proof architecture
- Maintains backward compatibility

The 2-3 month timeline to feature parity is reasonable given the quality of existing IR infrastructure. Success depends on starting small, iterating quickly, and leveraging LLVM's extensive documentation and community resources.

**Status:** Ready to begin implementation.
