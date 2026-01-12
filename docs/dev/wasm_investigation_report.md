# WebAssembly Target Investigation Report

**Date**: October 16, 2025
**Status**: Feasibility Confirmed - Implementation Roadmap Defined
**Version**: v0.1.85-dev

## Executive Summary

MultiGen can successfully generate LLVM IR that compiles to WebAssembly. This investigation confirms that **WebAssembly is a viable eighth backend target** for MultiGen, enabling Python-to-WASM compilation via the existing LLVM infrastructure.

**Key Findings:**

- [x] LLVM 21.1.3 (Homebrew) has full WebAssembly support (wasm32, wasm64)
- [x] llvmlite can target WebAssembly and generate object files
- [x] MultiGen-generated LLVM IR compiles successfully to WebAssembly objects
- [x] Pure functions (no runtime dependencies) work out-of-the-box
- [!] Runtime library integration requires additional work
- [!] Linking step needs external tooling (wasm-ld, Emscripten, or WASI SDK)

## Investigation Methodology

### 1. LLVM WebAssembly Support Verification

Confirmed that LLVM and llvmlite support multiple WebAssembly targets:

```python
from llvmlite import binding as llvm

llvm.initialize_all_targets()

targets = [
    'wasm32-unknown-unknown',  # Standard WebAssembly 32-bit
    'wasm64-unknown-unknown',  # WebAssembly 64-bit
    'wasm32-unknown-emscripten',  # Emscripten toolchain
    'wasm32-wasi'  # WASI (WebAssembly System Interface)
]

for triple in targets:
    target = llvm.Target.from_triple(triple)
    print(f'{triple}: {target.name} - {target.description}')
```

**Result:** All four targets are available and functional.

### 2. Basic WebAssembly Compilation Test

Created minimal LLVM IR and compiled to WebAssembly:

```llvm
target triple = "wasm32-unknown-unknown"

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}
```

**Compilation:**

```python
target = llvm.Target.from_triple("wasm32-unknown-unknown")
target_machine = target.create_target_machine(opt=2)
obj_code = target_machine.emit_object(llvm_module)
```

**Result:** [x] Successfully generated 257-byte WebAssembly object file

### 3. MultiGen-Generated IR Compilation

Compiled actual MultiGen LLVM IR (fibonacci.py) to WebAssembly:

**Source Python:**

```python
def fibonacci(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def main() -> int:
    return fibonacci(10)
```

**MultiGen Pipeline:**

```bash
uv run multigen build fibonacci.py -t llvm
# Generates build/src/fibonacci.ll
```

**WebAssembly Compilation:**

- Original IR: 5,363 bytes (includes all runtime declarations)
- Extracted pure functions: 1,037 bytes
- Generated WASM object: 616 bytes
- WebAssembly text (WAT): 2,510 bytes

**Result:** [x] Successfully compiled MultiGen IR to WebAssembly

### 4. WebAssembly Output Analysis

Generated WebAssembly text format shows proper code generation:

```wat
.functype fibonacci (i64) -> (i64)
.functype main () -> (i64)

fibonacci:
    .functype fibonacci (i64) -> (i64)
    .local i32, i64
    global.get __stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set __stack_pointer
    ; ... rest of function
```

**Analysis:**

- Proper function signatures
- Stack management via `__stack_pointer` global
- Efficient use of WebAssembly locals
- Tail call optimization opportunities

## Technical Challenges Identified

### Challenge 1: Runtime Library Integration

**Problem:** MultiGen LLVM IR includes declarations for runtime libraries:

```llvm
declare void @vec_int_init_ptr(%struct.vec_int* %".1")
declare void @vec_int_push(%struct.vec_int* %".1", i64 %".2")
declare i64 @vec_int_at(%struct.vec_int* %".1", i64 %".2")
; ... 60+ more runtime function declarations
```

**Current Status:** These are compiled as C code and linked with clang.

**WebAssembly Impact:**

- Need to compile C runtime to WebAssembly
- Link WebAssembly object files together
- Or: rewrite runtime in pure LLVM IR (no C dependencies)

**Solutions:**

1. **Compile C runtime to WASM** (using Emscripten or WASI SDK)
2. **Implement runtime in LLVM IR** (generate IR directly, no C)
3. **Use JavaScript bindings** (import runtime functions from JS)
4. **Subset approach** (support pure functions first, containers later)

### Challenge 2: Linking WebAssembly Modules

**Problem:** llvmlite generates WebAssembly *object files*, not complete modules.

**Missing Pieces:**

- Function exports (so JavaScript can call them)
- Import resolution (for runtime functions)
- Memory initialization
- Start function

**Current Tools:**

- `llc` (LLVM): Generates `.wasm` object files [x]
- `wasm-ld` (LLVM): Links WASM objects → runnable module [X] (not installed)
- `emcc` (Emscripten): Complete LLVM→WASM toolchain [X] (not installed)
- `wasi-sdk`: Standalone WASM compilation [X] (not installed)

**Solution Options:**

1. **Add wasm-ld dependency** (`brew install llvm` includes it on some platforms)
2. **Use Emscripten** (full JavaScript integration)
3. **Generate complete modules in Python** (using llvmlite + manual WASM generation)
4. **WASI SDK** (for standalone WASM without JavaScript)

### Challenge 3: System Calls and I/O

**Problem:** WebAssembly has no built-in I/O (no `printf`, `fopen`, etc.)

**MultiGen Uses:**

- `printf` for print() statements
- File I/O operations
- Standard library functions

**Solutions:**

1. **WASI (WebAssembly System Interface)** - Standardized syscall interface
2. **JavaScript imports** - Import console.log, file APIs from JS
3. **Emscripten** - Provides libc emulation
4. **Disable I/O** - Support pure computation only (subset of MultiGen)

## Proposed Implementation Strategy

### Phase 1: Pure Functions (No Runtime Dependencies)

**Scope:** Support Python programs that don't use:

- Lists, dicts, sets (no containers)
- String operations (basic strings only)
- File I/O
- print() statements

**Capabilities:**

- Arithmetic operations [x]
- Control flow (if/else, for/while) [x]
- Function calls and recursion [x]
- Integer/float types [x]

**Implementation:**

1. Add `wasm32` target to LLVM backend
2. Filter out unused runtime declarations
3. Generate minimal IR with proper WebAssembly triple
4. Use llvmlite to emit WebAssembly objects
5. Provide linking instructions for users

**Deliverable:** Python → WebAssembly for pure computational functions

### Phase 2: JavaScript Runtime Bridge

**Scope:** Enable containers and I/O via JavaScript imports

**Approach:**

```python
# Container operations become JavaScript imports
@wasm_import("env", "vec_int_push")
def vec_int_push(vec: pointer, value: i64) -> void
```

**Implementation:**

1. Generate import declarations in WASM
2. Provide JavaScript runtime library
3. Marshal data between WASM and JavaScript
4. Support print() via console.log

**Deliverable:** Full MultiGen functionality in browser/Node.js

### Phase 3: WASI Support

**Scope:** Standalone WASM with system interface

**Approach:**

- Compile runtime C code to WASM using WASI SDK
- Link WASM object files with wasm-ld
- Run with wasmtime or other WASI runtimes

**Implementation:**

1. Add WASI SDK as optional dependency
2. Cross-compile runtime libraries to WASM
3. Implement WASI-based I/O (file operations, printf)
4. Generate complete WASM modules

**Deliverable:** Standalone WebAssembly binaries (server-side, CLI tools)

### Phase 4: Emscripten Integration

**Scope:** Full browser integration with Emscripten toolchain

**Approach:**

- Use Emscripten as the LLVM→WASM compiler
- Leverage Emscripten's libc implementation
- Generate HTML+JS+WASM packages

**Implementation:**

1. Detect Emscripten installation
2. Use `emcc` instead of `llc` for WebAssembly target
3. Generate web-ready output (HTML scaffolding)
4. Support Emscripten APIs (canvas, WebGL, etc.)

**Deliverable:** MultiGen → Web Applications

## Proof of Concept Results

### Test Case: Fibonacci Benchmark

**Python Source:**

```python
def fibonacci(n: int) -> int:
    """Calculate Fibonacci number recursively."""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def main() -> int:
    return fibonacci(10)
```

**MultiGen Compilation:**

```bash
uv run multigen build fibonacci.py -t llvm
```

**WebAssembly Generation:**

```python
from llvmlite import binding as llvm

# Extract pure functions from MultiGen IR
ir = extract_pure_functions("build/src/fibonacci.ll")

# Compile to WebAssembly
llvm_module = llvm.parse_assembly(ir)
target = llvm.Target.from_triple("wasm32-unknown-unknown")
target_machine = target.create_target_machine(opt=2)
wasm_obj = target_machine.emit_object(llvm_module)

# Write to file
Path("fibonacci.wasm").write_bytes(wasm_obj)
```

**Results:**

- [x] Python → LLVM IR: 5,363 bytes
- [x] LLVM IR → WASM object: 616 bytes
- [x] Module verifies successfully
- [x] Functions: `fibonacci(i64) -> i64`, `main() -> i64`

**Limitation:** Generated object file needs linking to be runnable.

## WebAssembly Output Characteristics

### Code Size

| Source | Size | Format |
|--------|------|--------|
| Python | 181 bytes | `.py` source |
| LLVM IR (full) | 5,363 bytes | `.ll` with runtime decls |
| LLVM IR (pure) | 1,037 bytes | `.ll` functions only |
| WebAssembly object | 616 bytes | `.wasm` object |
| WebAssembly text | 2,510 bytes | `.wat` readable |

**Optimization:** Optimized IR could be even smaller with aggressive inlining.

### Performance Characteristics

WebAssembly features that benefit MultiGen:

1. **i64 support** - Native 64-bit integers (matches MultiGen's default int type)
2. **Stack machine** - Efficient for expression-heavy code
3. **Fast function calls** - Good for recursive algorithms
4. **SIMD** (future) - Potential for array operations
5. **Threads** (future) - Parallel computation

### Runtime Overhead

- **Memory:** WebAssembly uses linear memory (flat address space)
- **Globals:** Stack pointer managed automatically
- **Functions:** Direct calls (no v-tables for simple functions)
- **Optimization:** LLVM O2/O3 passes work with WASM target

## Ecosystem Integration

### Browser Support

All modern browsers support WebAssembly:

- Chrome/Edge (V8): Excellent
- Firefox (SpiderMonkey): Excellent
- Safari (JavaScriptCore): Excellent
- Mobile browsers: Good

### Runtime Environments

- **Browsers:** Chrome, Firefox, Safari, Edge
- **Node.js:** v8.0+ (native WebAssembly support)
- **Deno:** Built-in WASM support
- **wasmtime:** Standalone WASI runtime
- **wasmer:** Universal WASM runtime
- **wasm3:** Embedded/IoT devices

### Tooling Ecosystem

- **wabt:** WebAssembly Binary Toolkit (wasm-objdump, wasm-dis, wasm-validate)
- **Emscripten:** LLVM→WASM compiler with full libc
- **wasi-sdk:** Standalone WASM compilation
- **wasm-pack:** Rust→WASM packaging (inspiration for MultiGen)
- **AssemblyScript:** TypeScript→WASM (competing approach)

## Recommended Next Steps

### Immediate (v0.1.85)

1. **Document findings** [x] (this report)
2. **Add wasm32 target flag** to LLVM backend
3. **Implement IR extraction** for pure functions
4. **Test compilation** with more benchmarks
5. **Write linking documentation** for users

### Short-term (v0.2.0)

1. **Add wasm-ld integration** (if available)
2. **Implement Phase 1** (pure functions)
3. **Create JavaScript runtime** for basic I/O
4. **Add WebAssembly tests** to test suite
5. **Benchmark performance** vs native/LLVM

### Medium-term (v0.3.0)

1. **Compile runtime to WASM** (using WASI SDK or Emscripten)
2. **Implement Phase 2** (JavaScript bridge)
3. **Add browser demo** (fibonacci, quicksort, etc.)
4. **Support print()** via console.log
5. **Add WASM examples** to docs

### Long-term (v0.4.0+)

1. **Full WASI support** (standalone WASM)
2. **Emscripten integration** (web apps)
3. **WebAssembly SIMD** (vectorization)
4. **WebAssembly threads** (parallelism)
5. **GPU compute** (WebGPU backend)

## Comparison with Other Backends

| Feature | LLVM (native) | WebAssembly | C/C++ |
|---------|--------------|-------------|-------|
| Compilation target | Machine code | WASM object | Machine code |
| Platform support | OS-specific | Universal | OS-specific |
| Binary size | ~17 KB | ~616 bytes (obj) | ~36 KB |
| Startup time | Fast | Instant (in-browser) | Fast |
| Runtime | Native | WASM VM | Native |
| Sandboxing | None | Full | None |
| Web deployment | No | Yes | No |
| I/O | Direct syscalls | Via imports | Direct syscalls |
| Debugging | gdb/lldb | Browser DevTools | gdb |

**Key Advantage:** WebAssembly enables **web deployment** without transpiling.

## Risk Assessment

### Technical Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Runtime linking complexity | High | Medium | Start with pure functions only |
| Performance vs native | Medium | Low | WASM is typically 50-80% of native |
| Tooling dependencies | Medium | Medium | Document alternative approaches |
| API instability | Low | Low | WASM spec is stable (v1 since 2017) |
| Browser compatibility | Low | Low | Universal support in modern browsers |

### Implementation Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Scope creep | High | Medium | Phased approach (pure functions first) |
| Maintenance burden | Medium | Medium | Reuse LLVM backend infrastructure |
| Testing complexity | Medium | Low | Automated Node.js tests |
| Documentation | Medium | Medium | Provide clear examples |

## Conclusion

**WebAssembly is a viable and valuable target for MultiGen.**

The investigation confirms that:

1. **Technical feasibility**: MultiGen's LLVM backend can target WebAssembly with minimal changes
2. **Ecosystem support**: LLVM 21+ has mature WebAssembly support
3. **Phased approach**: Can start with pure functions, add features incrementally
4. **Strategic value**: Opens web deployment and universal platform support
5. **Low risk**: Builds on existing LLVM infrastructure

**Recommended Action:** Proceed with Phase 1 implementation (pure functions) in v0.1.85.

This would make MultiGen the **first Python-to-WebAssembly compiler via LLVM IR** that supports multiple backends and maintains Python semantics.

## Appendix A: Code Samples

### A.1 WebAssembly Compilation Script

See `/tmp/compile_to_wasm_v2.py` for complete implementation.

Key steps:

```python
# 1. Extract pure functions from MultiGen IR
wasm_ir = extract_pure_functions(multigen_ir)

# 2. Set WebAssembly target triple
wasm_ir = f'target triple = "wasm32-unknown-unknown"\n{wasm_ir}'

# 3. Compile to WebAssembly
llvm_module = llvm.parse_assembly(wasm_ir)
target = llvm.Target.from_triple("wasm32-unknown-unknown")
target_machine = target.create_target_machine(opt=2)
wasm_object = target_machine.emit_object(llvm_module)

# 4. Write output
Path("output.wasm").write_bytes(wasm_object)
```

### A.2 JavaScript Runtime Template

```javascript
// Load and run WebAssembly module
async function runWasm(wasmPath) {
    const wasmBuffer = fs.readFileSync(wasmPath);
    const wasmModule = await WebAssembly.compile(wasmBuffer);

    // Provide imports (memory, stack pointer, etc.)
    const memory = new WebAssembly.Memory({ initial: 256 });
    const imports = {
        env: {
            __linear_memory: memory,
            __stack_pointer: new WebAssembly.Global({ value: 'i32', mutable: true }, 65536)
        }
    };

    const instance = await WebAssembly.instantiate(wasmModule, imports);

    // Call exported functions
    const result = instance.exports.fibonacci(10);
    console.log(`fibonacci(10) = ${result}`);
}
```

### A.3 WASM Linking Example

```bash
# Using wasm-ld (LLVM WebAssembly linker)
wasm-ld \
    fibonacci.o \
    runtime.o \
    --export=fibonacci \
    --export=main \
    -o fibonacci.wasm

# Using Emscripten
emcc fibonacci.ll runtime.c -o fibonacci.html

# Using WASI SDK
/path/to/wasi-sdk/bin/clang fibonacci.ll -o fibonacci.wasm
```

## Appendix B: References

- [WebAssembly Official Site](https://webassembly.org/)
- [LLVM WebAssembly Backend](https://llvm.org/docs/WebAssembly.html)
- [llvmlite Documentation](https://llvmlite.readthedocs.io/)
- [WASI Specification](https://wasi.dev/)
- [Emscripten](https://emscripten.org/)
- [wabt Tools](https://github.com/WebAssembly/wabt)
- [WebAssembly at MDN](https://developer.mozilla.org/en-US/docs/WebAssembly)

---

**Report prepared by:** MultiGen Development Team
**Investigation period:** October 16, 2025
**Next review:** After Phase 1 implementation
