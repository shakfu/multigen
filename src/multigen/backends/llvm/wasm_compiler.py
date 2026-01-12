"""WebAssembly compilation support for LLVM backend.

This module provides functionality to compile MultiGen LLVM IR to WebAssembly.
Supports both object file generation (Phase 1) and future linking strategies.

IMPORTANT: This is experimental functionality. WebAssembly compilation requires:
- LLVM 13+ with WebAssembly target support
- Optional: wasm-ld for linking (not yet integrated)
- Optional: WASI SDK for standalone WASM (future)
- Optional: Emscripten for web deployment (future)

Current Status (v0.1.85-dev):
- Phase 1: Pure function compilation to WASM objects ✅
- Phase 2: JavaScript runtime bridge (planned)
- Phase 3: WASI support (planned)
- Phase 4: Emscripten integration (planned)
"""

import re
from pathlib import Path
from typing import Optional

try:
    from llvmlite import binding as llvm  # type: ignore[import-untyped]

    LLVMLITE_AVAILABLE = True
except ImportError:
    LLVMLITE_AVAILABLE = False


class WebAssemblyCompiler:
    """Compiler for generating WebAssembly from LLVM IR."""

    def __init__(self, target_triple: str = "wasm32-unknown-unknown") -> None:
        """Initialize WebAssembly compiler.

        Args:
            target_triple: WebAssembly target triple
                - wasm32-unknown-unknown: Standard WebAssembly 32-bit
                - wasm64-unknown-unknown: WebAssembly 64-bit
                - wasm32-wasi: WASI (WebAssembly System Interface)
                - wasm32-unknown-emscripten: Emscripten toolchain

        Raises:
            ImportError: If llvmlite is not available
            RuntimeError: If WebAssembly target is not supported
        """
        if not LLVMLITE_AVAILABLE:
            raise ImportError("llvmlite is required for WebAssembly compilation")

        self.target_triple = target_triple

        # Initialize LLVM targets
        llvm.initialize_all_targets()
        llvm.initialize_all_asmprinters()

        # Verify WebAssembly target is available
        try:
            self.target = llvm.Target.from_triple(target_triple)
        except Exception as e:
            raise RuntimeError(f"WebAssembly target {target_triple} not available: {e}") from e

    def extract_pure_functions(self, llvm_ir: str) -> str:
        """Extract pure user-defined functions from MultiGen LLVM IR.

        MultiGen generates LLVM IR with many runtime library declarations (vec_int_*, map_*, etc.).
        For standalone WebAssembly, we only need the actual function definitions.

        This function:
        1. Finds the first 'define' statement (start of user code)
        2. Extracts all function definitions and string constants
        3. Excludes all 'declare' statements (runtime library)

        Args:
            llvm_ir: Full LLVM IR from MultiGen (includes runtime declarations)

        Returns:
            Minimal LLVM IR with only user-defined functions

        Raises:
            ValueError: If no function definitions found in IR
        """
        lines = llvm_ir.split("\n")

        # Find where actual functions start (after runtime declarations)
        func_start = None
        for i, line in enumerate(lines):
            if line.startswith("define "):
                func_start = i
                break

        if func_start is None:
            raise ValueError("No function definitions found in LLVM IR")

        # Extract function definitions and string constants
        pure_ir_lines = []
        in_function = False
        brace_count = 0

        for line in lines[func_start:]:
            # Track function definitions
            if line.startswith("define "):
                in_function = True
                brace_count = 0

            if in_function:
                pure_ir_lines.append(line)
                # Count braces to know when function ends
                brace_count += line.count("{")
                brace_count -= line.count("}")
                if brace_count == 0 and line.strip() == "}":
                    in_function = False
            # Include global string constants (from docstrings, etc.)
            elif line.startswith("@") and "= internal constant" in line:
                pure_ir_lines.append(line)
            # Include blank lines for readability
            elif not line.strip():
                pure_ir_lines.append(line)

        return "\n".join(pure_ir_lines)

    def compile_to_wasm(
        self,
        llvm_ir: str,
        output_path: Path,
        opt_level: int = 2,
        text_format: bool = False,
    ) -> bool:
        """Compile LLVM IR to WebAssembly object file.

        Args:
            llvm_ir: LLVM IR source code
            output_path: Output file path (.wasm or .wat)
            opt_level: Optimization level (0-3)
            text_format: If True, generate WebAssembly text format (.wat)

        Returns:
            True if compilation succeeded

        Raises:
            ValueError: If IR is invalid or compilation fails
        """
        try:
            # Parse and verify IR
            llvm_module = llvm.parse_assembly(llvm_ir)
            llvm_module.verify()

            # Create WebAssembly target machine
            target_machine = self.target.create_target_machine(
                cpu="generic",
                features="",
                opt=opt_level,
                reloc="pic",
                codemodel="default",
            )

            # Emit object code or assembly
            if text_format:
                output_code = target_machine.emit_assembly(llvm_module)
                output_path.write_text(output_code)
            else:
                output_code = target_machine.emit_object(llvm_module)
                output_path.write_bytes(output_code)

            return True

        except Exception as e:
            raise ValueError(f"WebAssembly compilation failed: {e}") from e

    def compile_multigen_ir(
        self,
        ir_path: Path,
        output_path: Path,
        opt_level: int = 2,
        pure_functions_only: bool = True,
        text_format: bool = False,
    ) -> bool:
        """Compile MultiGen-generated LLVM IR to WebAssembly.

        Args:
            ir_path: Path to MultiGen LLVM IR file (.ll)
            output_path: Output WebAssembly file (.wasm or .wat)
            opt_level: Optimization level (0-3)
            pure_functions_only: If True, extract only user functions (no runtime)
            text_format: If True, generate text format (.wat) instead of binary

        Returns:
            True if compilation succeeded

        Raises:
            FileNotFoundError: If IR file doesn't exist
            ValueError: If compilation fails
        """
        if not ir_path.exists():
            raise FileNotFoundError(f"LLVM IR file not found: {ir_path}")

        # Read original IR
        full_ir = ir_path.read_text()

        # Extract pure functions if requested
        if pure_functions_only:
            ir_core = self.extract_pure_functions(full_ir)
        else:
            ir_core = full_ir

        # Build WebAssembly IR with proper target triple
        wasm_ir = f"""target triple = "{self.target_triple}"

{ir_core}
"""

        # Compile to WebAssembly
        return self.compile_to_wasm(wasm_ir, output_path, opt_level, text_format)

    @staticmethod
    def get_info() -> dict[str, str]:
        """Get WebAssembly compilation information.

        Returns:
            Dictionary with version and support info
        """
        info = {
            "llvmlite_available": str(LLVMLITE_AVAILABLE),
            "status": "experimental",
            "phase": "1 (pure functions)",
        }

        if LLVMLITE_AVAILABLE:
            # Check which targets are available
            llvm.initialize_all_targets()
            targets = []
            for triple in [
                "wasm32-unknown-unknown",
                "wasm64-unknown-unknown",
                "wasm32-wasi",
                "wasm32-unknown-emscripten",
            ]:
                try:
                    llvm.Target.from_triple(triple)
                    targets.append(triple)
                except Exception:
                    pass

            info["available_targets"] = ", ".join(targets)

        return info


def compile_to_webassembly(
    ir_path: Path,
    output_dir: Path,
    target: str = "wasm32-unknown-unknown",
    opt_level: int = 2,
    pure_only: bool = True,
) -> tuple[bool, Optional[str]]:
    """High-level function to compile MultiGen IR to WebAssembly.

    Args:
        ir_path: Path to LLVM IR file
        output_dir: Output directory for WebAssembly files
        target: WebAssembly target triple
        opt_level: Optimization level (0-3)
        pure_only: Extract pure functions only (no runtime dependencies)

    Returns:
        Tuple of (success, error_message)
    """
    try:
        compiler = WebAssemblyCompiler(target_triple=target)

        # Generate both binary and text formats
        wasm_path = output_dir / f"{ir_path.stem}.wasm"
        wat_path = output_dir / f"{ir_path.stem}.wat"

        # Compile to binary format
        compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wasm_path,
            opt_level=opt_level,
            pure_functions_only=pure_only,
            text_format=False,
        )

        # Compile to text format
        compiler.compile_multigen_ir(
            ir_path=ir_path,
            output_path=wat_path,
            opt_level=opt_level,
            pure_functions_only=pure_only,
            text_format=True,
        )

        return True, None

    except Exception as e:
        return False, str(e)
