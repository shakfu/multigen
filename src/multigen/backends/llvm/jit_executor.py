"""JIT executor for LLVM backend using llvmlite execution engine.

This module provides an alternative to AOT (ahead-of-time) compilation
using llvmlite's MCJIT compiler for in-memory execution.
"""

from ctypes import CFUNCTYPE, c_int64
from pathlib import Path
from typing import Any, Optional

import llvmlite.binding as llvm  # type: ignore[import-untyped]


class LLVMJITExecutor:
    """JIT executor for LLVM IR using llvmlite execution engine.

    This provides an alternative compilation mode that executes code
    in-memory without generating object files or executables.

    Benefits:
    - Faster development cycle (no llc/clang overhead)
    - Useful for testing and debugging
    - Simpler build process

    Limitations:
    - Cannot produce standalone executables
    - Runtime dependency on llvmlite
    - Limited to in-process execution
    """

    def __init__(self) -> None:
        """Initialize the JIT executor."""
        # Initialize LLVM native target and ASM printer
        # Note: llvm.initialize() is deprecated and handled automatically
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        self.engine: Optional[llvm.ExecutionEngine] = None
        self.modules: list[llvm.ModuleRef] = []

    def create_execution_engine(self) -> llvm.ExecutionEngine:
        """Create MCJIT execution engine.

        Returns:
            Execution engine instance
        """
        # Create target machine
        target = llvm.Target.from_default_triple()
        target_machine = target.create_target_machine()

        # Create backing module (empty module required by MCJIT)
        backing_mod = llvm.parse_assembly("")
        backing_mod.verify()

        # Create MCJIT compiler
        engine = llvm.create_mcjit_compiler(backing_mod, target_machine)
        self.engine = engine

        return engine

    def compile_ir(self, llvm_ir: str) -> llvm.ModuleRef:
        """Compile LLVM IR string to module.

        Args:
            llvm_ir: LLVM IR as string

        Returns:
            Compiled module reference

        Raises:
            RuntimeError: If IR verification fails
        """
        if self.engine is None:
            self.create_execution_engine()

        # Parse IR string to module
        mod = llvm.parse_assembly(llvm_ir)

        # Verify module
        try:
            mod.verify()
        except RuntimeError as e:
            raise RuntimeError(f"LLVM IR verification failed: {e}") from e

        # Add module to execution engine
        assert self.engine is not None, "Execution engine should be initialized"
        self.engine.add_module(mod)
        self.engine.finalize_object()
        self.engine.run_static_constructors()

        self.modules.append(mod)
        return mod

    def compile_ir_file(self, llvm_ir_file: str) -> llvm.ModuleRef:
        """Compile LLVM IR from file.

        Args:
            llvm_ir_file: Path to .ll file

        Returns:
            Compiled module reference
        """
        ir_path = Path(llvm_ir_file)
        if not ir_path.exists():
            raise FileNotFoundError(f"LLVM IR file not found: {llvm_ir_file}")

        llvm_ir = ir_path.read_text()
        return self.compile_ir(llvm_ir)

    def get_function_address(self, function_name: str) -> int:
        """Get address of compiled function.

        Args:
            function_name: Name of function (without @ prefix)

        Returns:
            Function address as integer

        Raises:
            RuntimeError: If function not found
        """
        if self.engine is None:
            raise RuntimeError("No execution engine created")

        try:
            addr = self.engine.get_function_address(function_name)
            # get_function_address returns 0 if function not found
            if addr == 0:
                raise RuntimeError(f"Function '{function_name}' not found")
            return addr
        except Exception as e:
            if "not found" not in str(e):
                raise RuntimeError(f"Function '{function_name}' not found: {e}") from e
            raise

    def execute_main(self) -> int:
        """Execute main() function and return result.

        Returns:
            Return value from main() as integer

        Raises:
            RuntimeError: If main() not found or execution fails
        """
        # Get function address
        func_addr = self.get_function_address("main")

        # Create ctypes function pointer
        # Signature: i64 main(void)
        cfunc = CFUNCTYPE(c_int64)(func_addr)

        # Execute and return result
        result = cfunc()
        return int(result)

    def execute_function(self, function_name: str, *args: Any) -> Any:
        """Execute arbitrary function with arguments.

        Args:
            function_name: Name of function to execute
            *args: Arguments to pass to function

        Returns:
            Function return value

        Note:
            This is a simplified interface. For complex types,
            you'll need to construct appropriate ctypes signatures.
        """
        func_addr = self.get_function_address(function_name)

        # For now, assume i64 return and i64 arguments
        # This can be extended with type introspection
        arg_types = [c_int64] * len(args)
        cfunc = CFUNCTYPE(c_int64, *arg_types)(func_addr)

        result = cfunc(*args)
        return int(result)

    def cleanup(self) -> None:
        """Clean up execution engine resources."""
        if self.engine:
            # Modules are owned by engine, will be cleaned up
            self.engine = None
            self.modules.clear()


def jit_compile_and_run(llvm_ir_file: str, verbose: bool = False) -> int:
    """Convenience function to JIT compile and execute LLVM IR file.

    Args:
        llvm_ir_file: Path to .ll file
        verbose: Print debug information

    Returns:
        Return value from main() function

    Example:
        >>> result = jit_compile_and_run("fibonacci.ll")
        >>> print(f"Result: {result}")
    """
    executor = LLVMJITExecutor()

    try:
        if verbose:
            print(f"Compiling {llvm_ir_file}...")

        # Compile IR
        mod = executor.compile_ir_file(llvm_ir_file)

        if verbose:
            print("Compilation successful")
            print("Executing main()...")

        # Execute main
        result = executor.execute_main()

        if verbose:
            print(f"Execution complete. Result: {result}")

        return result

    finally:
        executor.cleanup()


if __name__ == "__main__":
    """Example usage of JIT executor."""
    import sys

    if len(sys.argv) < 2:
        print("Usage: python jit_executor.py <llvm_ir_file.ll>")
        sys.exit(1)

    llvm_ir_file = sys.argv[1]

    try:
        result = jit_compile_and_run(llvm_ir_file, verbose=True)
        print(f"\nProgram returned: {result}")
        sys.exit(0)

    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)
