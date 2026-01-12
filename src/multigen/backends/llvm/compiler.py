"""LLVM IR compiler using llvmlite.

This module provides functionality to compile LLVM IR to machine code
using llvmlite's binding to LLVM.
"""

import subprocess
import tempfile
from pathlib import Path
from typing import Optional

from llvmlite import binding as llvm  # type: ignore[import-untyped]


class LLVMCompiler:
    """Compile LLVM IR to machine code using llvmlite."""

    def __init__(self) -> None:
        """Initialize the LLVM compiler."""
        # Initialize native target
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        # Create target machine for native platform
        target = llvm.Target.from_default_triple()
        self.target_machine = target.create_target_machine()

    def compile_ir_to_object(self, llvm_ir: str, output_path: Optional[str] = None) -> bytes:
        """Compile LLVM IR to object file.

        Args:
            llvm_ir: LLVM IR as string
            output_path: Optional path to write object file

        Returns:
            Object file bytes
        """
        # Parse and verify LLVM IR
        llvm_module = llvm.parse_assembly(llvm_ir)
        llvm_module.verify()

        # Compile to object file
        obj_bytes = self.target_machine.emit_object(llvm_module)

        # Write to file if path provided
        if output_path:
            Path(output_path).write_bytes(obj_bytes)

        return obj_bytes

    def compile_ir_to_executable(
        self,
        llvm_ir: str,
        output_path: str,
        linker: str = "clang",
        link_args: Optional[list[str]] = None,
        enable_asan: bool = False,
    ) -> bool:
        """Compile LLVM IR to executable.

        Args:
            llvm_ir: LLVM IR as string
            output_path: Path to output executable
            linker: Linker to use (default: clang)
            link_args: Additional linker arguments
            enable_asan: Enable AddressSanitizer for memory error detection

        Returns:
            True if successful, False otherwise
        """
        # Create temporary object file
        with tempfile.NamedTemporaryFile(suffix=".o", delete=False) as obj_file:
            obj_path = obj_file.name
            obj_bytes = self.compile_ir_to_object(llvm_ir, obj_path)

        try:
            # Link to executable
            cmd = [linker, obj_path, "-o", output_path]

            # Add ASAN flags if requested
            if enable_asan:
                cmd.extend(["-fsanitize=address", "-g"])

            if link_args:
                cmd.extend(link_args)

            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise RuntimeError(f"Linking failed: {result.stderr}")

            return True

        finally:
            # Clean up temporary object file
            Path(obj_path).unlink(missing_ok=True)

    def compile_and_run(
        self,
        llvm_ir: str,
        capture_output: bool = True,
    ) -> subprocess.CompletedProcess:
        """Compile LLVM IR and execute it.

        Args:
            llvm_ir: LLVM IR as string
            capture_output: Whether to capture stdout/stderr

        Returns:
            CompletedProcess with execution results
        """
        # Create temporary executable
        with tempfile.NamedTemporaryFile(suffix="", delete=False) as exe_file:
            exe_path = exe_file.name

        try:
            # Compile to executable
            self.compile_ir_to_executable(llvm_ir, exe_path)

            # Make executable
            Path(exe_path).chmod(0o755)

            # Execute
            result = subprocess.run(
                [exe_path],
                capture_output=capture_output,
                text=True,
            )

            return result

        finally:
            # Clean up temporary executable
            Path(exe_path).unlink(missing_ok=True)

    def get_target_triple(self) -> str:
        """Get the target triple for this compiler.

        Returns:
            Target triple string (e.g., "arm64-apple-darwin24.6.0")
        """
        return self.target_machine.triple
