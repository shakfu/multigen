"""LLVM builder for compilation and execution."""

import re
import shutil
import subprocess
from pathlib import Path
from typing import Any, Optional

from ..base import AbstractBuilder


class LLVMBuilder(AbstractBuilder):
    """Builder for compiling LLVM IR to native binaries."""

    def __init__(self) -> None:
        """Initialize the LLVM builder."""
        self.llc_path = self._find_llvm_tool("llc")
        self.clang_path = self._find_llvm_tool("clang")

    def _find_llvm_tool(self, tool_name: str) -> str:
        """Find LLVM tool in common locations.

        Args:
            tool_name: Name of the tool (e.g., 'llc', 'clang')

        Returns:
            Path to the tool or just the tool name if not found
        """
        # First check if it's in PATH
        tool_path = shutil.which(tool_name)
        if tool_path:
            return tool_path

        # Check common Homebrew LLVM installation paths
        homebrew_paths = [
            Path("/opt/homebrew/opt/llvm/bin") / tool_name,  # Apple Silicon
            Path("/usr/local/opt/llvm/bin") / tool_name,  # Intel Mac
        ]

        for path in homebrew_paths:
            if path.exists():
                return str(path)

        # Fall back to just the tool name (will fail if not in PATH)
        return tool_name

    def get_build_filename(self) -> str:
        """Get the build file name.

        Returns:
            Name of build file (Makefile)
        """
        return "Makefile"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate a Makefile for LLVM IR compilation.

        Args:
            source_files: List of LLVM IR (.ll) files
            target_name: Name of output executable

        Returns:
            Makefile content as string
        """
        ll_files = " ".join(Path(f).name for f in source_files if f.endswith(".ll"))

        makefile = f"""# Generated Makefile for LLVM IR compilation
# Target: {target_name}

LLC = llc
CLANG = clang
TARGET = {target_name}
LLVM_IR = {ll_files}
OBJECT_FILES = $(LLVM_IR:.ll=.o)

# Default target
all: $(TARGET)

# Compile LLVM IR to object files
%.o: %.ll
\t$(LLC) -filetype=obj $< -o $@

# Link object files to create executable
$(TARGET): $(OBJECT_FILES)
\t$(CLANG) $(OBJECT_FILES) -o $(TARGET)

# Clean build artifacts
clean:
\trm -f $(OBJECT_FILES) $(TARGET)

# Run the program
run: $(TARGET)
\t./$(TARGET)

.PHONY: all clean run
"""
        return makefile

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile LLVM IR directly to native binary.

        Args:
            source_file: Path to LLVM IR (.ll) file
            output_dir: Directory for output files
            **kwargs: Additional options:
                - enable_asan (bool): Enable AddressSanitizer for memory error detection
                - opt_level (int): Optimization level (0=O0, 1=O1, 2=O2, 3=O3, default=2)

        Returns:
            True if compilation succeeded
        """
        enable_asan = kwargs.get("enable_asan", False)
        opt_level = kwargs.get("opt_level", 2)
        try:
            # Use absolute paths to avoid cwd issues
            source_path = Path(source_file).resolve()
            output_path = Path(output_dir).resolve()
            executable_name = source_path.stem

            # Step 0: Apply LLVM optimization passes to IR
            from .optimizer import LLVMOptimizer

            llvm_ir = source_path.read_text()
            optimizer = LLVMOptimizer(opt_level=opt_level)
            optimized_ir = optimizer.optimize(llvm_ir)

            # Write optimized IR to a new file
            optimized_path = output_path / f"{executable_name}.opt.ll"
            optimized_path.write_text(optimized_ir)

            # Step 1: Compile optimized LLVM IR to object file using llc
            object_file = output_path / f"{executable_name}.o"
            llc_cmd = [
                self.llc_path,
                "-filetype=obj",
                str(optimized_path),  # Use optimized IR instead of original
                "-o",
                str(object_file),
            ]

            result = subprocess.run(llc_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"LLC compilation failed: {result.stderr}")
                return False

            # Step 2: Compile runtime libraries
            runtime_dir = Path(__file__).parent / "runtime"
            runtime_objects = []

            # Compile each runtime C file
            runtime_sources = [
                "vec_int_minimal.c",
                "vec_vec_int_minimal.c",
                "vec_str_minimal.c",
                "map_str_int_minimal.c",
                "map_int_int_minimal.c",
                "set_int_minimal.c",
                "multigen_llvm_string.c",
            ]

            for runtime_source in runtime_sources:
                runtime_c = runtime_dir / runtime_source
                if runtime_c.exists():
                    runtime_o = output_path / runtime_source.replace(".c", ".o")
                    clang_compile_runtime_cmd = [
                        self.clang_path,
                        "-c",
                        str(runtime_c),
                        "-o",
                        str(runtime_o),
                        "-I",
                        str(runtime_dir),  # Include runtime headers
                    ]

                    # Add ASAN flags if requested
                    if enable_asan:
                        clang_compile_runtime_cmd.extend(["-fsanitize=address", "-g"])

                    result = subprocess.run(clang_compile_runtime_cmd, capture_output=True, text=True)
                    if result.returncode != 0:
                        print(f"Runtime compilation failed for {runtime_source}: {result.stderr}")
                        return False

                    runtime_objects.append(str(runtime_o))

            # Step 3: Link object files to create executable using clang
            executable_path = output_path / executable_name
            clang_cmd = [
                self.clang_path,
                str(object_file),
                *runtime_objects,
                "-o",
                str(executable_path),
            ]

            # Add ASAN flags to linker if requested
            if enable_asan:
                clang_cmd.extend(["-fsanitize=address", "-g"])

            result = subprocess.run(clang_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"Linking failed: {result.stderr}")
                return False

            return True

        except FileNotFoundError as e:
            print(f"Tool not found: {e}. Make sure LLVM tools (llc, clang) are installed.")
            return False
        except Exception as e:
            print(f"Compilation error: {e}")
            return False

    def get_compile_flags(self) -> list[str]:
        """Get LLVM compilation flags.

        Returns:
            List of compilation flags
        """
        return [
            "-filetype=obj",  # Generate object file
        ]

    def get_optimization_level(self, level: int = 2) -> list[str]:
        """Get optimization flags for specified level.

        Args:
            level: Optimization level (0-3)

        Returns:
            List of optimization flags
        """
        return [f"-O{level}"]

    def generate_main_wrapper(self, llvm_ir_file: str) -> Optional[str]:
        """Generate C wrapper for LLVM main function if needed.

        Args:
            llvm_ir_file: Path to LLVM IR file

        Returns:
            C wrapper code if main function exists with quotes, None otherwise
        """
        # Read LLVM IR to check for quoted main function
        ir_path = Path(llvm_ir_file)
        if not ir_path.exists():
            return None

        ir_content = ir_path.read_text()

        # Check if there's a quoted main function: define ... @"main"(...)
        main_pattern = r'define\s+(\w+)\s+@"main"\s*\(([^)]*)\)'
        match = re.search(main_pattern, ir_content)

        if not match:
            return None

        return_type = match.group(1)
        params = match.group(2).strip()

        # Map LLVM types to C types
        c_return_type = self._llvm_type_to_c(return_type)

        # Parse parameters if any
        c_params = ""
        if params:
            # For now, assume no parameters for main
            c_params = "void"
        else:
            c_params = "void"

        # Generate wrapper that calls the LLVM-generated function
        # Since the LLVM function is named "main" with quotes, we need to provide
        # a C main that can link properly

        # The trick: The LLVM @"main" mangles to "main" in the object file,
        # but we can't have two main functions. So we'll use __attribute__((weak))
        # or just not generate a wrapper and rely on proper function naming.

        # For now, let's create a wrapper that assumes the LLVM function
        # will be available and we'll link them together properly
        wrapper = f"""// Auto-generated C wrapper for LLVM IR main function

// Forward declare the multigen-generated main function
extern {c_return_type} main({c_params});

// C standard main function
int main_wrapper(int argc, char** argv) {{
    (void)argc;  // Unused
    (void)argv;  // Unused
    {c_return_type} result = main();
    return (int)result;
}}
"""
        return wrapper

    def _llvm_type_to_c(self, llvm_type: str) -> str:
        """Map LLVM type to C type.

        Args:
            llvm_type: LLVM type string (e.g., 'i64', 'double', 'void')

        Returns:
            C type string
        """
        type_map = {
            "i1": "char",
            "i8": "char",
            "i16": "short",
            "i32": "int",
            "i64": "long long",
            "float": "float",
            "double": "double",
            "void": "void",
        }
        return type_map.get(llvm_type, "long long")  # Default to long long

    def compile_with_wrapper(self, source_file: str, output_dir: str) -> bool:
        """Compile LLVM IR with C wrapper for main function.

        Args:
            source_file: Path to LLVM IR (.ll) file
            output_dir: Directory for output files

        Returns:
            True if compilation succeeded
        """
        try:
            source_path = Path(source_file)
            output_path = Path(output_dir)
            executable_name = source_path.stem

            # Check if we need a wrapper
            wrapper_code = self.generate_main_wrapper(source_file)

            # Step 1: Compile LLVM IR to object file
            object_file = output_path / f"{executable_name}.o"
            llc_cmd = [
                self.llc_path,
                "-filetype=obj",
                str(source_path),
                "-o",
                str(object_file),
            ]

            result = subprocess.run(llc_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"LLC compilation failed: {result.stderr}")
                return False

            # Step 2: Compile wrapper if needed
            object_files = [str(object_file)]
            if wrapper_code:
                wrapper_file = output_path / f"{executable_name}_wrapper.c"
                wrapper_file.write_text(wrapper_code)

                wrapper_obj = output_path / f"{executable_name}_wrapper.o"
                clang_compile_cmd = [
                    self.clang_path,
                    "-c",
                    str(wrapper_file),
                    "-o",
                    str(wrapper_obj),
                ]

                result = subprocess.run(clang_compile_cmd, capture_output=True, text=True)
                if result.returncode != 0:
                    print(f"Wrapper compilation failed: {result.stderr}")
                    return False

                object_files.append(str(wrapper_obj))

            # Step 3: Link all object files
            executable_path = output_path / executable_name

            # If we have a wrapper, define the entry point
            link_flags = []
            if wrapper_code:
                link_flags = ["-e", "_multigen_main"]

            clang_cmd = [
                self.clang_path,
                *object_files,
                *link_flags,
                "-o",
                str(executable_path),
            ]

            result = subprocess.run(clang_cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"Linking failed: {result.stderr}")
                return False

            return True

        except FileNotFoundError as e:
            print(f"Tool not found: {e}. Make sure LLVM tools are installed.")
            return False
        except Exception as e:
            print(f"Compilation error: {e}")
            return False
