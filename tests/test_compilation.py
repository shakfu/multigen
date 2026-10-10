"""Compilation tests - verify all backends generate compilable, executable code.

This test suite is part of Phase 1: Compilation Verification.
It ensures that:
1. Generated code compiles without errors
2. Compiled executables run successfully
3. Output matches expected results
"""

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Optional

import pytest

from multigen.backends.registry import registry
from multigen.pipeline import BuildMode, MultiGenPipeline, PipelineConfig

# A registered backend is not a usable one: registration only needs the Python
# module, while compiling needs the target toolchain on PATH. CI runners ship
# gcc/g++/rustc/go/ghc but not ocamlc, so these tests have to ask about the
# compiler rather than the registry, the way the LLVM and Deno suites do.
BACKEND_COMPILERS = {
    "c": "gcc",
    "cpp": "g++",
    "rust": "rustc",
    "go": "go",
    "haskell": "ghc",
    "ocaml": "ocamlc",
}


def _ocaml_toolchain_available() -> bool:
    """Check for ocamlc directly or through an initialized opam switch."""
    if shutil.which("ocamlc"):
        return True
    if not shutil.which("opam"):
        return False
    try:
        result = subprocess.run(
            ["opam", "exec", "--", "ocamlc", "-version"], capture_output=True, text=True, timeout=30
        )
    except (OSError, subprocess.SubprocessError):
        return False
    return result.returncode == 0


def toolchain_available(backend: str) -> bool:
    """Report whether the compiler this backend shells out to is installed."""
    if backend == "ocaml":
        return _ocaml_toolchain_available()
    compiler = BACKEND_COMPILERS.get(backend)
    if compiler is None:
        return True
    return shutil.which(compiler) is not None


class CompilationTestHelper:
    """Helper class for compilation testing."""

    @staticmethod
    def get_fixture_path(filename: str) -> Path:
        """Get path to a test fixture file."""
        return Path(__file__).parent / "fixtures" / "compilation" / filename

    @staticmethod
    def compile_and_run(
        source_file: Path, backend: str, expected_output: str, timeout: int = 10
    ) -> tuple[bool, str, str]:
        """Generate, compile, and run code for a backend.

        Returns:
            (success, stdout, stderr)
        """
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        if not toolchain_available(backend):
            pytest.skip(f"Toolchain for backend {backend} not installed")

        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)

            # Generate code
            try:
                pipeline = MultiGenPipeline(
                    target_language=backend, config=PipelineConfig(target_language=backend, build_mode=BuildMode.DIRECT)
                )
                result = pipeline.convert(source_file, output_path=output_dir)

                if not result.success:
                    error_msg = "; ".join(result.errors) if result.errors else "Unknown error"
                    return False, "", f"Code generation failed: {error_msg}"

            except Exception as e:
                return False, "", f"Pipeline error: {str(e)}"

            # Get the executable path from result
            if result.executable_path:
                executable = Path(result.executable_path)
            else:
                # Fallback: search for executable
                executable = CompilationTestHelper._find_executable(output_dir, source_file.stem, backend)

            if not executable or not executable.exists():
                return (
                    False,
                    "",
                    f"No executable found. Result executable_path: {result.executable_path}, searched in: {output_dir}",
                )

            # Run the executable
            try:
                process = subprocess.run(
                    [str(executable)], capture_output=True, text=True, timeout=timeout, cwd=output_dir
                )

                stdout = process.stdout.strip()
                stderr = process.stderr.strip()

                # Check if output matches expected
                if stdout == expected_output.strip():
                    return True, stdout, stderr
                else:
                    return False, stdout, f"Output mismatch. Expected: '{expected_output}', Got: '{stdout}'"

            except subprocess.TimeoutExpired:
                return False, "", f"Execution timeout after {timeout}s"
            except Exception as e:
                return False, "", f"Execution error: {str(e)}"

    @staticmethod
    def _find_executable(output_dir: Path, base_name: str, backend: str) -> Optional[Path]:
        """Find the generated executable."""
        # Common patterns for executables
        candidates = [
            output_dir / base_name,  # Unix executable
            output_dir / f"{base_name}.exe",  # Windows executable
            output_dir / "target" / "debug" / base_name,  # Rust
            output_dir / "target" / "release" / base_name,  # Rust release
        ]

        for candidate in candidates:
            if candidate.exists() and candidate.is_file():
                # Check if it's executable (Unix) or exists (Windows)
                if candidate.stat().st_mode & 0o111 or candidate.suffix == ".exe":
                    return candidate

        return None


# Differential testing: CPython is the oracle for every program and backend.
TESTS_DIR = Path(__file__).parent
DIFFERENTIAL_PROGRAMS = sorted(
    [*(TESTS_DIR / "translation").glob("*.py"), *(TESTS_DIR / "fixtures" / "differential").glob("*.py")]
)
# TypeScript is left out: its xfail entries cannot be measured without deno.
DIFFERENTIAL_BACKENDS = ["c", "cpp", "rust", "go", "haskell", "ocaml", "llvm"]

# Known (backend, program) failures. strict=True turns a fix into an XPASS
# failure, so an entry must be deleted once its pair passes.
KNOWN_FAILURES: dict[tuple[str, str], str] = {
    (
        "c",
        "containers.py",
    ): "generation: C backend does not support: Only for loops with range() or container iteration supported",
    ("c", "int_width.py"): "int is 32-bit: 2**40 wraps to 0",
    ("c", "comprehension_filters.py"): "generation: Non-range iterables in dict comprehensions not yet supported",
    ("c", "assignment_targets.py"): "compile: expected expression before '{' token",
    ("cpp", "int_width.py"): "int is 32-bit: 2**40 wraps to 0",
    ("cpp", "test_dataclass_basic.py"): "compile: no matching function for call to 'Point::Point(int, int)'",
    ("cpp", "test_dict_comprehension.py"): "compile: 'str' was not declared in this scope; did you mean 'std'?",
    ("cpp", "test_math_import.py"): "compile: 'math' was not declared in this scope",
    ("cpp", "test_namedtuple_basic.py"): "compile: no matching function for call to 'Coordinate::Coordinate(int, int)'",
    ("cpp", "test_string_membership.py"): "compile: 'std::string' has no member named 'count'",
    ("cpp", "test_string_membership_simple.py"): "compile: 'std::string' has no member named 'count'",
    ("cpp", "test_string_methods.py"): "compile: 'std::string' has no member named 'count'",
    ("cpp", "test_string_methods_new.py"): "compile: 'std::string' has no member named 'count'",
    ("cpp", "test_struct_field_access.py"): "compile: 'class Rectangle' has no member named 'width'",
    ("go", "container_iteration_test.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_container_iteration.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_dataclass_basic.py"): 'compile: "multigenproject/multigen" imported and not used',
    (
        "go",
        "test_dict_comprehension.py",
    ): "compile: cannot use multigen.DictComprehensionFromRange[int, int](multigen.NewRange(3), func(x int)",
    ("go", "test_list_comprehension.py"): "compile: declared and not used: count1",
    ("go", "test_math_import.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_namedtuple_basic.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_set_support.py"): "compile: numbers.Add undefined (type map[int]bool has no field or method Add)",
    ("go", "test_string_membership.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_string_membership_simple.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("go", "test_string_methods.py"): "compile: assignment mismatch: 2 variables but 1 value",
    ("go", "test_string_methods_new.py"): "compile: assignment mismatch: 2 variables but 1 value",
    ("go", "test_struct_field_access.py"): 'compile: "multigenproject/multigen" imported and not used',
    ("haskell", "class_mutation.py"): "compile: type mismatch",
    (
        "haskell",
        "container_iteration_test.py",
    ): "generation: for loop at line 9 in pure function 'testContainerIteration' matches no fold pattern",
    ("haskell", "containers.py"): "generation: for loop at line 6 in pure function 'wordCount' matches no fold pattern",
    ("haskell", "exceptions.py"): "compile: parse error",
    ("haskell", "int_width.py"): "generation: for loop at line 6 in pure function 'big' matches no fold pattern",
    (
        "haskell",
        "nested_2d_params.py",
    ): "generation: for loop at line 7 in pure function 'sumMatrix' matches no fold pattern",
    (
        "haskell",
        "nested_2d_return.py",
    ): "generation: for loop at line 9 in pure function 'createIdentity' matches no fold pattern",
    (
        "haskell",
        "nested_containers_comprehensive.py",
    ): "generation: for loop at line 18 in pure function 'test2dListFunctionParams' matches no fold pattern",
    ("haskell", "test_2d_simple.py"): "compile: parse error",
    (
        "haskell",
        "test_container_iteration.py",
    ): "generation: for loop at line 9 in pure function 'testListIteration' matches no fold pattern",
    ("haskell", "test_control_flow.py"): "generation: While loops not directly supported in Haskell",
    ("haskell", "test_dataclass_basic.py"): "compile: multiple declarations",
    ("haskell", "test_math_import.py"): "compile: parse error",
    ("haskell", "test_namedtuple_basic.py"): "compile: multiple declarations",
    ("haskell", "test_set_support.py"): "compile: parse error",
    ("haskell", "test_simple_string_ops.py"): "compile: parse error",
    ("haskell", "test_string_membership.py"): "compile: no instance",
    ("haskell", "test_string_membership_simple.py"): "compile: no instance",
    ("haskell", "test_string_methods.py"): "compile: type mismatch",
    ("haskell", "test_string_methods_new.py"): "compile: type mismatch",
    ("haskell", "test_string_split_simple.py"): "compile: parse error",
    ("haskell", "test_struct_field_access.py"): "compile: multiple declarations",
    ("haskell", "loop_rebinding.py"): "generation: for loop in pure function 'doubled' matches no fold pattern",
    ("ocaml", "class_mutation.py"): "compile: Unbound value self",
    ("ocaml", "container_iteration_test.py"): "compile: This expression has type int array",
    ("ocaml", "containers.py"): "compile: This function has type int -> string",
    (
        "ocaml",
        "nested_containers_comprehensive.py",
    ): "compile: This function has type int -> Multigen_runtime.Range.range",
    ("ocaml", "recursion_and_lists.py"): "compile: This expression has type int array",
    ("ocaml", "simple_infer_test.py"): "compile: This expression has type int array",
    ("ocaml", "test_container_iteration.py"): "compile: This expression has type int array",
    ("ocaml", "test_control_flow.py"): "compile: This expression has type unit but an expression was expected of type",
    (
        "ocaml",
        "test_dataclass_basic.py",
    ): "generation: OCaml backend does not support class 'Point': fields declared in the class body (dataclass",
    ("ocaml", "test_dict_comprehension.py"): "compile: Unbound value str",
    ("ocaml", "test_list_comprehension.py"): "compile: This expression has type int list",
    ("ocaml", "test_math_import.py"): "generation: Unsupported statement: Import",
    (
        "ocaml",
        "test_namedtuple_basic.py",
    ): "generation: OCaml backend does not support class 'Coordinate': fields declared in the class body (data",
    ("ocaml", "test_set_support.py"): "compile: This expression has type 'a list",
    (
        "ocaml",
        "test_string_membership.py",
    ): "compile: This expression has type string but an expression was expected of type",
    (
        "ocaml",
        "test_string_membership_simple.py",
    ): "compile: This expression has type string but an expression was expected of type",
    (
        "ocaml",
        "test_string_methods.py",
    ): "compile: This expression has type string but an expression was expected of type",
    (
        "ocaml",
        "test_string_methods_new.py",
    ): "compile: This expression has type string but an expression was expected of type",
    ("ocaml", "test_string_split_simple.py"): "compile: This expression has type string list",
    ("ocaml", "comprehensions.py"): "compile: This expression has type int array",
    ("ocaml", "comprehension_filters.py"): "compile: This expression has type int array",
    ("ocaml", "assignment_targets.py"): "compile: This function has type int -> string",
    (
        "ocaml",
        "test_struct_field_access.py",
    ): "generation: OCaml backend does not support class 'Rectangle': fields declared in the class body (datac",
    ("llvm", "class_mutation.py"): "generation: LLVM backend does not support module-level ClassDef",
    ("llvm", "container_iteration_test.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "containers.py"): "generation: LLVM backend does not support expression type: Slice",
    ("llvm", "exceptions.py"): "generation: LLVM backend does not support module-level If",
    ("llvm", "int_width.py"): "generation: LLVM backend does not support module-level If",
    ("llvm", "nested_2d_params.py"): 'generation: Type of #1 arg mismatch: %"struct.vec_int"* != i64',
    ("llvm", "nested_2d_return.py"): 'generation: Type of #2 arg mismatch: i64 != %"struct.vec_int"*',
    ("llvm", "nested_2d_simple.py"): 'generation: Type of #2 arg mismatch: i64 != %"struct.vec_int"*',
    ("llvm", "nested_containers_comprehensive.py"): 'generation: Type of #2 arg mismatch: i64 != %"struct.vec_int"*',
    ("llvm", "recursion_and_lists.py"): "generation: LLVM backend does not support module-level If",
    ("llvm", "simple_infer_test.py"): "generation: LLVM backend cannot translate this Assign at line 2: numbers = []",
    ("llvm", "simple_test.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "string_methods_test.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_container_iteration.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_control_flow.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_dataclass_basic.py"): "generation: LLVM backend does not support module-level ClassDef",
    ("llvm", "test_dict_comprehension.py"): "generation: AST expression Call not implemented in comprehensions",
    ("llvm", "test_list_comprehension.py"): "exits 13 with correct stdout",
    ("llvm", "test_list_slicing.py"): "generation: LLVM backend does not support expression type: Slice",
    ("llvm", "test_math_import.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_namedtuple_basic.py"): "generation: LLVM backend does not support module-level ClassDef",
    ("llvm", "test_set_support.py"): "generation: LLVM backend does not support expression type: Set",
    ("llvm", "test_simple_slice.py"): "generation: LLVM backend does not support expression type: Slice",
    ("llvm", "test_simple_string_ops.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_string_membership.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_string_membership_simple.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_string_methods.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_string_methods_new.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_string_split_simple.py"): "generation: LLVM backend does not support statement type: Assert",
    ("llvm", "test_struct_field_access.py"): "generation: LLVM backend does not support expression type: Attribute",
    ("llvm", "comprehensions.py"): "generation: LLVM backend does not support expression type: Set",
    ("llvm", "comprehension_filters.py"): "generation: fails with an empty error message",
}


def _differential_params() -> list:
    params = []
    for backend in DIFFERENTIAL_BACKENDS:
        for program in DIFFERENTIAL_PROGRAMS:
            reason = KNOWN_FAILURES.get((backend, program.name))
            marks = [pytest.mark.xfail(strict=True, reason=reason)] if reason else []
            params.append(pytest.param(backend, program, marks=marks, id=f"{backend}-{program.stem}"))
    return params


def cpython_stdout(program: Path) -> str:
    """Run a program under CPython and return its stdout.

    The backends call `main()` whether or not a `__main__` guard exists, so an
    unguarded program has it called here too. Its return value is discarded,
    as the compiled programs discard it.
    """
    if "__main__" in program.read_text():
        script = "import runpy, sys; runpy.run_path(sys.argv[1], run_name='__main__')"
    else:
        script = "import runpy, sys; runpy.run_path(sys.argv[1])['main']()"
    proc = subprocess.run([sys.executable, "-c", script, str(program)], capture_output=True, text=True, timeout=30)
    assert proc.returncode == 0, proc.stderr
    return proc.stdout


@pytest.mark.slow
@pytest.mark.parametrize("backend,program", _differential_params())
def test_compiled_output_matches_cpython(backend: str, program: Path, tmp_path: Path) -> None:
    """Compiled stdout must equal CPython's, with exit status 0."""
    if not toolchain_available(backend):
        pytest.skip(f"Toolchain for backend {backend} not installed")
    expected = cpython_stdout(program)

    result = MultiGenPipeline(config=PipelineConfig(target_language=backend, build_mode=BuildMode.DIRECT)).convert(
        program, output_path=tmp_path
    )
    assert result.success, result.errors
    executable = Path(result.executable_path or "")
    assert executable.is_file(), f"no executable at {executable}"

    proc = subprocess.run([str(executable)], capture_output=True, text=True, timeout=30, cwd=tmp_path)
    assert (proc.stdout, proc.returncode) == (expected, 0), proc.stderr


class TestCBackendCompilation:
    """Test C backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "c", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "c", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestCppBackendCompilation:
    """Test C++ backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "cpp", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "cpp", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestRustBackendCompilation:
    """Test Rust backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "rust", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "rust", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestGoBackendCompilation:
    """Test Go backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "go", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "go", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestHaskellBackendCompilation:
    """Test Haskell backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "haskell", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "haskell", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestOCamlBackendCompilation:
    """Test OCaml backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "ocaml", "16")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, "ocaml", "HELLO")

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestCrossBackendConsistency:
    """Test that all backends produce consistent results."""

    @pytest.mark.parametrize("backend", ["c", "cpp", "rust", "go", "haskell", "ocaml"])
    def test_simple_math_consistency(self, backend):
        """Test that all backends produce the same output for simple math."""
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        if not toolchain_available(backend):
            pytest.skip(f"Toolchain for backend {backend} not installed")

        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, backend, "16")

        assert success, f"Backend {backend} failed: {stderr}"
        assert stdout == "16", f"Backend {backend} output mismatch: got '{stdout}'"

    @pytest.mark.parametrize("backend", ["c", "cpp", "rust", "go", "haskell", "ocaml"])
    def test_string_ops_consistency(self, backend):
        """Test that all backends produce the same output for string operations."""
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        if not toolchain_available(backend):
            pytest.skip(f"Toolchain for backend {backend} not installed")

        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(source, backend, "HELLO")

        assert success, f"Backend {backend} failed: {stderr}"
        assert stdout == "HELLO", f"Backend {backend} output mismatch: got '{stdout}'"
