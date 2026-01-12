"""Compilation tests - verify all backends generate compilable, executable code.

This test suite is part of Phase 1: Compilation Verification.
It ensures that:
1. Generated code compiles without errors
2. Compiled executables run successfully
3. Output matches expected results
"""

import subprocess
import tempfile
from pathlib import Path
from typing import Optional

import pytest

from multigen.pipeline import MultiGenPipeline, PipelineConfig, BuildMode
from multigen.backends.registry import registry


class CompilationTestHelper:
    """Helper class for compilation testing."""

    @staticmethod
    def get_fixture_path(filename: str) -> Path:
        """Get path to a test fixture file."""
        return Path(__file__).parent / "fixtures" / "compilation" / filename

    @staticmethod
    def compile_and_run(
        source_file: Path,
        backend: str,
        expected_output: str,
        timeout: int = 10
    ) -> tuple[bool, str, str]:
        """
        Generate, compile, and run code for a backend.

        Returns:
            (success, stdout, stderr)
        """
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)

            # Generate code
            try:
                pipeline = MultiGenPipeline(
                    target_language=backend,
                    config=PipelineConfig(
                        target_language=backend,
                        build_mode=BuildMode.DIRECT
                    )
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
                return False, "", f"No executable found. Result executable_path: {result.executable_path}, searched in: {output_dir}"

            # Run the executable
            try:
                process = subprocess.run(
                    [str(executable)],
                    capture_output=True,
                    text=True,
                    timeout=timeout,
                    cwd=output_dir
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
            output_dir / base_name,           # Unix executable
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


class TestCBackendCompilation:
    """Test C backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "c", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "c", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestCppBackendCompilation:
    """Test C++ backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "cpp", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "cpp", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestRustBackendCompilation:
    """Test Rust backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "rust", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "rust", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestGoBackendCompilation:
    """Test Go backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "go", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "go", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestHaskellBackendCompilation:
    """Test Haskell backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "haskell", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "haskell", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestOCamlBackendCompilation:
    """Test OCaml backend compilation."""

    def test_simple_math_compiles(self):
        """Test that simple math operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "ocaml", "16"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "16", f"Expected output '16', got '{stdout}'"

    def test_string_ops_compiles(self):
        """Test that string operations compile and run."""
        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, "ocaml", "HELLO"
        )

        assert success, f"Compilation/execution failed: {stderr}"
        assert stdout == "HELLO", f"Expected output 'HELLO', got '{stdout}'"


class TestCrossBackendConsistency:
    """Test that all backends produce consistent results."""

    @pytest.mark.parametrize("backend", ["c", "cpp", "rust", "go", "haskell", "ocaml"])
    def test_simple_math_consistency(self, backend):
        """Test that all backends produce the same output for simple math."""
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        source = CompilationTestHelper.get_fixture_path("simple_math.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, backend, "16"
        )

        assert success, f"Backend {backend} failed: {stderr}"
        assert stdout == "16", f"Backend {backend} output mismatch: got '{stdout}'"

    @pytest.mark.parametrize("backend", ["c", "cpp", "rust", "go", "haskell", "ocaml"])
    def test_string_ops_consistency(self, backend):
        """Test that all backends produce the same output for string operations."""
        if not registry.has_backend(backend):
            pytest.skip(f"Backend {backend} not available")

        source = CompilationTestHelper.get_fixture_path("string_ops.py")
        success, stdout, stderr = CompilationTestHelper.compile_and_run(
            source, backend, "HELLO"
        )

        assert success, f"Backend {backend} failed: {stderr}"
        assert stdout == "HELLO", f"Backend {backend} output mismatch: got '{stdout}'"
