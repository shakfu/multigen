#!/usr/bin/env python3
"""Automated benchmark runner for MultiGen backends.

This script runs benchmarks across all backends and collects performance metrics:
- Execution time (wall clock)
- Compilation time
- Binary size
- Lines of generated code
- Memory usage (basic)
"""

import argparse
import json
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from multigen.pipeline import MultiGenPipeline, PipelineConfig, BuildMode, OptimizationLevel
from multigen.backends.registry import registry


class BenchmarkMetrics:
    """Container for benchmark metrics."""

    def __init__(self, name: str, backend: str):
        self.name = name
        self.backend = backend
        self.compilation_time: float = 0.0
        self.execution_time: float = 0.0
        self.binary_size: int = 0
        self.lines_of_code: int = 0
        self.success: bool = False
        self.output: str = ""
        self.error: str = ""


class BenchmarkRunner:
    """Automated benchmark runner for all backends."""

    def __init__(self, output_dir: Path):
        self.output_dir = output_dir
        self.output_dir.mkdir(exist_ok=True)
        self.results: list[BenchmarkMetrics] = []

    def copy_runtime_libraries(self, build_dir: Path, backend: str) -> None:
        """Copy runtime libraries for the backend."""
        # Each backend has its own runtime directory
        src_runtime_dir = Path(__file__).parent.parent / "src" / "multigen" / "backends" / backend / "runtime"

        if src_runtime_dir.exists():
            # C++ expects runtime files in a runtime/ subdirectory
            if backend == "cpp":
                dest_runtime_dir = build_dir / "runtime"
                dest_runtime_dir.mkdir(exist_ok=True)
                for src_file in src_runtime_dir.glob("*"):
                    if src_file.is_file():
                        dest_file = dest_runtime_dir / src_file.name
                        shutil.copy2(src_file, dest_file)
            elif backend == "go":
                # Go expects runtime in multigen/ subdirectory with go.mod
                multigen_dir = build_dir / "multigen"
                multigen_dir.mkdir(exist_ok=True)
                for src_file in src_runtime_dir.glob("*"):
                    if src_file.is_file():
                        dest_file = multigen_dir / src_file.name
                        shutil.copy2(src_file, dest_file)
                # Create go.mod file
                go_mod_content = "module multigenproject\n\ngo 1.21\n"
                (build_dir / "go.mod").write_text(go_mod_content)
            elif backend == "llvm":
                # LLVM only needs vec_int_minimal.c (compilation handles linking)
                # Runtime is compiled during benchmark compilation, not copied
                pass  # No runtime files to copy
            else:
                # Other backends: copy directly to build directory
                for src_file in src_runtime_dir.glob("*"):
                    if src_file.is_file():
                        dest_file = build_dir / src_file.name
                        shutil.copy2(src_file, dest_file)

    def count_lines(self, file_path: Path) -> int:
        """Count lines of code in a file."""
        try:
            with open(file_path, "r") as f:
                return len(f.readlines())
        except Exception:
            return 0

    def get_binary_size(self, binary_path: Path) -> int:
        """Get size of compiled binary in bytes."""
        try:
            return binary_path.stat().st_size
        except Exception:
            return 0

    def find_generated_file(self, backend: str, benchmark_name: str) -> Path | None:
        """Find the generated source file for a backend."""
        backend_dir = self.output_dir / backend / benchmark_name
        extensions = {
            "c": ".c",
            "cpp": ".cpp",
            "rust": ".rs",
            "go": ".go",
            "haskell": ".hs",
            "ocaml": ".ml",
            "llvm": ".ll",
        }
        ext = extensions.get(backend)
        if not ext:
            return None

        # Look for the main file
        source_file = backend_dir / f"{benchmark_name}{ext}"
        if source_file.exists():
            return source_file

        # Look for any file with the extension
        files = list(backend_dir.glob(f"*{ext}"))
        return files[0] if files else None

    def find_binary(self, backend: str, benchmark_name: str, result=None) -> Path | None:
        """Find the compiled binary for a backend."""
        backend_dir = (self.output_dir / backend / benchmark_name).absolute()

        # Try result.executable_path first if available
        if result and result.executable_path:
            exe_path = Path(result.executable_path).absolute()
            if exe_path.exists() and exe_path.is_file():
                return exe_path

        # Common binary names and locations
        candidates = [
            backend_dir / benchmark_name,           # Unix executable
            backend_dir / f"{benchmark_name}.exe",  # Windows executable
            backend_dir / "target" / "debug" / "multigenproject",  # Rust debug
            backend_dir / "target" / "debug" / benchmark_name,  # Rust with name
            backend_dir / "a.out",                   # Default C/C++ output
            backend_dir / benchmark_name.replace("_", "-"),  # Kebab case
        ]

        for candidate in candidates:
            candidate = candidate.absolute()
            if candidate.exists() and candidate.is_file():
                # Check if it's executable (Unix) or exists (Windows)
                try:
                    if candidate.stat().st_mode & 0o111 or candidate.suffix == ".exe":
                        return candidate
                except:
                    if candidate.exists():
                        return candidate

        return None

    def compile_benchmark(
        self, source_file: Path, backend: str, benchmark_name: str
    ) -> tuple[bool, float, str, Any]:
        """Compile a benchmark and measure compilation time.

        Returns:
            (success, compilation_time, error, pipeline_result)
        """
        start_time = time.time()

        try:
            output_dir = self.output_dir / backend / benchmark_name
            output_dir.mkdir(parents=True, exist_ok=True)

            # Copy runtime libraries before compiling
            self.copy_runtime_libraries(output_dir, backend)

            # Step 1: Generate code only (no build)
            config = PipelineConfig(
                target_language=backend,
                build_mode=BuildMode.NONE
            )

            pipeline = MultiGenPipeline(config=config, target_language=backend)
            result = pipeline.convert(source_file, output_path=output_dir)

            if not result.success:
                compilation_time = time.time() - start_time
                error = "; ".join(result.errors) if result.errors else "Unknown error"
                return False, compilation_time, error, None

            # Step 2: Manually compile the generated code
            generated_file = self.find_generated_file(backend, benchmark_name)
            if not generated_file:
                compilation_time = time.time() - start_time
                return False, compilation_time, "Generated file not found", None

            # Compile based on backend
            success, compile_error = self._manual_compile(backend, generated_file, output_dir, benchmark_name)

            compilation_time = time.time() - start_time

            if not success:
                error_msg = f"Compilation failed: {compile_error}" if compile_error else "Manual compilation failed"
                return False, compilation_time, error_msg, None

            return True, compilation_time, "", result

        except Exception as e:
            compilation_time = time.time() - start_time
            return False, compilation_time, str(e), None

    def _manual_compile(self, backend: str, source_file: Path, output_dir: Path, executable_name: str) -> tuple[bool, str]:
        """Manually compile generated code.

        Returns:
            (success, error_message)
        """
        try:
            if backend == "c":
                # Compile C code - single-header containers but need runtime .c files
                # Containers are header-only, but other runtime files still have .c implementations
                project_root = Path(__file__).parent.parent
                runtime_path = project_root / "src" / "multigen" / "backends" / "c" / "runtime"
                c_backend_path = project_root / "src" / "multigen" / "backends" / "c"
                stc_include_path = project_root / "src" / "multigen" / "backends" / "c" / "ext" / "stc" / "include"

                # Find non-container runtime .c files (containers are header-only now)
                runtime_c_files = [
                    f for f in runtime_path.glob("*.c")
                    if not f.name.startswith("multigen_vec_") and
                       not f.name.startswith("multigen_set_") and
                       not f.name.startswith("multigen_map_")
                ]

                cmd = [
                    "gcc", "-Wall", "-Wextra", "-std=gnu11", "-O2",
                    f"-I{output_dir.absolute()}",
                    f"-I{runtime_path.absolute()}",
                    f"-I{c_backend_path.absolute()}",
                    f"-I{stc_include_path.absolute()}",
                    str(source_file.absolute()),
                    *[str(f.absolute()) for f in runtime_c_files],
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode != 0:
                    return False, result.stderr[:200]  # First 200 chars of error
                return True, ""

            elif backend == "cpp":
                # Compile C++ code - header-only runtime
                cmd = [
                    "g++", "-Wall", "-Wextra", "-std=c++17", "-O2",
                    f"-I{output_dir.absolute()}",
                    str(source_file.absolute()),
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            elif backend == "rust":
                # Compile Rust code - rustc with runtime module
                cmd = [
                    "rustc",
                    "-O",
                    str(source_file.absolute()),
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True, cwd=output_dir)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            elif backend == "go":
                # Compile Go code - go build with module support
                cmd = [
                    "go", "build",
                    "-o", str((output_dir / executable_name).absolute()),
                    str(source_file.absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True, cwd=output_dir)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            elif backend == "haskell":
                # Compile Haskell code - ghc with runtime module
                runtime_files = list(output_dir.glob("*.hs"))
                runtime_files = [f for f in runtime_files if f != source_file]
                cmd = [
                    "ghc", "-O2",
                    str(source_file.absolute()),
                    *[str(f.absolute()) for f in runtime_files],
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True, cwd=output_dir)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            elif backend == "ocaml":
                # Compile OCaml code - ocamlopt with runtime module via opam
                runtime_files = list(output_dir.glob("multigen_*.ml"))
                cmd = [
                    "opam", "exec", "--",
                    "ocamlopt",
                    *[str(f.absolute()) for f in runtime_files],
                    str(source_file.absolute()),
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True, cwd=output_dir)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            elif backend == "llvm":
                # Compile LLVM IR - llc + clang with vec_int runtime
                project_root = Path(__file__).parent.parent
                runtime_path = project_root / "src" / "multigen" / "backends" / "llvm" / "runtime"

                # Runtime C files - include all required runtime libraries
                runtime_c_files = [
                    runtime_path / "vec_int_minimal.c",
                    runtime_path / "vec_vec_int_minimal.c",
                    runtime_path / "vec_str_minimal.c",
                    runtime_path / "map_int_int_minimal.c",
                    runtime_path / "map_str_int_minimal.c",
                    runtime_path / "set_int_minimal.c",
                    runtime_path / "multigen_llvm_string.c",
                ]

                # Find llc (try Homebrew path first, then system)
                llc_paths = [
                    "/opt/homebrew/opt/llvm/bin/llc",
                    "llc"
                ]
                llc_cmd = next((p for p in llc_paths if shutil.which(p) or Path(p).exists()), None)
                if not llc_cmd:
                    return False, "llc not found"

                # Compile LLVM IR to object file
                obj_file = output_dir / f"{executable_name}.o"
                cmd = [llc_cmd, "-filetype=obj", str(source_file.absolute()), "-o", str(obj_file.absolute())]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode != 0:
                    return False, result.stderr[:200]

                # Compile runtime libraries
                runtime_objs = []
                for runtime_c_file in runtime_c_files:
                    if not runtime_c_file.exists():
                        continue  # Skip if doesn't exist
                    runtime_obj = output_dir / f"{runtime_c_file.stem}.o"
                    cmd = ["clang", "-c", "-o", str(runtime_obj.absolute()), str(runtime_c_file.absolute())]
                    result = subprocess.run(cmd, capture_output=True, text=True)
                    if result.returncode != 0:
                        return False, result.stderr[:200]
                    runtime_objs.append(runtime_obj)

                # Link object files
                cmd = [
                    "clang",
                    str(obj_file.absolute()),
                    *[str(obj.absolute()) for obj in runtime_objs],
                    "-o", str((output_dir / executable_name).absolute())
                ]
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode != 0:
                    return False, result.stderr[:200]
                return True, ""

            # Other backends would be handled here
            return False, f"Backend {backend} not supported for manual compilation"

        except Exception as e:
            return False, str(e)

    def run_benchmark(
        self, backend: str, benchmark_name: str
    ) -> tuple[bool, float, str, str]:
        """Run a compiled benchmark and measure execution time."""
        binary = self.find_binary(backend, benchmark_name)
        if not binary:
            return False, 0.0, "", "Binary not found"

        try:
            # Run the binary and measure time
            start_time = time.time()
            result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=30,
                cwd=binary.parent,
            )
            execution_time = time.time() - start_time

            success = result.returncode == 0
            return success, execution_time, result.stdout.strip(), result.stderr

        except subprocess.TimeoutExpired:
            return False, 30.0, "", "Timeout (30s)"
        except Exception as e:
            return False, 0.0, "", str(e)

    def benchmark_single(self, source_file: Path, backend: str) -> BenchmarkMetrics:
        """Run a single benchmark for a specific backend."""
        benchmark_name = source_file.stem
        metrics = BenchmarkMetrics(benchmark_name, backend)

        print(f"  [{backend}] {benchmark_name}...", end=" ", flush=True)

        # Compile and get result
        success, comp_time, error, result = self.compile_benchmark(
            source_file, backend, benchmark_name
        )
        metrics.compilation_time = comp_time

        if not success:
            metrics.error = error or "Compilation failed"
            print(f"COMPILE FAILED ({comp_time:.2f}s)")
            return metrics

        # Count LOC
        generated_file = self.find_generated_file(backend, benchmark_name)
        if generated_file:
            metrics.lines_of_code = self.count_lines(generated_file)

        # Find the compiled binary
        binary = self.find_binary(backend, benchmark_name, result)

        if binary and binary.exists():
            metrics.binary_size = self.get_binary_size(binary)

            # Run benchmark
            try:
                start_time = time.time()
                proc_result = subprocess.run(
                    [str(binary)],
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=binary.parent,
                )
                exec_time = time.time() - start_time

                metrics.execution_time = exec_time
                metrics.output = proc_result.stdout.strip()
                metrics.success = proc_result.returncode == 0

                if not metrics.success:
                    metrics.error = proc_result.stderr
                    print(f"RUN FAILED ({comp_time:.2f}s compile)")
                else:
                    print(
                        f"OK ({comp_time:.2f}s compile, {exec_time:.3f}s run, {metrics.binary_size // 1024}KB)"
                    )
            except subprocess.TimeoutExpired:
                metrics.error = "Timeout (30s)"
                print(f"TIMEOUT ({comp_time:.2f}s compile)")
            except Exception as e:
                metrics.error = str(e)
                print(f"ERROR ({comp_time:.2f}s compile)")
        else:
            metrics.error = f"Binary not found (searched in {self.output_dir / backend / benchmark_name})"
            print(f"BINARY NOT FOUND ({comp_time:.2f}s compile)")

        return metrics

    def benchmark_all(self, benchmark_files: list[Path], backends: list[str]) -> None:
        """Run all benchmarks for all backends."""
        print(f"\nRunning {len(benchmark_files)} benchmarks across {len(backends)} backends...\n")

        for benchmark_file in benchmark_files:
            print(f"Benchmark: {benchmark_file.stem}")
            for backend in backends:
                metrics = self.benchmark_single(benchmark_file, backend)
                self.results.append(metrics)
            print()

    def generate_summary(self) -> dict[str, Any]:
        """Generate summary statistics."""
        summary: dict[str, Any] = {
            "total_benchmarks": len(set(m.name for m in self.results)),
            "total_backends": len(set(m.backend for m in self.results)),
            "successful_runs": sum(1 for m in self.results if m.success),
            "failed_runs": sum(1 for m in self.results if not m.success),
            "by_backend": {},
        }

        for backend in set(m.backend for m in self.results):
            backend_results = [m for m in self.results if m.backend == backend]
            successful = [m for m in backend_results if m.success]

            summary["by_backend"][backend] = {
                "total": len(backend_results),
                "successful": len(successful),
                "failed": len(backend_results) - len(successful),
                "avg_compilation_time": (
                    sum(m.compilation_time for m in successful) / len(successful)
                    if successful
                    else 0
                ),
                "avg_execution_time": (
                    sum(m.execution_time for m in successful) / len(successful)
                    if successful
                    else 0
                ),
                "avg_binary_size": (
                    sum(m.binary_size for m in successful) / len(successful)
                    if successful
                    else 0
                ),
                "avg_lines_of_code": (
                    sum(m.lines_of_code for m in successful) / len(successful)
                    if successful
                    else 0
                ),
            }

        return summary

    def save_json_report(self, output_file: Path) -> None:
        """Save detailed results as JSON."""
        data = {
            "summary": self.generate_summary(),
            "results": [
                {
                    "name": m.name,
                    "backend": m.backend,
                    "success": m.success,
                    "compilation_time": m.compilation_time,
                    "execution_time": m.execution_time,
                    "binary_size": m.binary_size,
                    "lines_of_code": m.lines_of_code,
                    "output": m.output,
                    "error": m.error,
                }
                for m in self.results
            ],
        }

        with open(output_file, "w") as f:
            json.dump(data, f, indent=2)

        print(f"\nDetailed results saved to: {output_file}")

    def print_summary(self) -> None:
        """Print summary table to console."""
        summary = self.generate_summary()

        print("\n" + "=" * 80)
        print("BENCHMARK SUMMARY")
        print("=" * 80)
        print(
            f"Total: {summary['total_benchmarks']} benchmarks × {summary['total_backends']} backends = {len(self.results)} runs"
        )
        print(
            f"Success: {summary['successful_runs']} | Failed: {summary['failed_runs']}"
        )
        print()

        # Print backend comparison table
        print(f"{'Backend':<12} {'Success':<8} {'Compile (s)':<12} {'Run (s)':<12} {'Binary (KB)':<12} {'LOC':<8}")
        print("-" * 80)

        for backend, stats in sorted(summary["by_backend"].items()):
            print(
                f"{backend:<12} "
                f"{stats['successful']}/{stats['total']:<7} "
                f"{stats['avg_compilation_time']:<12.3f} "
                f"{stats['avg_execution_time']:<12.6f} "
                f"{stats['avg_binary_size'] / 1024:<12.1f} "
                f"{stats['avg_lines_of_code']:<8.0f}"
            )

        print("=" * 80)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Run MultiGen benchmarks")
    parser.add_argument(
        "--benchmarks",
        type=str,
        help="Directory containing benchmarks (default: benchmarks/)",
        default="tests/benchmarks",
    )
    parser.add_argument(
        "--output",
        type=str,
        help="Output directory for results (default: benchmark_results/)",
        default="build/benchmark_results",
    )
    parser.add_argument(
        "--backends",
        type=str,
        nargs="+",
        help="Backends to test (default: all)",
        default=None,
    )
    parser.add_argument(
        "--category",
        type=str,
        choices=["algorithms", "data_structures", "all"],
        default="all",
        help="Benchmark category to run",
    )

    args = parser.parse_args()

    # Get benchmark files
    benchmark_dir = Path(args.benchmarks)
    if not benchmark_dir.exists():
        print(f"Error: Benchmark directory not found: {benchmark_dir}")
        return 1

    benchmark_files: list[Path] = []
    if args.category == "all":
        benchmark_files = list(benchmark_dir.glob("**/*.py"))
    else:
        category_dir = benchmark_dir / args.category
        if category_dir.exists():
            benchmark_files = list(category_dir.glob("*.py"))

    if not benchmark_files:
        print(f"Error: No benchmark files found in {benchmark_dir}")
        return 1

    # Get backends
    backends = args.backends or registry.list_backends()

    # Create runner and run benchmarks
    output_dir = Path(args.output)
    runner = BenchmarkRunner(output_dir)

    runner.benchmark_all(benchmark_files, backends)
    runner.print_summary()
    runner.save_json_report(output_dir / "benchmark_results.json")

    return 0


if __name__ == "__main__":
    sys.exit(main())
