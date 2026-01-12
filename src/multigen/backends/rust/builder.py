"""Rust build system for MultiGen."""

import shutil
import subprocess
from pathlib import Path
from typing import Any

from ..base import AbstractBuilder


class RustBuilder(AbstractBuilder):
    """Rust build system implementation using Cargo."""

    def get_build_filename(self) -> str:
        """Return Cargo.toml as the build file name."""
        return "Cargo.toml"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate Cargo.toml for Rust project."""
        cargo_content = f"""[package]
name = "{target_name}"
version = "0.1.0"
edition = "2021"

[dependencies]
"""
        return cargo_content

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile Rust source directly using rustc."""
        try:
            source_path = Path(source_file).absolute()
            out_dir = Path(output_dir).absolute()
            executable_name = source_path.stem

            # Copy runtime module to the same directory as source file
            runtime_src = Path(__file__).parent / "runtime" / "multigen_rust_runtime.rs"
            if runtime_src.exists():
                # Copy to source directory, not output directory
                runtime_dst = source_path.parent / "multigen_rust_runtime.rs"
                shutil.copy2(runtime_src, runtime_dst)

            # Build rustc command with absolute paths
            cmd = ["rustc", str(source_path), "-o", str(out_dir / executable_name), "--edition", "2021"]

            # Run compilation (don't set cwd to avoid path issues)
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                # Print error for debugging
                if result.stderr:
                    print(f"Rust compilation error: {result.stderr}")
                return False

            return True

        except Exception as e:
            print(f"Rust compilation exception: {e}")
            return False

    def get_compile_flags(self) -> list[str]:
        """Get Rust compilation flags."""
        return ["--edition", "2021", "-O"]
