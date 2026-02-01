"""Rust build system for MultiGen."""

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
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)

        # Copy runtime module to source directory (rustc looks for modules there)
        self._copy_runtime_file("multigen_rust_runtime.rs", paths.source_path.parent)

        # Build rustc command
        cmd = [
            "rustc",
            str(paths.source_path),
            "-o",
            str(paths.executable_path),
            "--edition",
            "2021",
        ]

        # Run compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_compile_flags(self) -> list[str]:
        """Get Rust compilation flags."""
        return ["--edition", "2021", "-O"]
