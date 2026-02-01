"""Haskell build system for MultiGen."""

from typing import Any

from ..base import AbstractBuilder


class HaskellBuilder(AbstractBuilder):
    """Haskell build system implementation using Cabal."""

    def get_build_filename(self) -> str:
        """Return cabal project file name."""
        return "multigen-project.cabal"

    def generate_build_file(self, source_files: list[str], target_name: str) -> str:
        """Generate Cabal file for Haskell project."""
        cabal_content = f"""cabal-version: 2.4

name: {target_name}
version: 0.1.0.0
synopsis: Generated Haskell project from MultiGen
description: Automatically generated Haskell code from Python source using MultiGen
license: MIT
author: MultiGen
maintainer: multigen@example.com
build-type: Simple

executable {target_name}
    main-is: Main.hs
    default-language: Haskell2010
    default-extensions:
        OverloadedStrings
        FlexibleInstances
        TypeSynonymInstances
    build-depends:
        base ^>=4.16,
        containers,
        text
    ghc-options:
        -Wall
        -Wcompat
        -Widentities
        -Wincomplete-record-updates
        -Wincomplete-uni-patterns
        -Wmissing-export-lists
        -Wmissing-home-modules
        -Wpartial-fields
        -Wredundant-constraints
"""
        return cabal_content

    def compile_direct(self, source_file: str, output_dir: str, **kwargs: Any) -> bool:
        """Compile Haskell source directly using GHC."""
        # Resolve paths using base class helper
        paths = self._resolve_paths(source_file, output_dir)
        source_dir = paths.source_path.parent

        # Copy runtime module to source directory (GHC looks for imports there)
        runtime_path = self._copy_runtime_file("MultiGenRuntime.hs", source_dir)

        # Build GHC command
        cmd = [
            "ghc",
            str(paths.source_path),
            "-o",
            str(paths.executable_path),
            "-XOverloadedStrings",
            "-XFlexibleInstances",
            "-XTypeSynonymInstances",
            "-package",
            "containers",
        ]

        # Add runtime module path if it was copied
        if runtime_path is not None:
            cmd.append(str(runtime_path))

        # Run compilation using base class helper
        result = self._run_command(cmd)
        return result.success

    def get_compile_flags(self) -> list[str]:
        """Get Haskell compilation flags."""
        return [
            "-O2",  # Optimization
            "-XOverloadedStrings",
            "-XFlexibleInstances",
            "-XTypeSynonymInstances",
            "-Wall",  # Enable warnings
            "-fwarn-incomplete-patterns",
        ]
