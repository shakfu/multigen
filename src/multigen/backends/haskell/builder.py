"""Haskell build system for MultiGen."""

import shutil
import subprocess
from pathlib import Path
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
        try:
            source_path = Path(source_file).absolute()
            out_dir = Path(output_dir).absolute()
            executable_name = source_path.stem

            # Determine source directory (where .hs files are)
            source_dir = source_path.parent

            # Copy runtime module to source directory (GHC looks for imports there)
            runtime_src = Path(__file__).parent / "runtime" / "MultiGenRuntime.hs"
            if runtime_src.exists():
                runtime_dst = source_dir / "MultiGenRuntime.hs"
                shutil.copy2(runtime_src, runtime_dst)

            # Build GHC command with absolute paths
            cmd = [
                "ghc",
                str(source_path),
                "-o",
                str(out_dir / executable_name),
                "-XOverloadedStrings",
                "-XFlexibleInstances",
                "-XTypeSynonymInstances",
                "-package",
                "containers",
            ]

            # Add runtime module path if it exists
            runtime_path = source_dir / "MultiGenRuntime.hs"
            if runtime_path.exists():
                cmd.extend([str(runtime_path)])

            # Run compilation (don't set cwd to avoid path issues)
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                # Print error for debugging
                if result.stderr:
                    print(f"Haskell compilation error: {result.stderr}")
                return False

            return True

        except Exception as e:
            print(f"Haskell compilation exception: {e}")
            return False

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
