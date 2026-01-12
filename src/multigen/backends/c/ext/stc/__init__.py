"""STC (Smart Template Containers) Integration.

This module provides integration with the STC C library for high-performance,
type-safe container operations in generated C code.

STC is a modern, header-only C99 container library that provides:
- Dynamic arrays (vec)
- Hash maps and sets (hmap, hset)
- Strings with UTF-8 support (cstr)
- Smart pointers (arc, box)
- And many other containers

Repository: https://github.com/stclib/STC
License: MIT (see LICENSE file)
"""

import os
from pathlib import Path

# Get the directory containing STC headers and source
STC_BASE_DIR = Path(__file__).parent
STC_INCLUDE_DIR = STC_BASE_DIR / "include"
STC_SRC_DIR = STC_BASE_DIR / "src"


def get_stc_include_path() -> str:
    """Get the path to STC include directory."""
    return str(STC_INCLUDE_DIR)


def get_stc_src_path() -> str:
    """Get the path to STC source directory."""
    return str(STC_SRC_DIR)


def get_stc_headers() -> list:
    """Get list of available STC header files."""
    stc_headers_dir = STC_INCLUDE_DIR / "stc"
    if stc_headers_dir.exists():
        return [f.name for f in stc_headers_dir.glob("*.h")]
    return []


def get_stc_sources() -> list:
    """Get list of available STC source files."""
    if STC_SRC_DIR.exists():
        return [f.name for f in STC_SRC_DIR.glob("*.c")]
    return []


__all__ = [
    "get_stc_include_path",
    "get_stc_src_path",
    "get_stc_headers",
    "get_stc_sources",
    "STC_INCLUDE_DIR",
    "STC_SRC_DIR",
]
