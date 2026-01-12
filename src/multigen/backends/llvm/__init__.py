"""LLVM IR backend for MultiGen.

This backend converts MultiGen's Static Python IR to LLVM IR using llvmlite,
enabling compilation to native binaries, WebAssembly, and other LLVM targets.
"""

from .backend import LLVMBackend
from .compiler import LLVMCompiler
from .ir_to_llvm import IRToLLVMConverter

__all__ = ["LLVMBackend", "IRToLLVMConverter", "LLVMCompiler"]
