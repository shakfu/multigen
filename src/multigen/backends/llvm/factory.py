"""LLVM factory for creating backend components."""

from typing import Optional

from ..base import AbstractBuilder, AbstractEmitter, AbstractFactory
from .builder import LLVMBuilder
from .emitter import LLVMEmitter


class LLVMFactory(AbstractFactory):
    """Factory for creating LLVM backend components."""

    def create_emitter(self) -> AbstractEmitter:
        """Create an LLVM emitter.

        Returns:
            LLVM emitter instance
        """
        return LLVMEmitter()

    def create_builder(self) -> AbstractBuilder:
        """Create an LLVM builder.

        Returns:
            LLVM builder instance
        """
        return LLVMBuilder()

    def create_variable(self, name: str, type_name: str, value: Optional[str] = None) -> str:
        """Create LLVM variable declaration.

        Args:
            name: Variable name
            type_name: LLVM type (e.g., 'i64', 'double')
            value: Optional initial value

        Returns:
            LLVM IR for variable allocation
        """
        if value:
            return f"%{name} = alloca {type_name}\nstore {type_name} {value}, {type_name}* %{name}"
        return f"%{name} = alloca {type_name}"

    def create_function_signature(self, name: str, params: list[tuple], return_type: str) -> str:
        """Create LLVM function signature.

        Args:
            name: Function name
            params: List of (param_name, param_type) tuples
            return_type: LLVM return type

        Returns:
            LLVM IR function signature
        """
        param_strs = [f"{ptype} %{pname}" for pname, ptype in params]
        params_str = ", ".join(param_strs)
        return f"define {return_type} @{name}({params_str})"

    def create_comment(self, text: str) -> str:
        """Create LLVM comment.

        Args:
            text: Comment text

        Returns:
            LLVM IR comment
        """
        return f"; {text}"

    def create_include(self, library: str) -> str:
        """Create LLVM module declaration (LLVM doesn't have includes).

        Args:
            library: Library name (unused for LLVM)

        Returns:
            Empty string (LLVM IR doesn't have includes)
        """
        # LLVM IR doesn't have includes like C/C++
        # External functions are declared with 'declare' statements
        return ""
