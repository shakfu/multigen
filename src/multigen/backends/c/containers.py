"""C container system for MultiGen with STC support."""

from pathlib import Path
from typing import Optional

from ..base import AbstractContainerSystem


class CContainerSystem(AbstractContainerSystem):
    """C container system using STC (Smart Template Containers) with fallback."""

    def __init__(self) -> None:
        """Initialize container system with STC preference."""
        self.use_stc = True  # Default to STC, fallback available
        self._stc_available: Optional[bool] = None  # Cache for STC availability check

    def check_stc_availability(self) -> bool:
        """Check if STC library is available in the runtime directory.

        Returns:
            True if STC is available, False otherwise
        """
        if self._stc_available is not None:
            return self._stc_available

        # Get the directory of this file
        current_dir = Path(__file__).parent
        stc_dir = current_dir / "ext" / "stc" / "include" / "stc"

        # Check if STC directory exists and has header files
        self._stc_available = stc_dir.exists() and stc_dir.is_dir()

        if self._stc_available:
            # Verify key STC headers exist
            key_headers = ["vec.h", "map.h", "set.h"]
            for header in key_headers:
                if not (stc_dir / header).exists():
                    self._stc_available = False
                    break

        return self._stc_available

    def set_use_stc(self, use_stc: bool) -> None:
        """Manually set whether to use STC or fallback containers.

        Args:
            use_stc: True to use STC, False to use fallback
        """
        self.use_stc = use_stc

    def auto_detect_stc(self) -> None:
        """Automatically detect STC availability and set use_stc accordingly."""
        self.use_stc = self.check_stc_availability()

    def get_list_type(self, element_type: str) -> str:
        """Get C vector type for element type."""
        if self.use_stc:
            # Use STC vector template
            sanitized_type = self._sanitize_type_name(element_type)
            return f"vec_{sanitized_type}"
        else:
            # Fallback to basic array
            return f"{element_type}*"

    def get_dict_type(self, key_type: str, value_type: str) -> str:
        """Get C map type for key-value storage."""
        if self.use_stc:
            # Use STC map template
            sanitized_key = self._sanitize_type_name(key_type)
            sanitized_value = self._sanitize_type_name(value_type)
            return f"map_{sanitized_key}_{sanitized_value}"
        else:
            # Fallback to basic struct
            return f"struct {{{key_type} key; {value_type} value;}}"

    def get_set_type(self, element_type: str) -> str:
        """Get C set type for set storage."""
        if self.use_stc:
            # Use STC set template
            sanitized_type = self._sanitize_type_name(element_type)
            return f"set_{sanitized_type}"
        else:
            # Fallback to basic array
            return f"{element_type}*"

    def generate_container_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate C container operations using STC or fallback."""
        if self.use_stc:
            return self._generate_stc_operations(container_type, operations)
        else:
            return self._generate_basic_operations(container_type, operations)

    def get_required_imports(self) -> list[str]:
        """Get C headers required for container operations."""
        imports = [
            "#include <stdlib.h>",
            "#include <string.h>",
        ]

        if self.use_stc:
            imports.extend(
                [
                    '#include "multigen_stc_bridge.h"',
                    '#include "multigen_error_handling.h"',
                    '#include "multigen_python_ops.h"',
                    "// STC template headers will be included as needed",
                ]
            )
        else:
            imports.extend(
                [
                    '#include "multigen_error_handling.h"',
                    '#include "multigen_memory_ops.h"',
                    '#include "multigen_containers_fallback.h"',
                ]
            )

        return imports

    def generate_container_declarations(self, containers: list[tuple]) -> str:
        """Generate STC container template declarations."""
        if not self.use_stc:
            return "// Basic array-based containers - no declarations needed"

        declarations = []
        declarations.append("// STC container template declarations")
        declarations.append("#define STC_ENABLED")
        declarations.append("")

        # Generate STC template declarations for each container type
        seen_types = set()
        for _container_name, element_type in containers:
            if element_type not in seen_types:
                sanitized_type = self._sanitize_type_name(element_type)

                # Vector declaration
                declarations.append(f"#define i_type vec_{sanitized_type}")
                declarations.append(f"#define i_val {element_type}")
                declarations.append('#include "stc/vec.h"')
                declarations.append("")

                # Map declaration (for string keys)
                declarations.append(f"#define i_type map_str_{sanitized_type}")
                declarations.append("#define i_key_str")
                declarations.append(f"#define i_val {element_type}")
                declarations.append('#include "stc/map.h"')
                declarations.append("")

                # Set declaration
                declarations.append(f"#define i_type set_{sanitized_type}")
                declarations.append(f"#define i_key {element_type}")
                declarations.append('#include "stc/set.h"')
                declarations.append("")

                seen_types.add(element_type)

        # Special string container
        if "str" not in seen_types:
            declarations.append("// String containers")
            declarations.append("#define i_type vec_cstr")
            declarations.append("#define i_val_str")
            declarations.append('#include "stc/vec.h"')
            declarations.append("")

        # Include STC bridge implementation
        declarations.append("// Include MultiGen STC bridge helpers")
        declarations.append("MGEN_IMPLEMENT_STRING_SPLIT_HELPERS()")
        declarations.append("")

        return "\n".join(declarations)

    def _sanitize_type_name(self, type_name: str) -> str:
        """Sanitize type name for use in STC template names."""
        # Convert C types to safe identifier names
        type_map = {
            "int": "int",
            "char": "char",
            "float": "float",
            "double": "double",
            "char*": "str",
            "const char*": "cstr",
            "string": "str",
            "bool": "bool",
        }
        return type_map.get(type_name, type_name.replace("*", "ptr").replace(" ", "_"))

    def _generate_stc_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate STC-based container operations."""
        operations_code = []
        operations_code.append(f"// STC operations for {container_type}")

        for op in operations:
            if op == "append":
                operations_code.append(f"// {container_type}_push(&container, element);")
            elif op == "insert":
                operations_code.append(f"// {container_type}_insert_at(&container, index, element);")
            elif op == "remove":
                operations_code.append(f"// {container_type}_erase_at(&container, index);")
            elif op == "get":
                operations_code.append(f"// element = *{container_type}_at(&container, index);")
            elif op == "set":
                operations_code.append(f"// *{container_type}_at(&container, index) = element;")
            elif op == "size":
                operations_code.append(f"// size = {container_type}_size(&container);")
            elif op == "clear":
                operations_code.append(f"// {container_type}_clear(&container);")
            elif op == "contains":
                operations_code.append(f"// found = {container_type}_contains(&container, element);")

        operations_code.append("")
        operations_code.append("// Use MGEN_VEC_AT_SAFE for bounds checking")
        operations_code.append("// Use MGEN_IN_VEC for 'in' operator")
        operations_code.append("// Use MGEN_VEC_ENUMERATE for iteration")

        return "\n".join(operations_code)

    def _generate_basic_operations(self, container_type: str, operations: list[str]) -> str:
        """Generate basic fallback container operations using multigen_dyn_array."""
        operations_code = []
        operations_code.append(f"// Fallback operations for {container_type} using multigen_dyn_array")
        operations_code.append("// Include fallback container runtime:")
        operations_code.append('// #include "multigen_containers_fallback.h"')
        operations_code.append("")

        for op in operations:
            if op == "append":
                operations_code.append("// Append operation:")
                operations_code.append("// multigen_dyn_array_append(array, &element);")
            elif op == "insert":
                operations_code.append("// Insert operation:")
                operations_code.append("// multigen_dyn_array_insert(array, index, &element);")
            elif op == "remove":
                operations_code.append("// Remove operation:")
                operations_code.append("// multigen_dyn_array_remove(array, index);")
            elif op == "get":
                operations_code.append("// Get operation (bounds-checked):")
                operations_code.append("// element = *(type*)multigen_dyn_array_get(array, index);")
            elif op == "set":
                operations_code.append("// Set operation (bounds-checked):")
                operations_code.append("// multigen_dyn_array_set(array, index, &element);")
            elif op == "size":
                operations_code.append("// Size operation:")
                operations_code.append("// size = multigen_dyn_array_size(array);")
            elif op == "clear":
                operations_code.append("// Clear operation:")
                operations_code.append("// multigen_dyn_array_clear(array);")
            elif op == "contains":
                operations_code.append("// Contains operation (linear search):")
                operations_code.append("// found = multigen_dyn_array_contains(array, &element);")

        operations_code.append("")
        operations_code.append("// Example: Creating a typed dynamic array")
        operations_code.append("// multigen_dyn_array_t* int_array = multigen_dyn_array_new(sizeof(int), 0);")
        operations_code.append("// int value = 42;")
        operations_code.append("// multigen_dyn_array_append(int_array, &value);")
        operations_code.append("// int* retrieved = (int*)multigen_dyn_array_get(int_array, 0);")
        operations_code.append("// multigen_dyn_array_free(int_array);")

        return "\n".join(operations_code)

    def generate_container_includes(self) -> str:
        """Generate includes needed for containers."""
        if self.use_stc:
            return """// MultiGen runtime includes
#include "multigen_error_handling.h"
#include "multigen_python_ops.h"
#include "multigen_memory_ops.h"
#include "multigen_stc_bridge.h"

// STC will be included via template declarations
"""
        else:
            return """// MultiGen runtime includes (basic fallback)
#include "multigen_error_handling.h"
#include "multigen_memory_ops.h"
#include "multigen_containers_fallback.h"
"""
