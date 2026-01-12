"""Container Code Generator - Prototype.

This module generates type-specific container implementations directly into
the output C file, eliminating the need for external runtime libraries.

Philosophy: Code generators should produce self-contained, complete code.
Similar to C++ template monomorphization or Rust generic instantiation.

Status: Prototype - runs in parallel with existing runtime library approach
"""

from pathlib import Path
from typing import Optional

from .template_substitution import TemplateSubstitutionEngine
from .type_parameter_extractor import TypeParameterExtractor


class ContainerCodeGenerator:
    """Generate type-specific container implementations inline."""

    def __init__(self) -> None:
        """Initialize code generator with templates from runtime library."""
        self.runtime_dir = Path(__file__).parent / "runtime"
        self.template_dir = self.runtime_dir / "templates"
        self._template_cache: dict[str, str] = {}

        # Initialize template system
        self.extractor = TypeParameterExtractor()
        self.substitution_engine = TemplateSubstitutionEngine()

    def _load_template(self, filename: str) -> str:
        """Load a runtime library file as a code generation template.

        Args:
            filename: Runtime library filename (e.g., "multigen_str_int_map.c")

        Returns:
            Template content as string
        """
        if filename in self._template_cache:
            return self._template_cache[filename]

        template_path = self.runtime_dir / filename
        with open(template_path, encoding="utf-8") as f:
            content = f.read()

        self._template_cache[filename] = content
        return content

    def _load_generic_template(self, filename: str) -> str:
        """Load a generic template file from templates directory.

        Args:
            filename: Template filename (e.g., "vec_T.h.tmpl")

        Returns:
            Template content as string
        """
        cache_key = f"tmpl_{filename}"
        if cache_key in self._template_cache:
            return self._template_cache[cache_key]

        template_path = self.template_dir / filename
        with open(template_path, encoding="utf-8") as f:
            content = f.read()

        self._template_cache[cache_key] = content
        return content

    def _strip_includes_and_headers(self, code: str) -> str:
        """Strip #include directives from template code.

        These will be generated separately in the main includes section.

        Args:
            code: Template code with includes

        Returns:
            Code with includes removed
        """
        lines = code.split("\n")
        filtered_lines = []

        for line in lines:
            stripped = line.strip()
            # Skip include directives
            if stripped.startswith("#include"):
                continue
            filtered_lines.append(line)

        return "\n".join(filtered_lines)

    def _remove_error_handling_macros(self, code: str) -> str:
        """Remove MGEN_SET_ERROR macro calls for self-contained code.

        For prototype: Replace error macros with simple returns.
        Future: Generate error handling inline or make it optional.

        Args:
            code: Code with MGEN_SET_ERROR calls

        Returns:
            Code with error handling removed
        """
        lines = code.split("\n")
        filtered_lines = []

        for line in lines:
            line.strip()
            # Skip lines with MGEN_SET_ERROR macro
            if "MGEN_SET_ERROR" in line:
                continue
            filtered_lines.append(line)

        return "\n".join(filtered_lines)

    def generate_from_template(self, container_type: str) -> Optional[str]:
        """Generate container code from generic parameterized templates.

        This is the new parameterized approach that replaces hardcoded methods.
        Uses the template system to generate any container type from generic templates.

        Args:
            container_type: Container type identifier (e.g., "vec_int", "map_str_int")

        Returns:
            Generated C code, or None if type cannot be extracted
        """
        # Extract type parameters from container type
        info = self.extractor.extract(container_type)
        if not info:
            return None

        # Determine which template to use based on container family
        if info.family == "vec":
            header_template_name = "vec_T.h.tmpl"
            impl_template_name = "vec_T.c.tmpl"
        elif info.family == "map":
            header_template_name = "map_K_V.h.tmpl"
            impl_template_name = "map_K_V.c.tmpl"
        elif info.family == "set":
            header_template_name = "set_T.h.tmpl"
            impl_template_name = "set_T.c.tmpl"
        elif info.family == "vec_vec":
            # Nested vectors not yet supported by templates
            return None
        else:
            return None

        # Load generic templates
        header_template = self._load_generic_template(header_template_name)
        impl_template = self._load_generic_template(impl_template_name)

        # Substitute placeholders with actual types
        header_code = self.substitution_engine.substitute_from_container_info(header_template, info)
        impl_code = self.substitution_engine.substitute_from_container_info(impl_template, info)

        # Strip includes from implementation
        impl_code = self._strip_includes_and_headers(impl_code)

        # Remove error handling macros for self-contained code
        impl_code = self._remove_error_handling_macros(impl_code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in header_code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            f"// ========== Generated Container: {container_type} ==========",
            f"// {info.family} container generated from parameterized template",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// Implementation",
            impl_code.strip(),
            "",
            f"// ========== End of Generated Container: {container_type} ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_str_int_map(self) -> str:
        """Generate complete implementation for string→int hash table.

        This is a prototype that uses the existing runtime library as a template.
        Future versions will support parameterized generation for any key/value types.

        Returns:
            Complete C code for string→int map implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_str_int_map.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and extern C wrappers
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip extern C wrappers
            if (
                stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or (stripped.startswith("#endif") and not in_header_guard)
                or (stripped == "}" and not in_header_guard)
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Return generated container with header comment
        sections = [
            "// ========== Generated Container: str_int_map ==========",
            "// String → int hash table implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_vec_int(self) -> str:
        """Generate complete implementation for integer vector (dynamic array).

        Returns:
            Complete C code for integer vector implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_vec_int.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            "// ========== Generated Container: vec_int ==========",
            "// Integer vector (dynamic array) implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_set_int(self) -> str:
        """Generate complete implementation for integer hash set.

        Returns:
            Complete C code for integer hash set implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_set_int.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            "// ========== Generated Container: set_int ==========",
            "// Integer hash set implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_map_int_int(self) -> str:
        """Generate complete implementation for int→int hash map.

        Returns:
            Complete C code for int→int map implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_map_int_int.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            "// ========== Generated Container: map_int_int ==========",
            "// Integer → Integer hash map implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_vec_vec_int(self) -> str:
        """Generate complete implementation for 2D integer arrays (vector of vectors).

        Returns:
            Complete C code for vec_vec_int implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_vec_vec_int.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            "// ========== Generated Container: vec_vec_int ==========",
            "// 2D integer array (vector of vectors) implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_vec_cstr(self) -> str:
        """Generate complete implementation for string arrays (vector of C strings).

        Returns:
            Complete C code for vec_cstr implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_vec_cstr.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes, extern C
            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        # Combine into generated implementation
        sections = [
            "// ========== Generated Container: vec_cstr ==========",
            "// String array (vector of C strings) implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_vec_float(self) -> str:
        """Generate complete implementation for float arrays.

        Returns:
            Complete C code for vec_float implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_vec_float.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        sections = [
            "// ========== Generated Container: vec_float ==========",
            "// Float vector (dynamic array) implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_vec_double(self) -> str:
        """Generate complete implementation for double arrays.

        Returns:
            Complete C code for vec_double implementation
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_vec_double.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            if stripped.startswith("#ifndef") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#define") and "_H" in stripped:
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            if (
                stripped.startswith("#include")
                or stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("#endif")
                or stripped == "}"
            ):
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        sections = [
            "// ========== Generated Container: vec_double ==========",
            "// Double vector (dynamic array) implementation",
            "// Generated inline for this program (no external dependencies)",
            "",
            clean_code.strip(),
            "",
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_map_str_str(self) -> str:
        """Generate map_str_str (string→string hash map) implementation.

        Returns:
            Generated C code for string→string hash map
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_map_str_str.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") or stripped.startswith("#define") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes and extern C
            if stripped.startswith("#include"):
                continue
            if (
                stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("{")
            ):
                continue
            if stripped.startswith("#endif"):
                continue
            if stripped == "}":  # Closing brace for extern C
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        sections = [
            "// ========== Generated Container: map_str_str ==========",
            clean_code.strip(),
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_set_str(self) -> str:
        """Generate set_str (string hash set) implementation.

        Returns:
            Generated C code for string hash set
        """
        # Load single-header template (contains both interface and implementation)
        header = self._load_template("multigen_set_str.h")

        # Strip includes (we'll handle them separately)
        code = self._strip_includes_and_headers(header)

        # Remove error handling macros for self-contained code
        code = self._remove_error_handling_macros(code)

        # Strip header guards and includes from header
        clean_lines = []
        in_header_guard = False
        for line in code.split("\n"):
            stripped = line.strip()

            # Skip header guards
            if stripped.startswith("#ifndef") or stripped.startswith("#define") and "_H" in stripped:
                in_header_guard = True
                continue
            if stripped.startswith("#endif") and in_header_guard:
                in_header_guard = False
                continue

            # Skip includes and extern C
            if stripped.startswith("#include"):
                continue
            if (
                stripped.startswith("#ifdef __cplusplus")
                or stripped.startswith('extern "C"')
                or stripped.startswith("{")
            ):
                continue
            if stripped.startswith("#endif"):
                continue
            if stripped == "}":  # Closing brace for extern C
                continue

            clean_lines.append(line)

        clean_code = "\n".join(clean_lines)

        sections = [
            "// ========== Generated Container: set_str ==========",
            clean_code.strip(),
            "// ========== End of Generated Container ==========",
            "",
        ]

        return "\n".join(sections)

    def generate_container(self, container_type: str) -> Optional[str]:
        """Generate code for a specific container type.

        Uses the new parameterized template system when possible.
        Falls back to hardcoded methods for types not yet supported by templates.

        Args:
            container_type: Container type identifier

        Returns:
            Generated C code, or None if type not supported
        """
        # Try template-based generation first (new approach)
        template_code = self.generate_from_template(container_type)
        if template_code is not None:
            return template_code

        # Fall back to hardcoded methods for types not yet supported by templates
        if container_type == "map_str_int":
            return self.generate_str_int_map()
        elif container_type == "vec_int":
            return self.generate_vec_int()
        elif container_type == "set_int":
            return self.generate_set_int()
        elif container_type == "map_int_int":
            return self.generate_map_int_int()
        elif container_type == "vec_vec_int":
            return self.generate_vec_vec_int()
        elif container_type == "vec_cstr":
            return self.generate_vec_cstr()
        elif container_type == "vec_float":
            return self.generate_vec_float()
        elif container_type == "vec_double":
            return self.generate_vec_double()
        elif container_type == "map_str_str":
            return self.generate_map_str_str()
        elif container_type == "set_str":
            return self.generate_set_str()

        return None

    def get_required_includes(self, container_type: str) -> list[str]:
        """Get required includes for a container type.

        Args:
            container_type: Container type identifier

        Returns:
            List of required #include directives
        """
        # str_int_map needs: stdlib.h (malloc/free), string.h (strcmp/strdup)
        # vec_int needs: stdlib.h (malloc/free), stdbool.h (bool)
        # set_int needs: stdlib.h (malloc/free), stdbool.h (bool)
        # map_int_int needs: stdlib.h (malloc/free), stdbool.h (bool)
        # vec_vec_int needs: stdlib.h (malloc/free), stdbool.h (bool)
        # vec_cstr needs: stdlib.h (malloc/free), string.h (strdup), stdbool.h (bool)
        # vec_float needs: stdlib.h (malloc/free), stdbool.h (bool)
        # vec_double needs: stdlib.h (malloc/free), stdbool.h (bool)
        # map_str_str needs: stdlib.h (malloc/free), string.h (strcmp/strdup), stdbool.h (bool)
        # set_str needs: stdlib.h (malloc/free), string.h (strcmp/strdup), stdbool.h (bool)
        # These are already in standard includes, but we track them for completeness
        if container_type in ["map_str_int", "vec_cstr", "map_str_str", "set_str"]:
            return ["<stdlib.h>", "<string.h>", "<stdbool.h>"]
        elif container_type in ["vec_int", "set_int", "map_int_int", "vec_vec_int", "vec_float", "vec_double"]:
            return ["<stdlib.h>", "<stdbool.h>"]

        return []


# Example usage and testing
if __name__ == "__main__":
    """Test the container code generator."""

    generator = ContainerCodeGenerator()

    # Generate str_int_map implementation

    code = generator.generate_str_int_map()

    # Print first 50 lines to verify
    lines = code.split("\n")
    for _i, _line in enumerate(lines[:50], 1):
        pass
