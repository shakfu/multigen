"""Template substitution engine for parameterized container code generation.

This module substitutes type-specific placeholders in generic templates:
- {{T}} -> actual type (e.g., "int", "char*")
- {{T_SUFFIX}} -> type suffix for names (e.g., "int", "str")
- {{T_ZERO}} -> zero value (e.g., "0", "NULL")
- Conditional blocks: {{#T_NEEDS_DROP}}...{{/T_NEEDS_DROP}}
"""

import re
from typing import Any

from .type_parameter_extractor import ContainerTypeInfo
from .type_properties import get_type_properties


class TemplateSubstitutionEngine:
    """Engine for substituting placeholders in templates."""

    # Regex patterns for placeholders and conditional blocks
    PLACEHOLDER_PATTERN = re.compile(r"\{\{([A-Z_]+)\}\}")
    CONDITIONAL_START_PATTERN = re.compile(r"\{\{#([A-Z_]+)\}\}")
    CONDITIONAL_END_PATTERN = re.compile(r"\{\{/([A-Z_]+)\}\}")

    def __init__(self) -> None:
        """Initialize the substitution engine."""
        pass

    def substitute_vec_template(self, template: str, elem_type: str) -> str:
        """Substitute placeholders in a vector template.

        Args:
            template: Template string with placeholders
            elem_type: Element type name (e.g., "int", "str", "float")

        Returns:
            Template with substitutions applied

        Example:
            >>> engine = TemplateSubstitutionEngine()
            >>> template = "typedef struct { {{T}}* data; } vec_{{T_SUFFIX}};"
            >>> engine.substitute_vec_template(template, "int")
            'typedef struct { int* data; } vec_int;'
        """
        props = get_type_properties(elem_type)

        # Build substitution context
        context = {
            "T": props.c_type,
            "T_SUFFIX": props.suffix,
            "T_ZERO": props.zero_value,
            "T_PRINTF": props.printf_fmt,
            "T_COMPARE": props.compare_op,
            "T_HASH": props.hash_fn,
            "T_NEEDS_DROP": props.needs_drop,
            "T_NEEDS_COPY": props.needs_copy,
            "T_IS_POINTER": props.is_pointer,
        }

        return self._substitute(template, context)

    def substitute_map_template(self, template: str, key_type: str, val_type: str) -> str:
        """Substitute placeholders in a map template.

        Args:
            template: Template string with placeholders
            key_type: Key type name (e.g., "str", "int")
            val_type: Value type name (e.g., "int", "str")

        Returns:
            Template with substitutions applied

        Example:
            >>> engine = TemplateSubstitutionEngine()
            >>> template = "map_{{K_SUFFIX}}_{{V_SUFFIX}}"
            >>> engine.substitute_map_template(template, "str", "int")
            'map_str_int'
        """
        key_props = get_type_properties(key_type)
        val_props = get_type_properties(val_type)

        # Build substitution context with K_ and V_ prefixes
        context = {
            # Key properties
            "K": key_props.c_type,
            "K_SUFFIX": key_props.suffix,
            "K_ZERO": key_props.zero_value,
            "K_PRINTF": key_props.printf_fmt,
            "K_COMPARE": key_props.compare_op,
            "K_HASH": key_props.hash_fn,
            "K_NEEDS_DROP": key_props.needs_drop,
            "K_NEEDS_COPY": key_props.needs_copy,
            "K_IS_POINTER": key_props.is_pointer,
            # Value properties
            "V": val_props.c_type,
            "V_SUFFIX": val_props.suffix,
            "V_ZERO": val_props.zero_value,
            "V_PRINTF": val_props.printf_fmt,
            "V_COMPARE": val_props.compare_op,
            "V_HASH": val_props.hash_fn,
            "V_NEEDS_DROP": val_props.needs_drop,
            "V_NEEDS_COPY": val_props.needs_copy,
            "V_IS_POINTER": val_props.is_pointer,
            # Combined suffix for type name
            "KV_SUFFIX": f"{key_props.suffix}_{val_props.suffix}",
        }

        return self._substitute(template, context)

    def substitute_set_template(self, template: str, elem_type: str) -> str:
        """Substitute placeholders in a set template.

        Args:
            template: Template string with placeholders
            elem_type: Element type name (e.g., "int", "str")

        Returns:
            Template with substitutions applied
        """
        # Set templates use same placeholders as vector templates
        return self.substitute_vec_template(template, elem_type)

    def substitute_from_container_info(self, template: str, info: ContainerTypeInfo) -> str:
        """Substitute placeholders using ContainerTypeInfo.

        Args:
            template: Template string with placeholders
            info: Container type information

        Returns:
            Template with substitutions applied
        """
        if info.family == "vec":
            return self.substitute_vec_template(template, info.type_params[0])
        elif info.family == "set":
            return self.substitute_set_template(template, info.type_params[0])
        elif info.family == "map":
            return self.substitute_map_template(template, info.type_params[0], info.type_params[1])
        elif info.family == "vec_vec":
            # For nested vectors, use the inner type
            return self.substitute_vec_template(template, info.type_params[1])
        else:
            raise ValueError(f"Unknown container family: {info.family}")

    def _substitute(self, template: str, context: dict[str, Any]) -> str:
        """Internal substitution method that handles both placeholders and conditionals.

        Args:
            template: Template string
            context: Substitution context with variable values

        Returns:
            Template with substitutions applied
        """
        # First, handle conditional blocks
        result = self._process_conditionals(template, context)

        # Then, substitute simple placeholders
        result = self._substitute_placeholders(result, context)

        return result

    def _substitute_placeholders(self, template: str, context: dict[str, Any]) -> str:
        """Substitute simple placeholders like {{VAR}}.

        Args:
            template: Template string
            context: Substitution context

        Returns:
            Template with placeholders substituted
        """

        def replace_placeholder(match: re.Match) -> str:
            var_name = match.group(1)
            if var_name in context:
                value = context[var_name]
                # Convert boolean to empty string or keep the value
                if isinstance(value, bool):
                    return ""  # Booleans are only for conditionals
                return str(value)
            # Keep placeholder if not in context (might be used elsewhere)
            return match.group(0)

        return self.PLACEHOLDER_PATTERN.sub(replace_placeholder, template)

    def _process_conditionals(self, template: str, context: dict[str, Any]) -> str:
        """Process conditional blocks like {{#VAR}}...{{/VAR}}.

        Args:
            template: Template string
            context: Substitution context

        Returns:
            Template with conditionals processed
        """
        result = []
        pos = 0

        while pos < len(template):
            # Look for conditional start
            start_match = self.CONDITIONAL_START_PATTERN.search(template, pos)

            if not start_match:
                # No more conditionals, append rest of template
                result.append(template[pos:])
                break

            # Append text before conditional
            result.append(template[pos : start_match.start()])

            # Find matching end tag
            var_name = start_match.group(1)
            end_pattern = re.compile(rf"\{{\{{/{re.escape(var_name)}\}}\}}")
            end_match = end_pattern.search(template, start_match.end())

            if not end_match:
                # No matching end tag, treat as literal
                result.append(start_match.group(0))
                pos = start_match.end()
                continue

            # Extract conditional block content
            block_content = template[start_match.end() : end_match.start()]

            # Evaluate condition
            if var_name in context and context[var_name]:
                # Condition is true, include block (recursively process it)
                result.append(self._process_conditionals(block_content, context))
            # else: condition is false, omit block

            pos = end_match.end()

        return "".join(result)


__all__ = ["TemplateSubstitutionEngine"]
