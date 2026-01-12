"""Module System for MultiGen.

This module provides support for Python import statements and cross-module function calls
in the Python-to-multi-language translation system.

Current Support:
- Local module imports (import mymodule, from mymodule import function)
- Basic standard library modules (math)
- Cross-module function resolution

Future Enhancements:
- Full standard library support
- Module dependency analysis
- Circular dependency detection
"""

import ast
from pathlib import Path
from typing import Optional

from . import log


class ModuleInfo:
    """Information about a discovered module."""

    def __init__(self, name: str, path: Path):
        self.name = name
        self.path = path
        self.functions: dict[str, ast.FunctionDef] = {}
        self.imports: list[str] = []
        self.ast_module: Optional[ast.Module] = None
        self.dependencies: set[str] = set()


class StandardLibraryModule:
    """Information about a supported standard library module."""

    def __init__(self, name: str, functions: dict[str, str]):
        self.name = name
        self.functions = functions  # function_name -> C_equivalent


class ModuleResolver:
    """Resolves module imports and manages module dependencies."""

    def __init__(self) -> None:
        self.log = log.config(self.__class__.__name__)
        self.discovered_modules: dict[str, ModuleInfo] = {}
        self.stdlib_modules: dict[str, StandardLibraryModule] = {}
        self.current_search_paths: list[Path] = []

        # Initialize basic standard library support
        self._init_stdlib_modules()

    def _init_stdlib_modules(self) -> None:
        """Initialize supported standard library modules."""
        # Math module support
        math_functions = {
            "sqrt": "sqrt",
            "pow": "pow",
            "sin": "sin",
            "cos": "cos",
            "tan": "tan",
            "log": "log",
            "log10": "log10",
            "exp": "exp",
            "floor": "floor",
            "ceil": "ceil",
            "abs": "abs",
            "fabs": "fabs",
        }
        self.stdlib_modules["math"] = StandardLibraryModule("math", math_functions)

        # Type hinting and dataclass modules (no C equivalent needed)
        self.stdlib_modules["typing"] = StandardLibraryModule("typing", {})
        self.stdlib_modules["dataclasses"] = StandardLibraryModule("dataclasses", {})

        # Future: Add more standard library modules
        # self.stdlib_modules['os'] = StandardLibraryModule('os', {...})
        # self.stdlib_modules['sys'] = StandardLibraryModule('sys', {...})

    def add_search_path(self, path: Path) -> None:
        """Add a directory to the module search path."""
        if path.is_dir() and path not in self.current_search_paths:
            self.current_search_paths.append(path)
            self.log.debug(f"Added module search path: {path}")

    def discover_module(self, module_name: str) -> Optional[ModuleInfo]:
        """Discover and analyze a Python module."""
        # Check if it's a standard library module
        if module_name in self.stdlib_modules:
            self.log.debug(f"Found standard library module: {module_name}")
            return None  # Standard library modules don't need file analysis

        # Check if already discovered
        if module_name in self.discovered_modules:
            return self.discovered_modules[module_name]

        # Search for the module file
        module_file = self._find_module_file(module_name)
        if not module_file:
            self.log.warning(f"Module not found: {module_name}")
            return None

        # Parse and analyze the module
        try:
            with open(module_file, encoding="utf-8") as f:
                source_code = f.read()

            ast_module = ast.parse(source_code, filename=str(module_file))
            module_info = ModuleInfo(module_name, module_file)
            module_info.ast_module = ast_module

            # Extract functions and imports
            self._analyze_module(module_info, ast_module)

            self.discovered_modules[module_name] = module_info
            self.log.info(f"Discovered module: {module_name} with {len(module_info.functions)} functions")
            return module_info

        except Exception as e:
            self.log.error(f"Failed to analyze module {module_name}: {e}")
            return None

    def _find_module_file(self, module_name: str) -> Optional[Path]:
        """Find the Python file for a given module name."""
        # Try each search path
        for search_path in self.current_search_paths:
            # Try module_name.py
            module_file = search_path / f"{module_name}.py"
            if module_file.exists():
                return module_file

            # Try module_name/__init__.py
            package_init = search_path / module_name / "__init__.py"
            if package_init.exists():
                return package_init

        return None

    def _analyze_module(self, module_info: ModuleInfo, ast_module: ast.Module) -> None:
        """Analyze a module's AST to extract functions and imports."""
        for node in ast.walk(ast_module):
            if isinstance(node, ast.FunctionDef):
                module_info.functions[node.name] = node
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    module_info.imports.append(alias.name)
                    module_info.dependencies.add(alias.name)
            elif isinstance(node, ast.ImportFrom):
                if node.module:
                    module_info.imports.append(node.module)
                    module_info.dependencies.add(node.module)

    def resolve_import(self, import_node: ast.Import) -> list[tuple[str, Optional[ModuleInfo]]]:
        """Resolve an import statement to module information."""
        resolved = []
        for alias in import_node.names:
            module_name = alias.name
            module_info = self.discover_module(module_name)
            resolved.append((module_name, module_info))
        return resolved

    def resolve_from_import(self, from_import_node: ast.ImportFrom) -> tuple[Optional[ModuleInfo], list[str]]:
        """Resolve a from...import statement."""
        if not from_import_node.module:
            return None, []

        module_info = self.discover_module(from_import_node.module)
        imported_names = [alias.name for alias in from_import_node.names]
        return module_info, imported_names

    def get_function_signature(self, module_name: str, function_name: str) -> Optional[ast.FunctionDef]:
        """Get the AST function definition for a function in a module."""
        if module_name in self.discovered_modules:
            module_info = self.discovered_modules[module_name]
            return module_info.functions.get(function_name)
        return None

    def is_stdlib_function(self, module_name: str, function_name: str) -> bool:
        """Check if a function is from a supported standard library module."""
        if module_name in self.stdlib_modules:
            return function_name in self.stdlib_modules[module_name].functions
        return False

    def get_stdlib_c_equivalent(self, module_name: str, function_name: str) -> Optional[str]:
        """Get the C equivalent for a standard library function."""
        if module_name in self.stdlib_modules:
            stdlib_module = self.stdlib_modules[module_name]
            return stdlib_module.functions.get(function_name)
        return None

    def get_compilation_order(self) -> list[str]:
        """Get the order in which modules should be compiled (dependency-first)."""
        # Simple topological sort for module dependencies
        visited = set()
        result = []

        def visit(module_name: str) -> None:
            if module_name in visited or module_name not in self.discovered_modules:
                return

            visited.add(module_name)
            module_info = self.discovered_modules[module_name]

            # Visit dependencies first
            for dep in module_info.dependencies:
                if dep in self.discovered_modules:
                    visit(dep)

            result.append(module_name)

        # Visit all discovered modules
        for module_name in self.discovered_modules:
            visit(module_name)

        return result


class ImportHandler:
    """Handles import statement processing during Python-to-C conversion."""

    def __init__(self, module_resolver: ModuleResolver):
        self.log = log.config(self.__class__.__name__)
        self.module_resolver = module_resolver
        self.current_module_imports: dict[str, str] = {}  # alias -> module_name
        self.imported_functions: dict[str, tuple[str, str]] = {}  # function_name -> (module_name, original_name)

    def process_import(self, import_node: ast.Import) -> list[str]:
        """Process an import statement and return C include directives."""
        includes = []
        self.module_resolver.resolve_import(import_node)

        for alias_node in import_node.names:
            module_name = alias_node.name
            alias_name = alias_node.asname or module_name

            # Add to current module's import table
            self.current_module_imports[alias_name] = module_name

            # Generate appropriate C includes
            if module_name in self.module_resolver.stdlib_modules:
                # Standard library module
                if module_name == "math":
                    includes.append("#include <math.h>")
                # Note: typing and dataclasses modules don't need C includes
                # Add more standard library includes as needed
            else:
                # Local module - will need generated header
                includes.append(f'#include "{module_name}.h"')

        return includes

    def process_from_import(self, from_import_node: ast.ImportFrom) -> list[str]:
        """Process a from...import statement and return C include directives."""
        includes: list[str] = []
        module_info, imported_names = self.module_resolver.resolve_from_import(from_import_node)

        if not from_import_node.module:
            return includes

        module_name = from_import_node.module

        # Track imported functions
        for alias_node in from_import_node.names:
            function_name = alias_node.name
            alias_name = alias_node.asname or function_name
            self.imported_functions[alias_name] = (module_name, function_name)

        # Generate includes
        if module_name in self.module_resolver.stdlib_modules:
            if module_name == "math":
                includes.append("#include <math.h>")
            # Note: typing and dataclasses modules don't need C includes
        else:
            includes.append(f'#include "{module_name}.h"')

        return includes

    def resolve_function_call(self, function_name: str) -> tuple[str, bool]:
        """Resolve a function call to its C equivalent.

        Returns:
            (c_function_name, is_stdlib)
        """
        # Check if it's a directly imported function
        if function_name in self.imported_functions:
            module_name, original_name = self.imported_functions[function_name]
            if self.module_resolver.is_stdlib_function(module_name, original_name):
                c_equiv = self.module_resolver.get_stdlib_c_equivalent(module_name, original_name)
                return c_equiv or function_name, True
            else:
                # Local module function - prefix with module name to avoid conflicts
                return f"{module_name}_{original_name}", False

        # Default - assume it's a local function
        return function_name, False

    def resolve_module_function_call(self, module_alias: str, function_name: str) -> tuple[str, bool]:
        """Resolve a module.function() call to its C equivalent.

        Returns:
            (c_function_name, is_stdlib)
        """
        if module_alias not in self.current_module_imports:
            return f"{module_alias}_{function_name}", False

        module_name = self.current_module_imports[module_alias]

        if self.module_resolver.is_stdlib_function(module_name, function_name):
            c_equiv = self.module_resolver.get_stdlib_c_equivalent(module_name, function_name)
            return c_equiv or function_name, True
        else:
            return f"{module_name}_{function_name}", False
