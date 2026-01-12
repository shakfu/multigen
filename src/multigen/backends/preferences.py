"""Backend preference system for MultiGen language-specific optimizations."""

from dataclasses import dataclass, field
from typing import Any


@dataclass
class BackendPreferences:
    """Base preferences class for all language backends."""

    # Global preferences that apply to all backends
    debug_mode: bool = False
    optimize_for_readability: bool = True
    preserve_python_semantics: bool = True

    # Language-specific preferences (populated by subclasses)
    language_specific: dict[str, Any] = field(default_factory=dict)

    def get(self, key: str, default: Any = None) -> Any:
        """Get a preference value with fallback to default."""
        return self.language_specific.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """Set a language-specific preference."""
        self.language_specific[key] = value


@dataclass
class HaskellPreferences(BackendPreferences):
    """Haskell-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize Haskell-specific default preferences."""
        self.language_specific.update(
            {
                # Comprehension generation preferences
                "use_native_comprehensions": False,  # Default to runtime consistency
                "prefer_idiomatic_syntax": False,  # Default to cross-language consistency
                "enable_type_annotations": True,  # Always include type signatures
                "use_strict_mode": False,  # Use lazy evaluation by default
                # Code style preferences
                "camel_case_conversion": True,  # Convert snake_case to camelCase
                "module_pragmas": ["OverloadedStrings"],  # Default language extensions
                "import_qualified": True,  # Use qualified imports
                # Performance preferences
                "enable_fusion_rules": False,  # Enable list fusion optimizations
                "strict_data_types": False,  # Use strict data fields
                "unpack_pragmas": False,  # Add UNPACK pragmas
            }
        )


@dataclass
class CPreferences(BackendPreferences):
    """C-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize C-specific default preferences."""
        self.language_specific.update(
            {
                # Container implementation preferences
                "container_mode": "runtime",  # "runtime" or "generated" - how to implement containers
                "use_stc_containers": True,  # Use Smart Template Containers (only for runtime mode)
                # Code generation preferences
                "inline_functions": False,  # Add inline keywords
                "use_restrict_keywords": False,  # Use restrict pointers
                "enable_compiler_optimizations": True,
                # Memory management preferences
                "explicit_memory_management": True,  # Manual malloc/free
                "bounds_checking": True,  # Array bounds checking
                "null_pointer_checks": True,  # NULL pointer validation
                # Style preferences
                "brace_style": "k&r",  # K&R, allman, gnu, etc.
                "indent_size": 4,  # Indentation size
                "use_stdint_types": True,  # Use int32_t instead of int
            }
        )


@dataclass
class CppPreferences(BackendPreferences):
    """C++-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize C++-specific default preferences."""
        self.language_specific.update(
            {
                # Language standard preferences
                "cpp_standard": "c++17",  # C++ standard version
                "use_modern_cpp": True,  # Modern C++ features
                "enable_concepts": False,  # C++20 concepts (if supported)
                # STL and library preferences
                "use_stl_containers": True,  # Use std::vector, std::map, etc.
                "prefer_algorithms": False,  # Use <algorithm> functions
                "use_smart_pointers": False,  # std::unique_ptr, std::shared_ptr
                "enable_move_semantics": False,  # Move constructors/assignment
                # Code generation preferences
                "use_auto_keyword": False,  # Auto type deduction
                "use_range_based_loops": False,  # for (auto& x : container)
                "inline_functions": False,  # Add inline keywords
                "use_constexpr": False,  # constexpr functions
                # Memory management preferences
                "raii_style": True,  # Resource Acquisition Is Initialization
                "exception_safety": True,  # Exception-safe code generation
                "bounds_checking": True,  # Array bounds validation
                # Style preferences
                "brace_style": "k&r",  # K&R, allman, gnu, etc.
                "indent_size": 4,  # Indentation size
                "namespace_style": "nested",  # nested, compact
            }
        )


@dataclass
class RustPreferences(BackendPreferences):
    """Rust-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize Rust-specific default preferences."""
        self.language_specific.update(
            {
                # Language edition preferences
                "rust_edition": "2021",  # Rust edition (2018, 2021)
                "use_modern_rust": True,  # Modern Rust idioms
                # Memory safety preferences
                "prefer_borrowing": True,  # Use references over owned values
                "explicit_lifetimes": False,  # Add explicit lifetime annotations
                "use_box_for_recursion": True,  # Box<T> for recursive types
                # Ownership and borrowing
                "clone_strategy": "minimal",  # minimal, explicit, liberal
                "use_cow": False,  # Cow<'_, str> for string handling
                "prefer_into_iter": True,  # .into_iter() over .iter()
                # Error handling preferences
                "error_handling": "result",  # result, panic, unwrap
                "use_anyhow": False,  # anyhow crate for error handling
                "explicit_error_types": True,  # Custom error types
                # Collections and iterators
                "use_iterators": True,  # Iterator chains over loops
                "collect_strategy": "smart",  # smart, explicit, minimal
                "use_hashmap": True,  # HashMap over BTreeMap
                # Code style preferences
                "snake_case_conversion": True,  # Keep Python snake_case
                "use_derives": True,  # #[derive(...)] attributes
                "explicit_returns": False,  # Explicit return statements
                # Performance preferences
                "zero_cost_abstractions": True,  # Prefer zero-cost patterns
                "inline_hints": False,  # #[inline] attributes
                "simd_optimizations": False,  # SIMD where applicable
            }
        )


@dataclass
class GoPreferences(BackendPreferences):
    """Go-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize Go-specific default preferences."""
        self.language_specific.update(
            {
                # Language version preferences
                "go_version": "1.21",  # Minimum Go version
                "use_generics": True,  # Go 1.18+ generics
                # Package and module preferences
                "module_structure": "single",  # single, multi-package
                "package_naming": "lowercase",  # lowercase, descriptive
                "use_internal_packages": False,  # internal/ directory structure
                # Concurrency preferences
                "use_goroutines": False,  # Concurrent processing
                "channel_strategy": "minimal",  # minimal, explicit, buffered
                "context_usage": "explicit",  # explicit, implicit, none
                # Error handling preferences
                "error_handling": "explicit",  # explicit, wrapped, panic
                "use_errors_is": True,  # errors.Is/As for error checking
                "custom_error_types": False,  # Custom error types
                # Memory management preferences
                "pointer_usage": "minimal",  # minimal, explicit, performance
                "slice_capacity": "smart",  # smart, explicit, generous
                "string_builder": True,  # strings.Builder for concatenation
                # Code style preferences
                "naming_convention": "go_style",  # go_style, camelCase, preserved
                "use_short_names": True,  # Go-style short variable names
                "explicit_interfaces": False,  # Explicit interface definitions
                # Standard library preferences
                "prefer_standard_lib": True,  # Use standard library over runtime
                "use_fmt_package": True,  # fmt package for output
                "json_tags": False,  # Add json struct tags
            }
        )


@dataclass
class OCamlPreferences(BackendPreferences):
    """OCaml-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize OCaml-specific default preferences."""
        self.language_specific.update(
            {
                # Language version preferences
                "ocaml_version": "4.14",  # OCaml version targeting
                "use_modern_syntax": True,  # Modern OCaml syntax features
                # Functional programming preferences
                "prefer_immutable": True,  # Immutable data structures
                "use_pattern_matching": True,  # Pattern matching over conditionals
                "tail_recursion_opt": True,  # Tail recursion optimization
                "curried_functions": True,  # Use curried function style
                # Module system preferences
                "module_structure": "nested",  # nested, flat, hierarchical
                "use_functors": False,  # Higher-order modules
                "signature_files": False,  # Generate .mli files
                # Type system preferences
                "type_annotations": True,  # Explicit type annotations
                "polymorphic_variants": False,  # Use polymorphic variants
                "gadts": False,  # Generalized ADTs (if supported)
                "objects": False,  # OCaml object system vs records
                # Standard library preferences
                "list_operations": "functional",  # functional, imperative, mixed
                "string_handling": "stdlib",  # stdlib, bytes, custom
                "hashtables": "stdlib",  # Use Hashtbl module
                # Code style preferences
                "naming_convention": "snake_case",  # snake_case, camelCase
                "indent_size": 2,  # Indentation size
                "match_style": "aligned",  # aligned, compact
                # Performance preferences
                "lazy_evaluation": False,  # Use lazy values
                "mutable_optimization": False,  # Mutable optimizations where safe
                "inline_hints": False,  # Compiler inline hints
            }
        )


class LLVMPreferences(BackendPreferences):
    """LLVM-specific backend preferences."""

    def __post_init__(self) -> None:
        """Initialize LLVM-specific default preferences."""
        self.language_specific.update(
            {
                # LLVM version and target preferences
                "llvm_version": "18.0",  # LLVM version targeting
                "target_triple": "native",  # native, x86_64, aarch64, wasm32, etc.
                "data_layout": "auto",  # Auto-detect or custom data layout
                # Optimization preferences
                "optimization_level": 2,  # 0-3, corresponds to -O0 through -O3
                "lto": False,  # Link-time optimization
                "vectorization": True,  # Auto-vectorization
                "loop_unrolling": True,  # Loop unrolling optimization
                # Code generation preferences
                "ssa_form": True,  # Generate SSA form (always true for LLVM)
                "named_values": True,  # Use descriptive names for values
                "debug_info": False,  # Generate DWARF debug information
                "metadata": True,  # Include LLVM metadata
                # Compilation preferences
                "compile_to": "binary",  # binary, object, assembly, llvm-ir
                "static_linking": False,  # Static vs dynamic linking
                "position_independent": False,  # Position-independent code
                # Runtime preferences
                "use_runtime": False,  # Use MultiGen runtime library (future)
                "memory_model": "default",  # default, safe, unsafe
                "bounds_checking": False,  # Runtime bounds checking
                # Backend tool preferences
                "llc_path": "llc",  # Path to llc compiler
                "clang_path": "clang",  # Path to clang for linking
                "opt_path": "opt",  # Path to LLVM optimizer
            }
        )


class PreferencesRegistry:
    """Registry for backend preference classes."""

    _preferences_map: dict[str, type[BackendPreferences]] = {
        "haskell": HaskellPreferences,
        "c": CPreferences,
        "cpp": CppPreferences,
        "rust": RustPreferences,
        "go": GoPreferences,
        "ocaml": OCamlPreferences,
        "llvm": LLVMPreferences,
    }

    @classmethod
    def get_preferences_class(cls, backend_name: str) -> type[BackendPreferences]:
        """Get preference class for a backend."""
        return cls._preferences_map.get(backend_name, BackendPreferences)

    @classmethod
    def create_preferences(cls, backend_name: str, **kwargs: Any) -> BackendPreferences:
        """Create preferences instance for a backend."""
        prefs_class = cls.get_preferences_class(backend_name)
        return prefs_class(**kwargs)

    @classmethod
    def register_backend(cls, backend_name: str, preferences_class: type[BackendPreferences]) -> None:
        """Register a new backend preference class."""
        cls._preferences_map[backend_name] = preferences_class
