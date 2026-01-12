"""LLVM IR runtime declarations for C runtime library.

This module generates LLVM IR struct definitions and extern function declarations
that correspond to the C runtime library (vec_int, map_int_int, set_int, etc.).
"""

from llvmlite import ir  # type: ignore[import-untyped]


class LLVMRuntimeDeclarations:
    """Generate LLVM IR declarations for C runtime library."""

    def __init__(self, module: ir.Module) -> None:
        """Initialize runtime declarations.

        Args:
            module: LLVM module to add declarations to
        """
        self.module = module
        self.struct_types: dict[str, ir.Type] = {}
        self.function_decls: dict[str, ir.Function] = {}

    def get_vec_int_type(self) -> ir.Type:
        """Get or create vec_int struct type.

        C struct definition:
            typedef struct {
                long long* data;
                size_t size;
                size_t capacity;
            } vec_int;

        Returns:
            LLVM struct type for vec_int
        """
        if "vec_int" in self.struct_types:
            return self.struct_types["vec_int"]

        # Create named struct type (required for alloca)
        # struct vec_int { i64*, i64, i64 }
        # data: i64* (pointer to array of 64-bit integers)
        # size: i64 (size_t on 64-bit systems)
        # capacity: i64 (size_t on 64-bit systems)
        vec_int_type = self.module.context.get_identified_type("struct.vec_int")

        # Only set body if not already defined (avoid re-definition in shared context)
        if not vec_int_type.is_opaque:
            # Type already has a body, just use it
            self.struct_types["vec_int"] = vec_int_type
            return vec_int_type

        vec_int_type.set_body(
            ir.IntType(64).as_pointer(),  # data
            ir.IntType(64),  # size
            ir.IntType(64),  # capacity
        )

        self.struct_types["vec_int"] = vec_int_type
        return vec_int_type

    def declare_vec_int_functions(self) -> None:
        """Declare vec_int C runtime functions in LLVM IR."""
        vec_int_type = self.get_vec_int_type()
        vec_int_ptr = vec_int_type.as_pointer()
        i64 = ir.IntType(64)
        i64_ptr = i64.as_pointer()
        void = ir.VoidType()

        # void vec_int_init_ptr(vec_int* out) - initialize via pointer
        func_type = ir.FunctionType(void, [vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_int_init_ptr")
        self.function_decls["vec_int_init_ptr"] = func

        # void vec_int_push(vec_int* vec, long long value)
        func_type = ir.FunctionType(void, [vec_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_int_push")
        self.function_decls["vec_int_push"] = func

        # long long vec_int_at(vec_int* vec, size_t index)
        func_type = ir.FunctionType(i64, [vec_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_int_at")
        self.function_decls["vec_int_at"] = func

        # size_t vec_int_size(vec_int* vec)
        func_type = ir.FunctionType(i64, [vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_int_size")
        self.function_decls["vec_int_size"] = func

        # void vec_int_free(vec_int* vec)
        func_type = ir.FunctionType(void, [vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_int_free")
        self.function_decls["vec_int_free"] = func

        # long long* vec_int_data(vec_int* vec)
        func_type = ir.FunctionType(i64_ptr, [vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_int_data")
        self.function_decls["vec_int_data"] = func

        # void vec_int_clear(vec_int* vec)
        func_type = ir.FunctionType(void, [vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_int_clear")
        self.function_decls["vec_int_clear"] = func

        # void vec_int_reserve(vec_int* vec, size_t new_capacity)
        func_type = ir.FunctionType(void, [vec_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_int_reserve")
        self.function_decls["vec_int_reserve"] = func

        # void vec_int_set(vec_int* vec, size_t index, long long value)
        func_type = ir.FunctionType(void, [vec_int_ptr, i64, i64])
        func = ir.Function(self.module, func_type, name="vec_int_set")
        self.function_decls["vec_int_set"] = func

    def get_function(self, name: str) -> ir.Function:
        """Get declared function by name.

        Args:
            name: Function name

        Returns:
            LLVM function declaration

        Raises:
            KeyError: If function not declared
        """
        if name not in self.function_decls:
            raise KeyError(f"Function '{name}' not declared. Call declare_*_functions() first.")
        return self.function_decls[name]

    def get_vec_vec_int_type(self) -> ir.Type:
        """Get or create vec_vec_int struct type.

        C struct definition:
            typedef struct {
                vec_int* data;
                size_t size;
                size_t capacity;
            } vec_vec_int;

        Returns:
            LLVM struct type for vec_vec_int
        """
        if "vec_vec_int" in self.struct_types:
            return self.struct_types["vec_vec_int"]

        # Get vec_int type first
        vec_int_type = self.get_vec_int_type()
        vec_int_ptr = vec_int_type.as_pointer()

        # Create named struct type for vec_vec_int
        vec_vec_int_type = self.module.context.get_identified_type("struct.vec_vec_int")

        if not vec_vec_int_type.is_opaque:
            self.struct_types["vec_vec_int"] = vec_vec_int_type
            return vec_vec_int_type

        vec_vec_int_type.set_body(
            vec_int_ptr,  # data: vec_int* (pointer to array of vec_int)
            ir.IntType(64),  # size
            ir.IntType(64),  # capacity
        )

        self.struct_types["vec_vec_int"] = vec_vec_int_type
        return vec_vec_int_type

    def declare_vec_vec_int_functions(self) -> None:
        """Declare vec_vec_int C runtime functions in LLVM IR."""
        vec_vec_int_type = self.get_vec_vec_int_type()
        vec_vec_int_ptr = vec_vec_int_type.as_pointer()
        vec_int_type = self.get_vec_int_type()
        vec_int_ptr = vec_int_type.as_pointer()
        i64 = ir.IntType(64)
        void = ir.VoidType()

        # void vec_vec_int_init_ptr(vec_vec_int* out)
        func_type = ir.FunctionType(void, [vec_vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_vec_int_init_ptr")
        self.function_decls["vec_vec_int_init_ptr"] = func

        # void vec_vec_int_push(vec_vec_int* vec, vec_int* row)
        # Note: row is passed by pointer (avoids struct-by-value issues)
        func_type = ir.FunctionType(void, [vec_vec_int_ptr, vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_vec_int_push")
        self.function_decls["vec_vec_int_push"] = func

        # vec_int* vec_vec_int_at(vec_vec_int* vec, size_t index)
        func_type = ir.FunctionType(vec_int_ptr, [vec_vec_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_vec_int_at")
        self.function_decls["vec_vec_int_at"] = func

        # size_t vec_vec_int_size(vec_vec_int* vec)
        func_type = ir.FunctionType(i64, [vec_vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_vec_int_size")
        self.function_decls["vec_vec_int_size"] = func

        # void vec_vec_int_free(vec_vec_int* vec)
        func_type = ir.FunctionType(void, [vec_vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_vec_int_free")
        self.function_decls["vec_vec_int_free"] = func

        # void vec_vec_int_clear(vec_vec_int* vec)
        func_type = ir.FunctionType(void, [vec_vec_int_ptr])
        func = ir.Function(self.module, func_type, name="vec_vec_int_clear")
        self.function_decls["vec_vec_int_clear"] = func

    def get_string_array_type(self) -> ir.Type:
        """Get or create multigen_string_array_t struct type.

        C struct definition:
            typedef struct {
                char** strings;
                size_t count;
                size_t capacity;
            } multigen_string_array_t;

        Returns:
            LLVM struct type for multigen_string_array_t
        """
        if "string_array" in self.struct_types:
            return self.struct_types["string_array"]

        # Create named struct type
        string_array_type = self.module.context.get_identified_type("struct.multigen_string_array_t")

        if not string_array_type.is_opaque:
            self.struct_types["string_array"] = string_array_type
            return string_array_type

        i8_ptr = ir.IntType(8).as_pointer()  # char*
        i8_ptr_ptr = i8_ptr.as_pointer()  # char**

        string_array_type.set_body(
            i8_ptr_ptr,  # strings: char**
            ir.IntType(64),  # count: size_t
            ir.IntType(64),  # capacity: size_t
        )

        self.struct_types["string_array"] = string_array_type
        return string_array_type

    def get_vec_str_type(self) -> ir.Type:
        """Get or create vec_str struct type.

        C struct definition:
            typedef struct {
                char** data;
                size_t size;
                size_t capacity;
            } vec_str;

        Returns:
            LLVM struct type for vec_str
        """
        if "vec_str" in self.struct_types:
            return self.struct_types["vec_str"]

        # Create named struct type
        vec_str_type = self.module.context.get_identified_type("struct.vec_str")

        if not vec_str_type.is_opaque:
            self.struct_types["vec_str"] = vec_str_type
            return vec_str_type

        i8_ptr = ir.IntType(8).as_pointer()  # char*
        i8_ptr_ptr = i8_ptr.as_pointer()  # char**

        vec_str_type.set_body(
            i8_ptr_ptr,  # data: char**
            ir.IntType(64),  # size
            ir.IntType(64),  # capacity
        )

        self.struct_types["vec_str"] = vec_str_type
        return vec_str_type

    def declare_vec_str_functions(self) -> None:
        """Declare vec_str C runtime functions in LLVM IR."""
        vec_str_type = self.get_vec_str_type()
        vec_str_ptr = vec_str_type.as_pointer()
        i8_ptr = ir.IntType(8).as_pointer()  # char*
        i8_ptr_ptr = i8_ptr.as_pointer()  # char**
        i64 = ir.IntType(64)
        void = ir.VoidType()

        # void vec_str_init_ptr(vec_str* out)
        func_type = ir.FunctionType(void, [vec_str_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_init_ptr")
        self.function_decls["vec_str_init_ptr"] = func

        # void vec_str_push(vec_str* vec, char* value)
        func_type = ir.FunctionType(void, [vec_str_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_push")
        self.function_decls["vec_str_push"] = func

        # char* vec_str_at(vec_str* vec, size_t index)
        func_type = ir.FunctionType(i8_ptr, [vec_str_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_str_at")
        self.function_decls["vec_str_at"] = func

        # size_t vec_str_size(vec_str* vec)
        func_type = ir.FunctionType(i64, [vec_str_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_size")
        self.function_decls["vec_str_size"] = func

        # void vec_str_free(vec_str* vec)
        func_type = ir.FunctionType(void, [vec_str_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_free")
        self.function_decls["vec_str_free"] = func

        # char** vec_str_data(vec_str* vec)
        func_type = ir.FunctionType(i8_ptr_ptr, [vec_str_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_data")
        self.function_decls["vec_str_data"] = func

        # void vec_str_clear(vec_str* vec)
        func_type = ir.FunctionType(void, [vec_str_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_clear")
        self.function_decls["vec_str_clear"] = func

        # void vec_str_reserve(vec_str* vec, size_t new_capacity)
        func_type = ir.FunctionType(void, [vec_str_ptr, i64])
        func = ir.Function(self.module, func_type, name="vec_str_reserve")
        self.function_decls["vec_str_reserve"] = func

        # void vec_str_set(vec_str* vec, size_t index, char* value)
        func_type = ir.FunctionType(void, [vec_str_ptr, i64, i8_ptr])
        func = ir.Function(self.module, func_type, name="vec_str_set")
        self.function_decls["vec_str_set"] = func

    def declare_string_functions(self) -> None:
        """Declare string operation C runtime functions in LLVM IR."""
        i8_ptr = ir.IntType(8).as_pointer()  # char*
        i64 = ir.IntType(64)
        void = ir.VoidType()
        string_array_type = self.get_string_array_type()
        string_array_ptr = string_array_type.as_pointer()

        # multigen_string_array_t* multigen_str_split(const char* str, const char* delimiter)
        func_type = ir.FunctionType(string_array_ptr, [i8_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_split")
        self.function_decls["multigen_str_split"] = func

        # char* multigen_str_lower(const char* str)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_lower")
        self.function_decls["multigen_str_lower"] = func

        # char* multigen_str_strip(const char* str)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_strip")
        self.function_decls["multigen_str_strip"] = func

        # char* multigen_str_concat(const char* str1, const char* str2)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_concat")
        self.function_decls["multigen_str_concat"] = func

        # const char* multigen_string_array_get(multigen_string_array_t* arr, size_t index)
        func_type = ir.FunctionType(i8_ptr, [string_array_ptr, i64])
        func = ir.Function(self.module, func_type, name="multigen_string_array_get")
        self.function_decls["multigen_string_array_get"] = func

        # size_t multigen_string_array_size(multigen_string_array_t* arr)
        func_type = ir.FunctionType(i64, [string_array_ptr])
        func = ir.Function(self.module, func_type, name="multigen_string_array_size")
        self.function_decls["multigen_string_array_size"] = func

        # void multigen_string_array_free(multigen_string_array_t* arr)
        func_type = ir.FunctionType(void, [string_array_ptr])
        func = ir.Function(self.module, func_type, name="multigen_string_array_free")
        self.function_decls["multigen_string_array_free"] = func

        # char* multigen_str_join(const char* separator, multigen_string_array_t* strings)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr, string_array_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_join")
        self.function_decls["multigen_str_join"] = func

        # char* multigen_str_replace(const char* str, const char* old, const char* new_str)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr, i8_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_replace")
        self.function_decls["multigen_str_replace"] = func

        # char* multigen_str_upper(const char* str)
        func_type = ir.FunctionType(i8_ptr, [i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_upper")
        self.function_decls["multigen_str_upper"] = func

        # int multigen_str_startswith(const char* str, const char* prefix)
        func_type = ir.FunctionType(ir.IntType(32), [i8_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_startswith")
        self.function_decls["multigen_str_startswith"] = func

        # int multigen_str_endswith(const char* str, const char* suffix)
        func_type = ir.FunctionType(ir.IntType(32), [i8_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="multigen_str_endswith")
        self.function_decls["multigen_str_endswith"] = func

    def get_map_str_int_type(self) -> ir.Type:
        """Get or create map_str_int struct type.

        C struct definition (opaque - internal details hidden):
            typedef struct {
                map_entry* entries;
                size_t size;
                size_t capacity;
            } map_str_int;

        Returns:
            LLVM struct type for map_str_int
        """
        if "map_str_int" in self.struct_types:
            return self.struct_types["map_str_int"]

        # Create named struct type
        # We'll keep it opaque since we only ever pass pointers
        # and don't need to access fields directly in LLVM IR
        map_str_int_type = self.module.context.get_identified_type("struct.map_str_int")

        if not map_str_int_type.is_opaque:
            self.struct_types["map_str_int"] = map_str_int_type
            return map_str_int_type

        # Define the struct body to match C definition
        # map_entry* entries (opaque pointer)
        i8_ptr = ir.IntType(8).as_pointer()

        map_str_int_type.set_body(
            i8_ptr,  # entries: map_entry* (treated as opaque i8*)
            ir.IntType(64),  # size: size_t
            ir.IntType(64),  # capacity: size_t
        )

        self.struct_types["map_str_int"] = map_str_int_type
        return map_str_int_type

    def declare_map_str_int_functions(self) -> None:
        """Declare map_str_int C runtime functions in LLVM IR."""
        map_str_int_type = self.get_map_str_int_type()
        map_str_int_ptr = map_str_int_type.as_pointer()
        i8_ptr = ir.IntType(8).as_pointer()  # char*
        i64 = ir.IntType(64)
        i32 = ir.IntType(32)  # for boolean return
        void = ir.VoidType()

        # void map_str_int_init_ptr(map_str_int* out)
        func_type = ir.FunctionType(void, [map_str_int_ptr])
        func = ir.Function(self.module, func_type, name="map_str_int_init_ptr")
        self.function_decls["map_str_int_init_ptr"] = func

        # void map_str_int_set(map_str_int* map, const char* key, long long value)
        func_type = ir.FunctionType(void, [map_str_int_ptr, i8_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_str_int_set")
        self.function_decls["map_str_int_set"] = func

        # long long map_str_int_get(map_str_int* map, const char* key)
        func_type = ir.FunctionType(i64, [map_str_int_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="map_str_int_get")
        self.function_decls["map_str_int_get"] = func

        # int map_str_int_contains(map_str_int* map, const char* key)
        func_type = ir.FunctionType(i32, [map_str_int_ptr, i8_ptr])
        func = ir.Function(self.module, func_type, name="map_str_int_contains")
        self.function_decls["map_str_int_contains"] = func

        # size_t map_str_int_size(map_str_int* map)
        func_type = ir.FunctionType(i64, [map_str_int_ptr])
        func = ir.Function(self.module, func_type, name="map_str_int_size")
        self.function_decls["map_str_int_size"] = func

        # void map_str_int_free(map_str_int* map)
        func_type = ir.FunctionType(void, [map_str_int_ptr])
        func = ir.Function(self.module, func_type, name="map_str_int_free")
        self.function_decls["map_str_int_free"] = func

    def get_map_int_int_type(self) -> ir.Type:
        """Get or create map_int_int struct type.

        C struct definition:
            typedef struct {
                map_int_entry* entries;
                size_t size;
                size_t capacity;
            } map_int_int;

        Returns:
            LLVM struct type for map_int_int
        """
        if "map_int_int" in self.struct_types:
            return self.struct_types["map_int_int"]

        # Create named struct type
        # We'll keep it opaque since we only ever pass pointers
        map_int_int_type = self.module.context.get_identified_type("struct.map_int_int")

        if not map_int_int_type.is_opaque:
            self.struct_types["map_int_int"] = map_int_int_type
            return map_int_int_type

        # Define the struct body to match C definition
        # map_int_entry* entries (opaque pointer)
        i8_ptr = ir.IntType(8).as_pointer()

        map_int_int_type.set_body(
            i8_ptr,  # entries: map_int_entry* (treated as opaque i8*)
            ir.IntType(64),  # size: size_t
            ir.IntType(64),  # capacity: size_t
        )

        self.struct_types["map_int_int"] = map_int_int_type
        return map_int_int_type

    def declare_map_int_int_functions(self) -> None:
        """Declare map_int_int C runtime functions in LLVM IR."""
        map_int_int_type = self.get_map_int_int_type()
        map_int_int_ptr = map_int_int_type.as_pointer()
        i64 = ir.IntType(64)
        i32 = ir.IntType(32)  # for boolean return
        void = ir.VoidType()

        # void map_int_int_init_ptr(map_int_int* out)
        func_type = ir.FunctionType(void, [map_int_int_ptr])
        func = ir.Function(self.module, func_type, name="map_int_int_init_ptr")
        self.function_decls["map_int_int_init_ptr"] = func

        # void map_int_int_set(map_int_int* map, long long key, long long value)
        func_type = ir.FunctionType(void, [map_int_int_ptr, i64, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_set")
        self.function_decls["map_int_int_set"] = func

        # long long map_int_int_get(map_int_int* map, long long key)
        func_type = ir.FunctionType(i64, [map_int_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_get")
        self.function_decls["map_int_int_get"] = func

        # int map_int_int_contains(map_int_int* map, long long key)
        func_type = ir.FunctionType(i32, [map_int_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_contains")
        self.function_decls["map_int_int_contains"] = func

        # size_t map_int_int_size(map_int_int* map)
        func_type = ir.FunctionType(i64, [map_int_int_ptr])
        func = ir.Function(self.module, func_type, name="map_int_int_size")
        self.function_decls["map_int_int_size"] = func

        # void map_int_int_free(map_int_int* map)
        func_type = ir.FunctionType(void, [map_int_int_ptr])
        func = ir.Function(self.module, func_type, name="map_int_int_free")
        self.function_decls["map_int_int_free"] = func

        # size_t map_int_int_capacity(map_int_int* map)
        func_type = ir.FunctionType(i64, [map_int_int_ptr])
        func = ir.Function(self.module, func_type, name="map_int_int_capacity")
        self.function_decls["map_int_int_capacity"] = func

        # int map_int_int_entry_is_occupied(map_int_int* map, size_t index)
        func_type = ir.FunctionType(i32, [map_int_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_entry_is_occupied")
        self.function_decls["map_int_int_entry_is_occupied"] = func

        # long long map_int_int_entry_key(map_int_int* map, size_t index)
        func_type = ir.FunctionType(i64, [map_int_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_entry_key")
        self.function_decls["map_int_int_entry_key"] = func

        # long long map_int_int_entry_value(map_int_int* map, size_t index)
        func_type = ir.FunctionType(i64, [map_int_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="map_int_int_entry_value")
        self.function_decls["map_int_int_entry_value"] = func

    def get_set_int_type(self) -> ir.Type:
        """Get or create set_int struct type.

        C struct definition:
            typedef struct {
                multigen_set_int_entry_t** buckets;
                size_t bucket_count;
                size_t size;
            } set_int;

        Returns:
            LLVM struct type for set_int
        """
        if "set_int" in self.struct_types:
            return self.struct_types["set_int"]

        # Create named struct type
        set_int_type = self.module.context.get_identified_type("struct.set_int")

        if not set_int_type.is_opaque:
            self.struct_types["set_int"] = set_int_type
            return set_int_type

        # Define the struct body to match C definition
        # multigen_set_int_entry_t** buckets (opaque pointer)
        i8_ptr = ir.IntType(8).as_pointer()

        set_int_type.set_body(
            i8_ptr,  # buckets: multigen_set_int_entry_t** (treated as opaque i8*)
            ir.IntType(64),  # bucket_count: size_t
            ir.IntType(64),  # size: size_t
        )

        self.struct_types["set_int"] = set_int_type
        return set_int_type

    def declare_set_int_functions(self) -> None:
        """Declare set_int C runtime functions in LLVM IR."""
        set_int_type = self.get_set_int_type()
        set_int_ptr = set_int_type.as_pointer()
        i64 = ir.IntType(64)
        i32 = ir.IntType(32)  # for boolean return (in C, bool -> int)
        i1 = ir.IntType(1)  # for bool return
        void = ir.VoidType()

        # set_int set_int_init(void)
        func_type = ir.FunctionType(set_int_type, [])
        func = ir.Function(self.module, func_type, name="set_int_init")
        self.function_decls["set_int_init"] = func

        # void set_int_init_ptr(set_int* out) - initialize via pointer
        func_type = ir.FunctionType(void, [set_int_ptr])
        func = ir.Function(self.module, func_type, name="set_int_init_ptr")
        self.function_decls["set_int_init_ptr"] = func

        # bool set_int_insert(set_int* set, int value)
        func_type = ir.FunctionType(i1, [set_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="set_int_insert")
        self.function_decls["set_int_insert"] = func

        # bool set_int_contains(const set_int* set, int value)
        func_type = ir.FunctionType(i1, [set_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="set_int_contains")
        self.function_decls["set_int_contains"] = func

        # size_t set_int_size(const set_int* set)
        func_type = ir.FunctionType(i64, [set_int_ptr])
        func = ir.Function(self.module, func_type, name="set_int_size")
        self.function_decls["set_int_size"] = func

        # long long set_int_get_nth_element(const set_int* set, size_t n)
        func_type = ir.FunctionType(i64, [set_int_ptr, i64])
        func = ir.Function(self.module, func_type, name="set_int_get_nth_element")
        self.function_decls["set_int_get_nth_element"] = func

        # void set_int_drop(set_int* set)
        func_type = ir.FunctionType(void, [set_int_ptr])
        func = ir.Function(self.module, func_type, name="set_int_drop")
        self.function_decls["set_int_drop"] = func

    def declare_all(self) -> None:
        """Declare all runtime library functions and types."""
        self.declare_vec_int_functions()
        self.declare_vec_vec_int_functions()
        self.declare_vec_str_functions()
        self.declare_map_str_int_functions()
        self.declare_map_int_int_functions()
        self.declare_set_int_functions()
        self.declare_string_functions()
