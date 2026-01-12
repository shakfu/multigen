/**
 * MultiGen LLVM Backend - String Operations Runtime
 *
 * Minimal string operations for LLVM backend
 */

#ifndef MGEN_LLVM_STRING_H
#define MGEN_LLVM_STRING_H

#include <stddef.h>

// String array type (for split results)
typedef struct {
    char** strings;
    size_t count;
    size_t capacity;
} multigen_string_array_t;

/**
 * Create a new string array
 */
multigen_string_array_t* multigen_string_array_new(void);

/**
 * Free a string array
 */
void multigen_string_array_free(multigen_string_array_t* arr);

/**
 * Add a string to the array
 */
void multigen_string_array_add(multigen_string_array_t* arr, char* str);

/**
 * Get string at index
 */
const char* multigen_string_array_get(multigen_string_array_t* arr, size_t index);

/**
 * Get array size
 */
size_t multigen_string_array_size(multigen_string_array_t* arr);

/**
 * Python str.split() equivalent
 * Returns an array of strings split by delimiter (whitespace if delimiter is NULL/empty)
 */
multigen_string_array_t* multigen_str_split(const char* str, const char* delimiter);

/**
 * Python str.lower() equivalent
 */
char* multigen_str_lower(const char* str);

/**
 * Python str.strip() equivalent
 */
char* multigen_str_strip(const char* str);

/**
 * String concatenation
 */
char* multigen_str_concat(const char* str1, const char* str2);

/**
 * String duplicate
 */
char* multigen_strdup(const char* str);

/**
 * Python str.join() equivalent
 * Joins strings in array with separator
 * Example: multigen_str_join(", ", ["a", "b", "c"]) -> "a, b, c"
 */
char* multigen_str_join(const char* separator, multigen_string_array_t* strings);

/**
 * Python str.replace() equivalent
 * Replaces all occurrences of old with new
 * Example: multigen_str_replace("hello world", "world", "python") -> "hello python"
 */
char* multigen_str_replace(const char* str, const char* old, const char* new_str);

/**
 * Python str.upper() equivalent
 */
char* multigen_str_upper(const char* str);

/**
 * Python str.startswith() equivalent
 */
int multigen_str_startswith(const char* str, const char* prefix);

/**
 * Python str.endswith() equivalent
 */
int multigen_str_endswith(const char* str, const char* suffix);

#endif // MGEN_LLVM_STRING_H
