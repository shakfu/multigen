/**
 * MultiGen Runtime Library - String Operations Implementation
 */

#include "multigen_string_ops.h"
#include <stdarg.h>
#include <stdbool.h>

// String array implementation
multigen_string_array_t* multigen_string_array_new(void) {
    multigen_string_array_t* arr = malloc(sizeof(multigen_string_array_t));
    if (!arr) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate string array");
        return NULL;
    }

    arr->strings = NULL;
    arr->count = 0;
    arr->capacity = 0;
    return arr;
}

void multigen_string_array_free(multigen_string_array_t* arr) {
    if (!arr) return;

    for (size_t i = 0; i < arr->count; i++) {
        free(arr->strings[i]);
    }
    free(arr->strings);
    free(arr);
}

multigen_error_t multigen_string_array_add(multigen_string_array_t* arr, char* str) {
    if (!arr) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (arr->count >= arr->capacity) {
        size_t new_capacity = arr->capacity == 0 ? 8 : arr->capacity * 2;
        char** new_strings = realloc(arr->strings, new_capacity * sizeof(char*));
        if (!new_strings) {
            MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to resize string array");
            return MGEN_ERROR_MEMORY;
        }
        arr->strings = new_strings;
        arr->capacity = new_capacity;
    }

    arr->strings[arr->count++] = str;
    return MGEN_OK;
}

const char* multigen_string_array_get(multigen_string_array_t* arr, size_t index) {
    if (!arr || index >= arr->count) {
        return NULL;
    }
    return arr->strings[index];
}

size_t multigen_string_array_size(multigen_string_array_t* arr) {
    return arr ? arr->count : 0;
}

char* multigen_join(const char* delimiter, multigen_string_array_t* strings) {
    if (!strings || strings->count == 0) {
        return multigen_strdup("");
    }

    if (!delimiter) delimiter = "";

    // Calculate total length needed
    size_t total_len = 0;
    size_t delim_len = strlen(delimiter);

    for (size_t i = 0; i < strings->count; i++) {
        if (strings->strings[i]) {
            total_len += strlen(strings->strings[i]);
        }
        if (i < strings->count - 1) {
            total_len += delim_len;
        }
    }

    char* result = malloc(total_len + 1);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for joined string");
        return NULL;
    }

    result[0] = '\0';
    for (size_t i = 0; i < strings->count; i++) {
        if (strings->strings[i]) {
            strcat(result, strings->strings[i]);
        }
        if (i < strings->count - 1) {
            strcat(result, delimiter);
        }
    }

    return result;
}

char* multigen_strdup(const char* str) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    size_t len = strlen(str);
    char* result = malloc(len + 1);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for string");
        return NULL;
    }

    strcpy(result, str);
    return result;
}

char* multigen_str_upper(const char* str) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    char* result = multigen_strdup(str);
    if (!result) return NULL;

    for (char* p = result; *p; p++) {
        *p = toupper((unsigned char)*p);
    }

    return result;
}

char* multigen_str_lower(const char* str) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    char* result = multigen_strdup(str);
    if (!result) return NULL;

    for (char* p = result; *p; p++) {
        *p = tolower((unsigned char)*p);
    }

    return result;
}

char* multigen_str_strip(const char* str) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    // Find start (skip leading whitespace)
    while (*str && isspace((unsigned char)*str)) {
        str++;
    }

    if (!*str) {
        // String was all whitespace
        return multigen_strdup("");
    }

    // Find end (skip trailing whitespace)
    const char* end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char)*end)) {
        end--;
    }

    // Calculate length and allocate
    size_t len = end - str + 1;
    char* result = malloc(len + 1);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for string");
        return NULL;
    }

    strncpy(result, str, len);
    result[len] = '\0';

    return result;
}

char* multigen_str_strip_chars(const char* str, const char* chars) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    if (!chars || !*chars) {
        return multigen_str_strip(str);
    }

    // Find start (skip leading chars)
    while (*str && strchr(chars, *str)) {
        str++;
    }

    if (!*str) {
        // String was all strip chars
        return multigen_strdup("");
    }

    // Find end (skip trailing chars)
    const char* end = str + strlen(str) - 1;
    while (end > str && strchr(chars, *end)) {
        end--;
    }

    // Calculate length and allocate
    size_t len = end - str + 1;
    char* result = malloc(len + 1);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for string");
        return NULL;
    }

    strncpy(result, str, len);
    result[len] = '\0';

    return result;
}

int multigen_str_find(const char* str, const char* substring) {
    if (!str || !substring) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String or substring is NULL");
        return -1;
    }

    const char* found = strstr(str, substring);
    if (found) {
        return (int)(found - str);
    }

    return -1;  // Not found
}

char* multigen_str_replace(const char* str, const char* old_str, const char* new_str) {
    if (!str || !old_str || !new_str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String arguments cannot be NULL");
        return NULL;
    }

    size_t old_len = strlen(old_str);
    size_t new_len = strlen(new_str);

    if (old_len == 0) {
        return multigen_strdup(str);  // Can't replace empty string
    }

    // Count occurrences to calculate result size
    const char* pos = str;
    int count = 0;
    while ((pos = strstr(pos, old_str)) != NULL) {
        count++;
        pos += old_len;
    }

    if (count == 0) {
        return multigen_strdup(str);  // No replacements needed
    }

    // Calculate result size
    size_t result_len = strlen(str) - count * old_len + count * new_len;
    char* result = malloc(result_len + 1);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for string");
        return NULL;
    }

    // Build result string
    char* dest = result;
    const char* src = str;

    while ((pos = strstr(src, old_str)) != NULL) {
        // Copy part before match
        size_t prefix_len = pos - src;
        strncpy(dest, src, prefix_len);
        dest += prefix_len;

        // Copy replacement
        strcpy(dest, new_str);
        dest += new_len;

        // Move past the match
        src = pos + old_len;
    }

    // Copy remaining part
    strcpy(dest, src);

    return result;
}

multigen_string_array_t* multigen_str_split(const char* str, const char* delimiter) {
    if (!str) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String is NULL");
        return NULL;
    }

    multigen_string_array_t* result = multigen_string_array_new();
    if (!result) return NULL;

    // Make a copy since strtok modifies the string
    char* str_copy = multigen_strdup(str);
    if (!str_copy) {
        multigen_string_array_free(result);
        return NULL;
    }

    char* token;
    char* saveptr = NULL;  // For thread-safe strtok_r

    if (!delimiter) {
        // Split on whitespace
        token = strtok_r(str_copy, " \t\n\r\f\v", &saveptr);
    } else {
        // Split on delimiter
        token = strtok_r(str_copy, delimiter, &saveptr);
    }

    while (token != NULL) {
        char* token_copy = multigen_strdup(token);
        if (token_copy) {
            multigen_string_array_add(result, token_copy);
        }

        if (!delimiter) {
            token = strtok_r(NULL, " \t\n\r\f\v", &saveptr);
        } else {
            token = strtok_r(NULL, delimiter, &saveptr);
        }
    }

    free(str_copy);
    return result;
}

// F-string support functions

char* multigen_int_to_string(int value) {
    // Allocate enough space for the string representation
    // Max int is 11 chars (-2147483648) + null terminator
    char* result = malloc(12);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for int to string");
        return NULL;
    }
    snprintf(result, 12, "%d", value);
    return result;
}

char* multigen_float_to_string(double value) {
    // Allocate enough space for float representation
    // Use 32 chars to handle most float values
    char* result = malloc(32);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for float to string");
        return NULL;
    }
    snprintf(result, 32, "%g", value);  // %g removes trailing zeros
    return result;
}

const char* multigen_bool_to_string(bool value) {
    // Return static strings - no need to free
    return value ? "true" : "false";
}

char* multigen_sprintf_string(const char* format, ...) {
    if (!format) {
        return multigen_strdup("");
    }

    va_list args, args_copy;
    va_start(args, format);

    // Make a copy for the second pass
    va_copy(args_copy, args);

    // Determine required size
    int size = vsnprintf(NULL, 0, format, args);
    va_end(args);

    if (size < 0) {
        va_end(args_copy);
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Failed to format string");
        return NULL;
    }

    // Allocate buffer
    char* result = malloc(size + 1);
    if (!result) {
        va_end(args_copy);
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for formatted string");
        return NULL;
    }

    // Format the string
    vsnprintf(result, size + 1, format, args_copy);
    va_end(args_copy);

    return result;
}

char* multigen_str_concat(const char* str1, const char* str2) {
    if (!str1 || !str2) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Cannot concatenate NULL strings");
        return NULL;
    }

    // Calculate total length needed
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t total_len = len1 + len2 + 1;  // +1 for null terminator

    // Allocate memory for concatenated string
    char* result = malloc(total_len);
    if (!result) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate memory for string concatenation");
        return NULL;
    }

    // Copy first string
    strcpy(result, str1);
    // Append second string
    strcat(result, str2);

    return result;
}