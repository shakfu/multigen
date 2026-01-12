/**
 * MultiGen LLVM Backend - String Operations Runtime Implementation
 */

#include "multigen_llvm_string.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// String array implementation
multigen_string_array_t* multigen_string_array_new(void) {
    multigen_string_array_t* arr = malloc(sizeof(multigen_string_array_t));
    if (!arr) return NULL;

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

void multigen_string_array_add(multigen_string_array_t* arr, char* str) {
    if (!arr) return;

    if (arr->count >= arr->capacity) {
        size_t new_capacity = arr->capacity == 0 ? 8 : arr->capacity * 2;
        char** new_strings = realloc(arr->strings, new_capacity * sizeof(char*));
        if (!new_strings) return;
        arr->strings = new_strings;
        arr->capacity = new_capacity;
    }

    arr->strings[arr->count++] = str;
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

char* multigen_strdup(const char* str) {
    if (!str) return NULL;

    size_t len = strlen(str);
    char* result = malloc(len + 1);
    if (!result) return NULL;

    strcpy(result, str);
    return result;
}

multigen_string_array_t* multigen_str_split(const char* str, const char* delimiter) {
    if (!str) return NULL;

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

    // Python's split() without args splits on any whitespace
    if (!delimiter || delimiter[0] == '\0') {
        token = strtok_r(str_copy, " \t\n\r\f\v", &saveptr);
        while (token != NULL) {
            char* token_copy = multigen_strdup(token);
            if (token_copy) {
                multigen_string_array_add(result, token_copy);
            }
            token = strtok_r(NULL, " \t\n\r\f\v", &saveptr);
        }
    } else {
        token = strtok_r(str_copy, delimiter, &saveptr);
        while (token != NULL) {
            char* token_copy = multigen_strdup(token);
            if (token_copy) {
                multigen_string_array_add(result, token_copy);
            }
            token = strtok_r(NULL, delimiter, &saveptr);
        }
    }

    free(str_copy);
    return result;
}

char* multigen_str_lower(const char* str) {
    if (!str) return NULL;

    char* result = multigen_strdup(str);
    if (!result) return NULL;

    for (char* p = result; *p; p++) {
        *p = tolower((unsigned char)*p);
    }

    return result;
}

char* multigen_str_strip(const char* str) {
    if (!str) return NULL;

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
    if (!result) return NULL;

    strncpy(result, str, len);
    result[len] = '\0';

    return result;
}

char* multigen_str_concat(const char* str1, const char* str2) {
    if (!str1 && !str2) return multigen_strdup("");
    if (!str1) return multigen_strdup(str2);
    if (!str2) return multigen_strdup(str1);

    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    char* result = malloc(len1 + len2 + 1);
    if (!result) return NULL;

    strcpy(result, str1);
    strcat(result, str2);

    return result;
}

char* multigen_str_join(const char* separator, multigen_string_array_t* strings) {
    if (!strings || strings->count == 0) {
        return multigen_strdup("");
    }

    if (strings->count == 1) {
        return multigen_strdup(strings->strings[0]);
    }

    const char* sep = separator ? separator : "";
    size_t sep_len = strlen(sep);

    // Calculate total length needed
    size_t total_len = 0;
    for (size_t i = 0; i < strings->count; i++) {
        total_len += strlen(strings->strings[i]);
        if (i < strings->count - 1) {
            total_len += sep_len;
        }
    }

    // Allocate result
    char* result = malloc(total_len + 1);
    if (!result) return NULL;

    // Build joined string
    char* ptr = result;
    for (size_t i = 0; i < strings->count; i++) {
        const char* str = strings->strings[i];
        size_t str_len = strlen(str);

        memcpy(ptr, str, str_len);
        ptr += str_len;

        // Add separator (except after last string)
        if (i < strings->count - 1 && sep_len > 0) {
            memcpy(ptr, sep, sep_len);
            ptr += sep_len;
        }
    }
    *ptr = '\0';

    return result;
}

char* multigen_str_replace(const char* str, const char* old, const char* new_str) {
    if (!str || !old || !new_str) return multigen_strdup(str);
    if (old[0] == '\0') return multigen_strdup(str);

    size_t old_len = strlen(old);
    size_t new_len = strlen(new_str);

    // Count occurrences of old in str
    size_t count = 0;
    const char* p = str;
    while ((p = strstr(p, old)) != NULL) {
        count++;
        p += old_len;
    }

    if (count == 0) {
        return multigen_strdup(str);
    }

    // Calculate new string length
    size_t str_len = strlen(str);
    size_t result_len = str_len + count * (new_len - old_len);

    // Allocate result
    char* result = malloc(result_len + 1);
    if (!result) return NULL;

    // Build result string
    char* dst = result;
    const char* src = str;
    while (*src) {
        const char* match = strstr(src, old);
        if (match) {
            // Copy text before match
            size_t prefix_len = match - src;
            memcpy(dst, src, prefix_len);
            dst += prefix_len;

            // Copy replacement
            memcpy(dst, new_str, new_len);
            dst += new_len;

            // Move past matched text
            src = match + old_len;
        } else {
            // Copy remaining text
            strcpy(dst, src);
            break;
        }
    }

    return result;
}

char* multigen_str_upper(const char* str) {
    if (!str) return NULL;

    char* result = multigen_strdup(str);
    if (!result) return NULL;

    for (char* p = result; *p; p++) {
        *p = toupper((unsigned char)*p);
    }

    return result;
}

int multigen_str_startswith(const char* str, const char* prefix) {
    if (!str || !prefix) return 0;

    size_t str_len = strlen(str);
    size_t prefix_len = strlen(prefix);

    if (prefix_len > str_len) return 0;

    return strncmp(str, prefix, prefix_len) == 0;
}

int multigen_str_endswith(const char* str, const char* suffix) {
    if (!str || !suffix) return 0;

    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);

    if (suffix_len > str_len) return 0;

    return strcmp(str + str_len - suffix_len, suffix) == 0;
}
