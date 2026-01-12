/**
 * Dynamic array of C strings (char*)
 * Clean, type-safe implementation for code generation
 * stb-library style: static functions for single-file output
 * STC-compatible naming: vec_cstr
 */

#ifndef MGEN_VEC_CSTR_H
#define MGEN_VEC_CSTR_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Dynamic array of C strings structure
typedef struct {
    char** data;         // Array of string pointers
    size_t size;         // Number of strings
    size_t capacity;     // Allocated capacity
} vec_cstr;

/**
 * Create a new string vector
 * Initial capacity defaults to 8
 */

/**
 * Append a string to the end (STC-compatible)
 * Makes a copy of the string (uses strdup)
 * No return value - for compatibility with STC vec_cstr_push(&vec, str)
 */

/**
 * Get pointer to string at index (STC-compatible)
 * Returns pointer to string pointer if valid, NULL if out of bounds
 */

/**
 * Get number of strings
 */

/**
 * Get allocated capacity
 */

/**
 * Remove last string (and free its memory)
 */

/**
 * Clear all strings (free each string's memory)
 */

/**
 * Free all memory (STC-compatible drop function)
 * Frees each string and the array itself
 */

/**
 * Check if vector is empty
 */

/**
 * Reserve capacity
 */

/**
 * Dynamic array of C strings (char*) - Implementation
 * STC-compatible naming for drop-in replacement
 */

#include "multigen_error_handling.h"
#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 8
#define GROWTH_FACTOR 2

static vec_cstr vec_cstr_init(void) {
    vec_cstr vec;
    vec.capacity = DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(DEFAULT_CAPACITY * sizeof(char*));
    if (!vec.data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate string vector");
        vec.capacity = 0;
    }
    return vec;
}

static void vec_cstr_grow(vec_cstr* vec) {
    // Handle first allocation if capacity is 0
    size_t new_capacity = (vec->capacity == 0) ? DEFAULT_CAPACITY : vec->capacity * GROWTH_FACTOR;
    char** new_data = realloc(vec->data, new_capacity * sizeof(char*));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow string vector");
        return;
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

static void vec_cstr_push(vec_cstr* vec, const char* str) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string vector");
        return;
    }

    if (vec->size >= vec->capacity) {
        vec_cstr_grow(vec);
    }

    // Duplicate the string to take ownership
    // Handle NULL strings by storing NULL
    if (str) {
        vec->data[vec->size] = strdup(str);
        if (!vec->data[vec->size]) {
            MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to duplicate string");
            return;
        }
    } else {
        vec->data[vec->size] = NULL;
    }

    vec->size++;
}

static char** vec_cstr_at(vec_cstr* vec, size_t index) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string vector");
        return NULL;
    }

    if (index >= vec->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Index out of bounds");
        return NULL;
    }

    return &vec->data[index];
}

static inline size_t vec_cstr_size(const vec_cstr* vec) {
    return vec ? vec->size : 0;
}

static inline size_t vec_cstr_capacity(const vec_cstr* vec) {
    return vec ? vec->capacity : 0;
}

static void vec_cstr_pop(vec_cstr* vec) {
    if (!vec || vec->size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Empty or NULL string vector");
        return;
    }

    // Free the last string before removing it
    vec->size--;
    free(vec->data[vec->size]);
    vec->data[vec->size] = NULL;
}

static inline void vec_cstr_clear(vec_cstr* vec) {
    if (!vec) {
        return;
    }

    // Free all strings
    for (size_t i = 0; i < vec->size; i++) {
        free(vec->data[i]);
        vec->data[i] = NULL;
    }

    vec->size = 0;
}

static void vec_cstr_drop(vec_cstr* vec) {
    if (!vec) {
        return;
    }

    // Free all strings
    for (size_t i = 0; i < vec->size; i++) {
        free(vec->data[i]);
    }

    free(vec->data);
    vec->data = NULL;
    vec->size = 0;
    vec->capacity = 0;
}

static inline bool vec_cstr_empty(const vec_cstr* vec) {
    return !vec || vec->size == 0;
}

static void vec_cstr_reserve(vec_cstr* vec, size_t new_capacity) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string vector");
        return;
    }

    if (new_capacity <= vec->capacity) {
        return; // Already have enough capacity
    }

    char** new_data = realloc(vec->data, new_capacity * sizeof(char*));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to reserve capacity");
        return;
    }

    vec->data = new_data;
    vec->capacity = new_capacity;
}


// Implementation

#ifdef __cplusplus
}
#endif

#endif // MGEN_VEC_CSTR_H
