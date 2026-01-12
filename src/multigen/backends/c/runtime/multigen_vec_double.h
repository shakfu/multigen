/**
 * Simple dynamic array for doubles
 * Single-header implementation for code generation
 * stb-library style: static functions for single-file output
 */

#ifndef MGEN_VEC_DOUBLE_H
#define MGEN_VEC_DOUBLE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "multigen_error_handling.h"

#ifdef __cplusplus
extern "C" {
#endif

#define VEC_DOUBLE_DEFAULT_CAPACITY 8
#define VEC_DOUBLE_GROWTH_FACTOR 2

// Dynamic double array structure (STC-compatible naming)
typedef struct {
    double* data;           // Array data
    size_t size;         // Number of elements
    size_t capacity;     // Allocated capacity
} vec_double;

// Internal helper function
static void vec_double_grow(vec_double* vec) {
    // Handle first allocation if capacity is 0
    size_t new_capacity = (vec->capacity == 0) ? VEC_DOUBLE_DEFAULT_CAPACITY : vec->capacity * VEC_DOUBLE_GROWTH_FACTOR;
    double* new_data = realloc(vec->data, new_capacity * sizeof(double));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow vector");
        return;
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

/**
 * Create a new double vector
 * Initial capacity defaults to 8
 */
static vec_double vec_double_init(void) {
    vec_double vec;
    vec.capacity = VEC_DOUBLE_DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(VEC_DOUBLE_DEFAULT_CAPACITY * sizeof(double));
    if (!vec.data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate vector");
        vec.capacity = 0;
    }
    return vec;
}

/**
 * Append an element to the end (STC-compatible)
 * No return value - for compatibility with STC vec_double_push(&vec, value)
 */
static void vec_double_push(vec_double* vec, double value) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL vector");
        return;
    }

    if (vec->size >= vec->capacity) {
        vec_double_grow(vec);
    }

    vec->data[vec->size++] = value;
}

/**
 * Get pointer to element at index (STC-compatible)
 * Returns pointer to element if valid, NULL if out of bounds
 */
static double* vec_double_at(vec_double* vec, size_t index) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL vector");
        return NULL;
    }

    if (index >= vec->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Index out of bounds");
        return NULL;
    }

    return &vec->data[index];
}

/**
 * Get number of elements
 */
static inline size_t vec_double_size(const vec_double* vec) {
    return vec ? vec->size : 0;
}

/**
 * Get allocated capacity
 */
static inline size_t vec_double_capacity(const vec_double* vec) {
    return vec ? vec->capacity : 0;
}

/**
 * Remove last element
 */
static void vec_double_pop(vec_double* vec) {
    if (!vec || vec->size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Empty or NULL vector");
        return;
    }
    vec->size--;
}

/**
 * Clear all elements (keep capacity)
 */
static inline void vec_double_clear(vec_double* vec) {
    if (vec) {
        vec->size = 0;
    }
}

/**
 * Free all memory (STC-compatible drop function)
 */
static void vec_double_drop(vec_double* vec) {
    if (!vec) {
        return;
    }
    free(vec->data);
    vec->data = NULL;
    vec->size = 0;
    vec->capacity = 0;
}

/**
 * Check if vector is empty
 */
static inline bool vec_double_empty(const vec_double* vec) {
    return !vec || vec->size == 0;
}

/**
 * Reserve capacity
 */
static void vec_double_reserve(vec_double* vec, size_t new_capacity) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL vector");
        return;
    }

    if (new_capacity <= vec->capacity) {
        return; // Already have enough capacity
    }

    double* new_data = realloc(vec->data, new_capacity * sizeof(double));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to reserve capacity");
        return;
    }

    vec->data = new_data;
    vec->capacity = new_capacity;
}

#ifdef __cplusplus
}
#endif

#endif // MGEN_VEC_DOUBLE_H
