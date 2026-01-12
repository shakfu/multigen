/**
 * Simple dynamic array for floats
 * Single-header implementation for code generation
 * stb-library style: static functions for single-file output
 */

#ifndef MGEN_VEC_FLOAT_H
#define MGEN_VEC_FLOAT_H

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "multigen_error_handling.h"

#ifdef __cplusplus
extern "C" {
#endif

#define VEC_FLOAT_DEFAULT_CAPACITY 8
#define VEC_FLOAT_GROWTH_FACTOR 2

// Dynamic float array structure (STC-compatible naming)
typedef struct {
    float* data;           // Array data
    size_t size;         // Number of elements
    size_t capacity;     // Allocated capacity
} vec_float;

// Internal helper function
static void vec_float_grow(vec_float* vec) {
    // Handle first allocation if capacity is 0
    size_t new_capacity = (vec->capacity == 0) ? VEC_FLOAT_DEFAULT_CAPACITY : vec->capacity * VEC_FLOAT_GROWTH_FACTOR;
    float* new_data = realloc(vec->data, new_capacity * sizeof(float));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow vector");
        return;
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

/**
 * Create a new float vector
 * Initial capacity defaults to 8
 */
static vec_float vec_float_init(void) {
    vec_float vec;
    vec.capacity = VEC_FLOAT_DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(VEC_FLOAT_DEFAULT_CAPACITY * sizeof(float));
    if (!vec.data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate vector");
        vec.capacity = 0;
    }
    return vec;
}

/**
 * Append an element to the end (STC-compatible)
 * No return value - for compatibility with STC vec_float_push(&vec, value)
 */
static void vec_float_push(vec_float* vec, float value) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL vector");
        return;
    }

    if (vec->size >= vec->capacity) {
        vec_float_grow(vec);
    }

    vec->data[vec->size++] = value;
}

/**
 * Get pointer to element at index (STC-compatible)
 * Returns pointer to element if valid, NULL if out of bounds
 */
static float* vec_float_at(vec_float* vec, size_t index) {
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
static inline size_t vec_float_size(const vec_float* vec) {
    return vec ? vec->size : 0;
}

/**
 * Get allocated capacity
 */
static inline size_t vec_float_capacity(const vec_float* vec) {
    return vec ? vec->capacity : 0;
}

/**
 * Remove last element
 */
static void vec_float_pop(vec_float* vec) {
    if (!vec || vec->size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Empty or NULL vector");
        return;
    }
    vec->size--;
}

/**
 * Clear all elements (keep capacity)
 */
static inline void vec_float_clear(vec_float* vec) {
    if (vec) {
        vec->size = 0;
    }
}

/**
 * Free all memory (STC-compatible drop function)
 */
static void vec_float_drop(vec_float* vec) {
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
static inline bool vec_float_empty(const vec_float* vec) {
    return !vec || vec->size == 0;
}

/**
 * Reserve capacity
 */
static void vec_float_reserve(vec_float* vec, size_t new_capacity) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL vector");
        return;
    }

    if (new_capacity <= vec->capacity) {
        return; // Already have enough capacity
    }

    float* new_data = realloc(vec->data, new_capacity * sizeof(float));
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

#endif // MGEN_VEC_FLOAT_H
