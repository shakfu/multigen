/**
 * Dynamic array of integer vectors (2D arrays)
 * Clean, type-safe implementation for code generation
 * stb-library style: static functions for single-file output
 * STC-compatible naming: vec_vec_int
 */

#ifndef MGEN_VEC_VEC_INT_H
#define MGEN_VEC_VEC_INT_H

#include <stddef.h>
#include <stdbool.h>
#include "multigen_vec_int.h"

#ifdef __cplusplus
extern "C" {
#endif

// Dynamic array of vec_int (2D integer array structure)
typedef struct {
    vec_int* data;       // Array of vec_int structs
    size_t size;         // Number of rows
    size_t capacity;     // Allocated capacity
} vec_vec_int;

/**
 * Create a new 2D integer vector
 * Initial capacity defaults to 8
 */

/**
 * Append a row (vec_int) to the end (STC-compatible)
 * Takes ownership of the vec_int and copies it
 */

/**
 * Get pointer to row at index (STC-compatible)
 * Returns pointer to vec_int if valid, NULL if out of bounds
 */

/**
 * Get number of rows
 */

/**
 * Get allocated capacity
 */

/**
 * Remove last row (and free its memory)
 */

/**
 * Clear all rows (free each row's memory)
 */

/**
 * Free all memory (STC-compatible drop function)
 * Calls vec_int_drop on each row
 */

/**
 * Check if 2D vector is empty
 */

/**
 * Reserve capacity for rows
 */

/**
 * Dynamic array of integer vectors (2D arrays) - Implementation
 * STC-compatible naming for drop-in replacement
 */

#include "multigen_error_handling.h"
#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 8
#define GROWTH_FACTOR 2

static vec_vec_int vec_vec_int_init(void) {
    vec_vec_int vec;
    vec.capacity = DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(DEFAULT_CAPACITY * sizeof(vec_int));
    if (!vec.data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate 2D vector");
        vec.capacity = 0;
    }
    return vec;
}

static void vec_vec_int_grow(vec_vec_int* vec) {
    // Handle first allocation if capacity is 0
    size_t new_capacity = (vec->capacity == 0) ? DEFAULT_CAPACITY : vec->capacity * GROWTH_FACTOR;
    vec_int* new_data = realloc(vec->data, new_capacity * sizeof(vec_int));
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow 2D vector");
        return;
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

static void vec_vec_int_push(vec_vec_int* vec, vec_int row) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL 2D vector");
        return;
    }

    if (vec->size >= vec->capacity) {
        vec_vec_int_grow(vec);
    }

    // Copy the row into the array
    vec->data[vec->size++] = row;
}

static vec_int* vec_vec_int_at(vec_vec_int* vec, size_t index) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL 2D vector");
        return NULL;
    }

    if (index >= vec->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Index out of bounds");
        return NULL;
    }

    return &vec->data[index];
}

static inline size_t vec_vec_int_size(const vec_vec_int* vec) {
    return vec ? vec->size : 0;
}

static inline size_t vec_vec_int_capacity(const vec_vec_int* vec) {
    return vec ? vec->capacity : 0;
}

static void vec_vec_int_pop(vec_vec_int* vec) {
    if (!vec || vec->size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Empty or NULL 2D vector");
        return;
    }

    // Drop the last row's memory before removing it
    vec_int_drop(&vec->data[vec->size - 1]);
    vec->size--;
}

static inline void vec_vec_int_clear(vec_vec_int* vec) {
    if (!vec) {
        return;
    }

    // Drop each row's memory
    for (size_t i = 0; i < vec->size; i++) {
        vec_int_drop(&vec->data[i]);
    }

    vec->size = 0;
}

static void vec_vec_int_drop(vec_vec_int* vec) {
    if (!vec) {
        return;
    }

    // Drop each row's memory
    for (size_t i = 0; i < vec->size; i++) {
        vec_int_drop(&vec->data[i]);
    }

    free(vec->data);
    vec->data = NULL;
    vec->size = 0;
    vec->capacity = 0;
}

static inline bool vec_vec_int_empty(const vec_vec_int* vec) {
    return !vec || vec->size == 0;
}

static void vec_vec_int_reserve(vec_vec_int* vec, size_t new_capacity) {
    if (!vec) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL 2D vector");
        return;
    }

    if (new_capacity <= vec->capacity) {
        return; // Already have enough capacity
    }

    vec_int* new_data = realloc(vec->data, new_capacity * sizeof(vec_int));
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

#endif // MGEN_VEC_VEC_INT_H
