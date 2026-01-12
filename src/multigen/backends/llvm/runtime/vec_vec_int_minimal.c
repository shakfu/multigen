/**
 * Minimal vec_vec_int runtime for LLVM backend
 * 2D integer array (nested list) support
 */

#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#define VEC_VEC_INT_DEFAULT_CAPACITY 8
#define VEC_VEC_INT_GROWTH_FACTOR 2

// vec_int structure (must match vec_int_minimal.c)
typedef struct {
    long long* data;
    size_t size;
    size_t capacity;
} vec_int;

// vec_vec_int structure (2D array)
typedef struct {
    vec_int* data;      // Array of vec_int structs
    size_t size;        // Number of rows
    size_t capacity;    // Allocated capacity
} vec_vec_int;

// Internal helper function
static void vec_vec_int_grow(vec_vec_int* vec) {
    size_t new_capacity = (vec->capacity == 0) ? VEC_VEC_INT_DEFAULT_CAPACITY : vec->capacity * VEC_VEC_INT_GROWTH_FACTOR;
    vec_int* new_data = realloc(vec->data, new_capacity * sizeof(vec_int));
    if (!new_data) {
        exit(1);
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

// Create a new 2D integer vector
vec_vec_int vec_vec_int_init(void) {
    vec_vec_int vec;
    vec.capacity = VEC_VEC_INT_DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(VEC_VEC_INT_DEFAULT_CAPACITY * sizeof(vec_int));
    if (!vec.data) {
        vec.capacity = 0;
        exit(1);
    }
    return vec;
}

// Initialize 2D vector via pointer (for LLVM calling convention)
void vec_vec_int_init_ptr(vec_vec_int* out) {
    if (!out) {
        exit(1);
    }
    *out = vec_vec_int_init();
}

// Append a row (vec_int) to the end
// Note: This DEEP copies the vec_int structure (allocates new memory for data)
void vec_vec_int_push(vec_vec_int* vec, vec_int* row) {
    if (!vec) {
        exit(1);
    }
    if (!row) {
        exit(1);
    }

    if (vec->size >= vec->capacity) {
        vec_vec_int_grow(vec);
    }

    // Deep copy the row - allocate new memory and copy data
    vec_int row_copy;
    row_copy.size = row->size;
    row_copy.capacity = row->capacity;
    row_copy.data = malloc(row->capacity * sizeof(long long));
    if (!row_copy.data) {
        exit(1);
    }
    // Copy the actual data elements
    if (row->data && row->size > 0) {
        memcpy(row_copy.data, row->data, row->size * sizeof(long long));
    }

    // Store the deep copy in the array
    vec->data[vec->size++] = row_copy;
}

// Get pointer to row at index
vec_int* vec_vec_int_at(vec_vec_int* vec, size_t index) {
    if (!vec || index >= vec->size) {
        exit(1);
    }
    return &vec->data[index];
}

// Get size of 2D vector (number of rows)
size_t vec_vec_int_size(vec_vec_int* vec) {
    if (!vec) {
        return 0;
    }
    return vec->size;
}

// Free 2D vector memory
void vec_vec_int_free(vec_vec_int* vec) {
    if (vec && vec->data) {
        // Free each row's data
        for (size_t i = 0; i < vec->size; i++) {
            if (vec->data[i].data) {
                free(vec->data[i].data);
            }
        }
        // Free the array of rows
        free(vec->data);
        vec->data = NULL;
        vec->size = 0;
        vec->capacity = 0;
    }
}

// Clear 2D vector (keep capacity, free row data)
void vec_vec_int_clear(vec_vec_int* vec) {
    if (vec) {
        // Free each row's data
        for (size_t i = 0; i < vec->size; i++) {
            if (vec->data[i].data) {
                free(vec->data[i].data);
            }
        }
        vec->size = 0;
    }
}
