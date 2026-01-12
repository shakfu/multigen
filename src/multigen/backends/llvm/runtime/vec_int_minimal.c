/**
 * Minimal vec_int runtime for LLVM backend
 * Single file implementation with all dependencies included
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

// Error handling placeholder
#define MGEN_SET_ERROR(code, msg) ((void)0)
#define MGEN_ERROR_MEMORY 1
#define MGEN_ERROR_VALUE 2

#define VEC_INT_DEFAULT_CAPACITY 8
#define VEC_INT_GROWTH_FACTOR 2

// Dynamic integer array structure
typedef struct {
    long long* data;     // Array data (using long long for i64 compatibility)
    size_t size;         // Number of elements
    size_t capacity;     // Allocated capacity
} vec_int;

// Internal helper function
static void vec_int_grow(vec_int* vec) {
    size_t new_capacity = (vec->capacity == 0) ? VEC_INT_DEFAULT_CAPACITY : vec->capacity * VEC_INT_GROWTH_FACTOR;
    long long* new_data = realloc(vec->data, new_capacity * sizeof(long long));
    if (!new_data) {
        fprintf(stderr, "vec_int error: Failed to allocate memory for capacity %zu\n", new_capacity);
        exit(1);
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

// Create a new integer vector
vec_int vec_int_init(void) {
    vec_int vec;
    vec.capacity = VEC_INT_DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(VEC_INT_DEFAULT_CAPACITY * sizeof(long long));
    if (!vec.data) {
        vec.capacity = 0;
        fprintf(stderr, "vec_int error: Failed to allocate initial memory\n");
        exit(1);
    }
    return vec;
}

// Initialize vector via pointer (for LLVM calling convention)
void vec_int_init_ptr(vec_int* out) {
    if (!out) {
        fprintf(stderr, "vec_int error: NULL pointer passed to vec_int_init_ptr\n");
        exit(1);
    }
    *out = vec_int_init();
}

// Append an element to the end
void vec_int_push(vec_int* vec, long long value) {
    if (!vec) {
        fprintf(stderr, "vec_int error: NULL pointer passed to vec_int_push\n");
        exit(1);
    }

    if (vec->size >= vec->capacity) {
        vec_int_grow(vec);
    }

    vec->data[vec->size++] = value;
}

// Get element at index
long long vec_int_at(vec_int* vec, size_t index) {
    if (!vec) {
        fprintf(stderr, "vec_int error: NULL pointer passed to vec_int_at\n");
        exit(1);
    }
    if (index >= vec->size) {
        fprintf(stderr, "vec_int error: Index %zu out of bounds (size = %zu)\n", index, vec->size);
        exit(1);
    }
    return vec->data[index];
}

// Set element at index
void vec_int_set(vec_int* vec, size_t index, long long value) {
    if (!vec) {
        fprintf(stderr, "vec_int error: NULL pointer passed to vec_int_set\n");
        exit(1);
    }
    if (index >= vec->size) {
        fprintf(stderr, "vec_int error: Index %zu out of bounds for set (size = %zu)\n", index, vec->size);
        exit(1);
    }
    vec->data[index] = value;
}

// Get size of vector
size_t vec_int_size(vec_int* vec) {
    if (!vec) {
        return 0;
    }
    return vec->size;
}

// Free vector memory
void vec_int_free(vec_int* vec) {
    if (vec && vec->data) {
        free(vec->data);
        vec->data = NULL;
        vec->size = 0;
        vec->capacity = 0;
    }
}

// Get pointer to data array
long long* vec_int_data(vec_int* vec) {
    if (!vec) {
        return NULL;
    }
    return vec->data;
}

// Clear vector (keep capacity)
void vec_int_clear(vec_int* vec) {
    if (vec) {
        vec->size = 0;
    }
}

// Reserve capacity
void vec_int_reserve(vec_int* vec, size_t new_capacity) {
    if (!vec || new_capacity <= vec->capacity) {
        return;
    }

    long long* new_data = realloc(vec->data, new_capacity * sizeof(long long));
    if (!new_data) {
        fprintf(stderr, "vec_int error: Failed to reserve capacity %zu\n", new_capacity);
        exit(1);
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}
