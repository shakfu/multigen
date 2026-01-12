/**
 * Minimal vec_str runtime for LLVM backend
 * Dynamic string array implementation for list[str] support
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define VEC_STR_DEFAULT_CAPACITY 8
#define VEC_STR_GROWTH_FACTOR 2

// Dynamic string array structure
typedef struct {
    char** data;         // Array of string pointers
    size_t size;         // Number of elements
    size_t capacity;     // Allocated capacity
} vec_str;

// Internal helper function to grow capacity
static void vec_str_grow(vec_str* vec) {
    size_t new_capacity = (vec->capacity == 0) ? VEC_STR_DEFAULT_CAPACITY : vec->capacity * VEC_STR_GROWTH_FACTOR;
    char** new_data = realloc(vec->data, new_capacity * sizeof(char*));
    if (!new_data) {
        exit(1);  // Simple error handling
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}

// Create a new string vector
vec_str vec_str_init(void) {
    vec_str vec;
    vec.capacity = VEC_STR_DEFAULT_CAPACITY;
    vec.size = 0;
    vec.data = malloc(VEC_STR_DEFAULT_CAPACITY * sizeof(char*));
    if (!vec.data) {
        vec.capacity = 0;
        exit(1);
    }
    return vec;
}

// Initialize vector via pointer (for LLVM calling convention)
void vec_str_init_ptr(vec_str* out) {
    if (!out) {
        exit(1);
    }
    *out = vec_str_init();
}

// Append a string to the end (stores pointer, doesn't copy)
void vec_str_push(vec_str* vec, char* value) {
    if (!vec) {
        exit(1);
    }

    if (vec->size >= vec->capacity) {
        vec_str_grow(vec);
    }

    vec->data[vec->size++] = value;
}

// Get string pointer at index
char* vec_str_at(vec_str* vec, size_t index) {
    if (!vec || index >= vec->size) {
        exit(1);
    }
    return vec->data[index];
}

// Set string at index
void vec_str_set(vec_str* vec, size_t index, char* value) {
    if (!vec || index >= vec->size) {
        exit(1);
    }
    vec->data[index] = value;
}

// Get size of vector
size_t vec_str_size(vec_str* vec) {
    if (!vec) {
        return 0;
    }
    return vec->size;
}

// Free vector memory (doesn't free individual strings)
void vec_str_free(vec_str* vec) {
    if (vec && vec->data) {
        free(vec->data);
        vec->data = NULL;
        vec->size = 0;
        vec->capacity = 0;
    }
}

// Get pointer to data array
char** vec_str_data(vec_str* vec) {
    if (!vec) {
        return NULL;
    }
    return vec->data;
}

// Clear vector (keep capacity)
void vec_str_clear(vec_str* vec) {
    if (vec) {
        vec->size = 0;
    }
}

// Reserve capacity
void vec_str_reserve(vec_str* vec, size_t new_capacity) {
    if (!vec || new_capacity <= vec->capacity) {
        return;
    }

    char** new_data = realloc(vec->data, new_capacity * sizeof(char*));
    if (!new_data) {
        exit(1);
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
}
