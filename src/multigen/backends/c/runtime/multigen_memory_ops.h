/**
 * MultiGen Runtime Library - Memory Management Utilities
 *
 * Provides safe memory management utilities for generated C code.
 * These functions handle error checking and automatic cleanup.
 */

#ifndef MGEN_MEMORY_OPS_H
#define MGEN_MEMORY_OPS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "multigen_error_handling.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Safe memory allocation with error handling
 */
void* multigen_malloc(size_t size);

/**
 * Safe memory reallocation with error handling
 */
void* multigen_realloc(void* ptr, size_t new_size);

/**
 * Safe memory allocation for arrays with overflow checking
 */
void* multigen_calloc(size_t count, size_t size);

/**
 * Safe memory deallocation (sets pointer to NULL)
 */
void multigen_free(void** ptr);

/**
 * Safe memory copy with bounds checking
 */
multigen_error_t multigen_memcpy_safe(void* dest, size_t dest_size,
                             const void* src, size_t src_size);

/**
 * Safe memory move with bounds checking
 */
multigen_error_t multigen_memmove_safe(void* dest, size_t dest_size,
                              const void* src, size_t src_size);

/**
 * Safe memory set with bounds checking
 */
multigen_error_t multigen_memset_safe(void* dest, int value, size_t count, size_t dest_size);

/**
 * Memory pool for efficient allocation/deallocation
 */
typedef struct multigen_memory_pool multigen_memory_pool_t;

/**
 * Create a new memory pool
 */
multigen_memory_pool_t* multigen_memory_pool_new(size_t initial_size);

/**
 * Allocate from memory pool
 */
void* multigen_memory_pool_alloc(multigen_memory_pool_t* pool, size_t size);

/**
 * Reset memory pool (deallocates all at once)
 */
void multigen_memory_pool_reset(multigen_memory_pool_t* pool);

/**
 * Free memory pool
 */
void multigen_memory_pool_free(multigen_memory_pool_t* pool);

/**
 * Automatic memory management with scope-based cleanup
 */
typedef struct multigen_scope_allocator multigen_scope_allocator_t;

/**
 * Create a new scope allocator
 */
multigen_scope_allocator_t* multigen_scope_new(void);

/**
 * Allocate memory that will be automatically freed when scope ends
 */
void* multigen_scope_alloc(multigen_scope_allocator_t* scope, size_t size);

/**
 * Register existing pointer for automatic cleanup
 */
multigen_error_t multigen_scope_register(multigen_scope_allocator_t* scope, void* ptr);

/**
 * Free all allocations in scope
 */
void multigen_scope_free(multigen_scope_allocator_t* scope);

/**
 * Memory debugging and leak detection
 */
typedef struct multigen_memory_stats {
    size_t total_allocated;
    size_t total_freed;
    size_t current_allocated;
    size_t peak_allocated;
    size_t allocation_count;
    size_t free_count;
} multigen_memory_stats_t;

/**
 * Enable memory tracking
 */
void multigen_memory_tracking_enable(void);

/**
 * Disable memory tracking
 */
void multigen_memory_tracking_disable(void);

/**
 * Get current memory statistics
 */
multigen_memory_stats_t multigen_get_memory_stats(void);

/**
 * Print memory statistics
 */
void multigen_print_memory_stats(void);

/**
 * Check for memory leaks
 */
int multigen_check_memory_leaks(void);

/**
 * Reference counting utilities
 */
typedef struct multigen_refcounted {
    int refcount;
    void (*destructor)(void* data);
    char data[];
} multigen_refcounted_t;

/**
 * Create reference counted object
 */
multigen_refcounted_t* multigen_refcounted_new(size_t data_size, void (*destructor)(void*));

/**
 * Increment reference count
 */
multigen_refcounted_t* multigen_refcounted_retain(multigen_refcounted_t* obj);

/**
 * Decrement reference count (frees if reaches 0)
 */
void multigen_refcounted_release(multigen_refcounted_t* obj);

/**
 * Get reference count
 */
int multigen_refcounted_count(multigen_refcounted_t* obj);

/**
 * Get data pointer from reference counted object
 */
void* multigen_refcounted_data(multigen_refcounted_t* obj);

/**
 * Buffer management for string operations
 */
typedef struct multigen_buffer {
    char* data;
    size_t size;
    size_t capacity;
} multigen_buffer_t;

/**
 * Create a new dynamic buffer
 */
multigen_buffer_t* multigen_buffer_new(size_t initial_capacity);

/**
 * Append data to buffer
 */
multigen_error_t multigen_buffer_append(multigen_buffer_t* buffer, const char* data, size_t len);

/**
 * Append string to buffer
 */
multigen_error_t multigen_buffer_append_str(multigen_buffer_t* buffer, const char* str);

/**
 * Append formatted string to buffer
 */
multigen_error_t multigen_buffer_append_fmt(multigen_buffer_t* buffer, const char* format, ...);

/**
 * Get buffer data as C string
 */
const char* multigen_buffer_cstr(multigen_buffer_t* buffer);

/**
 * Get buffer size
 */
size_t multigen_buffer_size(multigen_buffer_t* buffer);

/**
 * Clear buffer (reset size to 0)
 */
void multigen_buffer_clear(multigen_buffer_t* buffer);

/**
 * Free buffer
 */
void multigen_buffer_free(multigen_buffer_t* buffer);

/**
 * RAII-style cleanup macros
 */
#define MGEN_SCOPE_BEGIN() \
    do { \
        multigen_scope_allocator_t* __scope = multigen_scope_new(); \
        if (!__scope) break;

#define MGEN_SCOPE_END() \
        multigen_scope_free(__scope); \
    } while(0)

#define MGEN_SCOPE_ALLOC(size) \
    multigen_scope_alloc(__scope, (size))

#define MGEN_SCOPE_REGISTER(ptr) \
    multigen_scope_register(__scope, (ptr))

/**
 * Memory pool macros for common patterns
 */
#define MGEN_POOL_BEGIN(size) \
    do { \
        multigen_memory_pool_t* __pool = multigen_memory_pool_new(size); \
        if (!__pool) break;

#define MGEN_POOL_END() \
        multigen_memory_pool_free(__pool); \
    } while(0)

#define MGEN_POOL_ALLOC(size) \
    multigen_memory_pool_alloc(__pool, (size))

#ifdef __cplusplus
}
#endif

#endif // MGEN_MEMORY_OPS_H