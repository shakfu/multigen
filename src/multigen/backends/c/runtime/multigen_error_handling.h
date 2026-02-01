/**
 * MultiGen Runtime Library - Error Handling
 *
 * Provides error handling and reporting utilities for generated C code.
 * This module handles runtime errors in a consistent, Python-like manner.
 */

#ifndef MGEN_ERROR_HANDLING_H
#define MGEN_ERROR_HANDLING_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

// Error codes matching common Python exceptions
typedef enum {
    MGEN_OK = 0,
    MGEN_ERROR_GENERIC = 1,
    MGEN_ERROR_MEMORY = 2,          // MemoryError
    MGEN_ERROR_INDEX = 3,           // IndexError
    MGEN_ERROR_KEY = 4,             // KeyError
    MGEN_ERROR_VALUE = 5,           // ValueError
    MGEN_ERROR_TYPE = 6,            // TypeError
    MGEN_ERROR_IO = 7,              // IOError/OSError
    MGEN_ERROR_FILE_NOT_FOUND = 8, // FileNotFoundError
    MGEN_ERROR_PERMISSION = 9,      // PermissionError
    MGEN_ERROR_RUNTIME = 10         // RuntimeError
} multigen_error_t;

// Error context structure
typedef struct {
    multigen_error_t code;
    char message[512];
    const char* file;
    int line;
    const char* function;
} multigen_error_context_t;

// Global error context (thread-local in multi-threaded environments)
extern multigen_error_context_t multigen_last_error;

/**
 * Set error with detailed context information
 */
void multigen_set_error(multigen_error_t code, const char* message,
                   const char* file, int line, const char* function);

/**
 * Set error with formatted message
 */
void multigen_set_error_fmt(multigen_error_t code, const char* file, int line,
                       const char* function, const char* format, ...);

/**
 * Get the last error code
 */
multigen_error_t multigen_get_last_error(void);

/**
 * Get the last error message
 */
const char* multigen_get_last_error_message(void);

/**
 * Clear the last error
 */
void multigen_clear_error(void);

/**
 * Check if there's a pending error
 */
int multigen_has_error(void);

/**
 * Print error information to stderr
 */
void multigen_print_error(void);

/**
 * Convert system errno to MultiGen error code
 */
multigen_error_t multigen_errno_to_error(int errno_val);

/**
 * Get error name as string
 */
const char* multigen_error_name(multigen_error_t code);

// Convenience macros for error handling
#define MGEN_SET_ERROR(code, msg) \
    multigen_set_error((code), (msg), __FILE__, __LINE__, __func__)

#define MGEN_SET_ERROR_FMT(code, fmt, ...) \
    multigen_set_error_fmt((code), __FILE__, __LINE__, __func__, (fmt), ##__VA_ARGS__)

#define MGEN_RETURN_IF_ERROR(expr) \
    do { \
        if ((expr) != MGEN_OK) { \
            return multigen_get_last_error(); \
        } \
    } while(0)

#define MGEN_CHECK_NULL(ptr, msg) \
    do { \
        if ((ptr) == NULL) { \
            MGEN_SET_ERROR(MGEN_ERROR_MEMORY, (msg)); \
            return MGEN_ERROR_MEMORY; \
        } \
    } while(0)

#define MGEN_CHECK_BOUNDS(index, size, msg) \
    do { \
        if ((index) < 0 || (index) >= (size)) { \
            MGEN_SET_ERROR_FMT(MGEN_ERROR_INDEX, "%s: index %d out of bounds [0, %d)", \
                               (msg), (index), (size)); \
            return MGEN_ERROR_INDEX; \
        } \
    } while(0)

// Exception handling using setjmp/longjmp
// Maximum nesting depth for try/catch blocks
#define MGEN_MAX_TRY_DEPTH 16

// Exception context for setjmp/longjmp-based try/catch
typedef struct {
    jmp_buf env;
    multigen_error_t exception_type;
    char exception_message[256];
    int active;
} mgen_exception_context_t;

// Global exception stack
extern mgen_exception_context_t mgen_exception_stack[MGEN_MAX_TRY_DEPTH];
extern int mgen_exception_depth;

/**
 * Initialize exception handling system
 */
void mgen_exception_init(void);

/**
 * Push a new try block onto the exception stack
 */
int mgen_try_push(void);

/**
 * Pop a try block from the exception stack
 */
void mgen_try_pop(void);

/**
 * Throw an exception
 */
void mgen_throw(multigen_error_t type, const char* message);

/**
 * Get current exception type (for catch blocks)
 */
multigen_error_t mgen_current_exception_type(void);

/**
 * Get current exception message (for catch blocks)
 */
const char* mgen_current_exception_message(void);

/**
 * Re-throw current exception
 */
void mgen_rethrow(void);

// Macros for try/catch syntax
#define MGEN_TRY \
    do { \
        int _mgen_try_idx = mgen_try_push(); \
        if (_mgen_try_idx >= 0 && setjmp(mgen_exception_stack[_mgen_try_idx].env) == 0) {

#define MGEN_CATCH(exception_type) \
        } else if (mgen_current_exception_type() == (exception_type)) {

#define MGEN_CATCH_ALL \
        } else if (mgen_exception_depth >= 0) {

#define MGEN_END_TRY \
        } \
        mgen_try_pop(); \
    } while(0)

// Convenience macro for raising exceptions
#define MGEN_RAISE(exception_type, message) \
    mgen_throw((exception_type), (message))

// Map Python exception names to C error codes
#define MGEN_ZERO_DIVISION_ERROR MGEN_ERROR_VALUE
#define MGEN_VALUE_ERROR MGEN_ERROR_VALUE
#define MGEN_TYPE_ERROR MGEN_ERROR_TYPE
#define MGEN_KEY_ERROR MGEN_ERROR_KEY
#define MGEN_INDEX_ERROR MGEN_ERROR_INDEX
#define MGEN_RUNTIME_ERROR MGEN_ERROR_RUNTIME

#ifdef __cplusplus
}
#endif

#endif // MGEN_ERROR_HANDLING_H