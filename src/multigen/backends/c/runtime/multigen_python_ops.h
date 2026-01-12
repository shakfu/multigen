/**
 * MultiGen Runtime Library - Python-specific Operations
 *
 * Provides Python-specific operations that complement STC containers.
 * This module focuses on Python semantics not naturally provided by STC.
 */

#ifndef MGEN_PYTHON_OPS_H
#define MGEN_PYTHON_OPS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "multigen_error_handling.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Python built-in functions that aren't container-specific
 */

/**
 * Python bool() function
 */
int multigen_bool(const void* obj, int (*is_truthy)(const void*));

/**
 * Python bool() for integers
 */
int multigen_bool_int(int value);

/**
 * Python bool() for floats
 */
int multigen_bool_float(double value);

/**
 * Python bool() for strings (C string version)
 */
int multigen_bool_cstring(const char* str);

/**
 * Python abs() function
 */
int multigen_abs_int(int value);
double multigen_abs_float(double value);

/**
 * Python min() and max() for arrays
 */
int multigen_min_int_array(const int* arr, size_t size);
int multigen_max_int_array(const int* arr, size_t size);
double multigen_min_float_array(const double* arr, size_t size);
double multigen_max_float_array(const double* arr, size_t size);

/**
 * Python sum() for arrays
 */
int multigen_sum_int_array(const int* arr, size_t size);
double multigen_sum_float_array(const double* arr, size_t size);

/**
 * Python range() functionality
 */
typedef struct {
    int start;
    int stop;
    int step;
    int current;
} multigen_range_t;

multigen_range_t multigen_range(int stop);
multigen_range_t multigen_range_start_stop(int start, int stop);
multigen_range_t multigen_range_full(int start, int stop, int step);

int multigen_range_next(multigen_range_t* range);
int multigen_range_has_next(const multigen_range_t* range);

/**
 * Python-style string character classification
 */
int multigen_isalpha_char(char c);
int multigen_isdigit_char(char c);
int multigen_isspace_char(char c);
int multigen_isalnum_char(char c);

/**
 * Python-style string case conversion for single characters
 */
char multigen_lower_char(char c);
char multigen_upper_char(char c);

/**
 * Python ord() and chr() functions
 */
int multigen_ord(char c);
char multigen_chr(int code);

/**
 * Python-style comparison functions
 */
int multigen_cmp_int(int a, int b);
int multigen_cmp_float(double a, double b);
int multigen_cmp_string(const char* a, const char* b);

/**
 * Python-style slice object
 */
typedef struct {
    int start;
    int stop;
    int step;
    int has_start;
    int has_stop;
    int has_step;
} multigen_python_slice_t;

multigen_python_slice_t multigen_slice_new(void);
multigen_python_slice_t multigen_slice_start_stop(int start, int stop);
multigen_python_slice_t multigen_slice_full(int start, int stop, int step);

/**
 * Normalize Python slice for a given sequence length
 */
typedef struct {
    size_t start;
    size_t stop;
    size_t step;
    size_t length;
} multigen_normalized_slice_t;

multigen_error_t multigen_normalize_python_slice(const multigen_python_slice_t* slice,
                                        size_t seq_len,
                                        multigen_normalized_slice_t* result);

/**
 * Python-style exception information
 */
typedef struct {
    multigen_error_t type;
    char message[256];
    char traceback[512];
} multigen_exception_t;

extern multigen_exception_t multigen_current_exception;

void multigen_raise_exception(multigen_error_t type, const char* message);
void multigen_clear_exception(void);
int multigen_has_exception(void);
const multigen_exception_t* multigen_get_exception(void);

/**
 * Python-style assert
 */
#define multigen_assert(condition, message) \
    do { \
        if (!(condition)) { \
            multigen_raise_exception(MGEN_ERROR_RUNTIME, message); \
            return; \
        } \
    } while(0)

#define multigen_assert_return(condition, message, retval) \
    do { \
        if (!(condition)) { \
            multigen_raise_exception(MGEN_ERROR_RUNTIME, message); \
            return (retval); \
        } \
    } while(0)

/**
 * Python-style try/except simulation
 */
#define MGEN_TRY \
    do { \
        multigen_clear_exception(); \

#define MGEN_EXCEPT(error_type) \
        if (multigen_has_exception() && multigen_get_exception()->type == (error_type)) {

#define MGEN_EXCEPT_ANY \
        if (multigen_has_exception()) {

#define MGEN_FINALLY \
        } \
        if (1) {

#define MGEN_END_TRY \
        } \
    } while(0)

/**
 * Python-style truthiness testing
 */
int multigen_is_truthy_int(int value);
int multigen_is_truthy_float(double value);
int multigen_is_truthy_cstring(const char* str);
int multigen_is_truthy_pointer(const void* ptr);

/**
 * Python-style type checking
 */
typedef enum {
    MGEN_TYPE_NONE,
    MGEN_TYPE_BOOL,
    MGEN_TYPE_INT,
    MGEN_TYPE_FLOAT,
    MGEN_TYPE_STRING,
    MGEN_TYPE_LIST,
    MGEN_TYPE_DICT,
    MGEN_TYPE_SET,
    MGEN_TYPE_TUPLE
} multigen_python_type_t;

const char* multigen_type_name(multigen_python_type_t type);

/**
 * Python-style format string operations (simplified)
 */
char* multigen_format_simple(const char* template_str, const char* arg);
char* multigen_format_int(const char* template_str, int value);
char* multigen_format_float(const char* template_str, double value);

/**
 * Python zip() functionality for two arrays
 */
typedef struct {
    void* first;
    void* second;
    size_t index;
    size_t size1;
    size_t size2;
    size_t element_size1;
    size_t element_size2;
} multigen_zip_iterator_t;

multigen_zip_iterator_t multigen_zip_arrays(void* arr1, size_t size1, size_t elem_size1,
                                   void* arr2, size_t size2, size_t elem_size2);

int multigen_zip_next(multigen_zip_iterator_t* iter, void** elem1, void** elem2);

/**
 * Python-style iteration helpers
 */
typedef struct {
    size_t index;
    void* element;
} multigen_enumerate_item_t;

typedef void (*multigen_python_enumerate_callback_t)(const multigen_enumerate_item_t* item, void* userdata);

void multigen_enumerate_array(void* array, size_t size, size_t element_size,
                         multigen_python_enumerate_callback_t callback, void* userdata);

/**
 * Python print() function equivalents
 */
void print_int(int value);
void print_float(double value);
void print_string(const char* str);

// Generic print macro using C11 _Generic for type-based dispatch
#define print(x) _Generic((x), \
    int: print_int, \
    long: print_int, \
    float: print_float, \
    double: print_float, \
    char*: print_string, \
    const char*: print_string, \
    default: print_int \
)(x)

#ifdef __cplusplus
}
#endif

#endif // MGEN_PYTHON_OPS_H