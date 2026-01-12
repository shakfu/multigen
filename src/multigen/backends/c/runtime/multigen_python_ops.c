/**
 * MultiGen Runtime Library - Python-specific Operations Implementation
 */

#include "multigen_python_ops.h"
#include <math.h>
#include <limits.h>
#include <float.h>

// Global exception state
multigen_exception_t multigen_current_exception = {MGEN_OK, "", ""};

/**
 * Python bool() function implementations
 */
int multigen_bool(const void* obj, int (*is_truthy)(const void*)) {
    if (!obj || !is_truthy) return 0;
    return is_truthy(obj);
}

int multigen_bool_int(int value) {
    return value != 0;
}

int multigen_bool_float(double value) {
    return value != 0.0 && !isnan(value);
}

int multigen_bool_cstring(const char* str) {
    return str != NULL && str[0] != '\0';
}

/**
 * Python abs() function implementations
 */
int multigen_abs_int(int value) {
    return value < 0 ? -value : value;
}

double multigen_abs_float(double value) {
    return fabs(value);
}

/**
 * Python min() and max() implementations
 */
int multigen_min_int_array(const int* arr, size_t size) {
    if (!arr || size == 0) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "min() arg is an empty sequence");
        return 0;
    }

    int min_val = arr[0];
    for (size_t i = 1; i < size; i++) {
        if (arr[i] < min_val) {
            min_val = arr[i];
        }
    }
    return min_val;
}

int multigen_max_int_array(const int* arr, size_t size) {
    if (!arr || size == 0) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "max() arg is an empty sequence");
        return 0;
    }

    int max_val = arr[0];
    for (size_t i = 1; i < size; i++) {
        if (arr[i] > max_val) {
            max_val = arr[i];
        }
    }
    return max_val;
}

double multigen_min_float_array(const double* arr, size_t size) {
    if (!arr || size == 0) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "min() arg is an empty sequence");
        return 0.0;
    }

    double min_val = arr[0];
    for (size_t i = 1; i < size; i++) {
        if (arr[i] < min_val || isnan(min_val)) {
            min_val = arr[i];
        }
    }
    return min_val;
}

double multigen_max_float_array(const double* arr, size_t size) {
    if (!arr || size == 0) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "max() arg is an empty sequence");
        return 0.0;
    }

    double max_val = arr[0];
    for (size_t i = 1; i < size; i++) {
        if (arr[i] > max_val || isnan(max_val)) {
            max_val = arr[i];
        }
    }
    return max_val;
}

/**
 * Python sum() implementations
 */
int multigen_sum_int_array(const int* arr, size_t size) {
    if (!arr) return 0;

    int sum = 0;
    for (size_t i = 0; i < size; i++) {
        // Check for overflow
        if ((sum > 0 && arr[i] > INT_MAX - sum) ||
            (sum < 0 && arr[i] < INT_MIN - sum)) {
            multigen_raise_exception(MGEN_ERROR_VALUE, "Integer overflow in sum()");
            return 0;
        }
        sum += arr[i];
    }
    return sum;
}

double multigen_sum_float_array(const double* arr, size_t size) {
    if (!arr) return 0.0;

    double sum = 0.0;
    for (size_t i = 0; i < size; i++) {
        sum += arr[i];
    }
    return sum;
}

/**
 * Python range() implementations
 */
multigen_range_t multigen_range(int stop) {
    return multigen_range_full(0, stop, 1);
}

multigen_range_t multigen_range_start_stop(int start, int stop) {
    return multigen_range_full(start, stop, 1);
}

multigen_range_t multigen_range_full(int start, int stop, int step) {
    multigen_range_t range;
    range.start = start;
    range.stop = stop;
    range.step = step;
    range.current = start;

    if (step == 0) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "range() arg 3 must not be zero");
    }

    return range;
}

int multigen_range_next(multigen_range_t* range) {
    if (!range || !multigen_range_has_next(range)) {
        return 0;
    }

    int current = range->current;
    range->current += range->step;
    return current;
}

int multigen_range_has_next(const multigen_range_t* range) {
    if (!range) return 0;

    if (range->step > 0) {
        return range->current < range->stop;
    } else {
        return range->current > range->stop;
    }
}

/**
 * Character classification functions
 */
int multigen_isalpha_char(char c) {
    return isalpha((unsigned char)c);
}

int multigen_isdigit_char(char c) {
    return isdigit((unsigned char)c);
}

int multigen_isspace_char(char c) {
    return isspace((unsigned char)c);
}

int multigen_isalnum_char(char c) {
    return isalnum((unsigned char)c);
}

/**
 * Character case conversion
 */
char multigen_lower_char(char c) {
    return (char)tolower((unsigned char)c);
}

char multigen_upper_char(char c) {
    return (char)toupper((unsigned char)c);
}

/**
 * Python ord() and chr()
 */
int multigen_ord(char c) {
    return (int)(unsigned char)c;
}

char multigen_chr(int code) {
    if (code < 0 || code > 255) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "chr() arg not in range(256)");
        return '\0';
    }
    return (char)code;
}

/**
 * Comparison functions
 */
int multigen_cmp_int(int a, int b) {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}

int multigen_cmp_float(double a, double b) {
    if (isnan(a) || isnan(b)) {
        return isnan(a) ? (isnan(b) ? 0 : -1) : 1;
    }
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}

int multigen_cmp_string(const char* a, const char* b) {
    if (!a && !b) return 0;
    if (!a) return -1;
    if (!b) return 1;
    return strcmp(a, b);
}

/**
 * Python slice implementations
 */
multigen_python_slice_t multigen_slice_new(void) {
    multigen_python_slice_t slice = {0, 0, 1, 0, 0, 0};
    return slice;
}

multigen_python_slice_t multigen_slice_start_stop(int start, int stop) {
    multigen_python_slice_t slice = {start, stop, 1, 1, 1, 0};
    return slice;
}

multigen_python_slice_t multigen_slice_full(int start, int stop, int step) {
    multigen_python_slice_t slice = {start, stop, step, 1, 1, 1};
    return slice;
}

multigen_error_t multigen_normalize_python_slice(const multigen_python_slice_t* slice,
                                        size_t seq_len,
                                        multigen_normalized_slice_t* result) {
    if (!slice || !result) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Invalid slice parameters");
        return MGEN_ERROR_VALUE;
    }

    if (slice->has_step && slice->step == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Slice step cannot be zero");
        return MGEN_ERROR_VALUE;
    }

    int step = slice->has_step ? slice->step : 1;
    result->step = (size_t)abs(step);

    // Normalize start
    int start = slice->has_start ? slice->start : (step > 0 ? 0 : (int)seq_len - 1);
    if (start < 0) start += (int)seq_len;
    if (start < 0) start = (step > 0) ? 0 : -1;
    if (start >= (int)seq_len) start = (step > 0) ? (int)seq_len : (int)seq_len - 1;
    result->start = (size_t)start;

    // Normalize stop
    int stop = slice->has_stop ? slice->stop : (step > 0 ? (int)seq_len : -1);
    if (stop < 0) stop += (int)seq_len;
    if (stop < 0) stop = (step > 0) ? 0 : -1;
    if (stop >= (int)seq_len) stop = (step > 0) ? (int)seq_len : (int)seq_len - 1;
    result->stop = (size_t)stop;

    // Calculate length
    if (step > 0) {
        result->length = (result->start < result->stop) ?
                        (result->stop - result->start + result->step - 1) / result->step : 0;
    } else {
        result->length = (result->start > result->stop) ?
                        (result->start - result->stop + result->step - 1) / result->step : 0;
    }

    return MGEN_OK;
}

/**
 * Exception handling
 */
void multigen_raise_exception(multigen_error_t type, const char* message) {
    multigen_current_exception.type = type;
    if (message) {
        strncpy(multigen_current_exception.message, message,
                sizeof(multigen_current_exception.message) - 1);
        multigen_current_exception.message[sizeof(multigen_current_exception.message) - 1] = '\0';
    } else {
        multigen_current_exception.message[0] = '\0';
    }
    // Simple traceback - could be enhanced
    snprintf(multigen_current_exception.traceback, sizeof(multigen_current_exception.traceback),
             "Traceback: %s", multigen_error_name(type));
}

void multigen_clear_exception(void) {
    multigen_current_exception.type = MGEN_OK;
    multigen_current_exception.message[0] = '\0';
    multigen_current_exception.traceback[0] = '\0';
}

int multigen_has_exception(void) {
    return multigen_current_exception.type != MGEN_OK;
}

const multigen_exception_t* multigen_get_exception(void) {
    return &multigen_current_exception;
}

/**
 * Truthiness testing
 */
int multigen_is_truthy_int(int value) {
    return value != 0;
}

int multigen_is_truthy_float(double value) {
    return value != 0.0 && !isnan(value);
}

int multigen_is_truthy_cstring(const char* str) {
    return str != NULL && str[0] != '\0';
}

int multigen_is_truthy_pointer(const void* ptr) {
    return ptr != NULL;
}

/**
 * Type system
 */
const char* multigen_type_name(multigen_python_type_t type) {
    switch (type) {
        case MGEN_TYPE_NONE: return "NoneType";
        case MGEN_TYPE_BOOL: return "bool";
        case MGEN_TYPE_INT: return "int";
        case MGEN_TYPE_FLOAT: return "float";
        case MGEN_TYPE_STRING: return "str";
        case MGEN_TYPE_LIST: return "list";
        case MGEN_TYPE_DICT: return "dict";
        case MGEN_TYPE_SET: return "set";
        case MGEN_TYPE_TUPLE: return "tuple";
        default: return "unknown";
    }
}

/**
 * Simple format string operations
 */
char* multigen_format_simple(const char* template_str, const char* arg) {
    if (!template_str || !arg) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "Invalid format parameters");
        return NULL;
    }

    // Find {} in template and replace with arg
    const char* placeholder = strstr(template_str, "{}");
    if (!placeholder) {
        // No placeholder, just return copy of template
        size_t len = strlen(template_str);
        char* result = malloc(len + 1);
        if (!result) {
            multigen_raise_exception(MGEN_ERROR_MEMORY, "Failed to allocate format result");
            return NULL;
        }
        strcpy(result, template_str);
        return result;
    }

    size_t prefix_len = placeholder - template_str;
    size_t suffix_len = strlen(placeholder + 2); // Skip "{}"
    size_t arg_len = strlen(arg);
    size_t total_len = prefix_len + arg_len + suffix_len;

    char* result = malloc(total_len + 1);
    if (!result) {
        multigen_raise_exception(MGEN_ERROR_MEMORY, "Failed to allocate format result");
        return NULL;
    }

    memcpy(result, template_str, prefix_len);
    memcpy(result + prefix_len, arg, arg_len);
    strcpy(result + prefix_len + arg_len, placeholder + 2);

    return result;
}

char* multigen_format_int(const char* template_str, int value) {
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%d", value);
    return multigen_format_simple(template_str, buffer);
}

char* multigen_format_float(const char* template_str, double value) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%g", value);
    return multigen_format_simple(template_str, buffer);
}

/**
 * Python zip() implementation
 */
multigen_zip_iterator_t multigen_zip_arrays(void* arr1, size_t size1, size_t elem_size1,
                                   void* arr2, size_t size2, size_t elem_size2) {
    multigen_zip_iterator_t iter;
    iter.first = arr1;
    iter.second = arr2;
    iter.index = 0;
    iter.size1 = size1;
    iter.size2 = size2;
    iter.element_size1 = elem_size1;
    iter.element_size2 = elem_size2;
    return iter;
}

int multigen_zip_next(multigen_zip_iterator_t* iter, void** elem1, void** elem2) {
    if (!iter || !elem1 || !elem2) return 0;

    if (iter->index >= iter->size1 || iter->index >= iter->size2) {
        return 0; // End of iteration
    }

    *elem1 = (char*)iter->first + iter->index * iter->element_size1;
    *elem2 = (char*)iter->second + iter->index * iter->element_size2;
    iter->index++;

    return 1;
}

/**
 * Python enumerate() implementation
 */
void multigen_enumerate_array(void* array, size_t size, size_t element_size,
                         multigen_python_enumerate_callback_t callback, void* userdata) {
    if (!array || !callback) {
        multigen_raise_exception(MGEN_ERROR_VALUE, "Invalid enumerate parameters");
        return;
    }

    for (size_t i = 0; i < size; i++) {
        multigen_enumerate_item_t item;
        item.index = i;
        item.element = (char*)array + i * element_size;
        callback(&item, userdata);
    }
}

/**
 * Python print() function implementations
 */
void print_int(int value) {
    printf("%d\n", value);
}

void print_float(double value) {
    printf("%g\n", value);
}

void print_string(const char* str) {
    printf("%s\n", str ? str : "");
}