/**
 * MultiGen Runtime Library - Fallback Container Implementations
 *
 * Implementation of generic dynamic arrays for use when STC is not available.
 */

#include "multigen_containers_fallback.h"
#include "multigen_memory_ops.h"
#include <string.h>
#include <stdlib.h>

// Default initial capacity for dynamic arrays
#define MGEN_DYN_ARRAY_DEFAULT_CAPACITY 8

// Growth factor for dynamic arrays (multiply by 1.5)
#define MGEN_DYN_ARRAY_GROWTH_FACTOR(cap) ((cap) + (cap) / 2)

multigen_dyn_array_t* multigen_dyn_array_new(size_t element_size, size_t initial_capacity) {
    if (element_size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Element size cannot be zero");
        return NULL;
    }

    multigen_dyn_array_t* array = (multigen_dyn_array_t*)malloc(sizeof(multigen_dyn_array_t));
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate dynamic array");
        return NULL;
    }

    if (initial_capacity == 0) {
        initial_capacity = MGEN_DYN_ARRAY_DEFAULT_CAPACITY;
    }

    array->element_size = element_size;
    array->size = 0;
    array->capacity = initial_capacity;
    array->data = malloc(element_size * initial_capacity);

    if (!array->data) {
        free(array);
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to allocate array data");
        return NULL;
    }

    return array;
}

void multigen_dyn_array_free(multigen_dyn_array_t* array) {
    if (array) {
        if (array->data) {
            free(array->data);
        }
        free(array);
    }
}

multigen_error_t multigen_dyn_array_reserve(multigen_dyn_array_t* array, size_t new_capacity) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (new_capacity <= array->capacity) {
        return MGEN_OK; // Already have enough capacity
    }

    void* new_data = realloc(array->data, array->element_size * new_capacity);
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to reserve array capacity");
        return MGEN_ERROR_MEMORY;
    }

    array->data = new_data;
    array->capacity = new_capacity;
    return MGEN_OK;
}

multigen_error_t multigen_dyn_array_append(multigen_dyn_array_t* array, const void* element) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (!element) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Element is NULL");
        return MGEN_ERROR_VALUE;
    }

    // Grow array if needed
    if (array->size >= array->capacity) {
        size_t new_capacity = MGEN_DYN_ARRAY_GROWTH_FACTOR(array->capacity);
        if (new_capacity <= array->capacity) {
            new_capacity = array->capacity + 1; // Handle edge case
        }

        multigen_error_t result = multigen_dyn_array_reserve(array, new_capacity);
        if (result != MGEN_OK) {
            return result;
        }
    }

    // Copy element to end of array
    void* dest = (char*)array->data + (array->size * array->element_size);
    memcpy(dest, element, array->element_size);
    array->size++;

    return MGEN_OK;
}

multigen_error_t multigen_dyn_array_insert(multigen_dyn_array_t* array, size_t index, const void* element) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (!element) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Element is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (index > array->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Insert index out of bounds");
        return MGEN_ERROR_INDEX;
    }

    // Grow array if needed
    if (array->size >= array->capacity) {
        size_t new_capacity = MGEN_DYN_ARRAY_GROWTH_FACTOR(array->capacity);
        multigen_error_t result = multigen_dyn_array_reserve(array, new_capacity);
        if (result != MGEN_OK) {
            return result;
        }
    }

    // Move elements after insertion point
    if (index < array->size) {
        void* src = (char*)array->data + (index * array->element_size);
        void* dest = (char*)array->data + ((index + 1) * array->element_size);
        size_t move_size = (array->size - index) * array->element_size;
        memmove(dest, src, move_size);
    }

    // Insert new element
    void* dest = (char*)array->data + (index * array->element_size);
    memcpy(dest, element, array->element_size);
    array->size++;

    return MGEN_OK;
}

multigen_error_t multigen_dyn_array_remove(multigen_dyn_array_t* array, size_t index) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (index >= array->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Remove index out of bounds");
        return MGEN_ERROR_INDEX;
    }

    // Move elements after removal point
    if (index < array->size - 1) {
        void* dest = (char*)array->data + (index * array->element_size);
        void* src = (char*)array->data + ((index + 1) * array->element_size);
        size_t move_size = (array->size - index - 1) * array->element_size;
        memmove(dest, src, move_size);
    }

    array->size--;
    return MGEN_OK;
}

void* multigen_dyn_array_get(const multigen_dyn_array_t* array, size_t index) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return NULL;
    }

    if (index >= array->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Get index out of bounds");
        return NULL;
    }

    return (char*)array->data + (index * array->element_size);
}

multigen_error_t multigen_dyn_array_set(multigen_dyn_array_t* array, size_t index, const void* element) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (!element) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Element is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (index >= array->size) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Set index out of bounds");
        return MGEN_ERROR_INDEX;
    }

    void* dest = (char*)array->data + (index * array->element_size);
    memcpy(dest, element, array->element_size);

    return MGEN_OK;
}

size_t multigen_dyn_array_size(const multigen_dyn_array_t* array) {
    return array ? array->size : 0;
}

size_t multigen_dyn_array_capacity(const multigen_dyn_array_t* array) {
    return array ? array->capacity : 0;
}

void multigen_dyn_array_clear(multigen_dyn_array_t* array) {
    if (array) {
        array->size = 0;
    }
}

bool multigen_dyn_array_contains(const multigen_dyn_array_t* array, const void* element) {
    if (!array || !element) {
        return false;
    }

    for (size_t i = 0; i < array->size; i++) {
        void* item = (char*)array->data + (i * array->element_size);
        if (memcmp(item, element, array->element_size) == 0) {
            return true;
        }
    }

    return false;
}

multigen_error_t multigen_dyn_array_shrink_to_fit(multigen_dyn_array_t* array) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (array->size == 0) {
        // Keep at least minimal capacity
        return MGEN_OK;
    }

    if (array->size == array->capacity) {
        return MGEN_OK; // Already fitted
    }

    void* new_data = realloc(array->data, array->element_size * array->size);
    if (!new_data) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to shrink array");
        return MGEN_ERROR_MEMORY;
    }

    array->data = new_data;
    array->capacity = array->size;
    return MGEN_OK;
}

void* multigen_dyn_array_back(const multigen_dyn_array_t* array) {
    if (!array || array->size == 0) {
        return NULL;
    }

    return (char*)array->data + ((array->size - 1) * array->element_size);
}

multigen_error_t multigen_dyn_array_pop_back(multigen_dyn_array_t* array) {
    if (!array) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "Array is NULL");
        return MGEN_ERROR_VALUE;
    }

    if (array->size == 0) {
        MGEN_SET_ERROR(MGEN_ERROR_INDEX, "Cannot pop from empty array");
        return MGEN_ERROR_INDEX;
    }

    array->size--;
    return MGEN_OK;
}

bool multigen_dyn_array_empty(const multigen_dyn_array_t* array) {
    return !array || array->size == 0;
}
