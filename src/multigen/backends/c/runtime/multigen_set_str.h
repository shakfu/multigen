/**
 * Simple hash set for strings
 * Clean, type-safe implementation for code generation
 * stb-library style: static functions for single-file output
 */

#ifndef MGEN_SET_STR_H
#define MGEN_SET_STR_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Hash set entry structure
typedef struct {
    char* value;       // String value (owned)
    size_t hash;
    bool occupied;
} set_str_entry;

// Hash set structure
typedef struct {
    set_str_entry* buckets;
    size_t size;           // Number of entries
    size_t capacity;       // Number of buckets
} set_str;

/**
 * Create a new string set
 * Supports {0} initialization with lazy bucket allocation
 */

/**
 * Insert a string into the set
 * Takes ownership of a copy of the string
 * Returns true if inserted (new), false if already present
 */

/**
 * Check if string is in the set
 */

/**
 * Remove a string from the set
 * Returns true if removed, false if not found
 */

/**
 * Get number of entries
 */

/**
 * Check if set is empty
 */

/**
 * Clear all entries (keep capacity)
 */

/**
 * Free all memory
 */

/**
 * Reserve capacity
 */

/**
 * Simple hash set for strings - Implementation
 * STC-compatible naming for drop-in replacement
 */

#include "multigen_error_handling.h"
#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 16
#define LOAD_FACTOR 0.75
#define GROWTH_FACTOR 2

// FNV-1a hash function for strings
static size_t hash_string(const char* str) {
    if (!str) return 0;

    size_t hash = 2166136261u;
    while (*str) {
        hash ^= (unsigned char)(*str++);
        hash *= 16777619u;
    }
    return hash;
}

static set_str set_str_init(void) {
    set_str set;
    set.buckets = NULL;  // Lazy allocation
    set.size = 0;
    set.capacity = 0;
    return set;
}

static void set_str_grow(set_str* set) {
    size_t new_capacity = (set->capacity == 0) ? DEFAULT_CAPACITY : set->capacity * GROWTH_FACTOR;

    // Allocate new buckets
    set_str_entry* new_buckets = calloc(new_capacity, sizeof(set_str_entry));
    if (!new_buckets) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow string set");
        return;
    }

    // Rehash existing entries
    if (set->buckets) {
        for (size_t i = 0; i < set->capacity; i++) {
            if (set->buckets[i].occupied) {
                // Find new position
                size_t hash = set->buckets[i].hash;
                size_t idx = hash % new_capacity;

                // Linear probing
                while (new_buckets[idx].occupied) {
                    idx = (idx + 1) % new_capacity;
                }

                // Move entry (transfer ownership)
                new_buckets[idx] = set->buckets[i];
            }
        }
        free(set->buckets);
    }

    set->buckets = new_buckets;
    set->capacity = new_capacity;
}

static bool set_str_insert(set_str* set, const char* value) {
    if (!set) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string set");
        return false;
    }

    if (!value) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL value in string set");
        return false;
    }

    // Lazy initialization
    if (set->capacity == 0) {
        set_str_grow(set);
    }

    // Check load factor
    if ((double)set->size / set->capacity > LOAD_FACTOR) {
        set_str_grow(set);
    }

    size_t hash = hash_string(value);
    size_t idx = hash % set->capacity;
    size_t start_idx = idx;

    // Linear probing to find slot
    do {
        if (set->buckets[idx].occupied) {
            // Check if value already exists
            if (strcmp(set->buckets[idx].value, value) == 0) {
                return false;  // Already present
            }
        } else {
            // Found empty slot - insert here
            set->buckets[idx].value = strdup(value);
            if (!set->buckets[idx].value) {
                MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to duplicate string");
                return false;
            }
            set->buckets[idx].hash = hash;
            set->buckets[idx].occupied = true;
            set->size++;
            return true;  // Inserted
        }

        idx = (idx + 1) % set->capacity;
    } while (idx != start_idx);

    // Set is full (should not happen due to load factor check)
    MGEN_SET_ERROR(MGEN_ERROR_VALUE, "String set is full");
    return false;
}

static bool set_str_contains(const set_str* set, const char* value) {
    if (!set || !value || set->capacity == 0) {
        return false;
    }

    size_t hash = hash_string(value);
    size_t idx = hash % set->capacity;
    size_t start_idx = idx;

    // Linear probing to find value
    do {
        if (set->buckets[idx].occupied) {
            if (strcmp(set->buckets[idx].value, value) == 0) {
                return true;
            }
        } else {
            // Empty slot means value not found
            return false;
        }
        idx = (idx + 1) % set->capacity;
    } while (idx != start_idx);

    return false;
}

static bool set_str_erase(set_str* set, const char* value) {
    if (!set || !value || set->capacity == 0) {
        return false;
    }

    size_t hash = hash_string(value);
    size_t idx = hash % set->capacity;
    size_t start_idx = idx;

    // Linear probing to find value
    do {
        if (set->buckets[idx].occupied) {
            if (strcmp(set->buckets[idx].value, value) == 0) {
                // Free string and mark as unoccupied
                free(set->buckets[idx].value);
                set->buckets[idx].value = NULL;
                set->buckets[idx].occupied = false;
                set->size--;
                return true;
            }
        } else {
            // Empty slot means value not found
            return false;
        }
        idx = (idx + 1) % set->capacity;
    } while (idx != start_idx);

    return false;
}

static inline size_t set_str_size(const set_str* set) {
    return set ? set->size : 0;
}

static inline bool set_str_empty(const set_str* set) {
    return !set || set->size == 0;
}

static inline void set_str_clear(set_str* set) {
    if (!set || !set->buckets) {
        return;
    }

    // Free all strings
    for (size_t i = 0; i < set->capacity; i++) {
        if (set->buckets[i].occupied) {
            free(set->buckets[i].value);
            set->buckets[i].value = NULL;
            set->buckets[i].occupied = false;
        }
    }

    set->size = 0;
}

static void set_str_drop(set_str* set) {
    if (!set) {
        return;
    }

    // Free all strings
    if (set->buckets) {
        for (size_t i = 0; i < set->capacity; i++) {
            if (set->buckets[i].occupied) {
                free(set->buckets[i].value);
            }
        }
        free(set->buckets);
    }

    set->buckets = NULL;
    set->size = 0;
    set->capacity = 0;
}

static void set_str_reserve(set_str* set, size_t new_capacity) {
    if (!set) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string set");
        return;
    }

    if (new_capacity <= set->capacity) {
        return;
    }

    // Save old state
    set_str_entry* old_buckets = set->buckets;
    size_t old_capacity = set->capacity;

    // Allocate new buckets
    set->buckets = calloc(new_capacity, sizeof(set_str_entry));
    if (!set->buckets) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to reserve capacity");
        set->buckets = old_buckets;  // Restore
        return;
    }
    set->capacity = new_capacity;
    set->size = 0;  // Will be incremented during rehash

    // Rehash existing entries
    if (old_buckets) {
        for (size_t i = 0; i < old_capacity; i++) {
            if (old_buckets[i].occupied) {
                // Find new position
                size_t hash = old_buckets[i].hash;
                size_t idx = hash % new_capacity;

                // Linear probing
                while (set->buckets[idx].occupied) {
                    idx = (idx + 1) % new_capacity;
                }

                // Move entry (transfer ownership)
                set->buckets[idx] = old_buckets[i];
                set->size++;
            }
        }
        free(old_buckets);
    }
}


// Implementation

#ifdef __cplusplus
}
#endif

#endif // MGEN_SET_STR_H
