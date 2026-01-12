/**
 * Simple hash map for string→string mappings
 * Clean, type-safe implementation for code generation
 * stb-library style: static functions for single-file output
 */

#ifndef MGEN_MAP_STR_STR_H
#define MGEN_MAP_STR_STR_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Hash map entry structure
typedef struct {
    char* key;
    char* value;
    size_t hash;
    bool occupied;
} map_str_str_entry;

// Hash map structure
typedef struct {
    map_str_str_entry* buckets;
    size_t size;           // Number of entries
    size_t capacity;       // Number of buckets
} map_str_str;

/**
 * Create a new string→string map
 * Supports {0} initialization with lazy bucket allocation
 */

/**
 * Insert or update a key-value pair
 * Takes ownership of copies of both key and value
 */

/**
 * Get value for a key (returns pointer to value or NULL if not found)
 */

/**
 * Check if key exists in map
 */

/**
 * Get number of entries
 */

/**
 * Remove a key-value pair
 */

/**
 * Clear all entries (keep capacity)
 */

/**
 * Free all memory
 */

/**
 * Check if map is empty
 */

/**
 * Reserve capacity
 */

/**
 * Simple hash map for string→string mappings - Implementation
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

static map_str_str map_str_str_init(void) {
    map_str_str map;
    map.buckets = NULL;  // Lazy allocation
    map.size = 0;
    map.capacity = 0;
    return map;
}

static void map_str_str_grow(map_str_str* map) {
    size_t new_capacity = (map->capacity == 0) ? DEFAULT_CAPACITY : map->capacity * GROWTH_FACTOR;

    // Allocate new buckets
    map_str_str_entry* new_buckets = calloc(new_capacity, sizeof(map_str_str_entry));
    if (!new_buckets) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to grow string→string map");
        return;
    }

    // Rehash existing entries
    if (map->buckets) {
        for (size_t i = 0; i < map->capacity; i++) {
            if (map->buckets[i].occupied) {
                // Find new position
                size_t hash = map->buckets[i].hash;
                size_t idx = hash % new_capacity;

                // Linear probing
                while (new_buckets[idx].occupied) {
                    idx = (idx + 1) % new_capacity;
                }

                // Move entry (transfer ownership)
                new_buckets[idx] = map->buckets[i];
            }
        }
        free(map->buckets);
    }

    map->buckets = new_buckets;
    map->capacity = new_capacity;
}

static void map_str_str_insert(map_str_str* map, const char* key, const char* value) {
    if (!map) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string→string map");
        return;
    }

    if (!key) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL key in string→string map");
        return;
    }

    // Lazy initialization
    if (map->capacity == 0) {
        map_str_str_grow(map);
    }

    // Check load factor
    if ((double)map->size / map->capacity > LOAD_FACTOR) {
        map_str_str_grow(map);
    }

    size_t hash = hash_string(key);
    size_t idx = hash % map->capacity;

    // Linear probing to find slot
    while (map->buckets[idx].occupied) {
        // Update existing key
        if (strcmp(map->buckets[idx].key, key) == 0) {
            free(map->buckets[idx].value);
            map->buckets[idx].value = value ? strdup(value) : NULL;
            return;
        }
        idx = (idx + 1) % map->capacity;
    }

    // Insert new entry
    map->buckets[idx].key = strdup(key);
    map->buckets[idx].value = value ? strdup(value) : NULL;
    map->buckets[idx].hash = hash;
    map->buckets[idx].occupied = true;
    map->size++;
}

static char** map_str_str_get(map_str_str* map, const char* key) {
    if (!map || !key || map->capacity == 0) {
        return NULL;
    }

    size_t hash = hash_string(key);
    size_t idx = hash % map->capacity;
    size_t start_idx = idx;

    // Linear probing to find key
    do {
        if (map->buckets[idx].occupied) {
            if (strcmp(map->buckets[idx].key, key) == 0) {
                return &map->buckets[idx].value;
            }
        } else {
            // Empty slot means key not found
            return NULL;
        }
        idx = (idx + 1) % map->capacity;
    } while (idx != start_idx);

    return NULL;
}

static bool map_str_str_contains(map_str_str* map, const char* key) {
    return map_str_str_get(map, key) != NULL;
}

static inline size_t map_str_str_size(const map_str_str* map) {
    return map ? map->size : 0;
}

static void map_str_str_erase(map_str_str* map, const char* key) {
    if (!map || !key || map->capacity == 0) {
        return;
    }

    size_t hash = hash_string(key);
    size_t idx = hash % map->capacity;
    size_t start_idx = idx;

    // Linear probing to find key
    do {
        if (map->buckets[idx].occupied) {
            if (strcmp(map->buckets[idx].key, key) == 0) {
                // Free strings and mark as unoccupied
                free(map->buckets[idx].key);
                free(map->buckets[idx].value);
                map->buckets[idx].key = NULL;
                map->buckets[idx].value = NULL;
                map->buckets[idx].occupied = false;
                map->size--;
                return;
            }
        } else {
            // Empty slot means key not found
            return;
        }
        idx = (idx + 1) % map->capacity;
    } while (idx != start_idx);
}

static inline void map_str_str_clear(map_str_str* map) {
    if (!map || !map->buckets) {
        return;
    }

    // Free all strings
    for (size_t i = 0; i < map->capacity; i++) {
        if (map->buckets[i].occupied) {
            free(map->buckets[i].key);
            free(map->buckets[i].value);
            map->buckets[i].key = NULL;
            map->buckets[i].value = NULL;
            map->buckets[i].occupied = false;
        }
    }

    map->size = 0;
}

static void map_str_str_drop(map_str_str* map) {
    if (!map) {
        return;
    }

    // Free all strings
    if (map->buckets) {
        for (size_t i = 0; i < map->capacity; i++) {
            if (map->buckets[i].occupied) {
                free(map->buckets[i].key);
                free(map->buckets[i].value);
            }
        }
        free(map->buckets);
    }

    map->buckets = NULL;
    map->size = 0;
    map->capacity = 0;
}

static inline bool map_str_str_empty(const map_str_str* map) {
    return !map || map->size == 0;
}

static void map_str_str_reserve(map_str_str* map, size_t new_capacity) {
    if (!map) {
        MGEN_SET_ERROR(MGEN_ERROR_VALUE, "NULL string→string map");
        return;
    }

    if (new_capacity <= map->capacity) {
        return;
    }

    // Save old state
    map_str_str_entry* old_buckets = map->buckets;
    size_t old_capacity = map->capacity;

    // Allocate new buckets
    map->buckets = calloc(new_capacity, sizeof(map_str_str_entry));
    if (!map->buckets) {
        MGEN_SET_ERROR(MGEN_ERROR_MEMORY, "Failed to reserve capacity");
        map->buckets = old_buckets;  // Restore
        return;
    }
    map->capacity = new_capacity;
    map->size = 0;  // Will be incremented during rehash

    // Rehash existing entries
    if (old_buckets) {
        for (size_t i = 0; i < old_capacity; i++) {
            if (old_buckets[i].occupied) {
                // Find new position
                size_t hash = old_buckets[i].hash;
                size_t idx = hash % new_capacity;

                // Linear probing
                while (map->buckets[idx].occupied) {
                    idx = (idx + 1) % new_capacity;
                }

                // Move entry (transfer ownership)
                map->buckets[idx] = old_buckets[i];
                map->size++;
            }
        }
        free(old_buckets);
    }
}


// Implementation

#ifdef __cplusplus
}
#endif

#endif // MGEN_MAP_STR_STR_H
