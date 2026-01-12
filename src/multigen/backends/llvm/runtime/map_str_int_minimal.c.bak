/**
 * Minimal map_str_int runtime for LLVM backend
 * Simple hash map with string keys and integer values
 * Uses linear probing for collision resolution
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define MAP_DEFAULT_CAPACITY 16
#define MAP_GROWTH_FACTOR 2
#define MAP_LOAD_FACTOR 0.75

// Hash map entry
typedef struct {
    char* key;      // NULL indicates empty slot
    long long value;
    int is_occupied;
} map_entry;

// Hash map structure
typedef struct {
    map_entry* entries;
    size_t size;         // Number of key-value pairs
    size_t capacity;     // Total capacity
} map_str_int;

// Simple hash function (djb2)
static unsigned long hash_string(const char* str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    return hash;
}

// Internal helper to find entry slot
static map_entry* find_entry(map_entry* entries, size_t capacity, const char* key) {
    unsigned long hash = hash_string(key);
    size_t index = hash % capacity;

    // Linear probing
    for (size_t i = 0; i < capacity; i++) {
        size_t probe_index = (index + i) % capacity;
        map_entry* entry = &entries[probe_index];

        if (!entry->is_occupied) {
            return entry; // Found empty slot
        }

        if (entry->key && strcmp(entry->key, key) == 0) {
            return entry; // Found matching key
        }
    }

    return NULL; // Map is full (shouldn't happen with proper load factor)
}

// Grow the map capacity
static void map_str_int_grow(map_str_int* map) {
    size_t new_capacity = (map->capacity == 0) ? MAP_DEFAULT_CAPACITY : map->capacity * MAP_GROWTH_FACTOR;
    map_entry* new_entries = calloc(new_capacity, sizeof(map_entry));

    if (!new_entries) {
        exit(1);
    }

    // Rehash all existing entries
    if (map->entries) {
        for (size_t i = 0; i < map->capacity; i++) {
            map_entry* old_entry = &map->entries[i];
            if (old_entry->is_occupied && old_entry->key) {
                map_entry* new_entry = find_entry(new_entries, new_capacity, old_entry->key);
                if (new_entry) {
                    new_entry->key = old_entry->key; // Transfer ownership
                    new_entry->value = old_entry->value;
                    new_entry->is_occupied = 1;
                }
            }
        }
        free(map->entries);
    }

    map->entries = new_entries;
    map->capacity = new_capacity;
}

// Initialize empty map
map_str_int map_str_int_init(void) {
    map_str_int map;
    map.capacity = MAP_DEFAULT_CAPACITY;
    map.size = 0;
    map.entries = calloc(MAP_DEFAULT_CAPACITY, sizeof(map_entry));

    if (!map.entries) {
        map.capacity = 0;
        exit(1);
    }

    return map;
}

// Initialize via pointer (for LLVM calling convention)
void map_str_int_init_ptr(map_str_int* out) {
    if (!out) {
        exit(1);
    }
    *out = map_str_int_init();
}

// Set or update key-value pair
void map_str_int_set(map_str_int* map, const char* key, long long value) {
    if (!map || !key) {
        exit(1);
    }

    // Check if we need to grow
    if ((double)(map->size + 1) / map->capacity > MAP_LOAD_FACTOR) {
        map_str_int_grow(map);
    }

    map_entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry) {
        exit(1); // Map is full (shouldn't happen)
    }

    // Check if this is a new key
    int is_new_key = !entry->is_occupied || !entry->key;

    if (is_new_key) {
        // Allocate and copy key
        entry->key = strdup(key);
        if (!entry->key) {
            exit(1);
        }
        entry->is_occupied = 1;
        map->size++;
    }

    entry->value = value;
}

// Get value by key (returns 0 if key doesn't exist)
long long map_str_int_get(map_str_int* map, const char* key) {
    if (!map || !key) {
        return 0;
    }

    if (map->capacity == 0) {
        return 0;
    }

    map_entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry || !entry->is_occupied || !entry->key) {
        return 0;
    }

    return entry->value;
}

// Check if key exists in map
int map_str_int_contains(map_str_int* map, const char* key) {
    if (!map || !key || map->capacity == 0) {
        return 0;
    }

    map_entry* entry = find_entry(map->entries, map->capacity, key);
    return (entry && entry->is_occupied && entry->key) ? 1 : 0;
}

// Get number of entries
size_t map_str_int_size(map_str_int* map) {
    if (!map) {
        return 0;
    }
    return map->size;
}

// Free map memory
void map_str_int_free(map_str_int* map) {
    if (map && map->entries) {
        // Free all key strings
        for (size_t i = 0; i < map->capacity; i++) {
            if (map->entries[i].key) {
                free(map->entries[i].key);
            }
        }
        free(map->entries);
        map->entries = NULL;
        map->size = 0;
        map->capacity = 0;
    }
}
