/**
 * Minimal map_int_int runtime for LLVM backend
 * Simple hash map with integer keys and integer values
 * Uses linear probing for collision resolution
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#define MAP_DEFAULT_CAPACITY 16
#define MAP_GROWTH_FACTOR 2
#define MAP_LOAD_FACTOR 0.75

// Hash map entry
typedef struct {
    long long key;
    long long value;
    int is_occupied;
} map_int_entry;

// Hash map structure
typedef struct {
    map_int_entry* entries;
    size_t size;         // Number of key-value pairs
    size_t capacity;     // Total capacity
} map_int_int;

// Simple hash function for integers
static unsigned long hash_int(long long key) {
    // Knuth's multiplicative hash
    unsigned long hash = (unsigned long)key;
    hash = hash * 2654435761UL;
    return hash;
}

// Internal helper to find entry slot
static map_int_entry* find_entry(map_int_entry* entries, size_t capacity, long long key) {
    unsigned long hash = hash_int(key);
    size_t index = hash % capacity;

    // Linear probing
    for (size_t i = 0; i < capacity; i++) {
        size_t probe_index = (index + i) % capacity;
        map_int_entry* entry = &entries[probe_index];

        if (!entry->is_occupied) {
            return entry; // Found empty slot
        }

        if (entry->key == key) {
            return entry; // Found matching key
        }
    }

    return NULL; // Map is full (shouldn't happen with proper load factor)
}

// Grow the map capacity
static void map_int_int_grow(map_int_int* map) {
    size_t new_capacity = (map->capacity == 0) ? MAP_DEFAULT_CAPACITY : map->capacity * MAP_GROWTH_FACTOR;
    map_int_entry* new_entries = calloc(new_capacity, sizeof(map_int_entry));

    if (!new_entries) {
        fprintf(stderr, "map_int_int error: Failed to allocate memory for capacity %zu\n", new_capacity);
        exit(1);
    }

    // Rehash all existing entries
    if (map->entries) {
        for (size_t i = 0; i < map->capacity; i++) {
            map_int_entry* old_entry = &map->entries[i];
            if (old_entry->is_occupied) {
                map_int_entry* new_entry = find_entry(new_entries, new_capacity, old_entry->key);
                if (new_entry) {
                    new_entry->key = old_entry->key;
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
map_int_int map_int_int_init(void) {
    map_int_int map;
    map.capacity = MAP_DEFAULT_CAPACITY;
    map.size = 0;
    map.entries = calloc(MAP_DEFAULT_CAPACITY, sizeof(map_int_entry));

    if (!map.entries) {
        map.capacity = 0;
        fprintf(stderr, "map_int_int error: Failed to allocate initial memory\n");
        exit(1);
    }

    return map;
}

// Initialize via pointer (for LLVM calling convention)
void map_int_int_init_ptr(map_int_int* out) {
    if (!out) {
        fprintf(stderr, "map_int_int error: NULL pointer passed to map_int_int_init_ptr\n");
        exit(1);
    }
    *out = map_int_int_init();
}

// Set or update key-value pair
void map_int_int_set(map_int_int* map, long long key, long long value) {
    if (!map) {
        fprintf(stderr, "map_int_int error: NULL pointer passed to map_int_int_set\n");
        exit(1);
    }

    // Check if we need to grow
    if ((double)(map->size + 1) / map->capacity > MAP_LOAD_FACTOR) {
        map_int_int_grow(map);
    }

    map_int_entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry) {
        fprintf(stderr, "map_int_int error: Failed to find entry slot (map full)\n");
        exit(1);
    }

    // Check if this is a new key
    int is_new_key = !entry->is_occupied;

    if (is_new_key) {
        entry->key = key;
        entry->is_occupied = 1;
        map->size++;
    }

    entry->value = value;
}

// Get value by key (returns 0 if key doesn't exist)
long long map_int_int_get(map_int_int* map, long long key) {
    if (!map) {
        return 0;
    }

    if (map->capacity == 0) {
        return 0;
    }

    map_int_entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry || !entry->is_occupied) {
        return 0;
    }

    return entry->value;
}

// Check if key exists in map
int map_int_int_contains(map_int_int* map, long long key) {
    if (!map || map->capacity == 0) {
        return 0;
    }

    map_int_entry* entry = find_entry(map->entries, map->capacity, key);
    return (entry && entry->is_occupied) ? 1 : 0;
}

// Get number of entries
size_t map_int_int_size(map_int_int* map) {
    if (!map) {
        return 0;
    }
    return map->size;
}

// Free map memory
void map_int_int_free(map_int_int* map) {
    if (map && map->entries) {
        free(map->entries);
        map->entries = NULL;
        map->size = 0;
        map->capacity = 0;
    }
}

// Get capacity (total number of slots, including empty ones)
size_t map_int_int_capacity(map_int_int* map) {
    if (!map) {
        return 0;
    }
    return map->capacity;
}

// Check if entry at index is occupied
int map_int_int_entry_is_occupied(map_int_int* map, size_t index) {
    if (!map || !map->entries || index >= map->capacity) {
        return 0;
    }
    return map->entries[index].is_occupied;
}

// Get key at specific index (caller must check is_occupied first)
long long map_int_int_entry_key(map_int_int* map, size_t index) {
    if (!map || !map->entries || index >= map->capacity) {
        return 0;
    }
    return map->entries[index].key;
}

// Get value at specific index (caller must check is_occupied first)
long long map_int_int_entry_value(map_int_int* map, size_t index) {
    if (!map || !map->entries || index >= map->capacity) {
        return 0;
    }
    return map->entries[index].value;
}
