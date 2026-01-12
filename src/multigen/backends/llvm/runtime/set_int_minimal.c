/**
 * Minimal set_int runtime for LLVM backend
 * Hash set implementation for integers
 */

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

#define SET_INT_DEFAULT_BUCKET_COUNT 16

// Hash set entry for separate chaining
typedef struct multigen_set_int_entry {
    long long value;  // Use long long to match LLVM i64
    struct multigen_set_int_entry* next;
} multigen_set_int_entry_t;

// Hash set structure
typedef struct {
    multigen_set_int_entry_t** buckets;
    size_t bucket_count;
    size_t size;
} set_int;

// Internal helper: hash function
static size_t set_int_hash(long long value, size_t bucket_count) {
    unsigned long long u = (value < 0) ? (unsigned long long)(-value) : (unsigned long long)value;
    return (size_t)(u % bucket_count);
}

// Internal helper: create entry
static multigen_set_int_entry_t* set_int_entry_new(long long value) {
    multigen_set_int_entry_t* entry = malloc(sizeof(multigen_set_int_entry_t));
    if (!entry) {
        fprintf(stderr, "set_int error: Failed to allocate entry for value %lld\n", value);
        exit(1);
    }
    entry->value = value;
    entry->next = NULL;
    return entry;
}

// Internal helper: free entry chain
static void set_int_entry_free(multigen_set_int_entry_t* entry) {
    while (entry) {
        multigen_set_int_entry_t* next = entry->next;
        free(entry);
        entry = next;
    }
}

// Create a new integer set
set_int set_int_init(void) {
    set_int set;
    set.bucket_count = SET_INT_DEFAULT_BUCKET_COUNT;
    set.size = 0;
    set.buckets = calloc(SET_INT_DEFAULT_BUCKET_COUNT, sizeof(multigen_set_int_entry_t*));
    if (!set.buckets) {
        set.bucket_count = 0;
        fprintf(stderr, "set_int error: Failed to allocate initial memory\n");
        exit(1);
    }
    return set;
}

// Initialize set via pointer (for LLVM calling convention)
void set_int_init_ptr(set_int* out) {
    if (!out) {
        fprintf(stderr, "set_int error: NULL pointer passed to set_int_init_ptr\n");
        exit(1);
    }
    *out = set_int_init();
}

// Insert an element
bool set_int_insert(set_int* set, long long value) {
    if (!set) {
        fprintf(stderr, "set_int error: NULL pointer passed to set_int_insert\n");
        exit(1);
    }

    // Lazy initialization
    if (!set->buckets) {
        set->bucket_count = SET_INT_DEFAULT_BUCKET_COUNT;
        set->buckets = calloc(SET_INT_DEFAULT_BUCKET_COUNT, sizeof(multigen_set_int_entry_t*));
        if (!set->buckets) {
            set->bucket_count = 0;
            fprintf(stderr, "set_int error: Failed to allocate buckets during lazy initialization\n");
            exit(1);
        }
    }

    size_t index = set_int_hash(value, set->bucket_count);

    // Check if value already exists
    multigen_set_int_entry_t* entry = set->buckets[index];
    while (entry) {
        if (entry->value == value) {
            return false; // Already present
        }
        entry = entry->next;
    }

    // Insert new entry at head of chain
    multigen_set_int_entry_t* new_entry = set_int_entry_new(value);
    new_entry->next = set->buckets[index];
    set->buckets[index] = new_entry;
    set->size++;

    return true; // Inserted
}

// Check if element is in the set
bool set_int_contains(const set_int* set, long long value) {
    if (!set || !set->buckets) {
        return false;
    }

    size_t index = set_int_hash(value, set->bucket_count);

    multigen_set_int_entry_t* entry = set->buckets[index];
    while (entry) {
        if (entry->value == value) {
            return true;
        }
        entry = entry->next;
    }

    return false;
}

// Get number of elements in set
size_t set_int_size(const set_int* set) {
    return set ? set->size : 0;
}

// Get the Nth element in iteration order (0-indexed)
// Used for set iteration in for loops
// Returns the value of the Nth element
// Note: O(n) complexity - iterates through buckets/chains to find it
long long set_int_get_nth_element(const set_int* set, size_t n) {
    if (!set || !set->buckets || n >= set->size) {
        // Return 0 for invalid index (should not happen in correct code)
        return 0;
    }

    size_t count = 0;

    // Iterate through buckets
    for (size_t bucket_idx = 0; bucket_idx < set->bucket_count; bucket_idx++) {
        multigen_set_int_entry_t* entry = set->buckets[bucket_idx];

        // Iterate through chain in this bucket
        while (entry) {
            if (count == n) {
                return entry->value;
            }
            count++;
            entry = entry->next;
        }
    }

    // Should not reach here if n < set->size
    return 0;
}

// Free all memory
void set_int_drop(set_int* set) {
    if (!set) {
        return;
    }

    if (set->buckets) {
        for (size_t i = 0; i < set->bucket_count; i++) {
            if (set->buckets[i]) {
                set_int_entry_free(set->buckets[i]);
            }
        }
        free(set->buckets);
        set->buckets = NULL;
    }

    set->bucket_count = 0;
    set->size = 0;
}
