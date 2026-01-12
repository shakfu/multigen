/**
 * MultiGen Runtime Library - File I/O Operations
 *
 * Provides C implementations of common Python file operations.
 * These functions handle error reporting and resource management automatically.
 */

#ifndef MGEN_FILE_OPS_H
#define MGEN_FILE_OPS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include "multigen_error_handling.h"
#include "multigen_string_ops.h"

#ifdef __cplusplus
extern "C" {
#endif

// File handle structure for Python-like file operations
typedef struct {
    FILE* file;
    char* filename;
    char* mode;
    int is_open;
} multigen_file_t;

/**
 * Python open() equivalent
 * Returns a file handle that should be closed with multigen_close()
 */
multigen_file_t* multigen_open(const char* filename, const char* mode);

/**
 * Close a file handle (Python file.close() equivalent)
 */
multigen_error_t multigen_close(multigen_file_t* file);

/**
 * Python file.read() equivalent
 * Returns the entire file content (caller must free)
 * If size is 0, reads entire file; otherwise reads up to size bytes
 */
char* multigen_read(multigen_file_t* file, size_t size);

/**
 * Python file.readline() equivalent
 * Returns a single line (caller must free)
 */
char* multigen_readline(multigen_file_t* file);

/**
 * Python file.readlines() equivalent
 * Returns array of lines (caller must free)
 */
multigen_string_array_t* multigen_readlines(multigen_file_t* file);

/**
 * Python file.write() equivalent
 * Returns number of characters written or -1 on error
 */
int multigen_write(multigen_file_t* file, const char* data);

/**
 * Python file.writelines() equivalent
 */
multigen_error_t multigen_writelines(multigen_file_t* file, multigen_string_array_t* lines);

/**
 * Python os.path.exists() equivalent
 */
int multigen_exists(const char* path);

/**
 * Python os.path.isfile() equivalent
 */
int multigen_isfile(const char* path);

/**
 * Python os.path.isdir() equivalent
 */
int multigen_isdir(const char* path);

/**
 * Python os.path.getsize() equivalent
 * Returns file size in bytes or -1 on error
 */
long multigen_getsize(const char* path);

/**
 * Python os.path.basename() equivalent
 * Returns a new string with the basename (caller must free)
 */
char* multigen_basename(const char* path);

/**
 * Python os.path.dirname() equivalent
 * Returns a new string with the directory name (caller must free)
 */
char* multigen_dirname(const char* path);

/**
 * Python os.path.join() equivalent
 * Joins path components with platform-appropriate separator
 * Returns a new string (caller must free)
 */
char* multigen_path_join(const char* path1, const char* path2);

/**
 * Read entire file content (convenience function)
 * Equivalent to: with open(filename, 'r') as f: return f.read()
 */
char* multigen_read_file(const char* filename);

/**
 * Write string to file (convenience function)
 * Equivalent to: with open(filename, 'w') as f: f.write(content)
 */
multigen_error_t multigen_write_file(const char* filename, const char* content);

/**
 * Append string to file (convenience function)
 * Equivalent to: with open(filename, 'a') as f: f.write(content)
 */
multigen_error_t multigen_append_file(const char* filename, const char* content);

/**
 * Context manager-style file operations
 * These automatically handle file closing even on error
 */
typedef multigen_error_t (*multigen_file_operation_t)(multigen_file_t* file, void* userdata);

/**
 * Execute operation with automatic file management
 * Equivalent to Python's "with open(filename, mode) as f:"
 */
multigen_error_t multigen_with_file(const char* filename, const char* mode,
                           multigen_file_operation_t operation, void* userdata);

#ifdef __cplusplus
}
#endif

#endif // MGEN_FILE_OPS_H
