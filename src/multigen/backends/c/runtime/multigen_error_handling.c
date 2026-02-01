/**
 * MultiGen Runtime Library - Error Handling Implementation
 */

#include "multigen_error_handling.h"
#include <stdarg.h>

// Global error context
multigen_error_context_t multigen_last_error = {MGEN_OK, "", NULL, 0, NULL};

void multigen_set_error(multigen_error_t code, const char* message,
                   const char* file, int line, const char* function) {
    multigen_last_error.code = code;
    multigen_last_error.file = file;
    multigen_last_error.line = line;
    multigen_last_error.function = function;

    if (message) {
        strncpy(multigen_last_error.message, message, sizeof(multigen_last_error.message) - 1);
        multigen_last_error.message[sizeof(multigen_last_error.message) - 1] = '\0';
    } else {
        multigen_last_error.message[0] = '\0';
    }
}

void multigen_set_error_fmt(multigen_error_t code, const char* file, int line,
                       const char* function, const char* format, ...) {
    multigen_last_error.code = code;
    multigen_last_error.file = file;
    multigen_last_error.line = line;
    multigen_last_error.function = function;

    if (format) {
        va_list args;
        va_start(args, format);
        vsnprintf(multigen_last_error.message, sizeof(multigen_last_error.message), format, args);
        va_end(args);
    } else {
        multigen_last_error.message[0] = '\0';
    }
}

multigen_error_t multigen_get_last_error(void) {
    return multigen_last_error.code;
}

const char* multigen_get_last_error_message(void) {
    return multigen_last_error.message;
}

void multigen_clear_error(void) {
    multigen_last_error.code = MGEN_OK;
    multigen_last_error.message[0] = '\0';
    multigen_last_error.file = NULL;
    multigen_last_error.line = 0;
    multigen_last_error.function = NULL;
}

int multigen_has_error(void) {
    return multigen_last_error.code != MGEN_OK;
}

void multigen_print_error(void) {
    if (multigen_has_error()) {
        fprintf(stderr, "MultiGen Runtime Error [%s]: %s\n",
                multigen_error_name(multigen_last_error.code),
                multigen_last_error.message);

        if (multigen_last_error.file && multigen_last_error.function) {
            fprintf(stderr, "  at %s:%d in %s()\n",
                    multigen_last_error.file,
                    multigen_last_error.line,
                    multigen_last_error.function);
        }
    }
}

multigen_error_t multigen_errno_to_error(int errno_val) {
    switch (errno_val) {
        case ENOMEM:
            return MGEN_ERROR_MEMORY;
        case ENOENT:
            return MGEN_ERROR_FILE_NOT_FOUND;
        case EACCES:
        case EPERM:
            return MGEN_ERROR_PERMISSION;
        case EIO:
            return MGEN_ERROR_IO;
        case EINVAL:
            return MGEN_ERROR_VALUE;
        default:
            return MGEN_ERROR_RUNTIME;
    }
}

const char* multigen_error_name(multigen_error_t code) {
    switch (code) {
        case MGEN_OK:
            return "OK";
        case MGEN_ERROR_GENERIC:
            return "GenericError";
        case MGEN_ERROR_MEMORY:
            return "MemoryError";
        case MGEN_ERROR_INDEX:
            return "IndexError";
        case MGEN_ERROR_KEY:
            return "KeyError";
        case MGEN_ERROR_VALUE:
            return "ValueError";
        case MGEN_ERROR_TYPE:
            return "TypeError";
        case MGEN_ERROR_IO:
            return "IOError";
        case MGEN_ERROR_FILE_NOT_FOUND:
            return "FileNotFoundError";
        case MGEN_ERROR_PERMISSION:
            return "PermissionError";
        case MGEN_ERROR_RUNTIME:
            return "RuntimeError";
        default:
            return "UnknownError";
    }
}

// Exception handling implementation using setjmp/longjmp

// Global exception stack
mgen_exception_context_t mgen_exception_stack[MGEN_MAX_TRY_DEPTH];
int mgen_exception_depth = -1;

void mgen_exception_init(void) {
    mgen_exception_depth = -1;
    for (int i = 0; i < MGEN_MAX_TRY_DEPTH; i++) {
        mgen_exception_stack[i].active = 0;
        mgen_exception_stack[i].exception_type = MGEN_OK;
        mgen_exception_stack[i].exception_message[0] = '\0';
    }
}

int mgen_try_push(void) {
    if (mgen_exception_depth >= MGEN_MAX_TRY_DEPTH - 1) {
        fprintf(stderr, "MultiGen: Maximum try/catch nesting depth exceeded\n");
        return -1;
    }
    mgen_exception_depth++;
    mgen_exception_stack[mgen_exception_depth].active = 1;
    mgen_exception_stack[mgen_exception_depth].exception_type = MGEN_OK;
    mgen_exception_stack[mgen_exception_depth].exception_message[0] = '\0';
    return mgen_exception_depth;
}

void mgen_try_pop(void) {
    if (mgen_exception_depth >= 0) {
        mgen_exception_stack[mgen_exception_depth].active = 0;
        mgen_exception_depth--;
    }
}

void mgen_throw(multigen_error_t type, const char* message) {
    if (mgen_exception_depth >= 0 && mgen_exception_stack[mgen_exception_depth].active) {
        mgen_exception_stack[mgen_exception_depth].exception_type = type;
        if (message) {
            strncpy(mgen_exception_stack[mgen_exception_depth].exception_message,
                    message,
                    sizeof(mgen_exception_stack[mgen_exception_depth].exception_message) - 1);
            mgen_exception_stack[mgen_exception_depth].exception_message[
                sizeof(mgen_exception_stack[mgen_exception_depth].exception_message) - 1] = '\0';
        }
        longjmp(mgen_exception_stack[mgen_exception_depth].env, 1);
    } else {
        // No try block active, print error and abort
        fprintf(stderr, "Unhandled exception: %s: %s\n",
                multigen_error_name(type), message ? message : "");
        abort();
    }
}

multigen_error_t mgen_current_exception_type(void) {
    if (mgen_exception_depth >= 0) {
        return mgen_exception_stack[mgen_exception_depth].exception_type;
    }
    return MGEN_OK;
}

const char* mgen_current_exception_message(void) {
    if (mgen_exception_depth >= 0) {
        return mgen_exception_stack[mgen_exception_depth].exception_message;
    }
    return "";
}

void mgen_rethrow(void) {
    if (mgen_exception_depth >= 0) {
        multigen_error_t type = mgen_exception_stack[mgen_exception_depth].exception_type;
        const char* msg = mgen_exception_stack[mgen_exception_depth].exception_message;
        mgen_try_pop();
        mgen_throw(type, msg);
    }
}