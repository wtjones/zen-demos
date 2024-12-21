#ifndef DEBUG_H
#define DEBUG_H

#include "log.c/src/log.h"

#define RAS_MAX_LOG_BUFFER 100000

// FIXME: needed for vscode intellisense
#ifndef __FILE_NAME__
#    define __FILE_NAME__ ""
#endif

#ifndef DEBUG
#    define DEBUG 0
#endif

#define RAS_LOG_LEVEL_STRERR LOG_ERROR

#if (DEBUG)
#    define RAS_LOG_LEVEL_FILE LOG_TRACE
#else
#    define RAS_LOG_LEVEL_FILE LOG_INFO
#endif

#define debug_print(fmt, ...)                  \
    do {                                       \
        if (DEBUG)                             \
            fprintf(stderr, fmt, __VA_ARGS__); \
    } while (0)

#define ras_log_trace(...) log_log(LOG_TRACE, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_debug(...) log_log(LOG_DEBUG, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_info(...) log_log(LOG_INFO, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_warn(...) log_log(LOG_WARN, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_error(...) log_log(LOG_ERROR, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_fatal(...) log_log(LOG_FATAL, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_buffer(...) core_log_buffer(LOG_INFO, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_buffer_trace(...) core_log_buffer(LOG_TRACE, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_buffer_info(...) core_log_buffer(LOG_INFO, __FILE_NAME__, __LINE__, __VA_ARGS__)

typedef enum RasResult {
    RAS_RESULT_OK,
    RAS_RESULT_ERROR
} RasResult;

void core_log_buffer(int level, const char* file, int line, const char* fmt, ...);

void ras_log_flush();

void ras_log_clear();

#define RAS_CHECK_RESULT(result)                  \
    if ((result) != RAS_RESULT_OK) {              \
        ras_log_error("Result != RAS_RESULT_OK"); \
        return RAS_RESULT_ERROR;                  \
    }

#define RAS_CHECK_RESULT_AND_LOG(result, message, ...) \
    if ((result) != RAS_RESULT_OK) {                   \
        ras_log_error(message, ##__VA_ARGS__);         \
        return RAS_RESULT_ERROR;                       \
    }

#define RAS_CHECK_AND_LOG(condition, message, ...) \
    if (condition) {                               \
        ras_log_error(message, ##__VA_ARGS__);     \
        return RAS_RESULT_ERROR;                   \
    }

#endif
