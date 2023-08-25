#ifndef DEBUG_H
#define DEBUG_H

#include "log.c/src/log.h"
#include "repr.h"

#ifndef DEBUG
#    define DEBUG 0
#endif

#if (DEBUG)
#    define RAS_LOG_LEVEL LOG_TRACE
#else
#    define RAS_LOG_LEVEL LOG_INFO
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

#endif
