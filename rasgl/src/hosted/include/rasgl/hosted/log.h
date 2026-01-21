#ifndef HOSTED_LOG_H
#define HOSTED_LOG_H

#include "log.c/src/log.h"
#include "rasgl/core/debug.h"

// Undef the stub macros from debug.h
#undef ras_log_trace
#undef ras_log_debug
#undef ras_log_info
#undef ras_log_info_ex
#undef ras_log_warn
#undef ras_log_warn_ex
#undef ras_log_error
#undef ras_log_fatal

#define ras_log_trace(...) log_log_ex(LOG_TRACE, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_debug(...) log_log_ex(LOG_DEBUG, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_info(...) log_log_ex(LOG_INFO, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_info_ex(category, ...) log_log_ex(LOG_INFO, category, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_warn(...) log_log_ex(LOG_WARN, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_warn_ex(category, ...) log_log_ex(LOG_WARN, category, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_error(...) log_log_ex(LOG_ERROR, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)
#define ras_log_fatal(...) log_log_ex(LOG_FATAL, 0, __FILE_NAME__, __LINE__, __VA_ARGS__)

#endif
