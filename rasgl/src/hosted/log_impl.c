
#include "log.c/src/log.h"
// #include "rasgl/core/log.h"

void ras_log_impl(
    int level,
    int category,
    const char* file,
    int line,
    const char* fmt,
    ...)
{
    log_log_ex(level, category, file, line, fmt);
}
