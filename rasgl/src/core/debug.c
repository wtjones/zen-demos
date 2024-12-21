
#include "rasgl/core/debug.h"
#include <string.h>

char log_buffer[RAX_MAX_LOG_BUFFER];

void ras_log_flush()
{
    ras_log_trace("flushing buffer");
    ras_log_info(log_buffer);
}

void ras_log_clear()
{
    log_buffer[0] = '\0';
}

void core_log_buffer(int level, const char* file, int line, const char* fmt, ...)
{
    if (level < log_get_level()) {
        return;
    }

    char buffer[1000];
    va_list args;

    va_start(args, fmt);
    vsnprintf(buffer, sizeof buffer, fmt, args);
    va_end(args);
    strcat(buffer, "\n");
    strcat(log_buffer, buffer);
}
