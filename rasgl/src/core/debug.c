
#include "rasgl/core/debug.h"
#include <assert.h>
#include <string.h>

char log_buffer[RAS_MAX_LOG_BUFFER];
size_t log_buffer_offset = 0;

void ras_log_flush()
{
    ras_log_trace("flushing buffer");

    // Decode The buffer format of {level}{message}{\0}
    size_t offset = 0;
    while (offset < log_buffer_offset) {
        int level;
        memcpy(&level, log_buffer + offset, sizeof(level));
        offset += sizeof(level);

        const char* message = log_buffer + offset;
        size_t message_length = strlen(message) + 1;
        offset += message_length;

        // File and line number not available
        log_log(level, "", 0, message);
    }
}

void ras_log_clear()
{
    log_buffer[0] = '\0';
    log_buffer_offset = 0;
}

void core_log_buffer(int level, const char* file, int line, const char* fmt, ...)
{
    char buffer[1000];
    va_list args;

    va_start(args, fmt);
    vsnprintf(buffer, sizeof buffer, fmt, args);
    va_end(args);

    // Validate based on size of {level}{message}{\0}
    if (log_buffer_offset + sizeof(level) + strlen(buffer) + 1 > RAS_MAX_LOG_BUFFER) {
        log_error("Log buffer overflow! %d\n", strlen(log_buffer));

        return;
    }

    // Prefix the log level to the buffer
    memcpy(log_buffer + log_buffer_offset, &level, sizeof(level));
    log_buffer_offset += sizeof(level);

    size_t message_length = strlen(buffer) + 1; // +1 for the null terminator
    memcpy(log_buffer + log_buffer_offset, buffer, message_length);
    log_buffer_offset += message_length;
}
