
#include "rasgl/core/debug.h"
#include <assert.h>
#include <string.h>

char log_buffer[RAS_MAX_LOG_BUFFER];
size_t log_buffer_offset = 0;

void ras_log_flush()
{
    ras_log_info("flushing buffer with size of %d", log_buffer_offset);

    // Decode The buffer format of  {level}{line}{file}{\0}{message}{\0}
    size_t offset = 0;
    while (offset < log_buffer_offset) {
        int level;
        memcpy(&level, log_buffer + offset, sizeof(level));
        offset += sizeof(level);

        int line;
        memcpy(&line, log_buffer + offset, sizeof(line));
        offset += sizeof(line);

        const char* file = log_buffer + offset;
        size_t file_length = strlen(file) + 1;
        offset += file_length;

        const char* message = log_buffer + offset;
        size_t message_length = strlen(message) + 1;
        offset += message_length;

        log_log(level, file, line, message);
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

    // +2 for the null terminators
    size_t append_size = sizeof(level) + sizeof(line) + strlen(file) + strlen(buffer) + 2;
    // Validate based on size of {level}{line}{file}{\0}{message}{\0}
    if (log_buffer_offset + append_size > RAS_MAX_LOG_BUFFER) {
        log_error("Log buffer overflow! %d\n", strlen(log_buffer));
        return;
    }

    // Prefix the log level to the buffer
    memcpy(log_buffer + log_buffer_offset, &level, sizeof(level));
    log_buffer_offset += sizeof(level);

    // Prefix the line number to the buffer
    memcpy(log_buffer + log_buffer_offset, &line, sizeof(line));
    log_buffer_offset += sizeof(line);

    // Append the file name to the buffer
    size_t file_length = strlen(file) + 1; // +1 for the null terminator
    memcpy(log_buffer + log_buffer_offset, file, file_length);
    log_buffer_offset += file_length;

    // Append the message to the buffer
    size_t message_length = strlen(buffer) + 1; // +1 for the null terminator
    memcpy(log_buffer + log_buffer_offset, buffer, message_length);
    log_buffer_offset += message_length;
}
