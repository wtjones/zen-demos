#ifndef CORE_LINE_BUFFER
#define CORE_LINE_BUFFER

#include "debug.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * @brief Max count of lines indexed.
 *
 */
#define RAS_LINE_BUFFER_MAX_COUNT 1024

typedef struct RasLineBufferIndex {
    int32_t line_starts[RAS_LINE_BUFFER_MAX_COUNT];
    size_t max_count;
    size_t count;
} RasLineBufferIndex;

/**
 * @brief A ring buffer of string lines.
 *
 */
typedef struct RasLineBuffer {
    char* text;
    size_t head;
    size_t tail;
    // Allocated array capacity. One element is reserved.
    size_t capacity;
    // Capacity - 1;
    size_t max_count;

} RasLineBuffer;

/**
 * @brief Allocates the buffer.
 * Must be freed via core_line_buffer_free().
 *
 * @param capacity
 * @return RasLineBuffer*
 */
RasLineBuffer* core_line_buffer_init(size_t capacity);
void core_line_buffer_free(RasLineBuffer* line_buffer);

bool core_console_is_full(RasLineBuffer* line_buffer);

/**
 * @brief Append a line to the buffer. The line will be suffixed
 * with a newline.
 *
 * @param line_buffer
 * @param str
 * @return RasResult
 */
RasResult core_line_buffer_append(RasLineBuffer* line_buffer, const char* str);

/**
 * @brief Calculate the current count in bytes with consideration
 * of ring buffer wrap.
 *
 * @param line_buffer
 * @return size_t
 */
size_t core_line_buffer_count(RasLineBuffer* console);

/**
 * @brief Populate array of the start index for each line.
 *
 * @param line_buffer
 * @param line_index
 * @return RasResult
 */
RasResult core_line_buffer_build_index(
    RasLineBuffer* line_buffer,
    RasLineBufferIndex* line_index);

/**
 * @brief Fill string buffe. Buffer should match capacity of buffer.
 *
 * @param buffer
 * @param count
 * @param line_buffer
 * @return char*
 */
char* core_repr_line_buffer(char* buffer, size_t count, RasLineBuffer* line_buffer);

/**
 * @brief Fill string buffer from the start of a line within the buffer.
 * Copies until first newline.
 *
 * @param buffer
 * @param count
 * @param line_buffer
 * @param start Start postion.
 * @return char*
 */
char* core_repr_line_buffer_line(
    char* buffer,
    size_t count,
    RasLineBuffer* line_buffer,
    size_t start);

#endif
