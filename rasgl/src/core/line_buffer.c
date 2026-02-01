#include "rasgl/core/line_buffer.h"
#include <string.h>
RasLineBuffer* core_line_buffer_init(size_t capacity)
{
    RasLineBuffer* line_buffer = malloc(sizeof(RasLineBuffer));
    if (line_buffer == NULL) {
        ras_log_error("Unable to malloc().");
        return NULL;
    }

    line_buffer->text = malloc(capacity);
    if (line_buffer->text == NULL) {
        ras_log_error("Unable to malloc().");
        return NULL;
    }

    line_buffer->text[0] = '\0';
    line_buffer->head = 0;
    line_buffer->tail = 0;
    line_buffer->capacity = capacity;
    line_buffer->max_count = line_buffer->capacity - 1;
    return line_buffer;
}

void core_line_buffer_free(RasLineBuffer* line_buffer)
{
    free(line_buffer->text);
    free(line_buffer);
}

bool core_line_buffer_is_full(RasLineBuffer* line_buffer)
{
    return line_buffer->head == (line_buffer->tail + 1) % line_buffer->capacity;
}

size_t core_line_buffer_count(RasLineBuffer* line_buffer)
{
    // If tail has wrapped, add capacity to tail to calculate.
    //  T   H   t
    // 01234567xx
    return line_buffer->head <= line_buffer->tail
        ? line_buffer->tail - line_buffer->head
        : line_buffer->tail + line_buffer->capacity - line_buffer->head;
}

RasResult line_buffer_trim(RasLineBuffer* line_buffer, size_t append_count)
{
    ras_log_info("Line buffer trim free count before: %d",
        line_buffer->max_count - core_line_buffer_count(line_buffer));
    if (append_count > line_buffer->max_count) {
        ras_log_error("Trim request exceeds max_count.");
        return RAS_RESULT_ERROR;
    }

    size_t* head = &line_buffer->head;

    // Allow for newline delimitter.
    bool done = line_buffer->max_count - core_line_buffer_count(line_buffer) >= append_count + 1;
    while (!done) {
        while (line_buffer->text[*head] != '\n') {
            (*head)++;
        }
        (*head)++; // Eat the newline;

        done = line_buffer->max_count - core_line_buffer_count(line_buffer) >= append_count + 1;
    }

    ras_log_info("Line buffer trim free count after: %d",
        line_buffer->max_count - core_line_buffer_count(line_buffer));

    return RAS_RESULT_OK;
}

RasResult core_line_buffer_append(RasLineBuffer* line_buffer, const char* str)
{
    const char* src = str;

    if (RAS_RESULT_ERROR == line_buffer_trim(line_buffer, strlen(str))) {
        ras_log_error("Unable to trim buffer head.");
        return RAS_RESULT_ERROR;
    }

    while (*src != '\0') {
        line_buffer->text[line_buffer->tail] = *src;
        line_buffer->tail = (line_buffer->tail + 1) % line_buffer->capacity;
        src++;
    }
    line_buffer->text[line_buffer->tail] = '\n';
    line_buffer->tail = (line_buffer->tail + 1) % line_buffer->capacity;

    return RAS_RESULT_OK;
}

RasResult core_line_buffer_build_index(
    RasLineBuffer* line_buffer,
    RasLineBufferIndex* line_index)
{
    line_index->count = 0;
    line_index->max_count = RAS_LINE_BUFFER_MAX_COUNT;

    if (line_index == NULL) {
        ras_log_error("Null index");
        return RAS_RESULT_ERROR;
    }
    if (core_line_buffer_count(line_buffer) == 0) {
        return RAS_RESULT_OK;
    }

    int32_t current = line_buffer->head;
    int32_t last_start = line_buffer->head;
    size_t* ct = &line_index->count;

    while (true) {

        if (line_buffer->text[current] == '\n') {
            line_index->line_starts[(*ct)++] = last_start;

            current = (current + 1) % line_buffer->capacity;
            if (current == line_buffer->tail) {
                return RAS_RESULT_OK;
            }
            last_start = current;
            continue;
        }

        current = (current + 1) % line_buffer->capacity;
        if (current == line_buffer->tail) {
            ras_log_error("Unexpected that line buffer ends with non-newline.");
            return RAS_RESULT_ERROR;
        }
    }

    return RAS_RESULT_OK;
}

char* core_repr_line_buffer(char* buffer, size_t count, RasLineBuffer* line_buffer)
{
    size_t* tail = &line_buffer->tail;
    size_t current = line_buffer->head;
    char* dest = buffer;

    if (count < line_buffer->capacity) {
        ras_log_error("Given buffer should match size of console.");
        return NULL;
    }

    while (current != *tail) {
        *dest = line_buffer->text[current];
        current = (current + 1) % line_buffer->capacity;
        dest++;
    }

    *dest = '\0';
    return buffer;
}

char* core_repr_line_buffer_line(char* buffer, size_t count, RasLineBuffer* line_buffer, size_t start)
{
    size_t* tail = &line_buffer->tail;
    size_t current = start;
    char* dest = buffer;
    *dest = '\0';

    if (count < line_buffer->capacity) {
        ras_log_error("Given buffer should match size of console.");
        return NULL;
    }

    while (true) {
        *dest = line_buffer->text[current] == '\n'
            ? '\0'
            : line_buffer->text[current];
        if (*dest == '\0') {
            return buffer;
        }
        current = (current + 1) % line_buffer->capacity;
        if (current == *tail) {
            ras_log_error("Should not reach tail");
            return NULL;
        }
        dest++;
    }

    return buffer;
}
