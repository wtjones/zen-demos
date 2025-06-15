#ifndef CORE_CONSOLE_H
#define CORE_CONSOLE_H

#include "debug.h"
#include "graphics.h"
#include "text.h"

#define RAS_CONSOLE_DEFAULT_CAPACITY 1024 // Favor power-of-two
#define RAS_CONSOLE_DEFAULT_ROWS 20
#define RAS_CONSOLE_DEFAULT_COLS 30
#define RAS_CONSOLE_MAX_LINE_COUNT 100

typedef struct RasConsoleBuffer {
    char text[RAS_CONSOLE_DEFAULT_CAPACITY];
    size_t head;
    size_t tail;
    // Allocated array capacity. One element is reserved.
    size_t capacity;
    // Capacity - 1;
    size_t max_count;

} RasConsoleBuffer;

typedef struct RasConsole {
    int32_t visible_rows;
    int32_t visible_cols;
    RasConsoleBuffer buffer;
} RasConsole;

typedef struct RasConsoleLineIndex {
    int32_t line_starts[RAS_CONSOLE_MAX_LINE_COUNT];
    size_t max_count;
    size_t count;
} RasConsoleLineIndex;

RasResult core_console_init(RasConsole* console, ScreenSettings* settings);
bool core_console_is_full(RasConsole* console);

/**
 * @brief Iterate console buffer by char in reverse.
 * If buffer is emtpy:
 *  set dest = NULL
 *  return -1
 * If iterator is at head:
 *  set dest = head
 *  return -1
 * If iterator not at head:
 *  set dest = iterator
 *  return iterator - 1
 * FIXME: Deprecate?
 * @param console
 * @param end
 * @param dest
 * @return int32_t
 */
int32_t core_console_iter_char_rev(
    RasConsole* console, int32_t iterator, char* dest);
// FIXME: Deprecate?
int32_t core_console_iter_line_rev(
    RasConsole* console,
    int32_t iterator, char* dest, size_t count);

RasResult core_draw_console(RenderState* state, RasFont* font);
RasResult core_console_append(RasConsole* console, const char* str);
size_t core_console_count(RasConsole* console);
/**
 * @brief Populate array of the start index for each line.
 *
 * @param console
 * @param line_index
 * @return RasResult
 */
RasResult core_console_build_index(RasConsole* console, RasConsoleLineIndex* line_index);

#endif
