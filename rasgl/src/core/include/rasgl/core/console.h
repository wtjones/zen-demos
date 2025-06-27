#ifndef CORE_CONSOLE_H
#define CORE_CONSOLE_H

#include "debug.h"
#include "graphics.h"
#include "input.h"
#include "maths.h"
#include "text.h"

#define RAS_CONSOLE_DEFAULT_CAPACITY 1024 // Favor power-of-two
#define RAS_CONSOLE_DEFAULT_ROWS 30
#define RAS_CONSOLE_DEFAULT_COLS 30
#define RAS_CONSOLE_MAX_LINE_COUNT 100
#define RAS_CONSOLE_PROMPT_CHAR ">"

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
    Point2f screen_pos;
    char prompt_text[RAS_CONSOLE_DEFAULT_CAPACITY];
} RasConsole;

typedef struct RasConsoleLineIndex {
    int32_t line_starts[RAS_CONSOLE_MAX_LINE_COUNT];
    size_t max_count;
    size_t count;
} RasConsoleLineIndex;

RasResult core_console_init(RasConsole* console, ScreenSettings* settings);
void core_console_update(RasConsole* console, InputState* input_state);
bool core_console_is_full(RasConsole* console);

RasResult core_draw_console(RenderState* state, RasFont* font, RasConsole* console);
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

/**
 * @brief Fill string buffer from console text. Buffer should match
 * capacity of console buffer.
 *
 * @param buffer
 * @param count
 * @param console
 * @return char*
 */
char* repr_console_buffer(char* buffer, size_t count, RasConsole* console);

/**
 * @brief Fill string buffer from a line console text.
 * Copies until first newline.
 *
 * @param buffer
 * @param count
 * @param console
 * @param start Start postion.
 * @return char*
 */
char* repr_console_buffer_line(char* buffer, size_t count, RasConsole* console, size_t start);

#endif
