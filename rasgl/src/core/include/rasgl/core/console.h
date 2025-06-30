#ifndef CORE_CONSOLE_H
#define CORE_CONSOLE_H

#include "debug.h"
#include "graphics.h"
#include "input.h"
#include "line_buffer.h"
#include "maths.h"
#include "text.h"

#define RAS_CONSOLE_DEFAULT_CAPACITY 1024 // Favor power-of-two
#define RAS_CONSOLE_DEFAULT_ROWS 30
#define RAS_CONSOLE_DEFAULT_COLS 30
#define RAS_CONSOLE_MAX_LINE_COUNT 100
#define RAS_CONSOLE_PROMPT_CHAR ">"
#define RAS_CONSOLE_HISTORY_DEPTH_DEFAULT 1

typedef struct RasConsole {
    int32_t visible_rows;
    int32_t visible_cols;
    RasLineBuffer* buffer;
    RasLineBuffer* history;
    /**
     * @brief Current line in history to recall relative to most recent.
     * Decremented with subsequent history key presses.
     * A value of 1 indicates default state: no recall.
     * A recall back adds -1.
     * A value of 0 indicates a recall of the most recent history line.
     * Value is always <= 1.
     * Does not equate to size of history buffer.
     *
     */
    int32_t history_recall_depth;
    Point2f screen_pos;
    char prompt_text[RAS_CONSOLE_DEFAULT_CAPACITY];
} RasConsole;

/**
 * @brief Allocate a console based on settings. Must be freed via
 * core_console_free().
 *
 * @param settings
 * @return RasConsole*
 */
RasConsole* core_console_init(ScreenSettings* settings);
void core_console_free(RasConsole* console);

void core_console_update(RasConsole* console, InputState* input_state);

RasResult core_draw_console(RenderState* state, RasFont* font, RasConsole* console);

/**
 * @brief Fill string buffer from console text. Buffer should match
 * capacity of console buffer.
 *
 * @param buffer
 * @param count
 * @param console
 * @return char*
 */

#endif
