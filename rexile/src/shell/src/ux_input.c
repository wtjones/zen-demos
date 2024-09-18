#include "ux_input.h"
#include "log.c/src/log.h"
#include "rexile/core/board.h"
#include <stdio.h>

void ux_input_update(int* input_keys)
{
    size_t i;
    for (i = 0; i < INPUT_KEY_MAX; i++) {
        input_keys[i] = 0;
    }

    int ch = getch();
    if (ch == ERR) {
        return;
    }

    switch (ch) {

    case 'q':
        input_keys[INPUT_KEY_Q] = 1;
        break;
    case KEY_UP:
        input_keys[INPUT_KEY_UP] = 1;
        break;
    case KEY_DOWN:
        input_keys[INPUT_KEY_DOWN] = 1;
        break;
    case KEY_LEFT:
        input_keys[INPUT_KEY_LEFT] = 1;
        break;
    case KEY_RIGHT:
        input_keys[INPUT_KEY_RIGHT] = 1;
        break;
    case KEY_ENTER:
        input_keys[INPUT_KEY_ENTER] = 1;
        break;
    case '\n':
        input_keys[INPUT_KEY_ENTER] = 1;
        break;
    case ' ':
        input_keys[INPUT_KEY_SPACE] = 1;
        break;
    case 'n':
        input_keys[INPUT_KEY_N] = 1;
        break;
    case 'p':
        input_keys[INPUT_KEY_P] = 1;
        break;
    case 'c':
        input_keys[INPUT_KEY_C] = 1;
        break;
    case 'l':
        input_keys[INPUT_KEY_L] = 1;
        break;
    }
}

void ux_cursor_update(UXCursor* cursor, int* keys)
{
    if (keys[INPUT_KEY_UP]) {
        if (cursor->row > 0) {
            cursor->row--;
        }
    }
    if (keys[INPUT_KEY_DOWN]) {
        if (cursor->row < BOARD_ROWS - 1) {
            cursor->row++;
        }
    }
    if (keys[INPUT_KEY_LEFT]) {
        if (cursor->col > 0) {
            cursor->col--;
        }
    }
    if (keys[INPUT_KEY_RIGHT]) {
        if (cursor->col < BOARD_COLS - 1) {
            cursor->col++;
        }
    }
}
