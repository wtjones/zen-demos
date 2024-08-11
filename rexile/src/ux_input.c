#include "ux_input.h"
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
    case ' ':
        input_keys[INPUT_KEY_SPACE] = 1;
        break;
    case 'n':
        input_keys[INPUT_KEY_N] = 1;
        break;
    }
}

GameAction ux_input_to_action(int* keys)
{
    if (keys[INPUT_KEY_UP]) {
        return ACTION_UP;
    }
    if (keys[INPUT_KEY_DOWN]) {
        return ACTION_DOWN;
    }
    if (keys[INPUT_KEY_LEFT]) {
        return ACTION_LEFT;
    }
    if (keys[INPUT_KEY_RIGHT]) {
        return ACTION_RIGHT;
    }
    if (keys[INPUT_KEY_ENTER]) {
        return ACTION_SELECT;
    }
    return ACTION_NONE;
}
