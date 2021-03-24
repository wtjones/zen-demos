#include "input.h"
#include <stdio.h>

void input_update()
{
    size_t i;
    for (i = 0; i < GAME_MAX_KEYS; i++) {
        game_keys[i] = 0;
    }

    nodelay(stdscr, 1);
    keypad(stdscr, 1);

    int ch = getch();
    if (ch != ERR) {
        switch (ch) {
        case 'q':
            game_keys[GAME_KEY_Q] = 1;
            break;
        case KEY_UP:
            game_keys[GAME_KEY_UP] = 1;
            break;
        case KEY_DOWN:
            game_keys[GAME_KEY_DOWN] = 1;
            break;
        case KEY_LEFT:
            game_keys[GAME_KEY_LEFT] = 1;
            break;
        case KEY_RIGHT:
            game_keys[GAME_KEY_RIGHT] = 1;
            break;
        }
    }
}
