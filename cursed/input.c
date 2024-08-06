#include "input.h"
#include <stdio.h>

void input_update()
{
    size_t i;
    for (i = 0; i < GAME_MAX_KEYS; i++) {
        game_keys[i] = 0;
    }

    int ch = getch();
    if (ch == ERR) {
        return;
    }

    switch (ch) {
    case KEY_MOUSE:
        MEVENT event;
        if (getmouse(&event) == OK) {
            if (event.bstate & BUTTON1_RELEASED) {
                log_info("Mouse release at %d, %d", event.x, event.y);
                game_keys[GAME_KEY_MOUSE1] = 1;
                mouse_x = event.x / 2;
                mouse_y = event.y;
            }
        }
        break;
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
