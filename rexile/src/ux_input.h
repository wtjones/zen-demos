#ifndef UX_INPUT_H
#define UX_INPUT_H

#define _X_OPEN_SOURCE_EXTENDED
#include "game.h"
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>

#define INPUT_KEY_UP 0
#define INPUT_KEY_DOWN 1
#define INPUT_KEY_LEFT 2
#define INPUT_KEY_RIGHT 3
#define INPUT_KEY_Q 4
#define INPUT_KEY_MOUSE1 5
#define INPUT_KEY_SPACE 6
#define INPUT_KEY_N 7
#define INPUT_KEY_ENTER 8
#define INPUT_KEY_MAX 9

void ux_input_update(int* keys);
GameAction ux_input_to_action(int* keys);

#endif
