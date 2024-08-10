#ifndef UX_DRAW_H
#define UX_DRAW_H

#include "board.h"
#include "game.h"
#include "ux_state.h"
#include <ncurses.h>

#define CHAR_SQUARE "\u25A3"

void ux_draw_start(Game* game);

#endif
