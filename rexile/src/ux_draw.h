#ifndef UX_DRAW_H
#define UX_DRAW_H

#include "board.h"
#include "game.h"
#include "ux_state.h"
#include <ncurses.h>

#define UX_CHAR_SQUARE "\u25A3"
#define UX_WINDOW_BORDER_PADDING 1
#define UX_WINDOW_PADDING 1
#define UX_PARENT_WINDOW_HEIGHT 30
#define UX_PARENT_WINDOW_WIDTH 80
#define UX_BOARD_WINDOW_HEIGHT 16
#define UX_BOARD_WINDOW_WIDTH UX_PARENT_WINDOW_WIDTH - (2 * UX_WINDOW_PADDING)
#define UX_GAME_STATE_WINDOW_HEIGHT 6
#define UX_GAME_STATE_WINDOW_WIDTH UX_PARENT_WINDOW_WIDTH - (2 * UX_WINDOW_PADDING)
#define UX_MESSAGE_WINDOW_HEIGHT 4

typedef struct {

    WINDOW* main_window_border;
    WINDOW* main_window;
    WINDOW* board_window_border;
    WINDOW* board_window;
    WINDOW* game_state_window_border;
    WINDOW* game_state_window;
    WINDOW* message_window_border;
    WINDOW* message_window;
} UXLayout;

void ux_draw_init(UXLayout* layout);
void ux_draw_start(UXLayout* layout, Game* game);

#endif
