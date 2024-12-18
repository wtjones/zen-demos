#ifndef UX_DRAW_H
#define UX_DRAW_H

#include "rexile/core/board.h"
#include "rexile/core/game.h"
#include "rexile/core/score.h"
#include "ux_input.h"
#include "ux_state.h"
#include <ncurses.h>

#define UX_CHAR_SQUARE "\u25A3"
#define UX_CHAR_HEAVY_BALLOT_CHECK "\u2714"
#define UX_CHAR_HEAVY_BALLOT_X "\u2718"
#define UX_WINDOW_BORDER_PADDING 1
#define UX_WINDOW_PADDING 1
#define UX_WINDOW_PADDING_HEIGHT 1
#define UX_WINDOW_PADDING_WIDTH UX_WINDOW_PADDING_HEIGHT * 2
#define UX_PARENT_WINDOW_HEIGHT 30
#define UX_PARENT_WINDOW_WIDTH 80

#define UX_BOARD_WINDOW_HEIGHT 20
#define UX_BOARD_WINDOW_WIDTH UX_PARENT_WINDOW_WIDTH - (2 * UX_WINDOW_PADDING)

#define UX_CELL_WINDOW_HEIGHT (UX_BOARD_WINDOW_HEIGHT / BOARD_ROWS) - (UX_WINDOW_PADDING * 1)
#define UX_CELL_WINDOW_WIDTH 12

#define UX_CARD_WINDOW_HEIGHT 3
#define UX_CARD_WINDOW_WIDTH 4

#define UX_LEGEND_WINDOW_HEIGHT 22 + UX_WINDOW_BORDER_PADDING
#define UX_LEGEND_WINDOW_WIDTH (18 + (UX_WINDOW_BORDER_PADDING * 2))

#define UX_GAME_STATE_WINDOW_HEIGHT 2
#define UX_GAME_STATE_WINDOW_WIDTH UX_PARENT_WINDOW_WIDTH - (2 * UX_WINDOW_PADDING)
#define UX_MESSAGE_WINDOW_HEIGHT 4

#define UX_SCORE_WINDOW_HEIGHT 18
#define UX_SCORE_WINDOW_WIDTH 40

typedef struct {
    WINDOW* cell_window_border;
    WINDOW* cell_window;
    WINDOW* card_window;
    bool has_marker;
} UXCellLayout;

typedef struct {

    WINDOW* main_window_border;
    WINDOW* main_window;
    WINDOW* board_window_border;
    WINDOW* board_window;
    WINDOW* legend_window;
    WINDOW* game_state_window_border;
    WINDOW* game_state_window;
    WINDOW* message_window_border;
    WINDOW* message_window;
    UXCellLayout cells[BOARD_ROWS][BOARD_COLS];
    UXCursor cursor;
} UXLayout;

void ux_draw_init(UXLayout* layout);
void ux_draw_start(UXLayout* layout, Game* game);
void ux_clear_markers(UXLayout* layout);
void ux_draw_scores(UXLayout* layout, ScoreBoard* scores);
#endif
