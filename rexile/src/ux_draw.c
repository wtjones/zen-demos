#include "ux_draw.h"

WINDOW* g_main_window_border;
WINDOW* g_main_window;
WINDOW* g_board_window_border;
WINDOW* g_board_window;
WINDOW* g_game_state_window_border;
WINDOW* g_game_state_window;
WINDOW* g_message_window_border;
WINDOW* g_message_window;

void ux_draw_init(UXLayout* layout)
{
    layout->main_window_border = newwin(
        UX_PARENT_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_PARENT_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        0, 0);

    box(layout->main_window_border, 0, 0);
    refresh();
    wrefresh(layout->main_window_border);

    layout->main_window = derwin(
        layout->main_window_border,
        UX_PARENT_WINDOW_HEIGHT,
        UX_PARENT_WINDOW_WIDTH,
        UX_WINDOW_BORDER_PADDING,
        UX_WINDOW_BORDER_PADDING);

    wrefresh(layout->main_window);

    int main_window_contents_offset_y = 0;
    int main_window_contents_offset_x = 0;

    layout->board_window_border = derwin(
        layout->main_window,
        UX_BOARD_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_BOARD_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        main_window_contents_offset_y,
        main_window_contents_offset_x);

    main_window_contents_offset_y += UX_BOARD_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING;

    box(layout->board_window_border, 0, 0);

    layout->board_window = derwin(
        layout->board_window_border,
        UX_BOARD_WINDOW_HEIGHT,
        UX_BOARD_WINDOW_WIDTH,
        UX_WINDOW_BORDER_PADDING,
        UX_WINDOW_BORDER_PADDING);

    wrefresh(layout->board_window);

    layout->game_state_window_border = derwin(
        layout->main_window,
        UX_GAME_STATE_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_GAME_STATE_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        main_window_contents_offset_y,
        main_window_contents_offset_x);

    main_window_contents_offset_y += UX_GAME_STATE_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING;

    box(layout->game_state_window_border, 0, 0);

    layout->game_state_window = derwin(
        layout->game_state_window_border,
        UX_GAME_STATE_WINDOW_HEIGHT,
        UX_GAME_STATE_WINDOW_WIDTH,
        UX_WINDOW_BORDER_PADDING,
        UX_WINDOW_BORDER_PADDING);

    scrollok(layout->game_state_window, true);
    wrefresh(layout->board_window_border);

    wrefresh(layout->board_window);

    wprintw(layout->game_state_window, "Game State!");
    box(layout->game_state_window_border, 0, 0);

    wrefresh(layout->game_state_window_border);

    wrefresh(layout->game_state_window);
}

void draw_board(UXLayout* layout, Game* game)
{
    int x = 3, y = 3;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            BoardCell* cell = &game->board.cells[i][j];
            mvwprintw(layout->board_window, y, x, "%d", cell->type);

            if (game->cursor.row == i && game->cursor.col == j) {
                wattron(layout->board_window, A_REVERSE);

                mvwprintw(layout->board_window, y, x, "%d", cell->type);
                wattroff(layout->board_window, A_REVERSE);
            }
            x += 3;
        }
        x = 3;
        y += 3;
    }
    wrefresh(layout->board_window);
}

void ux_draw_start(UXLayout* layout, Game* game)
{
    draw_board(layout, game);
}
