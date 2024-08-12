#include "ux_draw.h"

void draw_board_init(UXLayout* layout)
{
    int cell_offset_x = 0, cell_offset_y = 0;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            layout->cells[i][j].cell_window_border = derwin(
                layout->board_window,
                UX_CELL_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
                UX_CELL_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
                cell_offset_y,
                cell_offset_x);

            box(layout->cells[i][j].cell_window_border, 0, 0);

            layout->cells[i][j].cell_window = derwin(
                layout->cells[i][j].cell_window_border,
                UX_CELL_WINDOW_HEIGHT,
                UX_CELL_WINDOW_WIDTH,
                UX_WINDOW_BORDER_PADDING,
                UX_WINDOW_BORDER_PADDING);

            wrefresh(layout->cells[i][j].cell_window_border);
            wrefresh(layout->cells[i][j].cell_window);

            cell_offset_x += UX_CELL_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING;
        }
        cell_offset_x = 0;
        cell_offset_y += UX_CELL_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING;
    }
}

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

    layout->board_window = derwin(
        layout->main_window,
        UX_BOARD_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_BOARD_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        main_window_contents_offset_y,
        main_window_contents_offset_x);

    main_window_contents_offset_y += UX_BOARD_WINDOW_HEIGHT;

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

    draw_board_init(layout);

    wprintw(layout->game_state_window, "Game State!");
    box(layout->game_state_window_border, 0, 0);

    wrefresh(layout->game_state_window_border);

    wrefresh(layout->game_state_window);
}

void draw_board(UXLayout* layout, Game* game)
{
    char buffer[1024];

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            BoardCell* cell = &game->board.cells[i][j];
            UXCellLayout* cell_layout = &layout->cells[i][j];

            bool is_selected = game->cursor.row == i && game->cursor.col == j;

            sprintf(buffer, "%d", cell->type);

            if (is_selected) {
                wattron(cell_layout->cell_window, A_REVERSE);
            }
            mvwprintw(
                cell_layout->cell_window, 1, 1, "%s", g_ux_cell_type_desc[cell->type]);

            if (is_selected) {
                wattroff(cell_layout->cell_window, A_REVERSE);
            }
            wrefresh(cell_layout->cell_window);
        }
    }
    wrefresh(layout->board_window);
}

void ux_draw_start(UXLayout* layout, Game* game)
{
    draw_board(layout, game);
}
