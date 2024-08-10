#include "ux_draw.h"

void draw_board(Game* game)
{
    int x = 3, y = 3;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            BoardCell* cell = &game->board.cells[i][j];
            mvprintw(y, x, "%d", cell->type);
            if (game->cursor.row == i && game->cursor.col == j) {
                attron(A_REVERSE);
                mvprintw(y, x, "%d", cell->type);
                attroff(A_REVERSE);
            }
            x += 3;
        }
        x = 3;
        y += 3;
    }
}

void draw_grid(int start_y, int start_x, int rows, int cols, int cell_height, int cell_width)
{
    // FIXME
    for (int i = 0; i <= rows; ++i) {
        for (int j = 0; j <= cols; ++j) {
            int y = start_y + i * cell_height;
            int x = start_x + j * cell_width;

            // Draw horizontal lines
            if (i < rows) {
                for (int k = 0; k < cell_width; ++k) {
                    mvaddch(y, x + k, '-');
                }
            }

            // Draw vertical lines
            if (j < cols) {
                for (int k = 0; k < cell_height; ++k) {
                    mvaddch(y + k, x, '|');
                }
            }

            attron(COLOR_PAIR(1));

            // Draw intersections
            if (i < rows && j < cols) {
                mvaddch(y, x, CHAR_SQUARE);
                mvaddch(y, x + 1, CHAR_SQUARE);
            }
            attroff(COLOR_PAIR(1));
        }
    }

    refresh();
}

void ux_draw_start(Game* game)
{
    int rows = 4;
    int cols = 4;
    int cell_height = 3;
    int cell_width = 3;

    draw_board(game);
}
