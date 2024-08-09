#include "ux_draw.h"

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

void ux_draw_start()
{
    int rows = 4;
    int cols = 4;
    int cell_height = 3;
    int cell_width = 3;

    draw_grid(0, 0, rows, cols, cell_height, cell_width);
}
