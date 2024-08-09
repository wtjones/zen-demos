#include "board.h"
#include <stdlib.h>

void board_init(Board* board)
{
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            board->cells[i][j].card = NULL;
        }
    }

    // Row 0
    board->cells[0][0].type = KING_REQUIRED;
    board->cells[0][1].type = QUEEN_REQUIRED;
    board->cells[0][2].type = QUEEN_REQUIRED;
    board->cells[0][3].type = KING_REQUIRED;

    // Row 1
    board->cells[1][0].type = JACK_REQUIRED;
    board->cells[1][1].type = WILD;
    board->cells[1][2].type = WILD;
    board->cells[1][3].type = JACK_REQUIRED;

    // Row 2
    board->cells[2][0].type = JACK_REQUIRED;
    board->cells[2][1].type = WILD;
    board->cells[2][2].type = WILD;
    board->cells[2][3].type = JACK_REQUIRED;

    // Row 3
    board->cells[3][0].type = KING_REQUIRED;
    board->cells[3][1].type = QUEEN_REQUIRED;
    board->cells[3][2].type = QUEEN_REQUIRED;
    board->cells[3][3].type = KING_REQUIRED;
}
