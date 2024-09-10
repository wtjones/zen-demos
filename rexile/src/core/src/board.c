#include "rexile/core/board.h"
#include <stdlib.h>

void board_init(Board* board)
{
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            card_stack_clear(&board->cells[i][j].card_stack);
        }
    }

    // Row 0
    board->cells[0][0].allowed_ranks = KING_ALLOWED_RANKS;
    board->cells[0][0].required_ranks = KING_REQUIRED_RANK;

    board->cells[0][1].allowed_ranks = QUEEN_ALLOWED_RANKS;
    board->cells[0][1].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[0][2].allowed_ranks = QUEEN_ALLOWED_RANKS;
    board->cells[0][2].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[0][3].allowed_ranks = KING_ALLOWED_RANKS;
    board->cells[0][3].required_ranks = KING_REQUIRED_RANK;

    // Row 1
    board->cells[1][0].allowed_ranks = JACK_ALLOWED_RANKS;
    board->cells[1][0].required_ranks = JACK_REQUIRED_RANK;

    board->cells[1][1].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[1][1].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[1][2].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[1][2].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[1][3].allowed_ranks = JACK_ALLOWED_RANKS;
    board->cells[1][3].required_ranks = JACK_REQUIRED_RANK;

    // Row 2
    board->cells[2][0].allowed_ranks = JACK_ALLOWED_RANKS;
    board->cells[2][0].required_ranks = JACK_REQUIRED_RANK;

    board->cells[2][1].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[2][1].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[2][2].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[2][2].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[2][3].allowed_ranks = JACK_ALLOWED_RANKS;
    board->cells[2][3].required_ranks = JACK_REQUIRED_RANK;

    // Row 3
    board->cells[3][0].allowed_ranks = KING_ALLOWED_RANKS;
    board->cells[3][0].required_ranks = KING_REQUIRED_RANK;

    board->cells[3][1].allowed_ranks = QUEEN_ALLOWED_RANKS;
    board->cells[3][1].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[3][2].allowed_ranks = QUEEN_ALLOWED_RANKS;
    board->cells[3][2].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[3][3].allowed_ranks = KING_ALLOWED_RANKS;
    board->cells[3][3].required_ranks = KING_REQUIRED_RANK;
}
