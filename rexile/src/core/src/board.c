#include "rexile/core/board.h"
#include <stdlib.h>

void board_init(Board* board)
{
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            board->cells[i][j].card = NULL;
            board->cells[i][j].token = TOKEN_NONE;

            card_stack_clear(&board->cells[i][j].card_stack);
        }
    }

    // deprecated
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

    // Row 0
    board->cells[0][0].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[0][0].required_ranks = KING_REQUIRED_RANK;

    board->cells[0][1].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[0][1].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[0][2].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[0][2].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[0][3].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[0][3].required_ranks = KING_REQUIRED_RANK;

    // Row 1
    board->cells[1][0].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[1][0].required_ranks = JACK_REQUIRED_RANK;

    board->cells[1][1].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[1][1].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[1][2].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[1][2].required_ranks = JACK_REQUIRED_RANK;

    board->cells[1][3].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[1][3].required_ranks = JACK_REQUIRED_RANK;

    // Row 2
    board->cells[2][0].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[2][0].required_ranks = JACK_REQUIRED_RANK;

    board->cells[2][1].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[2][1].required_ranks = NONE_REQUIRED_RANKS;

    board->cells[2][2].allowed_ranks = NON_FACE_REQUIRED_RANKS;
    board->cells[2][2].required_ranks = JACK_REQUIRED_RANK;

    board->cells[2][3].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[2][3].required_ranks = JACK_REQUIRED_RANK;

    // Row 3
    board->cells[3][0].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[3][0].required_ranks = KING_REQUIRED_RANK;

    board->cells[3][1].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[3][1].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[3][2].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[3][2].required_ranks = QUEEN_REQUIRED_RANK;

    board->cells[3][3].allowed_ranks = WILD_REQUIRED_RANKS;
    board->cells[3][3].required_ranks = KING_REQUIRED_RANK;
}

// deprecated
CardRank cell_type_to_rank(CellType type)
{
    switch (type) {
    case JACK_REQUIRED:
        return JACK;
    case QUEEN_REQUIRED:
        return QUEEN;
    case KING_REQUIRED:
        return KING;
    default:
        return -1;
    }
}
