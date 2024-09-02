#ifndef BOARD_H
#define BOARD_H

#include "deck.h"

#define BOARD_COLS 4
#define BOARD_ROWS 4
#define CARD_COUNT 52
#define NONE_REQUIRED_RANKS 0x0000
#define NON_FACE_REQUIRED_RANKS 0x03FF
#define FACE_REQUIRED_RANKS 0x1C00
#define WILD_REQUIRED_RANKS 0x1FFF
#define KING_REQUIRED_RANK 1 << (KING - 1)
#define QUEEN_REQUIRED_RANK 1 << (QUEEN - 1)
#define JACK_REQUIRED_RANK 1 << (JACK - 1)

typedef enum {
    WILD,
    JACK_REQUIRED,
    QUEEN_REQUIRED,
    KING_REQUIRED
} CellType;

// deprecated
typedef enum {
    TOKEN_NONE,
    TOKEN_MARKER
} CellToken;

typedef struct {
    // deprecated
    CellType type;
    // deprecated
    Card* card;
    // deprecated
    CellToken token;
    CardStack card_stack;
    int16_t allowed_ranks;
    int16_t required_ranks;
} BoardCell;

typedef struct {
    size_t row;
    size_t col;
} BoardCellPosition;

typedef struct {
    BoardCell cells[BOARD_ROWS][BOARD_COLS];
} Board;

void board_init(Board* board);

CardRank cell_type_to_rank(CellType type);

#endif
