#ifndef BOARD_H
#define BOARD_H

#include "deck.h"

#define BOARD_COLS 4
#define BOARD_ROWS 4
#define CARD_COUNT 52

typedef enum {
    WILD,
    JACK_REQUIRED,
    QUEEN_REQUIRED,
    KING_REQUIRED
} CellType;

typedef enum {
    TOKEN_NONE,
    TOKEN_MARKER
} CellToken;

typedef struct {
    CellType type;
    Card* card;
    CellToken token;
} BoardCell;

typedef struct {
    BoardCell cells[BOARD_ROWS][BOARD_COLS];
} Board;

void board_init(Board* board);

CardRank cell_type_to_rank(CellType type);

#endif
