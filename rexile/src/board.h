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
    NONE,
    MARKER
} CellToken;

typedef struct {
    CellType type;
    Card* card;
} BoardCell;

typedef struct {
    BoardCell cells[BOARD_ROWS][BOARD_COLS];
} Board;

void board_init(Board* board);

#endif
