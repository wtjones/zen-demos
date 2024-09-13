#ifndef BOARD_H
#define BOARD_H

#include "deck.h"
#include <stdint.h>

#define BOARD_COLS 4
#define BOARD_ROWS 4
#define CARD_COUNT 52
#define NONE_REQUIRED_RANKS 0x0000
#define NON_FACE_REQUIRED_RANKS 0x03FF
#define FACE_REQUIRED_RANKS 0x1C00
#define WILD_REQUIRED_RANKS 0x1FFF
#define KING_REQUIRED_RANK (1 << (KING - 1))
#define KING_ALLOWED_RANKS NON_FACE_REQUIRED_RANKS | KING_REQUIRED_RANK
#define QUEEN_REQUIRED_RANK (1 << (QUEEN - 1))
#define QUEEN_ALLOWED_RANKS NON_FACE_REQUIRED_RANKS | QUEEN_REQUIRED_RANK
#define JACK_REQUIRED_RANK (1 << (JACK - 1))
#define JACK_ALLOWED_RANKS NON_FACE_REQUIRED_RANKS | JACK_REQUIRED_RANK

typedef enum {
    WILD,
    JACK_REQUIRED,
    QUEEN_REQUIRED,
    KING_REQUIRED
} CellType;

typedef struct {
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

bool board_has_pair(Board* board);

#endif
