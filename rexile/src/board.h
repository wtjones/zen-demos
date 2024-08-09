#ifndef BOARD_H
#define BOARD_H

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
    HEARTS,
    DIAMONDS,
    CLUBS,
    SPADES
} CardSuit;

typedef enum {
    JOCKER,
    ACE,
    TWO,
    THREE,
    FOUR,
    FIVE,
    SIX,
    SEVEN,
    EIGHT,
    NINE,
    TEN,
    JACK,
    QUEEN,
    KING
} CardRank;

typedef struct {
    CardSuit suit;
    CardRank rank;
} Card;

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
