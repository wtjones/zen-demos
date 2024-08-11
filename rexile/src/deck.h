#ifndef DECK_H
#define DECK_H

#include <stdlib.h>

#define DECK_SIZE 52

typedef enum {
    HEARTS,
    DIAMONDS,
    CLUBS,
    SPADES
} CardSuit;

typedef enum {
    JOKER,
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

typedef struct {
    Card cards[52];
    size_t top_index;
} CardDeck;

void deck_init(CardDeck* deck);
void deck_shuffle(CardDeck* deck);

/**
 * @brief Decrement the top index of the deck and return the card at that index
 *
 * @param deck
 * @return Card* NULL if the deck is empty
 */
Card* deck_draw(CardDeck* deck);

#endif
