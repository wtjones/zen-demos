#ifndef DECK_H
#define DECK_H

#include <stdlib.h>

#define DECK_SIZE 52
#define CARD_STACK_MAX 52

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
    Card cards[CARD_STACK_MAX];
    size_t count;
} CardStack;

// deprecated
typedef struct {
    Card cards[52];
    size_t top_index;
} CardDeck;

/**
 * @brief Init to zero cards in the stack
 *
 * @param stack
 */
void card_stack_clear(CardStack* stack);

/**
 * @brief Fill the stack with 52 cards
 *
 * @param stack
 */
void card_stack_fill(CardStack* stack);

/**
 * @brief Push a card into the stack
 *
 * @param stack
 * @param card
 * @return size_t stack count
 */
size_t card_stack_push(CardStack* stack, Card card);

Card card_stack_pop(CardStack* stack);

/**
 * @brief Push source cards into the stack
 *
 * @param stack
 * @param card
 */
void card_stack_populate(CardStack* stack, Card source_cards[], size_t source_count);

/**
 * @brief Shuffle the stack
 *
 * @param stack
 */
void card_stack_shuffle(CardStack* stack);

// deprecated
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