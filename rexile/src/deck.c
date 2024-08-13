
#include "deck.h"
#include <time.h>

void deck_shuffle(CardDeck* deck)
{
    srand(time(NULL));

    for (size_t i = DECK_SIZE - 1; i > 0; i--) {
        // Generate a random index from 0 to i
        size_t j = rand() % (i + 1);

        Card temp = deck->cards[i];
        deck->cards[i] = deck->cards[j];
        deck->cards[j] = temp;
    }
}

void deck_init(CardDeck* deck)
{

    deck->top_index = -1;
    for (CardSuit i = HEARTS; i < SPADES + 1; i++) {
        for (CardRank j = ACE; j < KING + 1; j++) {
            deck->top_index++;
            deck->cards[deck->top_index].rank = j;
            deck->cards[deck->top_index].suit = i;
        }
    }
}

Card* deck_draw(CardDeck* deck)
{
    if (deck->top_index < 0) {
        return NULL;
    }

    return &deck->cards[deck->top_index--];
}
