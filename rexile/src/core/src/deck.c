
#include "rexile/core/deck.h"
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

void card_stack_clear(CardStack* stack)
{
    stack->count = 0;
}

void card_stack_populate(CardStack* stack, Card source_cards[], size_t source_count)
{
    for (size_t i = 0; i < source_count; i++) {
        stack->cards[stack->count++] = source_cards[i];
    }
}

size_t card_stack_push(CardStack* stack, Card card)
{
    Card* new_card = &stack->cards[stack->count++];

    new_card->rank = card.rank;
    new_card->suit = card.suit;
    return stack->count;
}

Card card_stack_pop(CardStack* stack)
{
    return stack->cards[--stack->count];
}
