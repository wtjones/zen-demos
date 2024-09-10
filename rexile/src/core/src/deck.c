
#include "rexile/core/deck.h"
#include "log.c/src/log.h"
#include "rexile/core/repr.h"
#include <time.h>

int card_value(Card* card)
{
    return is_face_card(card) ? 10 : card->rank;
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
    char buffer[255];
    log_info("Pushing card to position %d: %s", stack->count, repr_card(buffer, sizeof(buffer), card));
    Card* new_card = &stack->cards[stack->count++];

    new_card->rank = card.rank;
    new_card->suit = card.suit;
    return stack->count;
}

Card card_stack_pop(CardStack* stack)
{
    char buffer[255];
    Card card = card_stack_peek(stack);
    log_info("Popping card from position %d: %s", stack->count - 1, repr_card(buffer, sizeof(buffer), card));

    return stack->cards[--stack->count];
}

Card card_stack_peek(CardStack* stack)
{
    return stack->cards[stack->count - 1];
}

void card_stack_fill(CardStack* stack)
{
    for (CardSuit i = HEARTS; i < SPADES + 1; i++) {
        for (CardRank j = ACE; j < KING + 1; j++) {
            Card card = { .rank = j, .suit = i };
            card_stack_push(stack, card);
        }
    }
}

void card_stack_shuffle(CardStack* stack)
{
    srand(time(NULL));

    for (size_t i = stack->count - 1; i > 0; i--) {
        // Generate a random index from 0 to i
        size_t j = rand() % (i + 1);

        Card temp = stack->cards[i];
        stack->cards[i] = stack->cards[j];
        stack->cards[j] = temp;
    }
}

bool is_face_card(Card* card)
{
    return card->rank >= JACK;
}
