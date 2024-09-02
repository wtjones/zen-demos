#include "rexile/core/repr.h"
#include <stdio.h>

const char* repr_suit(CardSuit suit)
{
    switch (suit) {
    case CLUBS:
        return "Clubs";
    case DIAMONDS:
        return "Diamonds";
    case HEARTS:
        return "Hearts";
    case SPADES:
        return "Spades";
    default:
        return "Unknown";
    }
}

const char* repr_rank(CardRank rank)
{
    switch (rank) {
    case ACE:
        return "Ace";
    case TWO:
        return "Two";
    case THREE:
        return "Three";
    case FOUR:
        return "Four";
    case FIVE:
        return "Five";
    case SIX:
        return "Six";
    case SEVEN:
        return "Seven";
    case EIGHT:
        return "Eight";
    case NINE:
        return "Nine";
    case TEN:
        return "Ten";
    case JACK:
        return "Jack";
    case QUEEN:
        return "Queen";
    case KING:
        return "King";
    }
}

char* repr_card(char* buffer, size_t count, Card card)
{

    snprintf(
        buffer,
        count,
        "[%s of %s]",
        repr_rank(card.rank),
        repr_suit(card.suit));
    return buffer;
}
