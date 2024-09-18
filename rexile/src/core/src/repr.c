#include "rexile/core/repr.h"
#include "log.c/src/log.h"
#include <stdio.h>
#include <string.h>

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

static const char* g_repr_suit_simple[] = {
    "♥",
    "♦",
    "♣",
    "♠"
};

const char* repr_suit_simple(CardSuit suit)
{
    return g_repr_suit_simple[suit];
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

static const char* g_repr_rank_simple[] = {
    "J",
    "A",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "T",
    "J",
    "Q",
    "K"
};

const char* repr_rank_simple(CardRank rank)
{
    return g_repr_rank_simple[rank];
}

char* repr_card(char* buffer, size_t count, Card card)
{
    buffer[0] = '\0';
    snprintf(
        buffer,
        count,
        "[%s of %s]",
        repr_rank(card.rank),
        repr_suit(card.suit));
    return buffer;
}

char* repr_card_simple(char* buffer, size_t count, Card card)
{
    buffer[0] = '\0';
    snprintf(
        buffer,
        count,
        "%s%s",
        repr_rank_simple(card.rank),
        repr_suit_simple(card.suit));
    return buffer;
}

static const char* g_repr_game_move_type[] = {
    "Place  ",
    "Combine"
};

const char* repr_game_move_type(GameMoveType type)
{
    return g_repr_game_move_type[type];
}

static const char* g_repr_game_state[] = {
    "GAME_PLACE",
    "GAME_COMBINE",
    "COMBINE_OR_PLACE",
    "GAME_OVER_WIN",
    "GAME_OVER_LOSE"
};

const char* repr_game_state(GameState state)
{
    return g_repr_game_state[state];
}

char* repr_game_move(char* buffer, size_t count, GameMove* move)
{
    size_t pos = 0;
    char card_buffer[255];
    buffer[0] = '\0';
    card_buffer[0] = '\0';

    pos += snprintf(
        buffer + pos,
        count - pos,
        "{ %s: [",
        repr_game_move_type(move->type));

    for (int i = 0; i < move->action_count; i++) {
        GameMoveAction* action = &move->actions[i];

        repr_card_simple(card_buffer, 255, action->card);

        if (move->type == MOVE_COMBINE) {
            pos += snprintf(
                buffer + pos,
                count - pos,
                "%s at (%zu, %zu)%s",
                card_buffer,
                action->pos.row,
                action->pos.col,
                i < move->action_count - 1 ? ", " : "");
        } else {
            pos += snprintf(
                buffer + pos,
                count - pos,
                "%s to (%zu, %zu)",
                card_buffer,
                action->pos.row,
                action->pos.col);
        }
    }

    pos += snprintf(
        buffer + pos,
        count - pos,
        ", State: %s, Score: %d] }",
        repr_game_state(move->new_state),
        move->score_delta);

    return buffer;
}

char* repr_move_ledger(char* buffer, size_t count, Game* game)
{
    char move_buffer[255];
    char line_buffer[255] = "";

    buffer[0] = '\0';
    snprintf(buffer, count, "[\n");

    for (int i = 0; i < game->move_count; i++) {
        repr_game_move(move_buffer, 255, &game->moves[i]);

        strcat(line_buffer, "   ");
        strcat(line_buffer, move_buffer);
        if (i < game->move_count - 1) {
            strcat(line_buffer, ", ");
        }
        strcat(line_buffer, "\n");
        strcat(buffer, line_buffer);

        line_buffer[0] = '\0';
    }
    strcat(buffer, "]");
    return buffer;
}
