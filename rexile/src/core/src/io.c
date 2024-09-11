#include "rexile/core/io.h"
#include "log.c/src/log.h"
#include <string.h>

bool card_from_string(const char* in_rank, const char* in_suit, Card* card)
{
    if (in_rank == NULL || in_suit == NULL) {
        return false;
    }
    if (strcmp(in_rank, "A") == 0) {
        card->rank = ACE;
    } else if (strcmp(in_rank, "J") == 0) {
        card->rank = JACK;
    } else if (strcmp(in_rank, "Q") == 0) {
        card->rank = QUEEN;
    } else if (strcmp(in_rank, "K") == 0) {
        card->rank = KING;
    } else {
        card->rank = atoi(in_rank);
    }

    if (strcmp(in_suit, "♠") == 0) {
        card->suit = SPADES;
    } else if (strcmp(in_suit, "♣") == 0) {
        card->suit = CLUBS;
    } else if (strcmp(in_suit, "♦") == 0) {
        card->suit = DIAMONDS;
    } else if (strcmp(in_suit, "♥") == 0) {
        card->suit = HEARTS;
    } else {
        return false;
    }

    return card;
}

bool card_stack_load(const char* path, CardStack* stack)
{
    FILE* file = fopen(path, "r");
    if (file == NULL) {
        log_error("Failed to open file: %s", path);
        return false;
    }

    card_stack_clear(stack);

    Card card;

    // file input example:
    // A ♠
    // 10 ♣
    // J ♦
    // Q ♥
    char in_rank[3];
    char in_suit[3];

    while (fscanf(file, "%s %s", in_rank, in_suit) == 2) {
        log_info("Read card: %s %s", in_rank, in_suit);
        bool map_result = card_from_string(in_rank, in_suit, &card);
        if (!map_result) {
            log_error("Failed to map card from string: %s %s", in_rank, in_suit);
            fclose(file);
            return false;
        }
        card_stack_push(stack, card);
    }
    fclose(file);

    return true;
}
