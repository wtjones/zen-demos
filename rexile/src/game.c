#include "game.h"

void deck_init(Card* deck)
{
    int card_index = 0;
    for (CardSuit i = HEARTS; i < SPADES + 1; i++) {
        for (CardRank j = ACE; j < KING + 1; j++) {
            deck[card_index].rank = j;
            deck[card_index].suit = i;
        }
    }

    // TODO: Shuffle deck
}
void game_init(Game* game)
{
    game->score = 0;
    game->state = GAME_INIT;
    game->action_count = 0;
    game->message[0] = '\0';

    board_init(&game->board);
    deck_init(game->deck);
}

void game_update(GameAction action);
