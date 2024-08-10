#include "game.h"

void game_init(Game* game)
{
    game->score = 0;
    game->state = GAME_INIT;
    game->cursor.row = 0;
    game->cursor.col = 0;
    game->action_count = 0;
    game->message[0] = '\0';

    board_init(&game->board);
    deck_init(&game->deck);
    deck_shuffle(&game->deck);
}

void game_update(GameAction action);
