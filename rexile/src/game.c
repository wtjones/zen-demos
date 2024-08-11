#include "game.h"

void game_init(Game* game)
{
    game->score = 0;
    game->cursor.row = 0;
    game->cursor.col = 0;
    game->action_count = 0;
    game->message[0] = '\0';

    board_init(&game->board);
    deck_init(&game->deck);
    deck_shuffle(&game->deck);
    game->up_card = deck_draw(&game->deck);
    game->state = GAME_PLACE;
}

void game_update(Game* game, GameAction action)
{
    switch (game->state) {
    case GAME_INIT:
        game->state = GAME_PLACE;

    case GAME_PLACE:
        if (action == ACTION_SELECT) {
            // TODO
        } else {
            switch (action) {
            case ACTION_UP:
                if (game->cursor.row > 0) {
                    game->cursor.row--;
                }
                break;
            case ACTION_DOWN:
                if (game->cursor.row < 3) {
                    game->cursor.row++;
                }
                break;
            case ACTION_LEFT:
                if (game->cursor.col > 0) {
                    game->cursor.col--;
                }
                break;
            case ACTION_RIGHT:
                if (game->cursor.col < 3) {
                    game->cursor.col++;
                }
                break;
            default:
                break;
            }
        }
        break;
    case GAME_COMBINE:
        if (action == ACTION_SELECT) {
            // FIXME
            game->state = GAME_PLACE;
        }
        break;
    case GAME_LOSE:
        break;
    }
}
