#include "game.h"
#include <stdbool.h>

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

bool is_placement_valid(BoardCell* cell, Card* card)
{
    return cell->type == WILD
        || card->rank < JACK
        || (cell->type == KING_REQUIRED && card->rank == KING)
        || (cell->type == QUEEN_REQUIRED && card->rank == QUEEN)
        || (cell->type == JACK_REQUIRED && card->rank == JACK);
}

void game_update(Game* game, GameAction action)
{
    switch (game->state) {
    case GAME_PLACE:
        if (action == ACTION_SELECT) {

            BoardCell* selected = &game->board.cells[game->cursor.row][game->cursor.col];

            if (selected->card != NULL) {
                return;
            }

            if (!is_placement_valid(selected, game->up_card)) {
                return;
            }

            selected->card = game->up_card;
            game->up_card = deck_draw(&game->deck);
            if (game->up_card == NULL) {
                game->state = GAME_LOSE;
            }

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
