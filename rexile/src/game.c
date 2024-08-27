#include "game.h"
#include "external/log.c/src/log.h"
#include <stdbool.h>

void game_init(Game* game)
{
    game->score = 0;
    game->action_count = 0;
    game->message[0] = '\0';

    board_init(&game->board);
    deck_init(&game->deck);
    deck_shuffle(&game->deck);
    game->up_card = deck_draw(&game->deck);
    game->state = GAME_PLACE;
}

void game_init2(Game* game, CardStack* deck)
{
    game->score = 0;
    game->action_count = 0;
    game->message[0] = '\0';

    board_init(&game->board);
    game->state = GAME_PLACE;
}

bool is_placement_valid(BoardCell* cell, Card* card)
{
    return card->rank < JACK
        || (cell->type == KING_REQUIRED && card->rank == KING)
        || (cell->type == QUEEN_REQUIRED && card->rank == QUEEN)
        || (cell->type == JACK_REQUIRED && card->rank == JACK);
}

int count_marked_cell_rank(Board* board)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            if (board->cells[i][j].token == TOKEN_MARKER) {
                count += board->cells[i][j].card->rank;
            }
        }
    }
    return count;
}

/**
 * @brief Determine if empty cells remain for the given rank.
 *
 * @param board
 * @param rank
 * @return int
 */
int count_available_face_cells(Board* board, CardRank rank)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            CellType type = board->cells[i][j].type;
            if (board->cells[i][j].card == NULL
                && cell_type_to_rank(type) == rank) {
                count++;
            }
        }
    }
    return count;
}

int count_available_cells(Board* board)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            if (board->cells[i][j].card == NULL) {
                count++;
            }
        }
    }
    return count;
}

int count_placed_face_cards(Board* board)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {

            if (board->cells[i][j].card != NULL
                && board->cells[i][j].card->rank >= JACK
                && board->cells[i][j].type != WILD) {
                count++;
            }
        }
    }
    return count;
}

void discard_selected(Board* board)
{
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            if (board->cells[i][j].token == TOKEN_MARKER) {
                board->cells[i][j].token = TOKEN_NONE;
                board->cells[i][j].card = NULL;
            }
        }
    }
}

void game_update(Game* game, GameAction action)
{
    BoardCell* selected = action.cell;
    log_info("GameAction: %d", action.type);
    if (selected != NULL) {
        log_info("Selected cell: %d, %d", selected->type, selected->token);
    }
    switch (game->state) {
    case GAME_PLACE:
        if (action.type == ACTION_CONTINUE) {
            log_info("Action: Continue, ignoring.");
            return;
        }

        if (!is_placement_valid(selected, game->up_card)) {
            return;
        }

        // place the card
        selected->card = game->up_card;
        game->up_card = deck_draw(&game->deck);
        if (game->up_card == NULL) {
            game->state = GAME_LOSE;
        }

        if (count_placed_face_cards(&game->board) == 10) {
            game->state = GAME_WIN;
            return;
        }

        if (game->up_card->rank >= JACK
            && count_available_face_cells(&game->board, game->up_card->rank) == 0) {
            game->state = GAME_LOSE;
        }

        if (count_available_cells(&game->board) == 0) {
            game->state = GAME_COMBINE;
        }

        break;
    case GAME_COMBINE:

        // FIXME: detect unwinnable state
        if (action.type == ACTION_CONTINUE) {
            game->state = GAME_PLACE;
            return;
        }

        // Remove marker
        if (selected->token == TOKEN_MARKER) {
            selected->token = TOKEN_NONE;

            return;
        }

        if (selected->card != NULL && selected->card->rank < JACK) {
            selected->token = TOKEN_MARKER;

            if (count_marked_cell_rank(&game->board) == 10) {
                discard_selected(&game->board);
                game->score += 10;
            }
            return;
        }

        break;
    case GAME_LOSE:
        break;
    case GAME_WIN:
        break;
    }
}
