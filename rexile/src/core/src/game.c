#include "rexile/core/game.h"
#include "log.c/src/log.h"
#include <assert.h>
#include <stdbool.h>

void game_init(Game* game)
{
    log_info("Initializing game");
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
    log_info("Initializing game");
    game->score = 0;
    game->move_count = 0;
    game->message[0] = '\0';
    game->draw_deck = *deck;
    board_init(&game->board);
    game->state = GAME_PLACE;
}

bool is_position_valid(BoardCellPosition pos)
{
    return pos.row >= 0 && pos.row < BOARD_ROWS
        && pos.col >= 0 && pos.col < BOARD_COLS;
}

bool is_placement_valid(BoardCell* cell, Card* card)
{
    return card->rank < JACK
        || (cell->type == KING_REQUIRED && card->rank == KING)
        || (cell->type == QUEEN_REQUIRED && card->rank == QUEEN)
        || (cell->type == JACK_REQUIRED && card->rank == JACK);
}

bool is_placement_valid2(BoardCell* cell, Card* card)
{
    bool is_rank_allowed = cell->allowed_ranks & (1 << (card->rank - 1));
    return cell->card_stack.count == 0
        && is_rank_allowed;
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

GameResult game_action_place(
    Game* game, BoardCellPosition dest_cell_pos)
{
    log_info("Placing card at %d, %d", dest_cell_pos.row, dest_cell_pos.col);
    if (!is_position_valid(dest_cell_pos)) {
        return GAME_RESULT_INVALID;
    }

    if (game->draw_deck.count == 0) {
        log_error("Draw deck is empty");
        return GAME_RESULT_ERROR;
    }

    Card card = card_stack_peek(&game->draw_deck);
    log_info("Draw card: %d, %d", card.suit, card.rank);
    BoardCell* dest_cell = &game->board.cells[dest_cell_pos.row][dest_cell_pos.col];

    if (!is_placement_valid2(dest_cell, &card)) {
        return GAME_RESULT_INVALID;
    }

    GameMove move = {
        .type = MOVE_PLACE,
        .actions = { { .card = card, .pos = dest_cell_pos } },
        .action_count = 1,
        .score_delta = 10
    };

    // Will the placement result in a win?
    if (count_placed_face_cards(&game->board) == 9
        && is_face_card(&card)) {
        move.state = GAME_WIN;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    if (game->draw_deck.count == 1) {
        move.state = GAME_LOSE;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    if (count_available_cells(&game->board) == 1) {
        move.state = GAME_COMBINE;
    }
    game_move_push(game, &move);
    return GAME_RESULT_OK;
}

GameResult game_action_combine(
    Game* game,
    BoardCellPosition source_cells[MAX_COMBINE_CELLS],
    size_t source_count)
{

    if (source_count > MAX_COMBINE_CELLS) {
        return GAME_RESULT_INVALID;
    }
    return GAME_RESULT_OK;
}

void game_move_push(Game* game, GameMove* move)
{
    game->moves[game->move_count++] = *move;
    assert(game->move_count < MAX_GAME_MOVES);
    assert(move->action_count == 1);
    if (move->type == MOVE_PLACE) {
        Card draw_card = card_stack_pop(&game->draw_deck);
        BoardCell* dest_cell = &game->board.cells[move->actions[0].pos.row][move->actions[0].pos.col];
        card_stack_push(&dest_cell->card_stack, draw_card);
    }
    game->score += move->score_delta;
}
