#include "rexile/core/game.h"
#include "log.c/src/log.h"
#include "rexile/core/repr.h"
#include <assert.h>
#include <stdbool.h>

void game_init(Game* game, CardStack* deck)
{
    log_info("Initializing game");
    game->score = 0;
    game->move_count = 0;
    game->message[0] = '\0';
    card_stack_clear(&game->draw_deck);
    card_stack_populate(&game->draw_deck, deck->cards, deck->count);
    game->draw_deck = *deck;
    card_stack_clear(&game->discard_deck);
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
    bool is_rank_allowed = cell->allowed_ranks & (1 << (card->rank - 1));
    return cell->card_stack.count == 0
        && is_rank_allowed;
}

bool is_combine_valid(
    Game* game,
    BoardCellPosition source_cells[MAX_COMBINE_CELLS],
    size_t source_count)
{

    char buffer[255];
    if (source_count > MAX_COMBINE_CELLS) {
        return false;
    }

    int total = 0;
    for (size_t i = 0; i < source_count; ++i) {

        BoardCellPosition pos = source_cells[i];
        if (!is_position_valid(pos)) {
            return false;
        }
        BoardCell* cell = &game->board.cells[pos.row][pos.col];
        Card card = card_stack_peek(&cell->card_stack);
        if (is_face_card(&card)) {
            log_warn("Invalid combine card: %s", repr_card(buffer, sizeof(buffer), card));
            return false;
        }

        total += card_value(&card);
    }

    if (total != 10) {
        log_warn("Invalid combine total: %d", total);
        return false;
    }
    return true;
}

int count_available_cells_by_rank(Board* board, CardRank rank)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            BoardCell* cell = &board->cells[i][j];
            int16_t rank_mask = 1 << (rank - 1);
            bool is_empty = cell->card_stack.count == 0;
            count += is_empty && cell->allowed_ranks & rank_mask ? 1 : 0;
        }
    }
    return count;
}

int count_available_cells(Board* board)
{
    size_t count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {
            CardStack* stack = &board->cells[i][j].card_stack;
            count += stack->count == 0 ? 1 : 0;
        }
    }
    return count;
}

/**
 * @brief Count placed royals in goal cells
 *
 * @param board
 * @return int
 */
int count_placed_face_cards(Board* board)
{
    int count = 0;
    for (int i = 0; i < BOARD_ROWS; ++i) {
        for (int j = 0; j < BOARD_COLS; ++j) {

            BoardCell* cell = &board->cells[i][j];
            if (cell->card_stack.count == 0) {
                continue;
            }

            Card card = card_stack_peek(&cell->card_stack);
            if (!is_face_card(&card)) {
                continue;
            }

            int16_t rank_mask = 1 << (card.rank - 1);
            count += cell->required_ranks & rank_mask ? 1 : 0;
        }
    }
    return count;
}

GameResult game_action_place(
    Game* game, BoardCellPosition dest_cell_pos)
{
    char buffer[255];

    int prior_score = game->score;

    log_info("Placing card at %d, %d", dest_cell_pos.row, dest_cell_pos.col);
    if (!is_position_valid(dest_cell_pos)) {
        return GAME_RESULT_INVALID;
    }

    if (game->draw_deck.count == 0) {
        log_error("Draw deck is empty");
        return GAME_RESULT_ERROR;
    }

    Card card = card_stack_peek(&game->draw_deck);
    log_info("Peek draw card: %s", repr_card(buffer, sizeof(buffer), card));
    BoardCell* dest_cell = &game->board.cells[dest_cell_pos.row][dest_cell_pos.col];

    if (!is_placement_valid(dest_cell, &card)) {
        log_warn("Invalid placement");
        return GAME_RESULT_INVALID;
    }

    Card draw_card = card_stack_pop(&game->draw_deck);
    log_info("Drawing card: %s", repr_card(buffer, sizeof(buffer), draw_card));
    card_stack_push(&dest_cell->card_stack, draw_card);
    game->score += 10;

    GameMove move = {
        .type = MOVE_PLACE,
        .actions = { { .card = card, .pos = dest_cell_pos } },
        .action_count = 1,
        .prior_state = game->state
    };

    // Win?
    if (count_placed_face_cards(&game->board) == 10) {
        game->state = move.new_state = GAME_WIN;
        move.score_delta = game->score - prior_score;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    // Out of cards?
    if (game->draw_deck.count == 0) {
        game->state = move.new_state = GAME_LOSE;
        move.score_delta = game->score - prior_score;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    if (count_available_cells(&game->board) == 0) {
        if (!board_has_pair(&game->board)) {
            log_info("No combinations possible.");
            game->state = move.new_state = GAME_LOSE;
            move.score_delta = game->score - prior_score;
            game_move_push(game, &move);
            return GAME_RESULT_OK;
        }
        log_info("Last cell placed, switching to combine state");
        game->state = move.new_state = GAME_COMBINE;
        move.score_delta = game->score - prior_score;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    // Can't place face card?
    Card next_card = card_stack_peek(&game->draw_deck);
    int available_cells = count_available_cells_by_rank(&game->board, next_card.rank);

    if (is_face_card(&next_card)
        && available_cells == 0) {
        log_info("Next card is a royal that cannot be placed.");
        game->state = move.new_state = GAME_LOSE;
        move.score_delta = game->score - prior_score;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }

    game->state = move.new_state = GAME_PLACE;
    move.score_delta = game->score - prior_score;
    game_move_push(game, &move);
    return GAME_RESULT_OK;
}

GameResult game_action_combine(
    Game* game,
    BoardCellPosition source_cells[],
    size_t source_count)
{
    char buffer[255];
    int prior_score = game->score;

    log_info("Combining %d cards", source_count);
    if (!is_combine_valid(game, source_cells, source_count)) {
        log_warn("Invalid combine");
        return GAME_RESULT_INVALID;
    }

    GameMove move = {
        .type = MOVE_COMBINE,
        .action_count = source_count,
        .prior_state = game->state
    };

    for (size_t i = 0; i < source_count; i++) {
        BoardCell* source_cell = &game->board.cells[source_cells[i].row][source_cells[i].col];
        Card card = card_stack_pop(&source_cell->card_stack);
        log_info("Discarding card: %s", repr_card(buffer, sizeof(buffer), card));
        card_stack_push(&game->discard_deck, card);
        game->score += card_value(&card);
        move.actions[i].pos = source_cells[i];
        move.actions[i].card = card;
    }

    if (!board_has_pair(&game->board)) {
        log_info("No more combinations possible.");

        // Can we switch to place?
        Card next_card = card_stack_peek(&game->draw_deck);
        int available_cells = count_available_cells_by_rank(&game->board, next_card.rank);

        if (is_face_card(&next_card)
            && available_cells == 0) {
            log_info("Next card is a royal that cannot be placed.");
            game->state = move.new_state = GAME_LOSE;
            move.score_delta = game->score - prior_score;
            game_move_push(game, &move);
            return GAME_RESULT_OK;
        }
        move.score_delta = game->score - prior_score;
        game->state = move.new_state = GAME_PLACE;
        game_move_push(game, &move);
        return GAME_RESULT_OK;
    }
    move.score_delta = game->score - prior_score;
    game->state = move.new_state = GAME_COMBINE_OR_PLACE;
    game_move_push(game, &move);

    return GAME_RESULT_OK;
}

void game_move_push(Game* game, GameMove* move)
{
    char buffer[255];

    game->moves[game->move_count++] = *move;
    assert(game->move_count < MAX_GAME_MOVES);
    assert(move->action_count > 0);
}
