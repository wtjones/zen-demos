#include "log.c/src/log.h"
#include "rexile/core/game.h"
#include "rexile/core/io.h"
#include "rexile/core/repr.h"
#include "ux_args.h"
#include "ux_draw.h"
#include "ux_input.h"
#include <assert.h>
#include <locale.h>
#include <ncurses.h>

#define DELAY 16666 * 2 // 30 fps

void ux_init(UXLayout* layout)
{
    log_info("Initializing UX");
    setlocale(LC_ALL, "");
    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);
    layout->cursor.row = 0;
    layout->cursor.col = 0;
}

void ux_cleanup()
{
    endwin();
}

bool ux_new_game(Game* game, UXOptions* options)
{
    CardStack deck;
    card_stack_clear(&deck);

    if (options->initial_deck_file) {
        log_info("Loading deck from file: %s", options->initial_deck_file);

        if (!card_stack_load(options->initial_deck_file, &deck)) {
            log_error("Failed to load deck from file: %s", options->initial_deck_file);
            return false;
        }
    } else {

        card_stack_fill(&deck);
        card_stack_shuffle(&deck);
    }
    game_init(game, &deck);
    return true;
}

bool is_cell_face_card(BoardCell* cell)
{
    Card card = card_stack_peek(&cell->card_stack);
    return cell->card_stack.count > 0 && is_face_card(&card);
}

GameResult ux_input_to_action(int* keys, UXLayout* layout, Game* game)
{
    UXCursor* cursor = &layout->cursor;

    if (game->state == GAME_WIN || game->state == GAME_LOSE) {
        return GAME_RESULT_NONE;
    }

    if (keys[INPUT_KEY_ENTER] || keys[INPUT_KEY_SPACE]) {

        bool is_cursor_cell_empty = game->board.cells[cursor->row][cursor->col].card_stack.count == 0;
        bool intent_to_place = game->state == GAME_PLACE
            || (game->state == GAME_COMBINE_OR_PLACE && is_cursor_cell_empty);

        if (intent_to_place) {
            ux_clear_markers(layout);
            return game_action_place(
                game, (BoardCellPosition) { cursor->row, cursor->col });
        } else {
            BoardCell* cell = &game->board.cells[cursor->row][cursor->col];
            UXCellLayout* cell_layout = &layout->cells[cursor->row][cursor->col];

            cell_layout->has_marker = !cell_layout->has_marker && !is_cell_face_card(cell);

            BoardCellPosition marked[BOARD_COLS * BOARD_ROWS];
            size_t marked_count = 0;
            for (int i = 0; i < BOARD_ROWS; i++) {
                for (int j = 0; j < BOARD_COLS; j++) {
                    if (layout->cells[i][j].has_marker) {
                        log_info("Marked cell: %d, %d", i, j);
                        marked[marked_count++] = (BoardCellPosition) { i, j };
                    }
                }
            }

            GameResult result = game_action_combine(game, marked, marked_count);

            if (result == GAME_RESULT_OK) {
                ux_clear_markers(layout);
            }
            return result;
        }
    }
    return GAME_RESULT_NONE;
}

int main(int argc, char** argv)

{
    UXLayout layout;

    FILE* log_file = fopen("/tmp/rexile.log", "w");
    log_add_fp(log_file, LOG_INFO);
    log_set_quiet(0);
    log_set_level(LOG_ERROR);

    log_info("Starting Rexile");

    UXOptions options;
    if (!ux_parse_options(argc, argv, &options)) {
        log_error("Failed to parse options");
        return 1;
    }

    Game game;

    if (!ux_new_game(&game, &options)) {
        log_error("Failed to start new game");
        return 1;
    }

    ux_init(&layout);
    ux_draw_init(&layout);

    int input_keys[INPUT_KEY_MAX];
    bool should_exit = false;
    while (!should_exit) {
        ux_draw_start(&layout, &game);

        usleep(DELAY);

        ux_input_update(input_keys);

        if (input_keys[INPUT_KEY_Q]) {
            should_exit = true;
            io_save_game_ledger(&game);
            break;
        }
        if (input_keys[INPUT_KEY_L]) {
            char buffer[GAME_LEDGER_BUFFER_SIZE];
            repr_move_ledger(buffer, sizeof(buffer), &game);
            log_info("Move ledger:\n%s", buffer);
            io_save_game_ledger(&game);
        }
        if (input_keys[INPUT_KEY_N]) {
            ux_cleanup();
            ux_init(&layout);
            ux_new_game(&game, &options);
            ux_draw_init(&layout);
            continue;
        }

        ux_cursor_update(&layout.cursor, input_keys);

        GameResult result = ux_input_to_action(input_keys, &layout, &game);
        log_info("Game result: %d", result);
        if (result == GAME_RESULT_OK) {
            // TODO: handle win/lose
        }
    }

    ux_cleanup();
    fclose(log_file);
    return 0;
}
