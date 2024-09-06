#include "log.c/src/log.h"
#include "rexile/core/game.h"
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

void ux_new_game(Game* game)
{
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_fill(&deck);
    card_stack_shuffle(&deck);
    game_init2(game, &deck);
}

GameResult ux_input_to_action2(int* keys, UXLayout* layout, Game* game)
{
    UXCursor* cursor = &layout->cursor;

    if (keys[INPUT_KEY_ENTER] || keys[INPUT_KEY_SPACE]) {
        switch (game->state) {
        case GAME_PLACE:
            return game_action_place(
                game, (BoardCellPosition) { cursor->row, cursor->col });
            break;
        case GAME_COMBINE:
            // BoardCell* selected = &game->board.cells[cursor->row][cursor->col];
            UXCellLayout* cell_layout = &layout->cells[cursor->row][cursor->col];
            cell_layout->has_marker = !cell_layout->has_marker;

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

            return game_action_combine(game, marked, marked_count);

            break;
        default:
            assert(false);
            break;
        }
    }
    return GAME_RESULT_NONE;
}

int main()
{
    UXLayout layout;

    FILE* log_file = fopen("/tmp/rexile.log", "w");
    log_add_fp(log_file, LOG_INFO);
    log_set_quiet(1);
    log_set_level(LOG_INFO);

    log_info("Starting Rexile");

    ux_init(&layout);
    ux_draw_init(&layout);

    int input_keys[INPUT_KEY_MAX];
    Game game;
    ux_new_game(&game);

    bool should_exit = false;
    while (!should_exit) {

        ux_draw_start(&layout, &game);

        usleep(DELAY);

        ux_input_update(input_keys);

        if (input_keys[INPUT_KEY_Q]) {
            should_exit = true;
            break;
        }
        ux_cursor_update(&layout.cursor, input_keys);

        GameResult result = ux_input_to_action2(input_keys, &layout, &game);
        log_info("Game result: %d", result);
        if (result == GAME_RESULT_OK) {
            // TODO: handle win/lose
        }
    }

    ux_cleanup();
    fclose(log_file);
    return 0;
}
