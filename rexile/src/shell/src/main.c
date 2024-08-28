#include "log.c/src/log.h"
#include "rexile/core/game.h"
#include "ux_draw.h"
#include "ux_input.h"
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
    game_init(&game);

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

        GameAction action = ux_input_to_action(input_keys, &layout.cursor, &game.board);
        if (action.type != ACTION_NONE) {
            game_update(&game, action);
        }
    }

    ux_cleanup();
    fclose(log_file);
    return 0;
}
