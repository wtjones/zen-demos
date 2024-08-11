#include "game.h"
#include "ux_draw.h"
#include "ux_input.h"
#include <locale.h>
#include <ncurses.h>

#define DELAY 16666 * 2 // 30 fps

void ux_init()
{
    setlocale(LC_ALL, "");
    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);
}

void ux_cleanup()
{
    endwin();
}

int main()
{
    UXLayout layout;

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
        GameAction action = ux_input_to_action(input_keys);
        game_update(&game, action);
    }

    ux_cleanup();

    return 0;
}
