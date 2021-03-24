#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include "input.h"
#include <locale.h>
#include <ncurses.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define DELAY 200000

int main(int argc, char* argv[])
{
    if (argc == 2) {
        char* end;
        const long seed = strtol(argv[1], &end, 10);
        srand(seed);
    } else {
        time_t t;
        srand((unsigned)time(&t));
    }
    draw_init();

    game_init(g_max_x / 2, g_max_y);

    bool should_exit = false;
    while (!should_exit) {
        draw();
        refresh();

        usleep(DELAY);

        input_update();
        should_exit = game_update(g_max_x, g_max_y);
    }

    draw_cleanup();
}
