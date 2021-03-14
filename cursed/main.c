#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include "input.h"
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>

#define DELAY 30000

int main(int argc, char* argv[])
{
    draw_init();

    game_init(g_max_x, g_max_y);

    while (1) {
        draw();
        refresh();

        usleep(DELAY);

        input_update();
        game_update(g_max_x, g_max_y);
    }

    endwin();
}
