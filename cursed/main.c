#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include "input.h"
#include <locale.h>
#include <ncurses.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define DELAY 200000

static volatile sig_atomic_t signal_exit = false;

void sigsegv_handler(int _)
{
    draw_cleanup();
}

void sigint_handler(int _)
{
    printf("sigint");
    draw_cleanup();
    signal_exit = true;
}

int main(int argc, char* argv[])
{
    sigset(SIGSEGV, sigsegv_handler);
    sigset(SIGINT, sigint_handler);

    if (argc == 2) {
        char* end;
        const long seed = strtol(argv[1], &end, 10);
        srand(seed);
        srand(1);
    } else {
        time_t t;
        srand((unsigned)time(&t));
    }
    draw_init();

    game_init(g_max_x / 2, g_max_y);
    bool should_exit = false;
    while (!should_exit && !signal_exit) {
        draw();
        refresh();

        usleep(DELAY);

        input_update();
        should_exit = game_update(g_max_x / 2, g_max_y);
    }
    game_cleanup();
    draw_cleanup();
}
