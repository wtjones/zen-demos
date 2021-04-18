#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include "input.h"
#include "util.h"
#include <locale.h>
#include <ncurses.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define DELAY 200000

void sigsegv_handler(int _)
{
    log_info("sigsegv handled");
    draw_cleanup();
    exit(0);
}

void sigint_handler(int _)
{
    printf("sigint");
    log_info("sigint handled");
    draw_cleanup();
    exit(0);
}

int main(int argc, char* argv[])
{
    sigset(SIGSEGV, sigsegv_handler);
    sigset(SIGINT, sigint_handler);

    FILE* log_file = fopen(LOG_PATH, "w");
    log_add_fp(log_file, 0);
    log_set_quiet(true);

    if (argc == 2) {
        char* end;
        const long seed = strtol(argv[1], &end, 10);
        log_info("Using %d as random seed.", seed);
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
        should_exit = game_update(g_max_x / 2, g_max_y);
    }
    game_cleanup();
    draw_cleanup();
    printf("Log file saved to %s\n", LOG_PATH);
}
