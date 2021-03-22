#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>

int g_max_y = 0, g_max_x = 0;

void draw_init()
{
    setlocale(LC_ALL, "");
    initscr();
    noecho();
    curs_set(FALSE);
    getmaxyx(stdscr, g_max_y, g_max_x);
}

void draw_wall()
{
    for (int i = 0; i < g_max_y; i++) {
        mvprintw(i, 0, WALL);
        mvprintw(i, g_max_x - 1, WALL);
    }

    for (int i = 0; i < g_max_x; i += 2) {
        mvprintw(0, i, WALL);
        mvprintw(0, i + 1, " ");
        mvprintw(g_max_y - 1, i, WALL);
        mvprintw(g_max_y - 1, i + 1, " ");
    }
}

void draw_snakes()
{
    for (size_t i = 0; i < num_snakes; i++) {
        struct Snake* snake = &snakes[i];
        mvprintw(snake->nodes[0].y, snake->nodes[0].x * 2, SNAKE_HEAD);
        mvprintw(0, 0, "Window: %d %d Snake 0: %d %d",
            g_max_x,
            g_max_y,
            snake->nodes[0].x,
            snake->nodes[0].y);
    }
}

void draw()
{
    erase();
    draw_wall();
    draw_snakes();

    getmaxyx(stdscr, g_max_y, g_max_x);
}
