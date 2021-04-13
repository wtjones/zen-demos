#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>

WINDOW* mainwin;
int g_max_y = 0, g_max_x = 0;

void draw_init()
{
    setlocale(LC_ALL, "");
    mainwin = initscr();
    noecho();
    curs_set(FALSE);
    getmaxyx(stdscr, g_max_y, g_max_x);
    log_info("Init of ncurses with x = %d y = %d", g_max_x, g_max_y);
}

void draw_cleanup()
{
    delwin(mainwin);
    endwin();
}

void draw_wall()
{
    for (size_t i = 0; i < num_walls; i++) {
        struct Wall* wall = &walls[i];
        mvprintw(wall->world_entity.y, wall->world_entity.x * 2, WALL);
    }
}

void draw_snakes()
{
    for (size_t i = 0; i < MAX_SNAKES; i++) {
        struct Snake* snake = &snakes[i];
        if (entity_exists(snake->head)) {
            for (size_t j = 0; j < snake->num_nodes; j++) {
                mvprintw(
                    snake->nodes[j].y,
                    snake->nodes[j].x * 2,
                    j == 0 ? SNAKE_HEAD : SNAKE_BODY);
            }
            mvprintw(0, 0, "Window: %d %d Snake 0: %d %d",
                g_max_x,
                g_max_y,
                snake->nodes[0].x,
                snake->nodes[0].y);
        }
    }
}

void draw_pellets()
{
    for (size_t i = 0; i < num_pellets; i++) {
        struct Pellet* pellet = &pellets[i];
        if (pellet->world_entity.owner != NULL) {
            mvprintw(pellet->world_entity.y, pellet->world_entity.x * 2, PELLET);
        }
    }
}

void draw()
{
    erase();
    draw_wall();
    draw_snakes();
    draw_pellets();

    getmaxyx(stdscr, g_max_y, g_max_x);
}
