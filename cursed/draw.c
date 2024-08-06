#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "brains/alpha_brain.h"
#include "brains/random_brain.h"
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
    raw();
    keypad(stdscr, 1);
    noecho();
    clear();
    cbreak();
    curs_set(FALSE);
    mouseinterval(0);
    nodelay(stdscr, 1);

    mousemask(ALL_MOUSE_EVENTS, NULL);
    getmaxyx(stdscr, g_max_y, g_max_x);

    start_color();
    init_pair(WALL_COLOR, COLOR_GREEN, COLOR_BLACK);
    init_pair(SNAKE_COLOR, COLOR_WHITE, COLOR_BLACK);
    init_pair(PELLET_COLOR, COLOR_RED, COLOR_BLACK);
    init_pair(RANDOM_BRAIN_COLOR, COLOR_YELLOW, COLOR_BLACK);
    init_pair(ALPHA_BRAIN_COLOR, COLOR_MAGENTA, COLOR_BLACK);
    init_pair(TEXT_COLOR, COLOR_CYAN, COLOR_BLACK);

    log_info("Init of ncurses with x = %d y = %d", g_max_x, g_max_y);
}

void draw_cleanup()
{
    delwin(mainwin);
    endwin();
}

void draw_wall()
{
    attron(COLOR_PAIR(WALL_COLOR));
    for (size_t i = 0; i < num_walls; i++) {
        struct Wall* wall = &walls[i];
        mvprintw(wall->world_entity.y, wall->world_entity.x * 2, WALL);
    }
    attroff(COLOR_PAIR(WALL_COLOR));
}

void draw_snakes()
{
    attron(COLOR_PAIR(SNAKE_COLOR));
    for (size_t i = 0; i < MAX_SNAKES; i++) {
        struct Snake* snake = &snakes[i];
        if (entity_exists(snake->head)) {
            for (size_t j = 0; j < snake->num_nodes; j++) {
                if (snake->brain == run_alpha_brain) {
                    attron(COLOR_PAIR(ALPHA_BRAIN_COLOR));
                } else if (snake->brain == run_random_brain) {
                    attron(COLOR_PAIR(RANDOM_BRAIN_COLOR));
                }
                mvprintw(
                    snake->nodes[j].y,
                    snake->nodes[j].x * 2,
                    j == 0 ? SNAKE_HEAD : SNAKE_BODY);
            }
            attron(COLOR_PAIR(TEXT_COLOR));
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
    attron(COLOR_PAIR(PELLET_COLOR));
    for (size_t i = 0; i < num_pellets; i++) {
        struct Pellet* pellet = &pellets[i];
        if (pellet->world_entity.owner != NULL) {
            mvprintw(pellet->world_entity.y, pellet->world_entity.x * 2, PELLET);
        }
    }
    attroff(COLOR_PAIR(PELLET_COLOR));
}

void draw()
{
    erase();
    draw_wall();
    draw_snakes();
    draw_pellets();

    getmaxyx(stdscr, g_max_y, g_max_x);
}
