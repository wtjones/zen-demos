#define _X_OPEN_SOURCE_EXTENDED
#include "draw.h"
#include "game.h"
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

        nodelay(stdscr, 1);
        keypad(stdscr, 1);

        bool key_up = false;
        bool key_down = false;
        int ch = getch();
        if (ch != ERR) {
            switch (ch) {
            case KEY_UP:
                key_up = true;
                break;
            case KEY_DOWN:
                key_down = true;
                break;
            }
        }
        game_update(g_max_x, g_max_y, key_up, key_down);
    }

    endwin();
}
