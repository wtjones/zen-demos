#define _X_OPEN_SOURCE_EXTENDED
#include <locale.h>
#include <ncurses.h>
#include <unistd.h>

#define DELAY 30000
#define CHECK "\xe2\x9c\x93"
#define WALL "\u25A3"

int main(int argc, char* argv[])
{
    int x = 0, y = 0;
    int max_y = 0, max_x = 0;
    int next_x = 0;
    int direction = 1;

    setlocale(LC_ALL, "");
    initscr();
    noecho();
    curs_set(FALSE);

    getmaxyx(stdscr, max_y, max_x);

    while (1) {
        getmaxyx(stdscr, max_y, max_x);
        erase();
        mvaddstr(1, 0, CHECK);
        mvaddstr(2, 0, WALL);
        mvprintw(y, x, WALL);
        refresh();

        usleep(DELAY);

        next_x = x + direction;

        if (next_x >= max_x || next_x < 0) {
            direction *= -1;
        } else {
            x += direction;
        }
    }

    endwin();
}