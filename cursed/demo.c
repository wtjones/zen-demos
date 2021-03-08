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
    y = max_y / 2;

    while (1) {
        getmaxyx(stdscr, max_y, max_x);
        erase();

        for (int i = 0; i < max_y; i++) {
            mvprintw(i, 0, WALL);
            mvprintw(i, max_x - 1, WALL);
        }

        for (int i = 0; i < max_x; i += 2) {
            mvprintw(0, i, WALL);
            mvprintw(0, i + 1, " ");
            mvprintw(max_y - 1, i, WALL);
            mvprintw(max_y - 1, i + 1, " ");
        }

        mvprintw(y, x, CHECK);
        refresh();

        usleep(DELAY);

        next_x = x + direction;

        if (next_x >= max_x || next_x < 0) {
            direction *= -1;
        } else {
            x += direction;
        }

        nodelay(stdscr, 1);
        keypad(stdscr, 1);

        int ch = getch();
        if (ch != ERR) {
            switch (ch) {
            case KEY_UP:
                y -= 1;
                break;
            case KEY_DOWN:
                y += 1;
                break;
            }
        }
    }

    endwin();
}
