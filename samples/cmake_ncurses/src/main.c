#include <ncurses.h>

void plat_init()
{
    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);
}

void plat_cleanup()
{
    endwin();
}

int main()
{
    plat_init();

    box(stdscr, 0, 0);

    mvprintw(1, 1, "Hello, World!");

    refresh();

    getch();

    plat_cleanup();

    return 0;
}