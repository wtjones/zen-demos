#include "ux_draw.h"
#include <locale.h>
#include <ncurses.h>

void ux_init()
{
    setlocale(LC_ALL, "");
    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);
}

void ux_cleanup()
{
    endwin();
}

int main()
{
    ux_init();

    box(stdscr, 0, 0);

    Game game;
    game_init(&game);

    ux_draw_start(&game);

    refresh();

    getch();

    ux_cleanup();

    return 0;
}
