#include "ux_draw.h"
#include "log.c/src/log.h"
#include <string.h>

static const char* g_ux_cell_type_desc[] = {
    "      ",
    "[Jack]",
    "[Queen]",
    "[King]"
};

static const char* g_ux_game_state_desc[] = {
    "Place cards",
    "Combine to discard. Press p to continue",
    "Game Over: Win",
    "Game Over: Lose"
};

void card_to_string(Card* card, char* card_str)
{
    card_str[0] = '\0';

    switch (card->rank) {
    case ACE:
        strcat(card_str, "A");
        break;
    case JACK:
        strcat(card_str, "J");

        break;
    case QUEEN:
        strcat(card_str, "Q");

        break;
    case KING:
        strcat(card_str, "K");

        break;
    default:
        char temp[3];
        sprintf(temp, "%d", card->rank);
        strcat(card_str, temp);
        break;
    }

    switch (card->suit) {
    case HEARTS:
        strcat(card_str, "♠");
        break;
    case DIAMONDS:
        strcat(card_str, "♦");
        break;
    case CLUBS:
        strcat(card_str, "♣");
        break;
    case SPADES:
        strcat(card_str, "♠");
        break;
    };
}

void draw_board_init(UXLayout* layout)
{
    int cell_offset_x = 0, cell_offset_y = 0;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            layout->cells[i][j].cell_window_border = derwin(
                layout->board_window,
                UX_CELL_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING + 1,
                UX_CELL_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING + 1,
                cell_offset_y,
                cell_offset_x);

            box(layout->cells[i][j].cell_window_border, 0, 0);

            layout->cells[i][j].cell_window = derwin(
                layout->cells[i][j].cell_window_border,
                UX_CELL_WINDOW_HEIGHT - 2,
                UX_CELL_WINDOW_WIDTH - 2,
                UX_WINDOW_BORDER_PADDING * 1,
                UX_WINDOW_BORDER_PADDING * 1);

            wrefresh(layout->cells[i][j].cell_window_border);
            wrefresh(layout->cells[i][j].cell_window);

            cell_offset_x += UX_CELL_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING;
        }
        cell_offset_x = 0;
        cell_offset_y += UX_CELL_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING;
    }
}

void draw_card(UXCellLayout* cell_layout, Card* card)
{
    char card_repr[255];
    card_to_string(card, card_repr);
    mvwprintw(cell_layout->cell_window, 0, 0, "%s", card_repr);
    wrefresh(cell_layout->cell_window);
}

void draw_board(UXLayout* layout, Game* game)
{
    char buffer[1024];

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            BoardCell* cell = &game->board.cells[i][j];
            UXCellLayout* cell_layout = &layout->cells[i][j];

            bool is_selected = layout->cursor.row == i && layout->cursor.col == j;
            bool is_empty = cell->card == NULL;

            werase(cell_layout->cell_window);
            if (!is_empty) {
                draw_card(cell_layout, cell->card);
            }
            if (is_selected) {
                wattron(cell_layout->cell_window, A_REVERSE);
            }

            sprintf(buffer, "%d", cell->type);

            mvwprintw(
                cell_layout->cell_window, 1, 1, "%s", g_ux_cell_type_desc[cell->type]);

            if (is_selected) {
                wattroff(cell_layout->cell_window, A_REVERSE);
            }

            if (cell->token == TOKEN_MARKER) {
                mvwprintw(cell_layout->cell_window, 0, UX_CELL_WINDOW_WIDTH - 4, "⦿");
            }

            wrefresh(cell_layout->cell_window);
        }
    }
    wrefresh(layout->board_window);
}

void draw_game_state(UXLayout* layout, Game* game)
{
    char card_repr[255];
    card_to_string(game->up_card, card_repr);
    werase(layout->game_state_window);
    wprintw(layout->game_state_window, "Card: %s\t\t\t\tScore: %d\nPhase: %s",
        card_repr,
        game->score,
        g_ux_game_state_desc[game->state]);
    wrefresh(layout->game_state_window);
}

void ux_draw_init(UXLayout* layout)
{
    log_info("Initializing UX Draw");
    layout->main_window_border = newwin(
        UX_PARENT_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_PARENT_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        0, 0);

    box(layout->main_window_border, 0, 0);
    refresh();
    wrefresh(layout->main_window_border);

    layout->main_window = derwin(
        layout->main_window_border,
        UX_PARENT_WINDOW_HEIGHT,
        UX_PARENT_WINDOW_WIDTH,
        UX_WINDOW_BORDER_PADDING,
        UX_WINDOW_BORDER_PADDING);

    wrefresh(layout->main_window);

    int main_window_contents_offset_y = 0;
    int main_window_contents_offset_x = 0;

    layout->board_window = derwin(
        layout->main_window,
        UX_BOARD_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING,
        UX_BOARD_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        main_window_contents_offset_y,
        main_window_contents_offset_x);

    main_window_contents_offset_y += UX_BOARD_WINDOW_HEIGHT;

    wrefresh(layout->board_window);

    layout->game_state_window_border = derwin(
        layout->main_window,
        UX_GAME_STATE_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING * 2,
        UX_GAME_STATE_WINDOW_WIDTH + UX_WINDOW_BORDER_PADDING,
        main_window_contents_offset_y,
        main_window_contents_offset_x);

    main_window_contents_offset_y += UX_GAME_STATE_WINDOW_HEIGHT + UX_WINDOW_BORDER_PADDING;

    box(layout->game_state_window_border, 0, 0);

    layout->game_state_window = derwin(
        layout->game_state_window_border,
        UX_GAME_STATE_WINDOW_HEIGHT,
        UX_GAME_STATE_WINDOW_WIDTH - 4,
        UX_WINDOW_BORDER_PADDING,
        UX_WINDOW_BORDER_PADDING);

    scrollok(layout->game_state_window, true);
    wrefresh(layout->board_window_border);

    wrefresh(layout->board_window);

    draw_board_init(layout);

    box(layout->game_state_window_border, 0, 0);

    wrefresh(layout->game_state_window_border);

    wrefresh(layout->game_state_window);
}

void ux_draw_start(UXLayout* layout, Game* game)
{
    draw_board(layout, game);
    draw_game_state(layout, game);
}