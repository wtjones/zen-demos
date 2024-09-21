#include "ux_draw.h"
#include "log.c/src/log.h"
#include "rexile/core/board.h"
#include <string.h>

static const char* g_ux_cell_type_desc[] = {
    "      ",
    "[Jack]",
    "[Queen]",
    "[King]"
};

static const char* g_ux_game_state_desc[] = {
    "Place cards",
    "Combine to discard",
    "Combine or place",
    "Game Over: Win",
    "Game Over: Lose"
};

const char* repr_required_ranks(BoardCell* cell)
{

    switch (cell->required_ranks) {
    case KING_REQUIRED_RANK:
        return g_ux_cell_type_desc[3];
    case QUEEN_REQUIRED_RANK:
        return g_ux_cell_type_desc[2];
    case JACK_REQUIRED_RANK:
        return g_ux_cell_type_desc[1];
    default:
        return g_ux_cell_type_desc[0];
    }
}

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
            layout->cells[i][j].has_marker = false;
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
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            BoardCell* cell = &game->board.cells[i][j];
            UXCellLayout* cell_layout = &layout->cells[i][j];

            bool is_selected = layout->cursor.row == i && layout->cursor.col == j;
            bool is_empty = cell->card_stack.count == 0;

            werase(cell_layout->cell_window);
            if (!is_empty) {
                Card cell_card = card_stack_peek(&cell->card_stack);
                draw_card(cell_layout, &cell_card);
            }
            if (is_selected) {
                wattron(cell_layout->cell_window, A_REVERSE);
            }

            mvwprintw(
                cell_layout->cell_window, 1, 1, "%s", repr_required_ranks(cell));

            if (is_selected) {
                wattroff(cell_layout->cell_window, A_REVERSE);
            }

            if (cell_layout->has_marker) {
                mvwprintw(cell_layout->cell_window, 0, UX_CELL_WINDOW_WIDTH - 4, UX_CHAR_HEAVY_BALLOT_X);
            }

            wrefresh(cell_layout->cell_window);
        }
    }
    wrefresh(layout->board_window);
}

void draw_legend(UXLayout* layout)
{
    int y = 1;
    werase(layout->legend_window);
    mvwprintw(layout->legend_window, y++, 1, "Legend");
    mvwprintw(layout->legend_window, y++, 1, "♠: Spades");
    mvwprintw(layout->legend_window, y++, 1, "♦: Diamonds");
    mvwprintw(layout->legend_window, y++, 1, "♣: Clubs");
    mvwprintw(layout->legend_window, y++, 1, "♥: Hearts");
    mvwprintw(layout->legend_window, y++, 1, "A: Ace");
    mvwprintw(layout->legend_window, y++, 1, "J: Jack");
    mvwprintw(layout->legend_window, y++, 1, "Q: Queen");
    mvwprintw(layout->legend_window, y++, 1, "K: King");
    mvwprintw(layout->legend_window, y++, 1, "%s: Marker", UX_CHAR_HEAVY_BALLOT_X);
    y++;
    mvwprintw(layout->legend_window, y++, 1, "Controls:");
    mvwprintw(layout->legend_window, y++, 1, "Arrow keys: cursor");
    mvwprintw(layout->legend_window, y++, 1, "Space: place/combine");
    mvwprintw(layout->legend_window, y++, 1, "n: new game");
    mvwprintw(layout->legend_window, y++, 1, "q: quit");
    wrefresh(layout->legend_window);
}

void draw_game_state(UXLayout* layout, Game* game)
{
    char card_repr[255];
    Card up_card = card_stack_peek(&game->draw_deck);
    card_to_string(&up_card, card_repr);
    werase(layout->game_state_window);
    wprintw(layout->game_state_window, "Card: %s\t\t\t\tScore: %d\nPhase: %s",
        card_repr,
        game->score,
        g_ux_game_state_desc[game->state]);
    wrefresh(layout->game_state_window);
}

void ux_clear_markers(UXLayout* layout)
{
    for (int i = 0; i < BOARD_ROWS; i++) {
        for (int j = 0; j < BOARD_COLS; j++) {
            layout->cells[i][j].has_marker = false;
        }
    }
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

    int legend_window_offset_x = UX_PARENT_WINDOW_WIDTH - UX_LEGEND_WINDOW_WIDTH - 6;
    log_info("Legend window offset: %d", legend_window_offset_x);
    int legend_window_offset_y = 0;
    layout->legend_window = derwin(
        layout->main_window,
        UX_LEGEND_WINDOW_WIDTH,
        UX_LEGEND_WINDOW_HEIGHT,
        legend_window_offset_y,
        legend_window_offset_x);

    wrefresh(layout->legend_window);

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
    draw_legend(layout);
    draw_game_state(layout, game);
}
