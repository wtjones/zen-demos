#ifndef REPR_H
#define REPR_H

#include "deck.h"
#include "game.h"
#include <stdlib.h>

#define GAME_LEDGER_BUFFER_SIZE 5000

char* repr_card(char* buffer, size_t count, Card card);
const char* repr_card_simple(Card card);
char* repr_move_ledger(char* buffer, size_t count, Game* game);
char* repr_game_ledger(char* buffer, size_t count, Game* game);
char* repr_game_move(char* buffer, size_t count, GameMove* move);

#endif
