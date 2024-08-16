#ifndef GAME_H
#define GAME_H

#include "board.h"
#include <stdlib.h>

#define MAX_GAME_ACTIONS 1024
#define MAX_MESSAGE_LENGTH 1024

typedef enum {
    GAME_PLACE,
    GAME_COMBINE,
    GAME_WIN,
    GAME_LOSE
} GameState;

typedef enum {
    ACTION_UP,
    ACTION_DOWN,
    ACTION_LEFT,
    ACTION_RIGHT,
    ACTION_SELECT,
    ACTION_NONE
} GameAction;

typedef struct {
    int row;
    int col;
} GameCursor;

typedef struct {
    Board board;
    CardDeck deck;
    Card* up_card;
    int score;
    GameState state;
    GameCursor cursor;
    GameAction actions[MAX_GAME_ACTIONS];
    size_t action_count;
    char message[MAX_MESSAGE_LENGTH];
} Game;

void game_init(Game* game);

void game_update(Game* game, GameAction action);

#endif
