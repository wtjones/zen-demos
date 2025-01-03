#ifndef GAME_H
#define GAME_H

#include "board.h"
#include <stdint.h>
#include <stdlib.h>

#define MAX_GAME_ACTIONS 1024
#define MAX_GAME_MOVES 64
#define MAX_MOVE_ACTIONS 2
#define MAX_MESSAGE_LENGTH 1024
#define MIN_COMBINE_CELLS 1
#define MAX_COMBINE_CELLS 2
#define MOVE_SCORE_PLACE_FACE 100
#define MOVE_SCORE_PLACE_PIP 0
#define MOVE_SCORE_COMBINE_PIP 10

typedef enum {
    GAME_PLACE,
    GAME_COMBINE,
    GAME_COMBINE_OR_PLACE,
    GAME_WIN,
    GAME_LOSE
} GameState;

typedef enum {
    ACTION_UP,
    ACTION_DOWN,
    ACTION_LEFT,
    ACTION_RIGHT,
    ACTION_SELECT,
    ACTION_CONTINUE,
    ACTION_NONE
} GameActionType;

typedef enum {
    GAME_RESULT_NONE,
    GAME_RESULT_OK,
    GAME_RESULT_INVALID,
    GAME_RESULT_ERROR
} GameResult;

typedef enum {
    MOVE_PLACE,
    MOVE_COMBINE
} GameMoveType;

typedef struct {
    GameActionType type;
    BoardCell* cell;
} GameAction;

typedef struct {
    BoardCellPosition pos;
    Card card;
} GameMoveAction;

typedef struct {
    GameMoveType type;
    GameMoveAction actions[MAX_MOVE_ACTIONS];
    size_t action_count;
    int32_t score;
    GameState new_state;
    GameState prior_state;
} GameMove;

typedef struct {
    uint32_t game_id;
    Board board;
    CardStack draw_deck;
    CardStack discard_deck;
    int score;
    GameState state;
    GameAction actions[MAX_GAME_ACTIONS];
    GameMove moves[MAX_GAME_MOVES];
    size_t move_count;
    size_t action_count;
    char message[MAX_MESSAGE_LENGTH];
} Game;

void game_init(Game* game, CardStack* deck, uint32_t game_id);

/**
 * @brief Draw and place on the board
 *
 * @param game
 * @param dest_cell_row
 * @param dest_cell_col
 * @return GameResult
 */
GameResult game_action_place(
    Game* game, BoardCellPosition dest_cell);

GameResult game_action_combine(
    Game* game,
    BoardCellPosition source_cells[],
    size_t source_count);

// deprecated
void game_update(Game* game, GameAction action);

/**
 * @brief Apply game move to the game state and store it
 *
 * @param game
 * @param move
 */
void game_move_push(Game* game, GameMove* move);

#endif
