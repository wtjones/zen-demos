#ifndef SCORE_H
#define SCORE_H

#include "game.h"
#include <stdlib.h>

#define SCORES_MAX 100
#define SCORE_FILE_NAME ".rexile_scores"
#define SCORE_NAME_MAX 4

typedef struct {
    char name[SCORE_NAME_MAX];
    int score;
    size_t moves;
    GameState final_state;
} GameScore;

typedef struct {
    GameScore scores[SCORES_MAX];
    size_t count;
} ScoreBoard;

void scores_init(ScoreBoard* scores);
void scores_add(ScoreBoard* scores, GameScore* score);
bool scores_load(const char* path, ScoreBoard* scores);
bool scores_save(const char* path, ScoreBoard* scores);
void get_score_file_path(char* result, size_t count);

#endif
