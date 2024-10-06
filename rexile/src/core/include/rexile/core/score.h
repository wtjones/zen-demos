#ifndef SCORE_H
#define SCORE_H

#include "game.h"
#include <stdlib.h>

#define SCORES_MAX 100
#define SCORE_FILE_NAME ".rexile_scores"
#define SCORE_NAME_SIZE 4
#define SCORE_DATE_SIZE 11 // YYYY-MM-DD

typedef struct {
    char name[SCORE_NAME_SIZE];
    char date[SCORE_DATE_SIZE];
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
/**
 * @brief Attempts to load scores from the default path.
 * If unsuccessful, initializes an empty scoreboard.
 *
 * @param path
 * @param scores
 * @return true
 * @return false
 */
bool scores_load(const char* path, ScoreBoard* scores);
bool scores_save(const char* path, ScoreBoard* scores);
void get_score_file_path(char* result, size_t count);
/**
 * @brief Gets an appropriate default name based on the
 * environment.
 *
 * @param result
 * @param count SCORE_NAME_LENGTH_MAX + 1
 */
void get_score_default_name(char* result, size_t count);

void get_score_default_date(char* result, size_t count);

#endif
