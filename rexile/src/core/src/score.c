#include "rexile/core/score.h"
#include "log.c/src/log.h"
#include "rexile/core/io.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

void get_score_file_path(char* result, size_t count)
{
    const char* home_folder = io_get_home_directory();
    snprintf(result, count, "%s/%s", home_folder, SCORE_FILE_NAME);
}

void get_score_default_name(char* result, size_t count)
{
    assert(count >= SCORE_NAME_SIZE);
    const char* user = io_get_user_name();
    strncpy(result, user, SCORE_NAME_SIZE - 1);
    result[SCORE_NAME_SIZE - 1] = '\0';
}

void get_score_default_date(char* result, size_t count)
{
    time_t now = time(NULL);
    struct tm* t = localtime(&now);
    strftime(result, count, "%Y-%m-%d", t);
}

void scores_init(ScoreBoard* scores)
{
    scores->last_game_id = 0;
    scores->count = 0;
}

void scores_sort(ScoreBoard* scores)
{
    for (size_t i = 0; i < scores->count; i++) {
        for (size_t j = i + 1; j < scores->count; j++) {
            if (scores->scores[i].score < scores->scores[j].score) {
                GameScore temp = scores->scores[i];
                scores->scores[i] = scores->scores[j];
                scores->scores[j] = temp;
            }
        }
    }
}

void scores_add(ScoreBoard* scores, GameScore* score)
{
    assert(score->game_id > 0);
    size_t i = scores->count == SCORES_MAX ? SCORES_MAX - 1 : scores->count;
    scores->scores[i] = *score;
    scores->count = i + 1;
    scores_sort(scores);
}

bool scores_load(const char* path, ScoreBoard* scores)
{

    FILE* file = fopen(path, "r");
    scores_init(scores);

    if (!file) {
        log_info("No scores file found at %s. Initializing new.", path);
        scores_init(scores);
        return true;
    }

    char line[50];

    if (fgets(line, sizeof(line), file) == NULL) {
        log_warn("No scores found in file %s", path);
        scores->last_game_id = 0;
    } else {
        sscanf(line, "Game Count: %u", &scores->last_game_id);
    }

    while (fgets(line, sizeof(line), file) != NULL) {
        GameScore score;
        sscanf(line,
            "%zu %10s %d %zu %d %3s\n",
            &score.game_id,
            score.date,
            &score.score,
            &score.moves,
            (int*)&score.final_state,
            score.name);
        log_info("Read score: %s %d %zu %d %s",
            score.date, score.score, score.moves, (int)score.final_state, score.name);
        scores_add(scores, &score);
    }

    fclose(file);
    return true;
}

bool scores_save(const char* path, ScoreBoard* scores)
{
    FILE* file = fopen(path, "w");
    if (!file) {
        log_error("Failed to open file for writing: %s", path);
        return false;
    }

    fprintf(file, "Game Count: %u\n", scores->last_game_id);

    log_info("Saving scores to %s", path);

    for (size_t i = 0; i < scores->count; ++i) {
        GameScore* score = &scores->scores[i];
        fprintf(
            file,
            "%zu %10s %d %zu %d %3s\n",
            score->game_id,
            score->date,
            score->score,
            score->moves,
            (int)score->final_state,
            score->name);
    }

    fclose(file);
    return true;
}

GameScore get_score_from_game(Game* game, const char* name, size_t name_count)
{
    GameScore score;
    get_score_default_date(score.date, sizeof(score.date));
    score.score = game->score;
    score.game_id = game->game_id;
    score.moves = game->move_count;
    score.final_state = game->state;
    strncpy(score.name, name, SCORE_NAME_SIZE - 1);
    score.name[SCORE_NAME_SIZE - 1] = '\0';
    return score;
}
