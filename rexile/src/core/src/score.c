#include "rexile/core/score.h"
#include "log.c/src/log.h"
#include "rexile/core/io.h"
#include <assert.h>
#include <stdio.h>

void get_score_file_path(char* result, size_t count)
{
    const char* home_folder = io_get_home_directory();
    snprintf(result, count, "%s/%s", home_folder, SCORE_FILE_NAME);
}

void get_score_default_name(char* result, size_t count)
{
    assert(count >= SCORE_NAME_LENGTH_MAX + 1);
    const char* user = io_get_user_name();
    strncpy(result, user, SCORE_NAME_LENGTH_MAX);
    result[SCORE_NAME_LENGTH_MAX] = '\0';
}

void scores_init(ScoreBoard* scores)
{
    scores->count = 0;
}

void scores_add(ScoreBoard* scores, GameScore* score)
{
    scores->scores[scores->count++] = *score;
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
    while (fgets(line, sizeof(line), file) != NULL) {
        GameScore score;
        sscanf(line,
            "%d %zu %d %3s\n",
            &score.score,
            &score.moves,
            (int*)&score.final_state,
            score.name);
        log_info("Read score: %d %zu %d %s", score.score, score.moves, (int)score.final_state, score.name);
        scores_add(scores, &score);
    }

    fclose(file);
    return true;
}

bool scores_save(const char* path, ScoreBoard* scores)
{
    FILE* file = fopen(path, "w");
    if (!file) {
        return false;
    }

    for (size_t i = 0; i < scores->count; ++i) {
        GameScore* score = &scores->scores[i];
        fprintf(
            file,
            "%d %zu %d %3s\n",
            score->score,
            score->moves,
            (int)score->final_state,
            score->name);
    }

    fclose(file);
    return true;
}
