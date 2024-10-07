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
    while (fgets(line, sizeof(line), file) != NULL) {
        GameScore score;
        sscanf(line,
            "%10s %d %zu %d %3s\n",
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
        return false;
    }

    for (size_t i = 0; i < scores->count; ++i) {
        GameScore* score = &scores->scores[i];
        fprintf(
            file,
            "%10s %d %zu %d %3s\n",
            score->date,
            score->score,
            score->moves,
            (int)score->final_state,
            score->name);
    }

    fclose(file);
    return true;
}
