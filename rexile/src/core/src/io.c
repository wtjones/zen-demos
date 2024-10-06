#include "rexile/core/io.h"
#include "log.c/src/log.h"
#include "rexile/core/game.h"
#include "rexile/core/repr.h"
#include <pwd.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

bool card_from_string(const char* in_rank, const char* in_suit, Card* card)
{
    if (in_rank == NULL || in_suit == NULL) {
        return false;
    }
    if (strcmp(in_rank, "A") == 0) {
        card->rank = ACE;
    } else if (strcmp(in_rank, "J") == 0) {
        card->rank = JACK;
    } else if (strcmp(in_rank, "Q") == 0) {
        card->rank = QUEEN;
    } else if (strcmp(in_rank, "K") == 0) {
        card->rank = KING;
    } else {
        card->rank = atoi(in_rank);
    }

    if (strcmp(in_suit, "♠") == 0) {
        card->suit = SPADES;
    } else if (strcmp(in_suit, "♣") == 0) {
        card->suit = CLUBS;
    } else if (strcmp(in_suit, "♦") == 0) {
        card->suit = DIAMONDS;
    } else if (strcmp(in_suit, "♥") == 0) {
        card->suit = HEARTS;
    } else {
        return false;
    }

    return card;
}

bool card_stack_load(const char* path, CardStack* stack)
{
    FILE* file = fopen(path, "r");
    if (file == NULL) {
        log_error("Failed to open file: %s", path);
        return false;
    }

    card_stack_clear(stack);

    Card card;

    // file input example:
    // A ♠
    // 10 ♣
    // J ♦
    // Q ♥
    char in_rank[3];
    char in_suit[3];

    while (fscanf(file, "%s %s", in_rank, in_suit) == 2) {
        log_info("Read card: %s %s", in_rank, in_suit);
        bool map_result = card_from_string(in_rank, in_suit, &card);
        if (!map_result) {
            log_error("Failed to map card from string: %s %s", in_rank, in_suit);
            fclose(file);
            return false;
        }
        card_stack_push(stack, card);
    }
    card_stack_reverse(stack);
    fclose(file);

    return true;
}

const char* io_get_temp_folder()
{
    const char* temp_folder = getenv("TMPDIR");
    if (temp_folder == NULL) {
        temp_folder = getenv("TMP");
    }
    if (temp_folder == NULL) {
        temp_folder = getenv("TEMP");
    }
    if (temp_folder == NULL) {
        temp_folder = getenv("TEMPDIR");
    }
    if (temp_folder == NULL) {
        temp_folder = "/tmp";
    }
    return temp_folder;
}

void io_get_temp_file(char* result, size_t count, const char* file_name)
{
    const char* temp_folder = io_get_temp_folder();
    snprintf(result, count, "%s/%s", temp_folder, file_name);
}

const char* io_get_home_directory()
{
    const char* home = getenv("HOME");
    if (!home) {
        home = getpwuid(getuid())->pw_dir;
    }
    return home;
}

const char* io_get_user_name()
{
    const char* user = getenv("USER");
    if (!user) {
        user = getpwuid(getuid())->pw_name;
    }
    return user;
}

void io_get_game_ledger_file(char* result, size_t count)
{
    const char* temp_folder = io_get_temp_folder();
    snprintf(result, count, "%s/%s", temp_folder, "rexile_last_game.txt");
}

void io_save_game_ledger(Game* game)
{
    char file_path[255];
    io_get_game_ledger_file(file_path, 255);
    FILE* file = fopen(file_path, "w");
    if (file == NULL) {
        log_error("Failed to open file: %s", file_path);
        return;
    }

    char ledger_buffer[GAME_LEDGER_BUFFER_SIZE];
    repr_game_ledger(ledger_buffer, GAME_LEDGER_BUFFER_SIZE, game);
    fprintf(file, "%s\n", ledger_buffer);

    fclose(file);
}
