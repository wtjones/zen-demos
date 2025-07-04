#include "rasgl/core/string.h"
#include <ctype.h>

/**
 * From https://stackoverflow.com/a/47229318/107161
 * The original code is public domain -- Will Hartung 4/9/09
 * Modifications, public domain as well, by Antti Haapala, 11/10/17
 *  - Switched to getc on 5/23/19
 */
ssize_t core_getline(char** lineptr, size_t* n, FILE* stream)
{
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL) {
        errno = EINVAL;
        return -1;
    }

    c = getc(stream);
    if (c == EOF) {
        return -1;
    }

    if (*lineptr == NULL) {
        *lineptr = malloc(128);
        if (*lineptr == NULL) {
            return -1;
        }
        *n = 128;
    }

    pos = 0;
    while (c != EOF) {
        if (pos + 1 >= *n) {
            size_t new_size = *n + (*n >> 2);
            if (new_size < 128) {
                new_size = 128;
            }
            char* new_ptr = realloc(*lineptr, new_size);
            if (new_ptr == NULL) {
                return -1;
            }
            *n = new_size;
            *lineptr = new_ptr;
        }

        ((unsigned char*)(*lineptr))[pos++] = c;
        if (c == '\n') {
            break;
        }
        c = getc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
}

bool is_whitespace(const char* str)
{
    if (str == NULL)
        return false;

    while (*str != '\0') {
        if (!isspace((unsigned char)*str)) {
            return false;
        }
        str++;
    }
    return true;
}
