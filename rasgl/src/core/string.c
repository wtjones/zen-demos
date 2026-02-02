#include "rasgl/core/string.h"
#include <ctype.h>
#include <stddef.h>

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
