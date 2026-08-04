#pragma once

#include "ps1/gte.h"
#include "rasgl/core/fixed_maths.h"
#include <stdio.h>

static inline char* repr_gte_matrix(char* buffer, size_t count, const GTEMatrix* m)
{
    char value_buffer[32];

    buffer[0] = '\0';
    strncat(buffer, "[", count - 1);

    for (int i = 0; i < 3; i++) {
        strncat(buffer, "\n[", count - strlen(buffer) - 1);

        for (int j = 0; j < 3; j++) {
            if (j > 0) {
                strncat(buffer, ", ", count - strlen(buffer) - 1);
            }

            snprintf(
                value_buffer,
                sizeof(value_buffer),
                "%8.5f",
                fixed_20_12_to_float(m->values[i][j]));

            strncat(buffer, value_buffer, count - strlen(buffer) - 1);
        }
        strncat(buffer, "]", count - strlen(buffer) - 1);
    }
    strncat(buffer, "\n]", count - strlen(buffer) - 1);

    return buffer;
}
