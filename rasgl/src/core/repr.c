#include "rasgl/core/repr.h"

char* repr_point3f(char* buffer, size_t count, Point3f* p)
{
    snprintf(
        buffer,
        count,
        "[x: %f, y: %f, z: %f]",
        fixed_16_16_to_float(p->x),
        fixed_16_16_to_float(p->y),
        fixed_16_16_to_float(p->z));
    return buffer;
}

char* repr_fixed_16_16(char* buffer, size_t count, int32_t f)
{
    snprintf(
        buffer,
        count,
        "[%f (%d)]",
        fixed_16_16_to_float(f),
        f);
    return buffer;
}
char* repr_mat_4x4(char* buffer, size_t count, int32_t s1[4][4])
{
    char matrix_buffer[255];
    buffer[0] = '\0';
    strcat(buffer, "[");

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {

            if (j > 0) {
                strcat(buffer, ", ");
            } else {
                strcat(buffer, "\n[");
            }
            snprintf(
                matrix_buffer,
                sizeof matrix_buffer,
                "%12.5f",
                fixed_16_16_to_float(s1[i][j]));

            strcat(buffer, matrix_buffer);

            if (j == 3) {
                strcat(buffer, "]");
            }
        }
    }
    strcat(buffer, "]");
    return buffer;
}

char* repr_mat_4x1(char* buffer, size_t count, int32_t s[4])
{
    char matrix_buffer[255];
    buffer[0] = '\0';
    strcat(buffer, "[");

    for (int i = 0; i < 4; i++) {
        strcat(buffer, "\n[");

        snprintf(
            matrix_buffer,
            sizeof matrix_buffer,
            "%12.5f",
            fixed_16_16_to_float(s[i]));

        strcat(buffer, matrix_buffer);

        if (i == 3) {
            strcat(buffer, "]");
        }
    }

    strcat(buffer, "]");
    return buffer;
}
