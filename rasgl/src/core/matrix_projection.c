#include "rasgl/core/matrix_projection.h"

void mat_ortho_init(
    int32_t matrix[4][4],
    int32_t l,
    int32_t r,
    int32_t b,
    int32_t t,
    int32_t n,
    int32_t f)
{
    const int32_t two_f = INT_32_TO_FIXED_16_16(2);

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            matrix[i][j] = 0;
        }
    }

    matrix[0][0] = div_fixed_16_16_by_fixed_16_16(two_f, (r - l));
    matrix[0][1] = 0;
    matrix[0][2] = 0;
    matrix[0][3] = 0;

    matrix[1][0] = 0;
    matrix[1][1] = div_fixed_16_16_by_fixed_16_16(two_f, (t - b));
    matrix[1][2] = 0;
    matrix[1][3] = 0;

    matrix[2][0] = 0;
    matrix[2][1] = 0;
    matrix[2][2] = div_fixed_16_16_by_fixed_16_16(-two_f, (f - n));
    matrix[2][3] = 0;

    matrix[0][3] = div_fixed_16_16_by_fixed_16_16(-(r + l), (r - l));
    matrix[1][3] = div_fixed_16_16_by_fixed_16_16(-(t + b), (t - b));
    matrix[2][3] = div_fixed_16_16_by_fixed_16_16(-(f + n), (f - n));
    matrix[3][3] = INT_32_TO_FIXED_16_16(1);
}
