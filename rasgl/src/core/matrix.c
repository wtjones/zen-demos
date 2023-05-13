#include "rasgl/core/matrix.h"

void mat_mul_3x3_3x1(int32_t s1[3][3], int32_t* s2, int32_t* dest)
{
    for (int i = 0; i < 3; i++) {
        dest[i] = 0.0;
        for (int j = 0; j < 3; j++) {
            dest[i] += mul_fixed_16_16_by_fixed_16_16(s1[i][j], s2[j]);
        }
    }
}
