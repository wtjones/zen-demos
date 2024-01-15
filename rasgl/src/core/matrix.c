#include "rasgl/core/matrix.h"

void mat_set_identity_4x4(RasFixed s1[4][4])
{
    const RasFixed i = INT_32_TO_FIXED_16_16(1);
    RasFixed identity_matrix[4][4] = {
        { i, 0, 0, 0 },
        { 0, i, 0, 0 },
        { 0, 0, i, 0 },
        { 0, 0, 0, i }
    };

    memcpy(s1, identity_matrix, sizeof identity_matrix);
}

void mat_mul_3x3_3x1(RasFixed s1[3][3], RasFixed* s2, RasFixed* dest)
{
    for (int i = 0; i < 3; i++) {
        dest[i] = 0;
        for (int j = 0; j < 3; j++) {
            dest[i] += mul_fixed_16_16_by_fixed_16_16(s1[i][j], s2[j]);
        }
    }
}

void mat_mul_4x4_4x1(RasFixed s1[4][4], RasFixed* s2, RasFixed* dest)
{
    for (int i = 0; i < 4; i++) {
        dest[i] = 0;
        for (int j = 0; j < 4; j++) {
            dest[i] += mul_fixed_16_16_by_fixed_16_16(s1[i][j], s2[j]);
        }
    }
}

void mat_mul_4x4_4x4(RasFixed s1[4][4], RasFixed s2[4][4], RasFixed dest[4][4])
{
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            dest[i][j] = 0;
            for (int k = 0; k < 4; k++) {
                dest[i][j] += mul_fixed_16_16_by_fixed_16_16(s1[i][k], s2[k][j]);
            }
        }
    }
}
