#include "matrix.h"

void mat_mul_3x3_3x1(float s1[3][3], float* s2, float* dest)
{
    for (int i = 0; i < 3; i++) {
        dest[i] = 0.0;
        for (int j = 0; j < 3; j++) {
            dest[i] += s1[i][j] * s2[j];
        }
    }
}