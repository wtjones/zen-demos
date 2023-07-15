#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/matrix.h"
#include "rasgl/core/repr.h"
#include <stdint.h>
#include <stdio.h>

void repr_matrix_tests()
{
    char buffer[1000];

    const int32_t i = INT_32_TO_FIXED_16_16(1);
    int32_t scale_matrix[4][4] = {
        { i, 0, 0, 0 },
        { 0, i, 0, 0 },
        { 0, 0, i, 0 },
        { 0, 0, 0, i }
    };

    printf("Matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, scale_matrix));
}

void repr_fixed_tests()
{
    char buffer[100];
    Point3f p = {
        .x = float_to_fixed_16_16(1.0),
        .y = float_to_fixed_16_16(1.125),
        .z = float_to_fixed_16_16(50.777)
    };

    printf("Point: %s\n", repr_point3f(buffer, 100, &p));
}

int main(int argc, const char** argv)
{
    printf("rasgl tests...\n");
    repr_fixed_tests();
    repr_matrix_tests();
    return 0;
}
