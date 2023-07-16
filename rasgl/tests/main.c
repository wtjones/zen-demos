#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/matrix.h"
#include "rasgl/core/repr.h"
#include <stdint.h>
#include <stdio.h>

void repr_matrix_tests()
{
    char buffer[1000];
    int32_t matrix[4][4];
    mat_set_identity_4x4(matrix);
    printf("Matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, matrix));
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

void mat_mul_tests()
{
    char buffer[100];
    int32_t matrix[4][4];
    mat_set_identity_4x4(matrix);

    int32_t src[4] = {
        float_to_fixed_16_16(3.5),
        float_to_fixed_16_16(1.2),
        float_to_fixed_16_16(5.375),
        float_to_fixed_16_16(1.0)
    };

    int32_t dst[4];
    mat_mul_4x4_4x1(matrix, src, dst);
    printf("Resut of mul by identity: %s\n", repr_mat_4x1(buffer, sizeof buffer, dst));
}

int main(int argc, const char** argv)
{
    printf("rasgl tests...\n");
    repr_fixed_tests();
    repr_matrix_tests();
    mat_mul_tests();
    return 0;
}
