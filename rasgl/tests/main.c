#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/matrix.h"
#include "rasgl/core/repr.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

void repr_matrix_tests()
{
    char buffer[1000];
    int32_t matrix[4][4];
    mat_set_identity_4x4(matrix);
    ras_log_info("Matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, matrix));
}

void repr_fixed_tests()
{
    char buffer[100];
    Point3f p = {
        .x = float_to_fixed_16_16(1.0),
        .y = float_to_fixed_16_16(1.125),
        .z = float_to_fixed_16_16(50.777)
    };

    ras_log_info("Point: %s\n", repr_point3f(buffer, 100, &p));
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
    ras_log_info("Resut of mul by identity: %s\n", repr_mat_4x1(buffer, sizeof buffer, dst));
}

void fixed_mul_test(int32_t f1, int32_t f2, int32_t expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    int32_t result = mul_fixed_16_16_by_fixed_16_16(f1, f2);
    log_trace(
        "%s * %s = %s\n",
        repr_fixed_16_16(buffer1, sizeof buffer1, f1),
        repr_fixed_16_16(buffer2, sizeof buffer3, f2),
        repr_fixed_16_16(buffer3, sizeof buffer3, result));

    assert(result == expected);
}

void fixed_mul_tests()
{
    ras_log_info("fixed_mul_tests:\n");

    fixed_mul_test(
        float_to_fixed_16_16(0.125),
        float_to_fixed_16_16(2.5),
        20480);
}

void fixed_div_test(int32_t f1, int32_t f2, int32_t expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    int32_t result = div_fixed_16_16_by_fixed_16_16(f1, f2);
    log_trace(
        "%s / %s = %s\n",
        repr_fixed_16_16(buffer1, sizeof buffer1, f1),
        repr_fixed_16_16(buffer2, sizeof buffer3, f2),
        repr_fixed_16_16(buffer3, sizeof buffer3, result));

    assert(result == expected);
}
void fixed_div_tests()
{
    ras_log_info("fixed_div_tests:\n");
    fixed_div_test(
        float_to_fixed_16_16(34.5),
        float_to_fixed_16_16(0.125),
        18087936); // 276.0
    fixed_div_test(
        float_to_fixed_16_16(10.4),
        float_to_fixed_16_16(2.5),
        272629); // 4.16
}

void project_point_tests()
{
    Point3f transformed = {
        .x = float_to_fixed_16_16(30.0),
        .y = float_to_fixed_16_16(20.0),
        .z = -float_to_fixed_16_16(310.0)
    };

    Point2i projected = project_point(
        320,
        240,
        -float_to_fixed_16_16(2.0),
        transformed);
    char buffer[100];
    ras_log_info("projected: %s\n", repr_point2i(buffer, sizeof buffer, &projected));
}

int main(int argc, const char** argv)
{
    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL);
    log_set_level(RAS_LOG_LEVEL);
    log_set_quiet(false);

    ras_log_info("rasgl tests...\n");
    ras_log_trace("%s\n", "DEBUG = 1");

    repr_fixed_tests();
    repr_matrix_tests();
    mat_mul_tests();
    fixed_mul_tests();
    fixed_div_tests();
    project_point_tests();
    return 0;
}
