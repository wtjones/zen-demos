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
    ras_log_info("Result of mul by identity: %s\n", repr_mat_4x1(buffer, sizeof buffer, dst));
}

void mat_mul_4_tests()
{
    char buffer[500];
    int32_t m1[4][4] = {
        { INT_32_TO_FIXED_16_16(5), INT_32_TO_FIXED_16_16(7), INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10) },
        { INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(8) },
        { INT_32_TO_FIXED_16_16(8), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(8) }
    };

    int32_t m2[4][4] = {
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(18) },
        { INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(1), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(9) },
        { INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(2) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(10) }
    };

    int32_t dest[4][4];

    mat_mul_4x4_4x4(m1, m2, dest);
    ras_log_info("Result of 4x4 x 4x4: %s\n", repr_mat_4x4(buffer, sizeof buffer, dest));
}

void mat_projection_tests()
{
    const int32_t screen_width = 320;
    const int32_t screen_height = 240;
    char buffer[500];
    int32_t projection_matrix[4][4];
    float fov = 45.0f;            // Field of view in degrees
    float aspect_ration = 1.333f; // Aspect ratio (width/height)
    float near = 0.1f;            // Near clipping plane
    float far = 100.0f;           // Far clipping plane

    mat_projection_init(projection_matrix, fov, aspect_ration, near, far);
    ras_log_info("Result of mat_projection_init: %s\n", repr_mat_4x4(buffer, sizeof buffer, projection_matrix));

    Point3f transformed = {
        .x = float_to_fixed_16_16(30.0),
        .y = float_to_fixed_16_16(20.0),
        .z = float_to_fixed_16_16(-310.0)
    };

    Point2f projected = project_point(
        screen_width,
        screen_height,
        -float_to_fixed_16_16(2.0),
        transformed);

    ras_log_info("projected: %s\n", repr_point2f(buffer, sizeof buffer, &projected));

    int32_t world_vec[4] = {
        transformed.x,
        transformed.y,
        transformed.z,
        INT_32_TO_FIXED_16_16(1)
    };

    int32_t view_point[4];

    mat_mul_project(projection_matrix, world_vec, view_point);

    ras_log_info("after perspective divide: %s\n", repr_mat_4x1(buffer, sizeof buffer, view_point));

    int32_t half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    int32_t half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    Point2i screen = {
        .x = FIXED_16_16_TO_INT_32(mul_fixed_16_16_by_fixed_16_16(half_screen_width, view_point[0]) + half_screen_width),
        .y = FIXED_16_16_TO_INT_32(mul_fixed_16_16_by_fixed_16_16(half_screen_height, view_point[1]) + half_screen_height)
    };
    ras_log_info("screen after matrix proj: %s\n", repr_point2i(buffer, sizeof buffer, &screen));
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

    Point2f projected = project_point(
        320,
        240,
        -float_to_fixed_16_16(2.0),
        transformed);
    char buffer[100];
    ras_log_info("projected: %s\n", repr_point2f(buffer, sizeof buffer, &projected));
}

void normalize_tests()
{
    char buffer[100];
    Point3f v = {
        .x = float_to_fixed_16_16(3.4),
        .y = float_to_fixed_16_16(2.0),
        .z = -float_to_fixed_16_16(14.0)
    };

    int32_t length = vector_length(&v);

    ras_log_info("before normalize: %s\n", repr_point3f(buffer, sizeof buffer, &v));
    ras_log_info("before normalize length: %s\n", repr_fixed_16_16(buffer, sizeof buffer, length));

    normalize(&v);
    length = vector_length(&v);

    ras_log_info("after normalize: %s\n", repr_point3f(buffer, sizeof buffer, &v));
    ras_log_info("after normalize length: %s\n", repr_fixed_16_16(buffer, sizeof buffer, length));
}

int main(int argc, const char** argv)
{
    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL);
    log_set_level(RAS_LOG_LEVEL);
    log_set_quiet(false);

    ras_log_info("rasgl tests...\n");
    ras_log_trace("%s\n", "DEBUG = 1");

    mat_projection_tests();
    repr_fixed_tests();
    repr_matrix_tests();
    mat_mul_tests();
    mat_mul_4_tests();
    fixed_mul_tests();
    fixed_div_tests();
    project_point_tests();
    normalize_tests();
    return 0;
}
