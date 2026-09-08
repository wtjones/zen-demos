
#include "rasgl/core/debug.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/repr.h"
#include "test_support.h"
#include <math.h>

TEST(TEST_FOV_SCALE)
{
    float d2r = M_PI / 180.0f;
    for (float fov = 30.0f; fov <= 120.0f; fov += 1.0f) {
        float y_scale = 1.0f / tanf(d2r * fov / 2);
        ras_log_info("FOV, y scale %f, %f %d", fov, y_scale, float_to_fixed_16_16(y_scale));
    }
    return false;
}

static inline void log_fx(float f, RasFixed fx16, RasFixed_20_12 fx20)
{
    ras_log_info("Float: %f\nFixed16: %d, %f\nFixed20: %d, %f",
        f,
        fx16,
        fixed_16_16_to_float(fx16),
        fx20,
        fixed_20_12_to_float(fx20));
}

static inline bool fixed_20_validate(float f)
{
    RasFixed_20_12 fx20 = float_to_fixed_20_12(f);
    RasFixed fx16 = float_to_fixed_16_16(f);

    log_fx(f, fx16, fx20);

    RasFixed new16 = RAS_FIXED_20_12_TO_16_16(fx20);
    RasFixed_20_12 new20 = RAS_FIXED_16_16_TO_20_12(fx16);

    log_fx(f, new16, new20);
    return (fx16 == new16) && (fx20 == new20);
}

TEST(TEST_FIXED_20)
{
    assert(fixed_20_validate(1.0f));

    // Won't actually match due to rounding.
    fixed_20_validate(3.1459f);
    return false;
}

TEST(TEST_MAT_ORTHO)
{
    char buffer[1000];
    RasFixed matrix[4][4];
    RasFixed projected_point[4];
    Point2i screen_point;
    mat_ortho_init(
        matrix,
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1),
        -INT_32_TO_FIXED_16_16(1),
        INT_32_TO_FIXED_16_16(1));

    ras_log_info("Ortho matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, matrix));

    RasFixed v[4] = {
        -float_to_fixed_16_16(0.5),
        float_to_fixed_16_16(0.5),
        -float_to_fixed_16_16(0.5),
        float_to_fixed_16_16(1.0)
    };
    mat_mul_project(matrix, v, projected_point);
    ras_log_info("Ortho projected: %s\n", repr_mat_4x1(buffer, sizeof buffer, projected_point));

    projected_to_screen_point(100, 100, projected_point, &screen_point);
    ras_log_info("Ortho screen: %s\n", repr_point2i(buffer, sizeof buffer, &screen_point));
    return false;
}

TEST(TEST_MAT_PROJECTION)
{
    const RasFixed screen_width = 320;
    const RasFixed screen_height = 240;
    char buffer[500];
    RasFixed projection_matrix[4][4];
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

    RasFixed world_vec[4] = {
        transformed.x,
        transformed.y,
        transformed.z,
        INT_32_TO_FIXED_16_16(1)
    };

    RasFixed view_point[4];

    mat_mul_project(projection_matrix, world_vec, view_point);

    ras_log_info("after perspective divide: %s\n", repr_mat_4x1(buffer, sizeof buffer, view_point));

    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    Point2i screen = {
        .x = FIXED_16_16_TO_INT_32(mul_fixed_16_16_by_fixed_16_16(half_screen_width, view_point[0]) + half_screen_width),
        .y = FIXED_16_16_TO_INT_32(mul_fixed_16_16_by_fixed_16_16(half_screen_height, view_point[1]) + half_screen_height)
    };
    ras_log_info("screen after matrix proj: %s\n", repr_point2i(buffer, sizeof buffer, &screen));
    return false;
}

TEST(TEST_NORMALIZE)
{
    char buffer[100];
    Point3f v = {
        .x = float_to_fixed_16_16(3.4),
        .y = float_to_fixed_16_16(2.0),
        .z = -float_to_fixed_16_16(14.0)
    };

    RasFixed length = core_get_vec_length(&v);

    ras_log_info("before normalize: %s\n", repr_point3f(buffer, sizeof buffer, &v));
    ras_log_info("before normalize length: %s\n", repr_fixed_16_16(buffer, sizeof buffer, length));

    core_normalize(&v);
    length = core_get_vec_length(&v);

    ras_log_info("after normalize: %s\n", repr_point3f(buffer, sizeof buffer, &v));
    ras_log_info("after normalize length: %s\n", repr_fixed_16_16(buffer, sizeof buffer, length));
    return false;
}

TEST(TEST_CROSS_PRODUCT)
{
    char buffer[100];
    Point3f v1 = {
        .x = float_to_fixed_16_16(1.0),
        .y = float_to_fixed_16_16(2.0),
        .z = -float_to_fixed_16_16(3.0)
    };
    Point3f v2 = {
        .x = float_to_fixed_16_16(3.0),
        .y = float_to_fixed_16_16(4.0),
        .z = -float_to_fixed_16_16(5.0)
    };
    Point3f result;

    core_cross_product(&v1, &v2, &result);
    ras_log_info("cross product result: %s\n", repr_point3f(buffer, sizeof buffer, &result));
    return false;
}

TEST(TEST_DOT_PRODUCT)
{
    char buffer[100];
    Point3f v1 = {
        .x = float_to_fixed_16_16(1.0),
        .y = float_to_fixed_16_16(2.0),
        .z = -float_to_fixed_16_16(3.0)
    };
    Point3f v2 = {
        .x = float_to_fixed_16_16(3.0),
        .y = float_to_fixed_16_16(4.0),
        .z = -float_to_fixed_16_16(5.0)
    };

    RasFixed result = core_dot_product(&v1, &v2);
    ras_log_info("dot product result: %s\n", repr_fixed_16_16(buffer, sizeof buffer, result));
    return false;
}
