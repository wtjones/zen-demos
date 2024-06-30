#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/matrix.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/model.h"
#include "rasgl/core/plane.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void repr_matrix_tests()
{
    char buffer[1000];
    RasFixed matrix[4][4];
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
    RasFixed matrix[4][4];
    mat_set_identity_4x4(matrix);

    RasFixed src[4] = {
        float_to_fixed_16_16(3.5),
        float_to_fixed_16_16(1.2),
        float_to_fixed_16_16(5.375),
        float_to_fixed_16_16(1.0)
    };

    RasFixed dst[4];
    mat_mul_4x4_4x1(matrix, src, dst);
    ras_log_info("Result of mul by identity: %s\n", repr_mat_4x1(buffer, sizeof buffer, dst));
}

void mat_mul_4_tests()
{
    char buffer[500];
    RasFixed m1[4][4] = {
        { INT_32_TO_FIXED_16_16(5), INT_32_TO_FIXED_16_16(7), INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10) },
        { INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(8) },
        { INT_32_TO_FIXED_16_16(8), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(8) }
    };

    RasFixed m2[4][4] = {
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(18) },
        { INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(1), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(9) },
        { INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(2) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(10) }
    };

    RasFixed dest[4][4];

    mat_mul_4x4_4x4(m1, m2, dest);
    ras_log_info("Result of 4x4 x 4x4: %s\n", repr_mat_4x4(buffer, sizeof buffer, dest));
}

void mat_ortho_tests()
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
}

void mat_projection_tests()
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
}

void fixed_mul_test(RasFixed f1, RasFixed f2, RasFixed expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    RasFixed result = mul_fixed_16_16_by_fixed_16_16(f1, f2);
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

void fixed_div_test(RasFixed f1, RasFixed f2, RasFixed expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    RasFixed result = div_fixed_16_16_by_fixed_16_16(f1, f2);
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

void normalize_tests()
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
}

void cross_product_tests()
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
}

void dot_product_tests()
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
}

void plane_tests()
{
    // Test case generated by terrain_d:
    // Plane 1: [
    // [    -0.18973
    // [     0.83205
    // [    -0.52124
    // [   355.17627]]
    // Plane 2: [
    // [     0.34209
    // [     0.00000
    // [     0.93967
    // [  7178.25342]]
    // Plane 3: [
    // [    -0.97159
    // [     0.00000
    // [    -0.23665
    // [   773.27783]]
    // Plane intersect point: [
    // [  2915.04126
    // [ -5212.57520
    // [ -8700.36133
    // [     0.00000]]

    char buffer[255];

    RasPlane pl1 = {
        .normal = {
            .x = float_to_fixed_16_16(-0.18973),
            .y = float_to_fixed_16_16(0.83205),
            .z = float_to_fixed_16_16(-0.52124) },
        .distance = float_to_fixed_16_16(355.17627)
    };

    RasPlane pl2 = {
        .normal = {
            .x = float_to_fixed_16_16(0.34209),
            .y = float_to_fixed_16_16(0.0),
            .z = float_to_fixed_16_16(0.93967) },
        .distance = float_to_fixed_16_16(7178.25342)
    };

    RasPlane pl3 = {
        .normal = {
            .x = float_to_fixed_16_16(-0.97159),
            .y = float_to_fixed_16_16(0.0),
            .z = float_to_fixed_16_16(-0.23665) },
        .distance = float_to_fixed_16_16(773.27783)
    };

    Point3f result;

    core_get_3_plane_intersection(&pl1, &pl2, &pl3, &result);
    ras_log_info("3 plane intersect result: %s\n", repr_point3f(buffer, sizeof buffer, &result));
}

void model_tests()
{
    RasModel model;
    RasResult result = core_load_model("./assets/models/cube.obj", &model);

    if (result != RAS_RESULT_OK) {
        ras_log_error("core_load_model error\n");
    }
}

void scene_tests()
{
    // arrange
    const char* expected_name = "poly";
    const char* expected_model_name = "ico";
    const char* expected_path = "./assets/models/ico.obj";
    RasScene* scene = NULL;

    // act
    RasResult result = core_load_scene("./tests/data/scene01.lsp", &scene);

    // assert
    assert(result == RAS_RESULT_OK);
    bool pass = strcmp(scene->name, expected_name) == 0;
    RasSceneModel* model = &scene->models[0];
    assert(model != NULL);
    pass = pass && strcmp(model->name, expected_model_name) == 0;
    pass = pass && strcmp(model->path, expected_path) == 0;
    pass = pass && model->element.num_verts > 0;
    pass = pass && scene->num_objects == 1;
    pass = pass && scene->objects[0].element_ref != NULL;
    pass = pass && scene->objects[0].position.z == -float_to_fixed_16_16(2.5);
    pass = pass && scene->objects[0].rotation.y == float_to_fixed_16_16(0.5);
    pass = pass && scene->num_cameras == 1;
    pass = pass && scene->cameras[0].position.z == float_to_fixed_16_16(2.5);
    pass = pass && scene->cameras[0].angle == 180;
    assert(pass);
    core_free_scene(&scene);
}

int main()
{
    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(LOG_INFO);
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
    normalize_tests();
    cross_product_tests();
    dot_product_tests();
    plane_tests();
    mat_ortho_tests();
    model_tests();
    scene_tests();
    return 0;
}
