#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;

int32_t delta_rotation = 1;
int32_t counter = 0;
int32_t model_rotation_y = 0;
Point3f delta = {
    .x = RAS_FLOAT_TO_FIXED(0.2f),
    .y = RAS_FLOAT_TO_FIXED(0.2f),
    .z = RAS_FLOAT_TO_FIXED(0.2f)
};
Point3f model_pos = { .x = 0, .y = 0, .z = -RAS_FLOAT_TO_FIXED(10) }; // world space
Point3f view_pos = { .x = 0, .y = 0, .z = RAS_FLOAT_TO_FIXED(5) };    // world space

float aspect_ratio = 1.333f; // Aspect ratio (width/height)
float near = 0.1f;           // Near clipping plane
float far = 100.0f;          // Far clipping plane
float viewer_fov = 60.0f;

void proj_matrix_init(RenderState* render_state, int32_t projection_matrix[4][4])
{
    if (render_state->projection_mode == RAS_PERSPECTIVE_MATRIX) {
        mat_projection_init(projection_matrix, viewer_fov, aspect_ratio, near, far);
    } else {
        mat_ortho_init(
            projection_matrix,
            -INT_32_TO_FIXED_16_16(1),
            INT_32_TO_FIXED_16_16(1),
            -INT_32_TO_FIXED_16_16(1),
            INT_32_TO_FIXED_16_16(1),
            -INT_32_TO_FIXED_16_16(1),
            INT_32_TO_FIXED_16_16(1));
    }
}

void ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{
    char buffer[255];
    if (input_state->keys[RAS_KEY_UP] == 1) {

        model_pos.z -= delta.z;
    }
    if (input_state->keys[RAS_KEY_DOWN] == 1) {
        model_pos.z += delta.z;
    }
    if (input_state->keys[RAS_KEY_LEFT] == 1) {

        model_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1) {
        model_pos.x += delta.x;
    }
    ras_log_info("model_pos: %s", repr_point3f(buffer, sizeof(buffer), &model_pos));

    model_rotation_y = (model_rotation_y + delta_rotation) % 360;
    if (model_rotation_y < 0) {
        model_rotation_y += 360;
    }

    counter++;
}

void ras_app_render(RenderState* render_state)
{
    char buffer[500];

    // 2 ------ 3
    // |  \     |
    // |     \  |
    // 0 -------1
    RasVertex vertex_buffer[] = {
        { .position = { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(0.0) } },
        { .position = { float_to_fixed_16_16(0.5), float_to_fixed_16_16(-0.5), float_to_fixed_16_16(0.0) } },
        { .position = { float_to_fixed_16_16(-0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(0.0) } },
        { .position = { float_to_fixed_16_16(0.5), float_to_fixed_16_16(0.5), float_to_fixed_16_16(0.0) } }
    };

    uint32_t num_verts = 4;
    uint32_t element_indexes[] = { 0, 1, 2, 2, 1, 3 };
    uint32_t num_indexes = 6;

    int32_t model_world_matrix[4][4];
    int32_t world_view_matrix[4][4];
    int32_t projection_matrix[4][4];

    mat_set_identity_4x4(model_world_matrix);
    mat_set_identity_4x4(world_view_matrix);
    proj_matrix_init(render_state, projection_matrix);

    core_translate_apply(model_world_matrix, &model_pos);
    core_rotate_y_apply(model_world_matrix, model_rotation_y);
    core_translate_apply(world_view_matrix, &view_pos);

    ras_log_trace("model_world_matrix rot: %s\n", repr_mat_4x4(buffer, sizeof buffer, model_world_matrix));

    core_draw_elements(
        render_state,
        vertex_buffer,
        num_verts,
        element_indexes,
        num_indexes,
        model_world_matrix,
        world_view_matrix,
        projection_matrix);
}
