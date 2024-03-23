#include "rasgl/core/app.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/model.h"
#include "rasgl/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;

RasModel cube_model;
int32_t delta_rotation = 1;
int32_t model_rotation_x = 0;
int32_t model_rotation_y = 0;
int32_t model_rotation_z = 0;
Point3f delta = {
    .x = RAS_FLOAT_TO_FIXED(0.05f),
    .y = RAS_FLOAT_TO_FIXED(0.05f),
    .z = RAS_FLOAT_TO_FIXED(0.05f)
};
Point3f model_pos = { .x = 0, .y = 0, .z = -RAS_FLOAT_TO_FIXED(2.5) }; // world space

RasCamera camera = {
    .position = { .x = 0, .y = 0, .z = RAS_FLOAT_TO_FIXED(2.5) },
    .angle = 180,
    .aspect_ratio = RAS_CAMERA_DEFAULT_ASPECT_RATIO,
    .near = RAS_CAMERA_DEFAULT_NEAR,
    .far = RAS_CAMERA_DEFAULT_FAR,
    .fov = RAS_CAMERA_DEFAULT_FOV,
    .projection_mode = RAS_CAMERA_DEFAULT_PROJECTION_MODE
};

char* default_model = "./assets/models/ico.obj";

RasPipelineElement current_element;

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
    const char* model_path = (argc > 1) ? argv[1] : default_model;

    RasResult result = core_load_model(model_path, &cube_model);

    if (result != RAS_RESULT_OK) {
        ras_log_error("core_load_model error\n");
        return RAS_RESULT_ERROR;
    }
    RasModelGroup* group = &cube_model.groups[0];
    core_model_group_to_pipeline_element(group, &current_element);
    return RAS_RESULT_OK;
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{
    Point3f model_pos_prev;

    ras_camera_update(&camera, input_state);

    memcpy(&model_pos_prev, &model_pos, sizeof model_pos);

    bool modifiers = input_state->keys[RAS_KEY_LCTRL] == 1 || input_state->keys[RAS_KEY_LSHIFT] == 1;

    // Model X axis movement

    if (input_state->keys[RAS_KEY_LEFT] == 1 && !modifiers) {
        model_pos.x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1 && !modifiers) {
        model_pos.x += delta.x;
    }

    // Model Y axis movement

    if (input_state->keys[RAS_KEY_UP] == 1 && !modifiers) {
        model_pos.y += delta.y;
    }
    if (input_state->keys[RAS_KEY_DOWN] == 1 && !modifiers) {
        model_pos.y -= delta.y;
    }

    // Model Z axis movement

    if (input_state->keys[RAS_KEY_UP] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] != 1) {
        model_pos.z += delta.y;
    }
    if (input_state->keys[RAS_KEY_DOWN] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] != 1) {
        model_pos.z -= delta.y;
    }

    // Model X rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_LEFT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = 1;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -1;
    }

    model_rotation_x = (model_rotation_x + delta_rotation) % 360;
    if (model_rotation_x < 0) {
        model_rotation_x += 360;
    }

    // Model Y rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_LEFT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = 1;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -1;
    }

    model_rotation_y = (model_rotation_y + delta_rotation) % 360;
    if (model_rotation_y < 0) {
        model_rotation_y += 360;
    }

    // Model Z rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_DOWN] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = 1;
    }
    if (input_state->keys[RAS_KEY_UP] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -1;
    }

    model_rotation_z = (model_rotation_z + delta_rotation) % 360;
    if (model_rotation_z < 0) {
        model_rotation_z += 360;
    }

    if (!cmp_point3f(&model_pos, &model_pos_prev)) {
        char buffer[100];
        ras_log_info("model_pos: %s", repr_point3f(buffer, sizeof(buffer), &model_pos));
    }
}

void ras_app_render(__attribute__((unused)) RenderState* render_state)
{
    RasFixed model_world_matrix[4][4];
    RasFixed world_view_matrix[4][4];
    RasFixed projection_matrix[4][4];
    RasFixed combined_matrix[4][4];
    RasFrustum frustum;

    mat_set_identity_4x4(model_world_matrix);
    mat_set_identity_4x4(world_view_matrix);

    core_translate_apply(model_world_matrix, &model_pos);
    RasFixed model_world2[4][4];
    RasFixed model_world3[4][4];

    // FIXME: Should rotate around model origin
    core_rotate_x_apply(model_world_matrix, model_rotation_x);
    mat_rotate_y(model_world_matrix, model_rotation_y, model_world2);
    mat_rotate_z(model_world2, model_rotation_z, model_world3);

    core_translate_apply(model_world3, &model_pos);

    ras_camera_world_view_init(&camera, world_view_matrix);
    ras_camera_projection_init(&camera, projection_matrix);

    mat_mul_4x4_4x4(projection_matrix, world_view_matrix, combined_matrix);

    // TODO: unused
    core_frustum_init(projection_matrix, &frustum);

    core_draw_element(
        render_state,
        &current_element,
        model_world3,
        world_view_matrix,
        projection_matrix,
        &frustum);
}
