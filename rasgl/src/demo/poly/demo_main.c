#include "rasgl/core/app.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/model.h"
#include "rasgl/core/pipeline.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "rasgl/core/stages.h"
#include "rasgl/core/text.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;
RasScene* scene;
RasFont* ui_font;
RasFixed delta_rotation = RAS_FIXED_ONE;
RasPipeline pipeline = { 0 };
RasRenderData render_data = { 0 };

Point3f delta = {
    .x = RAS_FLOAT_TO_FIXED(0.05f),
    .y = RAS_FLOAT_TO_FIXED(0.05f),
    .z = RAS_FLOAT_TO_FIXED(0.05f)
};
const char* str = "Hello, no worries here!\nNext line.";
Point2f text_pos = {
    .x = RAS_FIXED_ZERO,
    .y = RAS_TEXT_LETTER_HEIGHT + RAS_TEXT_LETTER_SPACING
};

RasCamera* camera;
RasSceneObject* selected_object = NULL;
bool animation_enabled = true;

char* default_scene = "./assets/scenes/tri.lsp";

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
    core_pipeline_init(&pipeline);
    const char* scene_path = (argc > 1) ? argv[1] : default_scene;

    ui_font = core_get_font_system(RAS_SYSTEM_FONT_DEFAULT);

    RasResult result = core_load_scene(scene_path, &scene);

    RAS_CHECK_RESULT(result);

    camera = &scene->cameras[0];
    camera->aspect_ratio = RAS_CAMERA_DEFAULT_ASPECT_RATIO;
    camera->near = RAS_CAMERA_DEFAULT_NEAR;
    camera->far = RAS_CAMERA_DEFAULT_FAR;
    camera->fov = RAS_CAMERA_DEFAULT_FOV;
    camera->projection_mode = RAS_CAMERA_DEFAULT_PROJECTION_MODE;

    selected_object = scene->num_objects > 0 ? &scene->objects[0] : NULL;

    return RAS_RESULT_OK;
}

void ras_objects_update(InputState* input_state)
{
    if (input_state->keys[RAS_KEY_J] == RAS_KEY_EVENT_UP) {
        if (selected_object != NULL) {
            size_t new_index = (selected_object - scene->objects + 1) % scene->num_objects;
            selected_object = &scene->objects[new_index];
            ras_log_info("Selected object index %d",
                new_index);
        }
    }

    if (input_state->keys[RAS_KEY_K] == RAS_KEY_EVENT_UP) {
        animation_enabled = !animation_enabled;
        ras_log_info("Animation enabled: %s", animation_enabled ? "true" : "false");
    }

    if (!animation_enabled) {
        return;
    }

    for (size_t i = 0; i < scene->num_objects; i++) {
        RasSceneObject* current_object = &scene->objects[i];

        if (current_object != selected_object) {
            core_update_animation(current_object);
        }
    }
}

void ras_selected_object_update(__attribute__((unused)) InputState* input_state)
{
    RasVector3f model_pos_prev;
    RasVector3f model_rot_prev;

    if (selected_object == NULL) {
        return;
    }

    RasVector3f* model_pos = &selected_object->position;
    RasVector3f* model_rotation = &selected_object->rotation;

    memcpy(&model_pos_prev, &selected_object->position, sizeof model_pos_prev);
    memcpy(&model_rot_prev, &selected_object->rotation, sizeof model_rot_prev);

    bool modifiers = input_state->keys[RAS_KEY_LCTRL] == 1
        || input_state->keys[RAS_KEY_LSHIFT] == 1;

    // Model X axis movement

    if (input_state->keys[RAS_KEY_LEFT] == 1 && !modifiers) {
        model_pos->x -= delta.x;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1 && !modifiers) {
        model_pos->x += delta.x;
    }

    // Model Y axis movement

    if (input_state->keys[RAS_KEY_UP] == 1 && !modifiers) {
        model_pos->y += delta.y;
    }
    if (input_state->keys[RAS_KEY_DOWN] == 1 && !modifiers) {
        model_pos->y -= delta.y;
    }

    // Model Z axis movement

    if (input_state->keys[RAS_KEY_UP] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] != 1) {
        model_pos->z += delta.y;
    }
    if (input_state->keys[RAS_KEY_DOWN] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] != 1) {
        model_pos->z -= delta.y;
    }

    // Model X rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_LEFT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = RAS_FIXED_ONE;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] == 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -RAS_FIXED_ONE;
    }

    model_rotation->x = (model_rotation->x + delta_rotation) % INT_32_TO_FIXED_16_16(360);
    if (model_rotation->x < 0) {
        model_rotation->x += INT_32_TO_FIXED_16_16(360);
    }

    // Model Y rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_LEFT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = RAS_FIXED_ONE;
    }
    if (input_state->keys[RAS_KEY_RIGHT] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -RAS_FIXED_ONE;
    }

    model_rotation->y = (model_rotation->y + delta_rotation) % INT_32_TO_FIXED_16_16(360);
    if (model_rotation->y < 0) {
        model_rotation->y += INT_32_TO_FIXED_16_16(360);
    }

    // Model Z rotation

    delta_rotation = 0;
    if (input_state->keys[RAS_KEY_DOWN] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = RAS_FIXED_ONE;
    }
    if (input_state->keys[RAS_KEY_UP] == 1
        && input_state->keys[RAS_KEY_LSHIFT] != 1
        && input_state->keys[RAS_KEY_LCTRL] == 1) {
        delta_rotation = -RAS_FIXED_ONE;
    }

    model_rotation->z = (model_rotation->z + delta_rotation) % INT_32_TO_FIXED_16_16(360);
    if (model_rotation->z < 0) {
        model_rotation->z += INT_32_TO_FIXED_16_16(360);
    }

    if (!cmp_point3f(model_pos, &model_pos_prev)
        || !cmp_point3f(model_rotation, &model_rot_prev)) {
        char buffer[100];
        ras_log_buffer_ex(
            RAS_EVENT_SC_OBJ_MOVE,
            "new model_pos: %s", repr_point3f(buffer, sizeof(buffer), model_pos));
        ras_log_buffer_ex(
            RAS_EVENT_SC_OBJ_XFORM,
            "new model_rot: %s", repr_point3f(buffer, sizeof(buffer), model_rotation));
    }
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{

    ras_camera_update(camera, input_state);
    ras_objects_update(input_state);
    ras_selected_object_update(input_state);

    if (input_state->current_frame % 2 == 0) {

        text_pos.x = (text_pos.x + RAS_FIXED_ONE);

        if (text_pos.x == (INT_32_TO_FIXED_16_16(settings->screen_width))) {
            text_pos.x = -core_get_font_width(ui_font, str);
        }
    }

    RasVector3f* model_pos = &selected_object->position;
    RasVector3f* model_rotation = &selected_object->rotation;
    char buffer[100];
    ras_log_buffer_ex(
        RAS_EVENT_SC_OBJ_MOVE,
        "model_pos: %s", repr_point3f(buffer, sizeof(buffer), model_pos));
    ras_log_buffer_ex(
        RAS_EVENT_SC_OBJ_XFORM,
        "model_rot: %s", repr_point3f(buffer, sizeof(buffer), model_rotation));
}

void render_scene_classic(RenderState* render_state)
{

    RasFixed model_world_matrix[4][4];
    RasFixed world_view_matrix[4][4];
    RasFixed projection_matrix[4][4];
    RasFixed combined_matrix[4][4];
    RasFrustum frustum;
    RasSceneObject* current_object;

    ras_camera_projection_init(camera, projection_matrix);
    mat_set_identity_4x4(world_view_matrix);
    ras_camera_world_view_init(camera, world_view_matrix);

    for (size_t i = 0; i < scene->num_objects; i++) {
        current_object = &scene->objects[i];
        RasVector3f* model_pos = &current_object->position;
        RasVector3f* model_rotation = &current_object->rotation;

        mat_set_identity_4x4(model_world_matrix);

        // FIXME: Support all objects in scene
        core_translate_apply(model_world_matrix, model_pos);
        RasFixed model_world2[4][4];
        RasFixed model_world3[4][4];

        // FIXME: Should rotate around model origin
        core_rotate_x_apply(model_world_matrix, FIXED_16_16_TO_INT_32(model_rotation->x));
        mat_rotate_y(model_world_matrix, FIXED_16_16_TO_INT_32(model_rotation->y), model_world2);
        mat_rotate_z(model_world2, FIXED_16_16_TO_INT_32(model_rotation->z), model_world3);

        core_translate_apply(model_world3, model_pos);

        mat_mul_4x4_4x4(projection_matrix, world_view_matrix, combined_matrix);

        // TODO: unused
        core_frustum_init(projection_matrix, &frustum);

        core_draw_element(
            render_state,
            current_object->element_ref,
            model_world3,
            world_view_matrix,
            projection_matrix,
            &frustum);
    }
}

void render_scene_pipeline(RenderState* render_state)
{
    core_renderdata_init(
        &render_data,
        render_state,
        scene,
        &scene->cameras[0]);
    core_pipeline_run(
        &pipeline,
        &render_data);
}

void render_scene(RenderState* render_state)
{
    if (render_state->pipeline_mode == RAS_PIPELINE_MODE_OFF) {
        render_scene_classic(render_state);
    } else {
        render_scene_pipeline(render_state);
    }
}

void render_ui(RenderState* render_state)
{
    core_draw_textf(
        render_state,
        ui_font,
        text_pos,
        str);

    Point2f stat_pos = { .x = RAS_FIXED_ZERO, .y = RAS_FIXED_ZERO };
    RasOctant octant = core_angle_to_octant(camera->angle);
    static uint32_t ticks = 0;
    ticks = render_state->current_frame % 60 == 0 ? render_state->last_app_render_ticks : ticks;
    core_draw_textf(
        render_state, ui_font, stat_pos, "Frame: %d\nFrame time: %d\nAngle: %d\nOctant: %d",
        render_state->current_frame, ticks, camera->angle, octant);

    ras_log_buffer_trace("Text width %d",
        FIXED_16_16_TO_INT_32(core_get_font_width(ui_font, "Hello!")));
}

void ras_app_render(__attribute__((unused)) RenderState states[])
{
    render_scene(&states[RAS_LAYER_SCENE]);
    render_ui(&states[RAS_LAYER_UI]);
}
