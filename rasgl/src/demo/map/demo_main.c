#include "rasgl/core/app.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/grid.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/model.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "rasgl/core/text.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;
RasScene* scene;
RasFixed delta_rotation = RAS_FIXED_ONE;

Point3f delta = {
    .x = RAS_FLOAT_TO_FIXED(0.05f),
    .y = RAS_FLOAT_TO_FIXED(0.05f),
    .z = RAS_FLOAT_TO_FIXED(0.05f)
};

RasCamera* camera;
bool animation_enabled = true;

char* default_scene = "./assets/scenes/map01.lsp";

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
    const char* scene_path = (argc > 1) ? argv[1] : default_scene;

    RasResult result = core_load_scene(scene_path, &scene);

    RAS_CHECK_RESULT(result);

    camera = &scene->cameras[0];
    camera->aspect_ratio = RAS_CAMERA_DEFAULT_ASPECT_RATIO;
    camera->near = RAS_CAMERA_DEFAULT_NEAR;
    camera->far = RAS_CAMERA_DEFAULT_FAR;
    camera->fov = RAS_CAMERA_DEFAULT_FOV;
    camera->projection_mode = RAS_CAMERA_DEFAULT_PROJECTION_MODE;

    return RAS_RESULT_OK;
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{
    ras_camera_update(camera, input_state);
}

void render_map(__attribute__((unused)) RenderState* render_state)
{
    RasFixed model_world_matrix[4][4];
    RasFixed world_view_matrix[4][4];
    RasFixed projection_matrix[4][4];
    RasFrustum frustum;

    if (scene->num_maps == 0) {
        return;
    }

    ras_camera_projection_init(camera, projection_matrix);
    mat_set_identity_4x4(world_view_matrix);
    ras_camera_world_view_init(camera, world_view_matrix);

    mat_set_identity_4x4(model_world_matrix);
    core_frustum_init(projection_matrix, &frustum);

    ras_log_buffer("core_draw_element() deprecated");
}

void render_scene(RenderState* render_state __attribute__((unused)))
{
    RasFixed world_view_matrix[4][4];
    RasFixed projection_matrix[4][4];
    RasFrustum frustum;

    render_map(render_state);
    mat_set_identity_4x4(projection_matrix);
    mat_set_identity_4x4(world_view_matrix);

    ras_camera_projection_init(camera, projection_matrix);
    ras_camera_world_view_init(camera, world_view_matrix);

    core_frustum_init(projection_matrix, &frustum);

    core_draw_grid(
        render_state,
        world_view_matrix,
        projection_matrix);
}

void ras_app_render(__attribute__((unused)) RenderState states[])
{
    render_scene(&states[RAS_LAYER_SCENE]);
}
