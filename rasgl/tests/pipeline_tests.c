#include "rasgl/core/debug.h"
#include "rasgl/core/pipeline.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "rasgl/core/stages.h"
#include "tests.h"

typedef struct TestData {
    int value;
} TestData;

RenderState states[RAS_LAYER_COUNT] = { 0 };

void* foo(void* input)
{
    TestData* data = (TestData*)input;
    ras_log_info("foo: %d", data->value);
    data->value *= 5;
    return input;
}

void* bar(void* input)
{
    TestData* data = (TestData*)input;
    ras_log_info("bar: %d", data->value);
    return input;
}

void pipeline_tests()
{

    RasPipeline pipeline = {
        .num_stages = 2,
        .stages = {
            { .name = "FOO!!", foo },
            { .name = "BAR!!!", bar } }
    };
    TestData data = { .value = 67 };
    core_pipeline_run(&pipeline, &data);
}

void pipeline_scene_tests()
{
    // arrange
    core_renderstates_init(states);
    states[RAS_LAYER_SCENE].screen_settings.screen_width = 320;
    states[RAS_LAYER_SCENE].screen_settings.screen_height = 240;
    ras_log_info("Render state size: %zu", sizeof(RenderState));
    RasScene* scene = NULL;

    RasResult result = core_load_scene("./tests/data/scene02.lsp", &scene);
    assert(result == RAS_RESULT_OK);

    RasCamera* camera = &scene->cameras[0];
    // FIXME: Should be set in scene
    camera->aspect_ratio = RAS_CAMERA_DEFAULT_ASPECT_RATIO;
    camera->near = RAS_CAMERA_DEFAULT_NEAR;
    camera->far = RAS_CAMERA_DEFAULT_FAR;
    camera->fov = RAS_CAMERA_DEFAULT_FOV;
    camera->projection_mode = RAS_CAMERA_DEFAULT_PROJECTION_MODE;

    RasPipeline pipeline = {
        .num_stages = 6,
        .stages = {
            { .name = "core_sg_setup", core_sg_setup },
            { .name = "core_sg_xform_objects", core_sg_xform_objects },
            { .name = "core_sg_xform_aabb", core_sg_xform_aabb },
            { .name = "core_sg_render_aabb", core_sg_render_aabb },
            { .name = "core_sg_xform_object_verts", core_sg_xform_object_verts },
            { .name = "core_sg_project_verts", core_sg_project_verts } }
    };

    RasRenderData render_data;
    core_renderdata_init(
        &render_data,
        &states[RAS_LAYER_SCENE],
        scene,
        &scene->cameras[0]);

    // act
    RasRenderData* render_result = core_pipeline_run(&pipeline, &render_data);

    // assert
    assert(render_result != NULL);
    char buffer[1000];
    ras_log_info("Render data projection matrix: %s",
        repr_mat_4x4(buffer, sizeof(buffer), render_result->projection_matrix));
    ras_log_info("Render data world view matrix: %s",
        repr_mat_4x4(buffer, sizeof(buffer), render_result->world_view_matrix));

    core_free_scene(&scene);
}
