#include "rasgl/core/app.h"
#include "rasgl/core/camera.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/frustum.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/matrix_projection.h"
#include "rasgl/core/model.h"
#include "rasgl/core/pipeline.h"
#include "rasgl/core/scene.h"
#include "rasgl/core/stages.h"
#include <stdio.h>
#include <stdlib.h>

ScreenSettings* settings;
RasScene* scene;
RasFixed delta_rotation = RAS_FIXED_ONE;
RasPipeline pipeline = { 0 };
RasRenderData render_data = { 0 };

Point3f delta = {
    .x = RAS_FLOAT_TO_FIXED(0.05f),
    .y = RAS_FLOAT_TO_FIXED(0.05f),
    .z = RAS_FLOAT_TO_FIXED(0.05f)
};

RasCamera* camera;
RasSceneObject* selected_object = NULL;
bool animation_enabled = true;

/**
 * @brief Inline a simple scene with a triangle.
 *
 * @return RasScene*
 */
RasScene* build_scene()
{
    RasScene* scene = malloc(sizeof(RasScene));
    if (scene == NULL) {
        ras_log_error("Unable to malloc().");
        return NULL;
    }

    memset(scene, 0, sizeof(RasScene));

    scene->num_models = 1;
    scene->models = malloc(sizeof(RasSceneModel) * scene->num_models);
    if (scene->models == NULL) {
        ras_log_error("Unable to malloc() scene models.");
        free(scene);
        return NULL;
    }

    RasSceneModel* model = &scene->models[0];
    memset(model, 0, sizeof(RasSceneModel));
    snprintf(model->name, sizeof(model->name), "tri");
    snprintf(model->path, sizeof(model->path), "./assets/models/tri.obj");

    if (core_pipeline_element_alloc(3, 1, 3, 1, &model->element) != RAS_RESULT_OK) {
        ras_log_error("Failed to alloc pipeline element for tri model");
        free(scene->models);
        free(scene);
        return NULL;
    }

    /* Fill verts (from tri.obj):
     * v 0.0 0.8 0.0
     * v -0.8 -0.8 0.0
     * v 0.8 -0.8 0.0
     */
    model->element.num_verts = 3;
    model->element.verts[0].position.x = RAS_FLOAT_TO_FIXED(0.0f);
    model->element.verts[0].position.y = RAS_FLOAT_TO_FIXED(0.8f);
    model->element.verts[0].position.z = RAS_FLOAT_TO_FIXED(0.0f);

    model->element.verts[1].position.x = RAS_FLOAT_TO_FIXED(-0.8f);
    model->element.verts[1].position.y = RAS_FLOAT_TO_FIXED(-0.8f);
    model->element.verts[1].position.z = RAS_FLOAT_TO_FIXED(0.0f);

    model->element.verts[2].position.x = RAS_FLOAT_TO_FIXED(0.8f);
    model->element.verts[2].position.y = RAS_FLOAT_TO_FIXED(-0.8f);
    model->element.verts[2].position.z = RAS_FLOAT_TO_FIXED(0.0f);

    model->element.num_faces = 1;
    model->element.num_indexes = 3;
    model->element.indexes[0] = 0;
    model->element.indexes[1] = 1;
    model->element.indexes[2] = 2;

    model->element.num_material_indexes = 1;
    model->element.material_indexes[0] = 3;

    model->element.faces[0].normal.x = RAS_FLOAT_TO_FIXED(0.0f);
    model->element.faces[0].normal.y = RAS_FLOAT_TO_FIXED(0.0f);
    model->element.faces[0].normal.z = RAS_FLOAT_TO_FIXED(1.0f);
    model->element.faces[0].material_index = 0;

    core_get_element_aabb(&model->element, &model->element.aabb);

    scene->cameras = malloc(sizeof(RasCamera));
    if (scene->cameras == NULL) {
        ras_log_error("Unable to malloc() scene cameras.");
        free(scene);
        return NULL;
    }
    RasCamera* camera = &scene->cameras[0];
    memset(camera, 0, sizeof(RasCamera));
    camera->position.x = RAS_FLOAT_TO_FIXED(0.0f);
    camera->position.y = RAS_FLOAT_TO_FIXED(0.0f);
    camera->position.z = RAS_FLOAT_TO_FIXED(2.5f);
    camera->angle = 180;
    camera->aspect_ratio = RAS_CAMERA_DEFAULT_ASPECT_RATIO;
    camera->near = RAS_CAMERA_DEFAULT_NEAR;
    camera->far = RAS_CAMERA_DEFAULT_FAR;
    camera->fov = RAS_CAMERA_DEFAULT_FOV;
    camera->projection_mode = RAS_CAMERA_DEFAULT_PROJECTION_MODE;

    scene->num_cameras = 1;

    scene->num_objects = 1;
    scene->objects = malloc(sizeof(RasSceneObject) * scene->num_objects);
    if (scene->objects == NULL) {
        ras_log_error("Unable to malloc() scene objects.");
        core_pipeline_element_free(&model->element);
        free(scene->cameras);
        free(scene->models);
        free(scene);
        return NULL;
    }
    RasSceneObject* obj = &scene->objects[0];
    memset(obj, 0, sizeof(RasSceneObject));
    obj->model_index = 0;
    obj->position.x = RAS_FLOAT_TO_FIXED(0.0f);
    obj->position.y = RAS_FLOAT_TO_FIXED(0.0f);
    obj->position.z = RAS_FLOAT_TO_FIXED(-2.5f);
    obj->rotation.x = RAS_FLOAT_TO_FIXED(0.0f);
    obj->rotation.y = RAS_FLOAT_TO_FIXED(0.0f);
    obj->rotation.z = RAS_FLOAT_TO_FIXED(0.0f);
    obj->mesh_index = 0;
    // :animation (rotation :axis (vec 0.0 1.0 0.0) :speed 0.5))
    obj->animation = malloc(sizeof(RasSceneObjectAnimation));
    if (obj->animation == NULL) {
        ras_log_error("Unable to malloc() scene object animation.");
        free(scene->objects);
        core_pipeline_element_free(&model->element);
        free(scene->cameras);
        free(scene->models);
        free(scene);
        return NULL;
    }
    RasSceneObjectAnimation* anim = obj->animation;
    anim->rotation.axis.x = RAS_FLOAT_TO_FIXED(0.0f);
    anim->rotation.axis.y = RAS_FLOAT_TO_FIXED(1.0f);
    anim->rotation.axis.z = RAS_FLOAT_TO_FIXED(0.0f);
    anim->rotation.speed = RAS_FLOAT_TO_FIXED(0.5f);

    return scene;
}

RasResult ras_app_init(int argc, const char** argv, ScreenSettings* init_settings)
{
    ras_log_info("ras_app_init()... argc: %d argv: %s\n", argc, argv[0]);
    ras_log_info("ras_app_init()... screen_width.x: %d\n", init_settings->screen_width);
    settings = init_settings;
    core_init_maths_tables();

    scene = build_scene();
    if (scene == NULL) {
        return RAS_RESULT_ERROR;
    }
    core_pipeline_init(&pipeline);

    camera = &scene->cameras[0];

    return RAS_RESULT_OK;
}

RasResult ras_app_renderstates_init(RenderState states[])
{
    RAS_CHECK_RESULT(core_renderdata_init(
        &render_data,
        &states[RAS_LAYER_SCENE],
        scene,
        &scene->cameras[0]));

    return RAS_RESULT_OK;
}

void ras_objects_update(__attribute__((unused)) InputState* input_state)
{
    for (size_t i = 0; i < scene->num_objects; i++) {
        RasSceneObject* current_object = &scene->objects[i];
        core_update_animation(current_object);
    }
}

void ras_app_update(__attribute__((unused)) InputState* input_state)
{
    ras_camera_update(camera, input_state);
    ras_objects_update(input_state);
}

void render_scene_pipeline(__attribute__((unused)) RenderState* render_state)
{
    core_renderdata_clear(&render_data);

    core_pipeline_run(
        &pipeline,
        &render_data);
}

void render_scene(RenderState* render_state)
{
    render_scene_pipeline(render_state);
}

void ras_app_render(__attribute__((unused)) RenderState states[RAS_LAYER_COUNT])
{
    render_scene(&states[RAS_LAYER_SCENE]);
}
