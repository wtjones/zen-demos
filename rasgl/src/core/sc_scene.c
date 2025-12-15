#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include "rasgl/core/scene.h"
#include <string.h>

void core_free_scene_models(RasScene* scene)
{
    for (size_t i = 0; i < scene->num_models; i++) {
        core_pipeline_element_free(&scene->models[i].element);
    }
    free(scene->models);
    scene->models = NULL;
    scene->num_models = 0;
}

void core_free_scene_objects(RasScene* scene)
{
    for (size_t i = 0; i < scene->num_objects; i++) {
        if (scene->objects[i].animation != NULL) {
            free(scene->objects[i].animation);
            scene->objects[i].animation = NULL;
        }
    }

    free(scene->objects);
}

void core_free_scene_cameras(RasScene* scene)
{
    free(scene->cameras);
}

void core_free_scene_gridmaps(RasScene* scene)
{
    if (scene->num_gridmaps == 0) {
        return;
    }
    for (size_t i = 0; i < scene->num_gridmaps; i++) {
        core_pipeline_element_free(&scene->gridmaps[i].element);
    }
    free(scene->gridmaps);
    scene->gridmaps = NULL;
    scene->num_gridmaps = 0;
}

void core_free_scene(RasScene** scene)
{
    RasScene* s = *scene;
    core_free_scene_models(s);
    core_free_scene_objects(s);
    core_free_scene_cameras(s);
    core_free_scene_gridmaps(s);
    core_free_scene_tombmaps(s->tombmaps, s->num_tombmaps);
    free(s);
    *scene = NULL;
}
