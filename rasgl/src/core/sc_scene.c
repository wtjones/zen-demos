#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include "rasgl/core/scene.h"
#include <stdlib.h>
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
        if (scene->objects[i].animations != NULL) {
            free(scene->objects[i].animations);
            scene->objects[i].animations = NULL;
            scene->objects[i].num_animations = 0;
        }
    }
    free(scene->objects);
    scene->objects = NULL;
    scene->num_objects = 0;
}

void core_free_scene_cameras(RasScene* scene)
{
    free(scene->cameras);
    scene->cameras = NULL;
    scene->num_cameras = 0;
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
    if (scene == NULL || *scene == NULL) {
        return;
    }
    RasScene* s = *scene;
    core_free_scene_models(s);
    core_free_scene_objects(s);
    core_free_scene_cameras(s);
    core_free_scene_gridmaps(s);
    core_free_scene_tombmaps(s->tombmaps, s->num_tombmaps);
    free(s);
    *scene = NULL;
}
