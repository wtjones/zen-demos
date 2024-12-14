#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include "rasgl/core/scene.h"
#include <string.h>

void core_free_scene_models(RasScene* scene)
{
    // TODO: free RasModel
    free(scene->models);
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

void core_free_scene(RasScene** scene)
{
    RasScene* s = *scene;
    core_free_scene_models(s);
    core_free_scene_objects(s);
    free(s);
    *scene = NULL;
}
