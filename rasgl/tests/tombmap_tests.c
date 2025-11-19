#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "tests.h"

void tombmap_scene_test()
{
    // arrange
    RasScene* scene = NULL;

    RasResult result = core_load_scene("assets/scenes/tmap01.lsp", &scene);
    assert(result == RAS_RESULT_OK);
    assert(scene->num_tombmaps == 1);
    RasSceneTombMap* tombmap = &scene->tombmaps[0];
    assert(strcmp(tombmap->name, "tmap01") == 0);
    assert(tombmap->num_rooms > 0);
    core_free_scene(&scene);
}
