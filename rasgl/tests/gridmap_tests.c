#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "tests.h"

void gridmap_scene_test()
{
    // arrange
    RasScene* scene = NULL;

    RasResult result = core_load_scene("assets/scenes/gmap01.lsp", &scene);
    assert(result == RAS_RESULT_OK);
    assert(scene->num_gridmaps == 1);
    RasSceneGridMap* gridmap = &scene->gridmaps[0];
    assert(strcmp(gridmap->name, "gmap01") == 0);
    assert(gridmap->element.num_verts > 0);
    core_free_scene(&scene);
}
