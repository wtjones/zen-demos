
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/scene.h"
#include "test_support.h"
#include <math.h>

TEST(TEST_MODEL)
{
    RasModel* model = core_load_model("./assets/models/cube.obj");

    if (model == NULL) {
        ras_log_error("core_load_model error\n");
        return true;
    }
    core_free_model(model);
    return false;
}

TEST(TEST_SCENE)
{
    // arrange
    const char* expected_name = "poly";
    const char* expected_model_name = "ico";
    const char* expected_path = "./assets/models/ico.obj";
    RasScene* scene = NULL;

    // act
    RasResult result = core_load_scene("./tests/data/scene01.lsp", &scene);

    // assert
    assert(result == RAS_RESULT_OK);
    bool pass = strcmp(scene->name, expected_name) == 0;
    RasSceneModel* model = &scene->models[0];
    assert(model != NULL);
    pass = pass && strcmp(model->name, expected_model_name) == 0;
    pass = pass && strcmp(model->path, expected_path) == 0;
    pass = pass && model->element.num_verts > 0;
    pass = pass && scene->num_objects == 1;
    pass = pass && scene->objects[0].model_index != -1;
    pass = pass && scene->objects[0].position.z == -float_to_fixed_16_16(2.5);
    pass = pass && scene->objects[0].rotation.y == float_to_fixed_16_16(0.5);
    pass = pass && scene->objects[0].animations != NULL;
    pass = pass && scene->objects[0].animations[0].rotation.speed == float_to_fixed_16_16(0.5);
    pass = pass && scene->objects[0].animations[0].rotation.axis.y == float_to_fixed_16_16(1.0);
    pass = pass && scene->num_cameras == 1;
    pass = pass && scene->cameras[0].position.z == float_to_fixed_16_16(2.5);
    pass = pass && scene->cameras[0].angle == 180;
    assert(pass);
    core_free_scene(&scene);
    return false;
}
