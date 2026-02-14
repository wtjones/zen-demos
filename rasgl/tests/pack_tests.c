#include "rasgl/core/debug.h"
#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include "tests.h"
#include <sanitizer/lsan_interface.h>

void pack_scene_tests()
{
    // arrange
    RasScene* scene = NULL;
    char* encoded_scene = NULL;
    size_t encoded_size = 0;

    // act
    RasResult result = core_load_scene("./tests/data/scene01.lsp", &scene);
    assert(result == RAS_RESULT_OK);

    // assert
    encoded_scene = pack_encode_scene(scene, &encoded_size);
    assert(encoded_scene != NULL);
    assert(encoded_size > 0);
    ras_log_info("Encoded scene size: %zu", encoded_size);

    // act
    RasScene* decoded_scene = pack_decode_scene(encoded_scene, encoded_size);
    assert(decoded_scene != NULL);

    // assert
    assert(strcmp(scene->name, decoded_scene->name) == 0);
    assert(scene->num_models == decoded_scene->num_models);

    free(encoded_scene);
    core_free_scene(&scene);
    // FIXME: Enable once full packing implemented.
    // core_free_scene(&decoded_scene);
}

void pack_tests()
{
    ras_log_info("Running pack tests...");
    // FIXME: Enable once full packing implemented.
    __lsan_disable();
    pack_scene_tests();
    __lsan_enable();
}
