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
    ras_log_info("Test: Encoding scene...");
    encoded_scene = pack_encode_scene(scene, &encoded_size);
    assert(encoded_scene != NULL);
    assert(encoded_size > 0);
    ras_log_info("Encoded scene size: %zu", encoded_size);

    // act
    ras_log_info("Test: Decoding scene...");
    RasScene* decoded_scene = pack_decode_scene(encoded_scene, encoded_size);
    assert(decoded_scene != NULL);

    // assert
    assert(strcmp(scene->name, decoded_scene->name) == 0);
    assert(scene->num_models == decoded_scene->num_models);
    assert(strcmp(scene->models[0].name, decoded_scene->models[0].name) == 0);
    assert(strcmp(scene->models[0].path, decoded_scene->models[0].path) == 0);
    assert(scene->models[0].element.num_verts == decoded_scene->models[0].element.num_verts);
    assert(scene->num_objects == decoded_scene->num_objects);
    assert(scene->objects[0].model_index == decoded_scene->objects[0].model_index);
    assert(scene->objects[0].rotation.y == decoded_scene->objects[0].rotation.y);
    assert(scene->objects[0].position.z == decoded_scene->objects[0].position.z);
    assert(cmp_point3f((Point3f*)&scene->objects[0].position, (Point3f*)&decoded_scene->objects[0].position));
    assert(cmp_point3f((Point3f*)&scene->objects[0].rotation, (Point3f*)&decoded_scene->objects[0].rotation));

    assert(scene->objects[0].num_animations == decoded_scene->objects[0].num_animations);
    assert(scene->objects[0].animations[0].rotation.speed == decoded_scene->objects[0].animations[0].rotation.speed);
    assert(cmp_point3f((Point3f*)&scene->objects[0].animations[0].rotation.axis,
        (Point3f*)&decoded_scene->objects[0].animations[0].rotation.axis));

    assert(cmp_point3f(&scene->models[0].element.aabb.min, &decoded_scene->models[0].element.aabb.min));
    assert(cmp_point3f(&scene->models[0].element.aabb.max, &decoded_scene->models[0].element.aabb.max));

    // assert(scene->num_cameras == decoded_scene->num_cameras);
    // assert(cmp_point3f((Point3f*)&scene->cameras[0].position, (Point3f*)&decoded_scene->cameras[0].position));
    // assert(scene->cameras[0].angle == decoded_scene->cameras[0].angle);

    ras_log_info("Test: Freeing decoded scene...");
    free(encoded_scene);
    core_free_scene(&scene);
    core_free_scene(&decoded_scene);
}

void pack_tests()
{
    ras_log_info("Running pack tests...");
    // FIXME: Enable once full packing implemented.
    __lsan_disable();
    pack_scene_tests();
    __lsan_enable();
}
