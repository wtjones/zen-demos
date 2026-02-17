#include "rasgl/core/debug.h"
#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"
#include "tests.h"

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

    assert(scene->models[0].element.num_faces == decoded_scene->models[0].element.num_faces);
    if (scene->models[0].element.num_faces > 0) {
        assert(scene->models[0].element.faces[0].material_index == decoded_scene->models[0].element.faces[0].material_index);
        assert(scene->models[0].element.faces[0].outline_edges == decoded_scene->models[0].element.faces[0].outline_edges);
        assert(cmp_point3f(&scene->models[0].element.faces[0].normal, &decoded_scene->models[0].element.faces[0].normal));
    }

    assert(scene->models[0].element.num_indexes == decoded_scene->models[0].element.num_indexes);
    if (scene->models[0].element.num_indexes > 0) {
        assert(scene->models[0].element.indexes[0] == decoded_scene->models[0].element.indexes[0]);
    }

    assert(scene->models[0].element.num_material_indexes == decoded_scene->models[0].element.num_material_indexes);
    if (scene->models[0].element.num_material_indexes > 0) {
        assert(scene->models[0].element.material_indexes[0] == decoded_scene->models[0].element.material_indexes[0]);
    }

    assert(scene->num_cameras == decoded_scene->num_cameras);
    assert(cmp_point3f((Point3f*)&scene->cameras[0].position, (Point3f*)&decoded_scene->cameras[0].position));
    assert(scene->cameras[0].angle == decoded_scene->cameras[0].angle);

    // TombMap
    assert(scene->num_tombmaps == decoded_scene->num_tombmaps);
    if (scene->num_tombmaps > 0) {
        assert(strcmp(scene->tombmaps[0].name, decoded_scene->tombmaps[0].name) == 0);
        assert(scene->tombmaps[0].num_rooms == decoded_scene->tombmaps[0].num_rooms);
        if (scene->tombmaps[0].num_rooms > 0) {
            assert(scene->tombmaps[0].rooms[0].x == decoded_scene->tombmaps[0].rooms[0].x);
            assert(scene->tombmaps[0].rooms[0].z == decoded_scene->tombmaps[0].rooms[0].z);
        }
    }

    ras_log_info("Test: Freeing decoded scene...");
    free(encoded_scene);
    core_free_scene(&scene);
    core_free_scene(&decoded_scene);
}

void pack_tests()
{
    ras_log_info("Running pack tests...");
    pack_scene_tests();
}
