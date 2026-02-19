#include "rasgl/core/debug.h"
#include "rasgl/core/scene.h"
#include "rasgl/pack/pack.h"

extern const char scene_data[];
extern const uint32_t scene_data_size;

RasResult core_load_scene(const char* path, RasScene** scene)
{
    (void)path;

    ras_log_info("Decoding %d packed scene bytes in PSX binary...", scene_data_size);
    *scene = pack_decode_scene(scene_data, scene_data_size);

    if (*scene == NULL) {
        ras_log_error("Failed to decode scene");
        return RAS_RESULT_ERROR;
    }
    ras_log_info("Scene decoded successfully");

    return RAS_RESULT_OK;
}
