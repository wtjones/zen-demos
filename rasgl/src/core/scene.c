#include "include/rasgl/core/scene.h"
#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include <string.h>

RasResult core_load_scene(const char* path, RasScene** scene)
{
    LarScript* script;
    LarParseResult result = lar_parse_file(path, &script);

    if (result != LAR_PARSE_RESULT_OK) {
        ras_log_error("Failed to parse scene file: %s", path);
        return RAS_RESULT_ERROR;
    }
    char* repr = lar_repr_script(script);
    ras_log_info("Script: %s", repr);
    free(repr);
    lar_free_script(&script);
    RasScene* new_scene = (RasScene*)malloc(sizeof(RasScene));
    strcpy(new_scene->name, "test name");
    *scene = new_scene;

    return RAS_RESULT_OK;
}