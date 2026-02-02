#ifndef HOSTED_SC_LOAD_H
#define HOSTED_SC_LOAD_H

#include "rasgl/core/debug.h"
#include "rasgl/core/gridmap.h"
#include "rasgl/core/model.h"
#include "rasgl/core/tombmap.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

RasResult hosted_script_map_gridmap(
    LarNode* gridmap_exp,
    RasSceneGridMap* gridmap);

RasResult hosted_script_map_tombmaps(
    LarNode* scene_exp,
    RasSceneTombMap** out_tombmaps,
    size_t* out_num_tombmaps);

#endif
