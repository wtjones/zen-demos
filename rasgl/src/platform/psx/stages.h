#pragma once

#include "ps1/gte.h"
#include "rasgl/core/pipeline.h"
typedef struct RasPSXRenderData {
    GTEMatrix world_view_matrix;
    GTEMatrix model_world_matrix[RAS_MAX_MESHES];
    GTEMatrix model_view_matrix[RAS_MAX_MESHES];
} RasPSXRenderData;

void psx_pipeline_init(RasPipeline* pipeline);
