#pragma once

#include "ps1/gte.h"
#include "rasgl/core/pipeline.h"

typedef struct RasPSXMatrix {
    GTEMatrix m;
    GTEVector32 translation;
} RasPSXMatrix;

typedef struct RasPSXRenderData {
    RasPSXMatrix world_view_matrix;
    RasPSXMatrix model_world_matrix[RAS_MAX_MESHES];
    RasPSXMatrix model_view_matrix[RAS_MAX_MESHES];
} RasPSXRenderData;

void psx_pipeline_init(RasPipeline* pipeline);
