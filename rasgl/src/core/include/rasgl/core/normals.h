#ifndef CORE_NORMALS_H
#define CORE_NORMALS_H

#include "graphics.h"

void core_draw_mesh_normals(
    RenderState* render_state,
    RasPipelineMesh* mesh,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4]);

#endif
