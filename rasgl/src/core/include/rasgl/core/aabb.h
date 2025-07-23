#ifndef CORE_AABB_H
#define CORE_AABB_H

#include "frustum.h"
#include "graphics.h"
#include "maths.h"

void core_render_aabb(
    RenderState* render_state,
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum,
    RasAABB* aabb);

#endif
