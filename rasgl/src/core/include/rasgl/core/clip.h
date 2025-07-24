#ifndef CORE_CLIP_H
#define CORE_CLIP_H

#include "aabb.h"
#include "frustum.h"

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv);

#endif
