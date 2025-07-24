#include "rasgl/core/clip.h"
#include "rasgl/core/debug.h"

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv)
{
    pv->clip_flags = 0;

    if (aabb_flags & (1 << PLANE_NEAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_NEAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_NEAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_FAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_FAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_FAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_LEFT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_LEFT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_LEFT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_RIGHT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_RIGHT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_RIGHT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_TOP)) {
        RasPlane* plane = &view_frustum->planes[PLANE_TOP];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_TOP)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_BOTTOM)) {
        RasPlane* plane = &view_frustum->planes[PLANE_BOTTOM];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? (1 << PLANE_BOTTOM)
            : 0;
    }
}
