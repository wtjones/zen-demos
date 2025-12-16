#ifndef CORE_CLIP_H
#define CORE_CLIP_H

#include "frustum.h"
#include "graphics.h"
#include "maths.h"

#define RAS_MAX_CLIP_OUT_VERTS 64

/**
 * @brief Result structure to describe the necessary clipping scenario
 * required for a face.
 *
 * @note: See https://gabrielgambetta.com/computer-graphics-from-scratch/11-clipping.html
 *
 */
typedef struct RasClipFaceScenario {
    /**
     * @brief Number of vertices in the clipping volume
     *
     * Scenario A: 1 vertex in
     * Scenario B: 2 vertices in
     */
    int num_in;
    int first_in;
    int second_in;
} RasClipFaceScenario;

/**
 * @brief Sets clip flags for the view frustum a vertex lies outside of, based
 * on its homogeneous clip-space position (x, y, z, w).
 *
 * @param view_frustum
 * @param aabb_flags
 * @param pv
 */
void core_set_pv_clip_flags(
    RasPipelineVertex* pv);

/**
 * @brief Gets the signed distance to one of the canonical clip planes.
 * Allows for clipping in homogeneous coordinates.
 *
 * If val > 0 → the point is inside the clip volume.
 * If val == 0 → point is exactly on the clipping boundary.
 * If val < 0 → the point is outside the clip volume.
 *
 * @param v
 * @param plane
 * @return RasFixed
 */
RasFixed core_eval_clip_plane(RasVector4f* v, RasFrustumPlane plane);

/**
 * @brief Sets a vector at the intersection of the given line and clip plane.
 * Uses homogeneous coordinates.
 *
 * @param v1 line point outside of plane
 * @param v2 line point inside of plane
 * @param plane
 * @param dest_vec
 * @returns true if intersection is valid
 */
bool core_get_line_clip_intersect(
    RasVector4f* v1, RasVector4f* v2, RasFrustumPlane plane, RasVector4f* dest_vec);

/**
 * @brief Perform Sutherland–Hodgman clipping.
 *
 * @param frustum
 * @param face_clip_flags
 * @param in_verts
 * @param out_verts
 * @param num_out_verts
 * @param max_out_verts
 */
void core_clip_face(
    RasClipSideMode side_mode,
    RasPipelineVertex* in_verts[3],
    RasPipelineVertex* out_verts,
    size_t* num_out_verts,
    size_t max_out_verts);

RasFrustumPlane* get_side_mode_planes(RasClipSideMode side_mode, size_t* len);

#endif
