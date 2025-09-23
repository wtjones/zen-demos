#ifndef CORE_CLIP_H
#define CORE_CLIP_H

#include "frustum.h"
#include "graphics.h"
#include "maths.h"

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

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv);

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
    RasFrustum* frustum,
    RasClipSideMode side_mode,
    RasClipFlags face_clip_flags,
    RasPipelineVertex* in_verts[3],
    RasPipelineVertex* out_verts,
    size_t* num_out_verts,
    size_t max_out_verts);

void core_clip_face_alt(
    RasFrustum* frustum,
    RasClipSideMode side_mode,
    RasClipFlags face_clip_flags,
    RasPipelineVertex* in_verts[3],
    RasPipelineVertex* out_verts,
    size_t* num_out_verts,
    size_t max_out_verts);

#endif
