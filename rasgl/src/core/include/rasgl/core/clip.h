#ifndef CORE_CLIP_H
#define CORE_CLIP_H

#include "aabb.h"
#include "frustum.h"
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

void core_clip_face_scenario(
    RasFrustum* frustum,
    RasFrustumPlane side,
    RasPipelineMesh* mesh,
    uint32_t indexes[3],
    // output
    RasClipFaceScenario* scenario);

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv);

void core_clip_face(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RasPipelineMesh* mesh,
    uint32_t in_indexes[3],
    int32_t material_index,
    uint32_t face_index);

#endif
