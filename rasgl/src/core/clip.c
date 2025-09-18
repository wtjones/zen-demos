#include "rasgl/core/clip.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/repr.h"

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv)
{
    pv->clip_flags = 0;

    if (aabb_flags & core_to_clip_flag(PLANE_NEAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_NEAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_NEAR)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_FAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_FAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_FAR)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_LEFT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_LEFT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_LEFT)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_RIGHT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_RIGHT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_RIGHT)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_TOP)) {
        RasPlane* plane = &view_frustum->planes[PLANE_TOP];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_TOP)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_BOTTOM)) {
        RasPlane* plane = &view_frustum->planes[PLANE_BOTTOM];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position) == RAS_PLANE_SIDE_A;
        pv->clip_flags |= outside
            ? core_to_clip_flag(PLANE_BOTTOM)
            : 0;
    }
}

void core_clip_face_scenario(
    RasFrustumPlane side,
    RasPipelineVertex in_verts[3],
    RasClipFaceScenario* scenario)
{
    char buffer[255];

    scenario->num_in = 0;
    scenario->first_in = -1;
    scenario->second_in = -1;

    for (int i = 0; i < 3; i++) {
        RasPipelineVertex* pv = &in_verts[i];
        bool is_in = !(pv->clip_flags & core_to_clip_flag(side));

        if (is_in) {
            if (scenario->first_in == -1) {
                scenario->first_in = i;
            } else if (scenario->second_in == -1) {
                scenario->second_in = i;
            }
            scenario->num_in += 1;
        }
    }

    ras_log_buffer("scenario: num_in: %d\n", scenario->num_in);
    ras_log_buffer("scenario: first_in: %d, 2nd_in: %d\n", scenario->first_in, scenario->second_in);
}

/**
 * scenario: 1 vertex in
 * Identify verts ABC:
 * A = in vertex
 * B = out vertex left of A
 * C = out vertex right of A
 *
 * From https://gabrielgambetta.com/computer-graphics-from-scratch/11-clipping.html
 * > Let A be the vertex of the triangle ABC that is in front of the plane.
 * > In this case, we discard ABC, and add a new triangle AB′C′, where B′ and C′
 * > are the intersections of AB and AC with the clipping plane
 */
int32_t core_clip_face_a(
    RasPlane* plane,
    RasPipelineVertex in_verts[3],
    RasPipelineVertex out_verts[3],
    RasClipFaceScenario* scenario)
{

    // Identify vertex A
    int index_a = scenario->first_in;
    int index_b = scenario->first_in == 2 ? 0 : scenario->first_in + 1;
    int index_c = scenario->first_in == 0 ? 2 : scenario->first_in - 1;

    ras_log_buffer("clip_a: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

    RasPipelineVertex* pv_a = &in_verts[index_a];
    RasPipelineVertex* pv_b = &in_verts[index_b];
    RasPipelineVertex* pv_c = &in_verts[index_c];

    // Copy pv_a to the first element of the output.
    memcpy(&out_verts[0], pv_a, sizeof(RasPipelineVertex));

    // Allocate vertices B' and C' on the output array.
    uint32_t pv_b_alt_index = 1;
    RasPipelineVertex* pv_b_alt = &out_verts[pv_b_alt_index];
    uint32_t pv_c_alt_index = 2;
    RasPipelineVertex* pv_c_alt = &out_verts[pv_c_alt_index];

    pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;
    pv_c_alt->aabb_clip_flags = pv_c->aabb_clip_flags;

    if (!core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_b->view_space_position,
            plane,
            &pv_b_alt->view_space_position)) {
        return 0;
    }

    // Find C' to create side AC'
    if (!core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_c_alt->view_space_position)) {
        return 0;
    }

    char buffer[255];
    ras_log_buffer("clip a2: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));
    ras_log_buffer("clip a2: pv_c_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_c_alt->view_space_position));
    return 3;
}

/**
 * scenario: 2 vertices in
 * Identify verts ABC:
 * A = in vertex
 * B = in vertex
 * C = out vertex
 *
 * From https://gabrielgambetta.com/computer-graphics-from-scratch/11-clipping.html
 * > Let A and B be the vertices of the triangle ABC that are in front of the plane.
 * > In this case, we discard ABC and add two new triangles: ABA′ and A′BB′,
 * > where A′ and B′ are the intersections of AC and BC with the clipping plane.
 *
 */
int32_t core_clip_face_b(
    RasPlane* plane,
    RasPipelineVertex in_verts[3],
    RasPipelineVertex out_verts[6],
    RasClipFaceScenario* scenario)
{
    // Identify vertex A
    int index_a = -1, index_b = -1, index_c = -1;

    if (scenario->first_in == 0) {
        if (scenario->second_in == 1) {
            index_a = 1;
            index_b = 0;
            index_c = 2;
        } else {
            index_a = 0;
            index_b = 2;
            index_c = 1;
        }
    } else if (scenario->first_in == 1) {
        if (scenario->second_in == 2) {
            index_a = 2;
            index_b = 1;
            index_c = 0;
        } else {
            index_a = 1;
            index_b = 0;
            index_c = 2;
        }
    } else if (scenario->first_in == 2) {
        if (scenario->second_in == 0) {
            index_a = 0;
            index_b = 2;
            index_c = 1;
        } else {
            index_a = 2;
            index_b = 1;
            index_c = 0;
        }
    } else {
        assert(true);
    }

    ras_log_buffer("clip_face_b: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

    RasPipelineVertex* pv_a = &in_verts[index_a];
    RasPipelineVertex* pv_b = &in_verts[index_b];
    RasPipelineVertex* pv_c = &in_verts[index_c];

    // Desired structure of out_verts
    // Face 0: A -> A' -> B
    // [0] pv_a
    // [1] pv_alt_a
    // [2] pv_b
    // Face 1: A' -> B' -> B
    // [3] pv_alt_a
    // [4] pv_alt_b
    // [5] pv_b

    // Copy pv_a, pv_b the output.
    memcpy(&out_verts[0], pv_a, sizeof(RasPipelineVertex));
    memcpy(&out_verts[2], pv_b, sizeof(RasPipelineVertex));
    memcpy(&out_verts[5], pv_b, sizeof(RasPipelineVertex));

    // Allocate vertices A' and B' on the pv array
    RasPipelineVertex* pv_a_alt = &out_verts[1];
    RasPipelineVertex* pv_b_alt = &out_verts[4];

    pv_a_alt->aabb_clip_flags = pv_a->aabb_clip_flags;
    pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;

    // Find A' to create side AA'
    if (!core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_a_alt->view_space_position)) {
        return 0;
    }

    // Find B' to create side BB'
    if (!core_get_line_plane_intersect(
            &pv_b->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_b_alt->view_space_position)) {
        return 0;
    }

    char buffer[255];
    ras_log_buffer("cpp: clip: pv_a_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_a_alt->view_space_position));
    ras_log_buffer("cpp: clip: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));

    // Copy A' vert from first face to 2nd
    memcpy(&out_verts[3], pv_a_alt, sizeof(RasPipelineVertex));
    return 6;
}

RasFrustumPlane* get_side_mode_planes(RasClipSideMode side_mode, size_t* len)
{
    static RasFrustumPlane side_mode_planes[RAS_CLIP_SIDE_MODE_COUNT][6] = {
        { PLANE_NEAR, PLANE_FAR },
        { PLANE_NEAR,
            PLANE_FAR,
            PLANE_LEFT,
            PLANE_RIGHT,
            PLANE_BOTTOM,
            PLANE_TOP }
    };

    *len = side_mode == RAS_CLIP_SIDE_VS ? 6 : 2;
    return side_mode_planes[side_mode];
}

void core_clip_face(
    RasFrustum* frustum,
    RasClipSideMode side_mode,
    RasClipFlags face_clip_flags,
    RasPipelineVertex* in_verts[3],
    RasPipelineVertex* out_verts,
    size_t* num_out_verts,
    size_t max_out_verts)

{
    /**
     * // Sutherland–Hodgman pseudocode
     * triangles_in = [original triangle]
     *
     *  for each plane in frustum:
     *      triangles_out = []
     *
     *      for each tri in triangles_in:
     *          clipped = clip_triangle_against_plane(tri, plane)
     *          for each t in clipped:   // 0, 1, or 2 triangles
     *              triangles_out.append(t)
     *
     *      triangles_in = triangles_out
     */
    static RasPipelineVertex work_in_verts[RAS_MAX_MODEL_VERTS];
    static RasPipelineVertex work_out_verts[RAS_MAX_MODEL_VERTS];
    memcpy(&work_in_verts[0], in_verts[0], sizeof(RasPipelineVertex));
    memcpy(&work_in_verts[1], in_verts[1], sizeof(RasPipelineVertex));
    memcpy(&work_in_verts[2], in_verts[2], sizeof(RasPipelineVertex));
    size_t num_in_verts = 3;

    size_t num_frustum_planes = 0;
    RasFrustumPlane* frustum_planes = get_side_mode_planes(side_mode, &num_frustum_planes);
    for (int32_t fpi = 0; fpi < num_frustum_planes; fpi++) {
        int32_t i = frustum_planes[fpi];
        RasPlane* plane = &frustum->planes[i];

        *num_out_verts = 0;

        for (size_t j = 0; j < num_in_verts; j += 3) {
            RasPipelineVertex* pv0 = &work_in_verts[j];
            RasPipelineVertex* pv1 = &work_in_verts[j + 1];
            RasPipelineVertex* pv2 = &work_in_verts[j + 2];

            RasClipFaceScenario scenario;
            core_clip_face_scenario(
                (RasFrustumPlane)i,
                &work_in_verts[j],
                &scenario);

            if (scenario.num_in == 1) {

                int32_t result = core_clip_face_a(
                    plane,
                    &work_in_verts[j],
                    &work_out_verts[*num_out_verts],
                    &scenario);

                if (result == 0) {
                    continue;
                }
                *num_out_verts += result;

                core_set_pv_clip_flags(
                    frustum,
                    work_out_verts[1].aabb_clip_flags,
                    &work_out_verts[1]);

                if (work_out_verts[1].clip_flags & core_to_clip_flag(i)) {
                    ras_log_buffer("Not expecting clip flag %d to remain.", i);
                    work_out_verts[1].clip_flags &= ~core_to_clip_flag(i);
                }

                core_set_pv_clip_flags(
                    frustum,
                    work_out_verts[2].aabb_clip_flags,
                    &work_out_verts[2]);

                if (work_out_verts[2].clip_flags & core_to_clip_flag(i)) {
                    ras_log_buffer("Not expecting clip flag %d to remain.", i);
                    work_out_verts[2].clip_flags &= ~core_to_clip_flag(i);
                }

            } else if (scenario.num_in == 2) {

                int32_t result = core_clip_face_b(
                    plane,
                    &work_in_verts[j],
                    &work_out_verts[*num_out_verts],
                    &scenario);

                *num_out_verts += result;

                if (result == 0) {
                    continue;
                }
                uint32_t new_verts[] = { 1, 3, 4 };

                for (uint32_t k = 0; k < 3; k++) {
                    core_set_pv_clip_flags(
                        frustum,
                        work_out_verts[new_verts[k]].aabb_clip_flags,
                        &work_out_verts[new_verts[k]]);

                    if (work_out_verts[new_verts[k]].clip_flags & core_to_clip_flag(i)) {
                        ras_log_buffer("Not expecting clip flag %d to remain.", i);
                        work_out_verts[new_verts[k]].clip_flags &= ~core_to_clip_flag(i);
                    }
                }

            } else if (scenario.num_in == 3) {
                ras_log_buffer("clip2: num_in: %d\n", scenario.num_in);
                ras_log_buffer("clip2: first_in: %d, 2nd_in: %d\n", scenario.first_in, scenario.second_in);
                memcpy(
                    &work_out_verts[*num_out_verts],
                    &work_in_verts[j],
                    sizeof(RasPipelineVertex) * 3);
                *num_out_verts += 3;
            }
        }

        memcpy(work_in_verts, work_out_verts, sizeof(RasPipelineVertex) * *num_out_verts);
        num_in_verts = *num_out_verts;
    }

    memcpy(out_verts, work_in_verts, sizeof(RasPipelineVertex) * num_in_verts);
}
