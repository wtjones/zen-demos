#include "rasgl/core/clip.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/repr.h"

void core_set_pv_clip_flags(
    RasFrustum* view_frustum,
    RasClipFlags aabb_flags,
    RasPipelineVertex* pv)
{
    pv->clip_flags = 0;

    RasVector4f* p = &pv->clip_space_position;

    if (p->x < -p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_LEFT);
    }

    if (p->x > p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_RIGHT);
    }

    if (p->y > p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_BOTTOM);
    }

    if (p->y < -p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_TOP);
    }

    if (p->z < 0) {
        pv->clip_flags |= core_to_clip_flag(PLANE_NEAR);
    }

    if (p->z > p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_FAR);
    }
}

RasFixed core_eval_clip_plane(RasVector4f* v, RasFrustumPlane plane)
{
    switch (plane) {
    case PLANE_LEFT:
        return v->x + v->w;
    case PLANE_RIGHT:
        return v->w - v->x;
    case PLANE_BOTTOM:
        return v->w - v->y;
    case PLANE_TOP:
        return v->y + v->w;
    case PLANE_NEAR:
        return v->z;
    case PLANE_FAR:
        return v->w - v->z;
    }
    return 0;
}

void core_set_pv_clip_flags_vs(
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

bool core_get_line_clip_intersect(
    RasVector4f* v1, RasVector4f* v2, RasFrustumPlane plane, RasVector4f* dest_vec)
{
    RasFixed side1 = core_eval_clip_plane(v1, plane);
    RasFixed side2 = core_eval_clip_plane(v2, plane);

    if (side1 == 0) {
        *dest_vec = *v1;
        return true;
    }
    if (side2 == 0) {
        *dest_vec = *v2;
        return true;
    }

    // For intersection to be valid, vertices must be on opposite sides
    if ((side1 > 0 && side2 > 0) || (side1 < 0 && side2 < 0)) {
        ras_log_buffer_warn_ex(
            RAS_EVENT_INVALID_MATH,
            "Warning: both vertices on same side of clip plane");
        return false;
    }

    RasFixed scale = div_fixed_16_16_by_fixed_16_16(-side1, side2 - side1);

    dest_vec->x = v1->x + mul_fixed_16_16_by_fixed_16_16(v2->x - v1->x, scale);
    dest_vec->y = v1->y + mul_fixed_16_16_by_fixed_16_16(v2->y - v1->y, scale);
    dest_vec->z = v1->z + mul_fixed_16_16_by_fixed_16_16(v2->z - v1->z, scale);
    dest_vec->w = v1->w + mul_fixed_16_16_by_fixed_16_16(v2->w - v1->w, scale);
    return true;
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

/**
 * @brief Temp mode for testing new clipping algorithm
 *
 * @param side_mode
 * @param len
 * @return RasFrustumPlane*
 */
RasFrustumPlane* get_side_mode_planes_alt(RasClipSideMode side_mode, size_t* len)
{
    static RasFrustumPlane side_mode_planes[RAS_CLIP_SIDE_MODE_COUNT][6] = {
        { PLANE_NEAR, PLANE_FAR },
        { PLANE_BOTTOM }
    };

    *len = side_mode == RAS_CLIP_SIDE_VS ? 1 : 2;
    return side_mode_planes[side_mode];
}

/**
 * @brief Create a triangle fan from an n-gon.
 * Shared verts not currently supported.
 *
 * @param in_verts
 * @param num_in_verts
 * @param out_verts
 * @param num_out_verts
 */
void core_ngon_to_tris(
    RasPipelineVertex in_verts[6],
    size_t num_in_verts,
    RasPipelineVertex out_verts[12],
    size_t* num_out_verts)
{
    *num_out_verts = 0;

    if (num_in_verts < 3) {
        return;
    }

    for (size_t i = 1; i < num_in_verts - 1; i++) {
        RasPipelineVertex* v0 = &in_verts[0];
        RasPipelineVertex* v1 = &in_verts[i];
        RasPipelineVertex* v2 = &in_verts[i + 1];

        memcpy(&out_verts[*num_out_verts], v0, sizeof(RasPipelineVertex));
        (*num_out_verts)++;
        memcpy(&out_verts[*num_out_verts], v1, sizeof(RasPipelineVertex));
        (*num_out_verts)++;
        memcpy(&out_verts[*num_out_verts], v2, sizeof(RasPipelineVertex));
        (*num_out_verts)++;
    }
}

void core_clip_face_alt(
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
    static RasPipelineVertex work_in_verts[14];
    static RasPipelineVertex work_out_verts[14];
    memcpy(&work_in_verts[0], in_verts[0], sizeof(RasPipelineVertex));
    memcpy(&work_in_verts[1], in_verts[1], sizeof(RasPipelineVertex));
    memcpy(&work_in_verts[2], in_verts[2], sizeof(RasPipelineVertex));
    size_t num_in_verts = 3;

    size_t num_frustum_planes = 0;
    // FIXME: remove alt
    RasFrustumPlane* frustum_planes = get_side_mode_planes(
        side_mode, &num_frustum_planes);
    for (int32_t fpi = 0; fpi < num_frustum_planes; fpi++) {
        int32_t i = frustum_planes[fpi];
        RasPlane* plane = &frustum->planes[i];

        *num_out_verts = 0;
        RasPipelineVertex* dst_pv = NULL;

        // To allow for priming the first dest vert, start with first src vertex
        // that is inside the plane.
        int32_t si = 0;
        for (si = 0; si < num_in_verts; si++) {
            bool is_in = !(work_in_verts[si].clip_flags & core_to_clip_flag(i));
            if (is_in) {
                break;
            }
        }
        int32_t si_next = (si + 1) % num_in_verts;
        RasPipelineVertex* src_pv0 = &work_in_verts[si];
        RasPipelineVertex* src_pv1 = &work_in_verts[si_next];

        // Prime first dest vert
        dst_pv = &work_out_verts[0];
        memcpy(dst_pv, src_pv0, sizeof(RasPipelineVertex));
        (*num_out_verts) = 1;

        for (size_t j = 0; j < num_in_verts; j++) {

            bool src_pv0_is_in = !(src_pv0->clip_flags & core_to_clip_flag(i));
            bool src_pv1_is_in = !(src_pv1->clip_flags & core_to_clip_flag(i));

            if (!src_pv0_is_in && !src_pv1_is_in) {

                si = (si + 1) % num_in_verts;
                si_next = (si + 1) % num_in_verts;
                // Source edge verts
                src_pv0 = &work_in_verts[si];
                src_pv1 = &work_in_verts[si_next];

                continue;
            }

            if (src_pv0_is_in && src_pv1_is_in) {
                // Edge is fully in.

                // src_pv1: If not last src edge, add it.
                bool is_last_src_edge = (j == num_in_verts - 1);

                if (!is_last_src_edge) {
                    dst_pv = &work_out_verts[(*num_out_verts)];
                    (*num_out_verts)++;
                    if (*num_out_verts > sizeof(work_out_verts) / sizeof(work_out_verts[0])) {
                        ras_log_buffer("Exceeded max work_out_verts: %d", *num_out_verts);
                        ras_log_flush();
                        assert(false);
                    }
                    memcpy(dst_pv, src_pv1, sizeof(RasPipelineVertex));
                }

                si = (si + 1) % num_in_verts;
                si_next = (si + 1) % num_in_verts;
                // Source edge verts
                src_pv0 = &work_in_verts[si];
                src_pv1 = &work_in_verts[si_next];

                continue;
            }

            // Dest pv points to the 2nd vert of the dest edge.
            dst_pv = &work_out_verts[(*num_out_verts)];
            (*num_out_verts)++;
            if (*num_out_verts > sizeof(work_out_verts) / sizeof(work_out_verts[0])) {
                ras_log_buffer("Exceeded max work_out_verts: %d", *num_out_verts);
                ras_log_flush();
                assert(false);
            }

            /**
             * @brief Edge is one in, one out.
             *
             */
            if (src_pv0_is_in) {

                // Copy from a source vert
                // Does it matter which one?
                memcpy(dst_pv, src_pv0, sizeof(RasPipelineVertex));

                // Use midpoint for 2nd vert of dest edge
                bool math_result = core_get_line_clip_intersect(
                    &src_pv0->clip_space_position,
                    &src_pv1->clip_space_position,
                    i,
                    &dst_pv->clip_space_position);

                core_set_pv_clip_flags(
                    frustum,
                    dst_pv->aabb_clip_flags,
                    dst_pv);

                if (dst_pv->clip_flags & core_to_clip_flag(i)) {
                    ras_log_buffer("Not expecting clip flag %d to remain.", i);
                    dst_pv->clip_flags &= ~core_to_clip_flag(i);
                }

            } else {
                // pv1 is in:
                // - Add the intersection.
                // - If not the last src edge, add pv1.

                // Copy from a source vert
                // Does it matter which one?
                memcpy(dst_pv, src_pv1, sizeof(RasPipelineVertex));

                // Use midpoint for 1st vert of dest edge
                bool math_result = core_get_line_clip_intersect(
                    &src_pv1->clip_space_position,
                    &src_pv0->clip_space_position,
                    i,
                    &dst_pv->clip_space_position);

                core_set_pv_clip_flags(
                    frustum,
                    dst_pv->aabb_clip_flags,
                    dst_pv);

                if (dst_pv->clip_flags & core_to_clip_flag(i)) {
                    ras_log_buffer("Not expecting clip flag %d to remain.", i);
                    dst_pv->clip_flags &= ~core_to_clip_flag(i);
                }

                bool is_last_src_edge = (j == num_in_verts - 1);

                if (!is_last_src_edge) {
                    dst_pv = &work_out_verts[(*num_out_verts)];
                    (*num_out_verts)++;
                    if (*num_out_verts > sizeof(work_out_verts) / sizeof(work_out_verts[0])) {
                        ras_log_buffer("Exceeded max work_out_verts: %d", *num_out_verts);
                        ras_log_flush();
                        assert(false);
                    }
                    memcpy(dst_pv, src_pv1, sizeof(RasPipelineVertex));
                }
            }

            si = (si + 1) % num_in_verts;
            si_next = (si + 1) % num_in_verts;
            // Source edge verts
            src_pv0 = &work_in_verts[si];
            src_pv1 = &work_in_verts[si_next];

        } // for each edge

        memcpy(work_in_verts, work_out_verts, sizeof(RasPipelineVertex) * *num_out_verts);
        num_in_verts = *num_out_verts;
    }
    core_ngon_to_tris(work_in_verts, num_in_verts, out_verts, num_out_verts);
    ras_log_buffer("clipped to n-gon with %d verts fanned to %d", num_in_verts, *num_out_verts);
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
