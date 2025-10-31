#include "rasgl/core/clip.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/repr.h"

void core_set_pv_clip_flags(
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

    if (p->y < -p->w) {
        pv->clip_flags |= core_to_clip_flag(PLANE_BOTTOM);
    }

    if (p->y > p->w) {
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
    // >= 0 means inside the clip volume
    switch (plane) {
    case PLANE_LEFT:
        return v->x + v->w;
    case PLANE_RIGHT:
        return v->w - v->x;
    case PLANE_BOTTOM:
        return v->y + v->w;
    case PLANE_TOP:
        return v->w - v->y;
    case PLANE_NEAR:
        return v->z;
    case PLANE_FAR:
        return v->w - v->z;
    }
    return 0;
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
        { PLANE_NEAR,
            PLANE_LEFT,
            PLANE_RIGHT,
            PLANE_BOTTOM,
            PLANE_TOP },
        { PLANE_NEAR,
            PLANE_FAR,
            PLANE_LEFT,
            PLANE_RIGHT,
            PLANE_BOTTOM,
            PLANE_TOP },
        { PLANE_NEAR }
    };

    static size_t sizes[RAS_CLIP_SIDE_MODE_COUNT + 1] = {
        5, 6, 1, 0
    };

    *len = sizes[side_mode];
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

void core_clip_face(
    RasClipSideMode side_mode,
    RasPipelineVertex* in_verts[3],
    RasPipelineVertex* out_verts,
    size_t* num_out_verts,
    size_t max_out_verts)

{
    static RasPipelineVertex work_in_verts[14];
    static RasPipelineVertex work_out_verts[14];
    memcpy(&work_out_verts[0], in_verts[0], sizeof(RasPipelineVertex));
    memcpy(&work_out_verts[1], in_verts[1], sizeof(RasPipelineVertex));
    memcpy(&work_out_verts[2], in_verts[2], sizeof(RasPipelineVertex));
    *num_out_verts = 3;
    size_t num_in_verts = 0;

    size_t num_frustum_planes = 0;
    // FIXME: remove alt
    RasFrustumPlane* frustum_planes = get_side_mode_planes(
        side_mode, &num_frustum_planes);
    for (int32_t fpi = 0; fpi < num_frustum_planes; fpi++) {
        int32_t i = frustum_planes[fpi];

        // Sutherland–Hodgman pseudocode
        //
        // List inputList = outputList;
        memcpy(work_in_verts, work_out_verts, sizeof(RasPipelineVertex) * *num_out_verts);
        num_in_verts = *num_out_verts;
        // outputList.clear();
        *num_out_verts = 0;

        for (size_t j = 0; j < num_in_verts; j++) {

            // Sutherland–Hodgman pseudocode
            //
            // Point current_point = inputList[i];
            RasPipelineVertex* current_pv = &work_in_verts[j];

            // Point prev_point = inputList[(i − 1) % inputList.count];
            RasPipelineVertex* prev_pv = &work_in_verts[(j + num_in_verts - 1) % num_in_verts];

            bool current_is_in = core_eval_clip_plane(&current_pv->clip_space_position, i) >= 0;
            bool prev_is_in = core_eval_clip_plane(&prev_pv->clip_space_position, i) >= 0;

            // Sutherland–Hodgman pseudocode
            //
            //  if (current_point inside clipEdge) then
            //      if (prev_point not inside clipEdge) then
            //          outputList.add(Intersecting_point);
            //      end if
            //      outputList.add(current_point);

            // else if (prev_point inside clipEdge) then
            //     outputList.add(Intersecting_point);
            // end if

            if (current_is_in) {
                if (prev_is_in == false) {
                    RasPipelineVertex intersect_pv;
                    memcpy(&intersect_pv, current_pv, sizeof(RasPipelineVertex));
                    core_get_line_clip_intersect(
                        &prev_pv->clip_space_position,
                        &current_pv->clip_space_position,
                        i,
                        &intersect_pv.clip_space_position);
                    core_set_pv_clip_flags(&intersect_pv);
                    if (intersect_pv.clip_flags & core_to_clip_flag(i)) {
                        ras_log_buffer("Not expecting clip flag %d to remain.", i);
                        intersect_pv.clip_flags &= ~core_to_clip_flag(i);
                    }
                    memcpy(&work_out_verts[*num_out_verts], &intersect_pv, sizeof(RasPipelineVertex));
                    (*num_out_verts)++;
                }
                memcpy(&work_out_verts[*num_out_verts], current_pv, sizeof(RasPipelineVertex));
                (*num_out_verts)++;
            } else if (prev_is_in) {
                RasPipelineVertex intersect_pv;
                memcpy(&intersect_pv, current_pv, sizeof(RasPipelineVertex));
                core_get_line_clip_intersect(
                    &prev_pv->clip_space_position,
                    &current_pv->clip_space_position,
                    i,
                    &intersect_pv.clip_space_position);
                core_set_pv_clip_flags(&intersect_pv);
                if (intersect_pv.clip_flags & core_to_clip_flag(i)) {
                    ras_log_buffer("Not expecting clip flag %d to remain.", i);
                    intersect_pv.clip_flags &= ~core_to_clip_flag(i);
                }
                memcpy(&work_out_verts[*num_out_verts], &intersect_pv, sizeof(RasPipelineVertex));
                (*num_out_verts)++;
            }
            if (*num_out_verts >= max_out_verts) {
                ras_log_buffer("Exceeded max_out_verts: %d", *num_out_verts);
                ras_log_flush();
                assert(false);
            }
        } // for each edge
    }

    core_ngon_to_tris(work_out_verts, *num_out_verts, out_verts, num_out_verts);

    ras_log_buffer_trace("clipped to n-gon with %d verts fanned to %d", num_in_verts, *num_out_verts);
}
