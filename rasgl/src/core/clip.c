#include "rasgl/core/clip.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

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

void core_clip_face_scenario(
    RasFrustum* frustum,
    RasFrustumPlane side,
    RasPipelineMesh* mesh,
    uint32_t indexes[3],
    RasClipFaceScenario* scenario)
{
    assert(mesh->num_verts < MAX_PIPELINE_VERTS);
    RasPlane* plane = &frustum->planes[side];
    RasPipelineVertex* pipeline_verts = &mesh->verts[0];

    RasPipelineVertex* pv_0 = &pipeline_verts[indexes[0]];
    RasPipelineVertex* pv_1 = &pipeline_verts[indexes[1]];
    RasPipelineVertex* pv_2 = &pipeline_verts[indexes[2]];

    RasPipelineVertex* cur_pv = &pipeline_verts[indexes[0]];
    RasVector3f* cur_vert = &cur_pv->view_space_position;
    RasPipelineVertex* next_pv = &pipeline_verts[indexes[1]];
    RasVector3f* next_vert = &next_pv->view_space_position;
    char buffer[255];

    scenario->num_in = 0;
    scenario->first_in = -1;
    scenario->second_in = -1;

    for (int i = 0; i < 3; i++) {
        RasPipelineVertex* pv = &pipeline_verts[indexes[i]];
        bool is_in = !(pv->clip_flags & (1 << side));
        scenario->num_in += is_in ? 1 : 0;
        scenario->first_in = (scenario->first_in == -1 && scenario->num_in == 1 && is_in) ? i : scenario->first_in;
        scenario->second_in = (scenario->second_in == -1 && scenario->num_in == 2 && is_in) ? i : scenario->second_in;
    }

    ras_log_buffer("cpp: num_in: %d\n", scenario->num_in);
    ras_log_buffer("cpp: first_in: %d, 2nd_in: %d\n", scenario->first_in, scenario->second_in);
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
void core_clip_face_a(
    RasPlane* plane,
    RasPipelineVertex* verts,
    uint32_t* num_verts,
    uint32_t indexes[3],
    RasClipFaceScenario* scenario)
{

    // Identify vertex A
    int index_a = scenario->first_in;
    int index_b = scenario->first_in == 2 ? 0 : scenario->first_in + 1;
    int index_c = scenario->first_in == 0 ? 2 : scenario->first_in - 1;

    ras_log_buffer("clip_a: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

    int pv_a_index = indexes[index_a];
    int pv_b_index = indexes[index_b];
    int pv_c_index = indexes[index_c];

    RasPipelineVertex* pv_a = &verts[indexes[index_a]];
    RasPipelineVertex* pv_b = &verts[indexes[index_b]];
    RasPipelineVertex* pv_c = &verts[indexes[index_c]];

    // Allocate vertices B' and C' on the pv array
    uint32_t pv_b_alt_index = (*num_verts)++;
    RasPipelineVertex* pv_b_alt = &verts[pv_b_alt_index];
    uint32_t pv_c_alt_index = (*num_verts)++;
    RasPipelineVertex* pv_c_alt = &verts[pv_c_alt_index];

    pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;
    pv_c_alt->aabb_clip_flags = pv_c->aabb_clip_flags;

    core_get_line_plane_intersect(
        &pv_a->view_space_position,
        &pv_b->view_space_position,
        plane,
        &pv_b_alt->view_space_position);

    // Find C' to create side AC'
    core_get_line_plane_intersect(
        &pv_a->view_space_position,
        &pv_c->view_space_position,
        plane,
        &pv_c_alt->view_space_position);

    char buffer[255];
    ras_log_buffer("clip a: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));
    ras_log_buffer("clip a: pv_c_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_c_alt->view_space_position));

    // Connect A -> B'
    indexes[0] = pv_a_index;
    indexes[1] = pv_b_alt_index;

    // Connect B' -> C' -> A
    indexes[2] = pv_c_alt_index;
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
void core_clip_face_b(
    RasPlane* plane,
    RasPipelineMesh* mesh,
    uint32_t indexes[3],
    uint32_t face_index,
    RasClipFaceScenario* scenario)
{

    RasPipelineFace* original_face = &mesh->visible_faces[face_index];

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

    ras_log_buffer("cpp: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

    int pv_a_index = indexes[index_a];
    int pv_b_index = indexes[index_b];
    int pv_c_index = indexes[index_c];

    RasPipelineVertex* pv_a = &mesh->verts[indexes[index_a]];
    RasPipelineVertex* pv_b = &mesh->verts[indexes[index_b]];
    RasPipelineVertex* pv_c = &mesh->verts[indexes[index_c]];

    // Allocate vertices A' and B' on the pv array
    uint32_t pv_a_alt_index = mesh->num_verts++;
    RasPipelineVertex* pv_a_alt = &mesh->verts[pv_a_alt_index];
    uint32_t pv_b_alt_index = mesh->num_verts++;
    RasPipelineVertex* pv_b_alt = &mesh->verts[pv_b_alt_index];

    pv_a_alt->aabb_clip_flags = pv_a->aabb_clip_flags;
    pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;

    // Find A' to create side AA'
    core_get_line_plane_intersect(
        &pv_a->view_space_position,
        &pv_c->view_space_position,
        plane,
        &pv_a_alt->view_space_position);

    // Find B' to create side BB'
    core_get_line_plane_intersect(
        &pv_b->view_space_position,
        &pv_c->view_space_position,
        plane,
        &pv_b_alt->view_space_position);

    char buffer[255];
    ras_log_buffer("cpp: clip: pv_a_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_a_alt->view_space_position));
    ras_log_buffer("cpp: clip: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));

    uint32_t* vi = &mesh->num_visible_indexes;

    // The first face is the repurposed face.
    // Connect A -> A' -> B
    indexes[0] = pv_a_index;
    indexes[1] = pv_a_alt_index;
    indexes[2] = pv_b_index;

    // Connect A' -> B' -> B
    mesh->visible_indexes[(*vi)++] = pv_a_alt_index;
    mesh->visible_indexes[(*vi)++] = pv_b_alt_index;
    mesh->visible_indexes[(*vi)++] = pv_b_index;

    uint32_t* mi = &mesh->num_material_indexes;

    mesh->material_indexes[*mi] = mesh->material_indexes[face_index];
    (*mi)++;

    // Copy the original face to the new face
    uint32_t* num_dest_faces = &mesh->num_visible_faces;
    RasPipelineFace* new_face = &mesh->visible_faces[*num_dest_faces];
    (*num_dest_faces)++;
    memcpy(new_face, original_face, sizeof(RasPipelineFace));

    ras_log_buffer("clip_b: new face index: %d\n", *num_dest_faces - 1);
}

void core_clip_face(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RasPipelineMesh* mesh,
    uint32_t indexes[3],
    int32_t material_index,
    uint32_t face_index)
{
    for (uint8_t i = 0; i < FRUSTUM_PLANES; i++) {

        // recalc flags as the face may point to new verts
        RasPipelineVertex* pv1 = &mesh->verts[indexes[0]];
        RasPipelineVertex* pv2 = &mesh->verts[indexes[1]];
        RasPipelineVertex* pv3 = &mesh->verts[indexes[2]];
        face_clip_flags = pv1->clip_flags | pv2->clip_flags | pv3->clip_flags;

        uint8_t mask = 1 << i;
        if (face_clip_flags & mask) {
            ras_log_buffer("PV clipping against plane %d\n", i);

            RasClipFaceScenario scenario;

            core_clip_face_scenario(
                frustum, (RasFrustumPlane)i, mesh, indexes, &scenario);

            if (scenario.num_in == 1) {
                core_clip_face_a(
                    &frustum->planes[i],
                    mesh->verts,
                    &mesh->num_verts,
                    indexes,
                    &scenario);
            }

            else if (scenario.num_in == 2) {
                core_clip_face_b(
                    &frustum->planes[i],
                    mesh,
                    indexes,
                    face_index,
                    &scenario);
            } else {
                assert(true);
            }
        }
    }
}
