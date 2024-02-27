#include "rasgl/core/graphics.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

void core_aabb_init(RasAABB* aabb)
{
    aabb->min.x = RAS_FIXED_MAX;
    aabb->min.y = RAS_FIXED_MAX;
    aabb->min.z = RAS_FIXED_MAX;

    aabb->max.x = RAS_FIXED_MIN;
    aabb->max.y = RAS_FIXED_MIN;
    aabb->max.z = RAS_FIXED_MIN;
}

void core_aabb_to_points(RasAABB* aabb, RasVector3f points[8])
{
    // Considering right-handed system (see README.md), min is point 4, max is point 3.
    //
    //     6- - - - -  7
    //    /|          /|
    // 2/ -|- - - -3/  |
    // |   |       |   |
    // |   |       |   |
    // |   4- - - -| - 5
    // |  /        |  /
    // 0/ - - - - -1/

    points[0].x = aabb->min.x;
    points[0].y = aabb->min.y;
    points[0].z = aabb->max.z;

    points[1].x = aabb->max.x;
    points[1].y = aabb->min.y;
    points[1].z = aabb->max.z;

    points[2].x = aabb->min.x;
    points[2].y = aabb->max.y;
    points[2].z = aabb->max.z;

    points[3].x = aabb->max.x;
    points[3].y = aabb->max.y;
    points[3].z = aabb->max.z;

    points[4].x = aabb->min.x;
    points[4].y = aabb->min.y;
    points[4].z = aabb->min.z;

    points[5].x = aabb->max.x;
    points[5].y = aabb->min.y;
    points[5].z = aabb->min.z;

    points[6].x = aabb->min.x;
    points[6].y = aabb->max.y;
    points[6].z = aabb->min.z;

    points[7].x = aabb->max.x;
    points[7].y = aabb->max.y;
    points[7].z = aabb->min.z;
}

void core_aabb_xform(RasAABB* aabb, RasFixed matrix[4][4], RasAABB* dest)
{
    RasFixed vec_src[4];
    RasFixed vec_dest[4];

    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasVector3f points_rotated[RAS_MAX_AABB_POINTS];
    core_aabb_init(dest);

    // Rotate the 8 points of the box to get the full extent of the resulting box
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        core_vector3f_to_4x1(&points[i], vec_src);
        mat_mul_4x4_4x1(matrix, vec_src, vec_dest);
        core_4x1_to_vector3f(vec_dest, &points_rotated[i]);

        dest->min.x = vec_dest[0] < dest->min.x
            ? vec_dest[0]
            : dest->min.x;
        dest->min.y = vec_dest[1] < dest->min.y
            ? vec_dest[1]
            : dest->min.y;
        dest->min.z = vec_dest[2] < dest->min.z
            ? vec_dest[2]
            : dest->min.z;

        dest->max.x = vec_dest[0] > dest->max.x
            ? vec_dest[0]
            : dest->max.x;
        dest->max.y = vec_dest[1] > dest->max.y
            ? vec_dest[1]
            : dest->max.y;
        dest->max.z = vec_dest[2] > dest->max.z
            ? vec_dest[2]
            : dest->max.z;
    }
}

bool core_aabb_in_frustum(RasAABB* aabb, RasFrustum* frustum, RasClipFlags* flags)
{
    bool all_out = true;
    RasVector3f points[RAS_MAX_AABB_POINTS];
    *flags = 0;
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        RasClipFlags point_flags = core_point_in_frustum_planes(frustum, &points[i]);
        *flags = *flags | point_flags;
        all_out = point_flags == 0 ? false : all_out;
    }
    return all_out;
}

void core_renderstate_init(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
    state->current_frame = 0;
    state->max_frames = UINT32_MAX;
    state->projection_mode = RAS_PERSPECTIVE_MATRIX;
    state->backface_culling_mode = RAS_BACKFACE_CULLING_ON;
};

void core_renderstate_clear(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
}

void projected_to_screen_point(RasFixed screen_width, RasFixed screen_height, RasFixed projected_point[4], Point2i* screen_point)
{

    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width);
    screen_point->y = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height);
}

/**
 * Determine if poly is backfacing based on the normal's angle to the viewer
 * in screen space. Assumes vertices are counter-clockwise.
 * Based on https://github.com/wtjones/qbasic/blob/master/POLY3D.BAS
 */
bool core_is_backface(RasPipelineVertex* pipeline_verts, uint32_t indexes[3])
{
    RasPipelineVertex* pv0 = &pipeline_verts[indexes[2]];
    RasVector4f* sv0 = &pv0->screen_space_position;
    RasPipelineVertex* pv1 = &pipeline_verts[indexes[1]];
    RasVector4f* sv1 = &pv1->screen_space_position;
    RasPipelineVertex* pv2 = &pipeline_verts[indexes[0]];
    RasVector4f* sv2 = &pv2->screen_space_position;

    // norm1 = (1.x - 0.x) * (0.y - 2.y)
    // norm2 = (1.y - 0.y) * (0.x - 2.x)
    RasFixed norm1 = mul_fixed_16_16_by_fixed_16_16(sv1->x - sv0->x, sv0->y - sv2->y);
    RasFixed norm2 = mul_fixed_16_16_by_fixed_16_16(sv1->y - sv0->y, sv0->x - sv2->x);
    RasFixed norm = norm1 - norm2;
    return (norm1 - norm2 > 0);
}

/**
 * Sets the clip flags of a pipeline vert based on AABB clip flags.
 * Near and far planes checked in view space. The rest in screen space.
 */
void core_set_pv_clip_flags_alt(RasFrustum* view_frustum, RasClipFlags aabb_flags, RasPipelineVertex* pv)
{
    RasFixed right_x = INT_32_TO_FIXED_16_16(320);  // FIXME
    RasFixed bottom_y = INT_32_TO_FIXED_16_16(240); // FIXME

    pv->clip_flags = 0;

    if (aabb_flags & (1 << PLANE_NEAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_NEAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_NEAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_FAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_FAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_FAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_LEFT)) {
        pv->clip_flags |= pv->screen_space_position.x <= 0
            ? (1 << PLANE_LEFT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_RIGHT)) {
        pv->clip_flags |= pv->screen_space_position.x >= right_x
            ? (1 << PLANE_RIGHT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_TOP)) {
        pv->clip_flags |= pv->screen_space_position.y <= 0
            ? (1 << PLANE_TOP)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_BOTTOM)) {
        pv->clip_flags |= pv->screen_space_position.x >= bottom_y
            ? (1 << PLANE_BOTTOM)
            : 0;
    }
}

/**
 * Sets the clip flags of a pipeline vert based on AABB clip flags.
 * Planes checked in view space.
 */
void core_set_pv_clip_flags(RasFrustum* view_frustum, RasClipFlags aabb_flags, RasPipelineVertex* pv)
{
    pv->clip_flags = 0;

    if (aabb_flags & (1 << PLANE_NEAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_NEAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_NEAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_FAR)) {
        RasPlane* plane = &view_frustum->planes[PLANE_FAR];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_FAR)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_LEFT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_LEFT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_LEFT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_RIGHT)) {
        RasPlane* plane = &view_frustum->planes[PLANE_RIGHT];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_RIGHT)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_TOP)) {
        RasPlane* plane = &view_frustum->planes[PLANE_TOP];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_TOP)
            : 0;
    }

    if (aabb_flags & (1 << PLANE_BOTTOM)) {
        RasPlane* plane = &view_frustum->planes[PLANE_BOTTOM];
        bool outside = core_plane_vector_side(plane, &pv->view_space_position);
        pv->clip_flags |= outside
            ? (1 << PLANE_BOTTOM)
            : 0;
    }
}

/**
 * @brief Sets a vector at the intersection of the given line and plane.
 *
 * @param v1 line point outside of plane
 * @param v2 line point inside of plane
 * @param plane
 * @param dest_vec
 */
void core_get_line_plane_intersect(
    RasVector3f* v1, RasVector3f* v2, RasPlane* plane, RasVector3f* dest_vec)
{
    RasFixed pv_a_dot = core_dot_product(v1, &plane->normal);
    RasFixed pv_b_dot = core_dot_product(v2, &plane->normal);

    bool pv_a_in = pv_a_dot >= plane->distance;
    bool pv_b_in = pv_b_dot >= plane->distance;
    assert(pv_a_in);
    assert(!pv_b_in);

    RasFixed scale = div_fixed_16_16_by_fixed_16_16(plane->distance - pv_a_dot, pv_b_dot - pv_a_dot);

    RasVector3f* cur_vert = v1;
    RasVector3f* next_vert = v2;
    dest_vec->x = cur_vert->x + (mul_fixed_16_16_by_fixed_16_16(next_vert->x - cur_vert->x, scale));
    dest_vec->y = cur_vert->y + (mul_fixed_16_16_by_fixed_16_16(next_vert->y - cur_vert->y, scale));
    dest_vec->z = cur_vert->z + (mul_fixed_16_16_by_fixed_16_16(next_vert->z - cur_vert->z, scale));
}

void core_clip_poly_plane(
    RasPlane* plane,
    RasFrustumPlane side,
    RenderState* render_state,
    uint32_t indexes[3])
{
    RasPipelineVertex* pipeline_verts = &render_state->pipeline_verts[0];
    RasPipelineVertex* pv_0 = &pipeline_verts[indexes[0]];
    RasPipelineVertex* pv_1 = &pipeline_verts[indexes[1]];
    RasPipelineVertex* pv_2 = &pipeline_verts[indexes[1]];

    RasPipelineVertex* cur_pv = &pipeline_verts[indexes[0]];
    RasVector3f* cur_vert = &cur_pv->view_space_position;
    RasPipelineVertex* next_pv = &pipeline_verts[indexes[1]];
    RasVector3f* next_vert = &next_pv->view_space_position;
    char buffer[255];

    int num_in = 0;
    int first_in = -1, second_in = -1;
    for (int i = 0; i < 3; i++) {
        RasPipelineVertex* pv = &pipeline_verts[indexes[i]];
        bool is_in = !(pv->clip_flags & side);
        num_in += is_in ? 1 : 0;
        first_in = (first_in == -1 && num_in == 1 && is_in) ? i : first_in;
        second_in = (second_in == -1 && num_in == 2 && is_in) ? i : second_in;
    }

    ras_log_buffer("cpp: num_in: %d\n", num_in);
    ras_log_buffer("cpp: first_in: %d, 2nd_in: %d\n", first_in, second_in);

    /**
     * scenario: 1 vertex in
     * Identify verts ABC:
     * A = in vertex
     * B = out vertex left of A
     * C = out vertext right of A
     *
     * From https://gabrielgambetta.com/computer-graphics-from-scratch/11-clipping.html
     * > Let A be the vertex of the triangle ABC that is in front of the plane.
     * > In this case, we discard ABC, and add a new triangle AB′C′, where B′ and C′
     * > are the intersections of AB and AC with the clipping plane
     */
    if (num_in == 1) {

        // Identify vertex A

        int index_a = first_in;
        int index_b = first_in == 2 ? 0 : first_in + 1;
        int index_c = first_in == 0 ? 2 : first_in - 1;

        ras_log_buffer("cpp: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

        RasPipelineVertex* pv_a
            = &pipeline_verts[indexes[index_a]];
        RasPipelineVertex* pv_b = &pipeline_verts[indexes[index_b]];
        RasPipelineVertex* pv_c = &pipeline_verts[indexes[index_c]];

        // Allocate vertices B' and C' on the pv array
        uint32_t pv_b_alt_index = render_state->num_pipeline_verts++;
        RasPipelineVertex* pv_b_alt = &render_state->pipeline_verts[pv_b_alt_index];
        uint32_t pv_c_alt_index = render_state->num_pipeline_verts++;
        RasPipelineVertex* pv_c_alt = &render_state->pipeline_verts[pv_c_alt_index];

        // FIME: need to set other values on vertex, like screen pos

        // Find B' to create side AB'
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

        ras_log_buffer("cpp: clip: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));

        // Connect A -> B'
        uint32_t* vi = &render_state->num_visible_indexes;
        render_state->visible_indexes[(*vi)++] = index_a;
        render_state->visible_indexes[(*vi)++] = pv_b_alt_index;

        // Connect B' -> C' -> A
        render_state->visible_indexes[(*vi)++] = pv_c_alt_index;
    }
}

void core_clip_poly(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RenderState* render_state,
    uint32_t indexes[3])
{
    for (uint8_t i = 0; i < FRUSTUM_PLANES; i++) {
        uint8_t mask = 1 << i;
        if (face_clip_flags & mask) {
            ras_log_buffer("PV clipping against plane %d\n", i);
            core_clip_poly_plane(&frustum->planes[i], (RasFrustumPlane)i + 1, render_state, indexes);
        }
    }
}

/**
 * Add a screen-space point to the command list.
 */
void core_render_point(RenderState* render_state, RasVector4f* screen_space_position)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;

    screen_pos = &render_state->points[*num_points];
    screen_pos->x = FIXED_16_16_TO_INT_32(screen_space_position->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(screen_space_position->y);

    render_state->commands[*num_commands].num_points = 1;
    render_state->commands[*num_commands].point_indices[0] = *num_points;

    (*num_points)++;
    (*num_commands)++;
}

void core_projected_to_screen_point(int32_t screen_width, int32_t screen_height, RasFixed projected_point[4], RasVector4f* screen_point)
{
    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width;
    screen_point->y = mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height;
    screen_point->z = projected_point[2];
    screen_point->w = projected_point[3];
}

void core_render_aabb(
    RenderState* render_state,
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum,
    RasAABB* aabb)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    core_aabb_to_points(aabb, points);
    uint32_t num_points = 0;
    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        if (!core_point_in_frustum(frustum, &points[i])) {
            continue;
        }
        RasFixed view_vec[4];
        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&points[i], view_vec);

        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_vec, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        core_render_point(render_state, &screen_space_position);
        num_points++;
    }
    ras_log_buffer("AABB points rendered: %d\n", num_points);
}

void core_draw_element(
    RenderState* render_state,
    RasPipelineElement* element,
    RasFixed model_world_matrix[4][4],
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4],
    RasFrustum* frustum)
{
    char buffer[1000];
    char buffer2[1000];
    RasFixed model_view_matrix[4][4];
    RasFixed combined_matrix[4][4];
    RasFixed dest_vec[4];

    // model -> view transform
    mat_mul_4x4_4x4(world_view_matrix, model_world_matrix, model_view_matrix);

    // model/view -> projection
    mat_mul_4x4_4x4(proj_matrix, model_view_matrix, combined_matrix);

    // translate AABB to view space
    RasAABB view_aabb;
    core_aabb_xform(&element->aabb, model_view_matrix, &view_aabb);

    ras_log_buffer("AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_buffer("AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.min));
    ras_log_buffer("AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.max));

    RasClipFlags clip_flags = 0;
    bool all_out = core_aabb_in_frustum(&view_aabb, frustum, &clip_flags);

    ras_log_buffer("AABB flags: %hhu, all_out: %s\n", clip_flags, all_out ? "true" : "false");

    if (all_out) {
        return;
    }

    core_render_aabb(render_state, proj_matrix, frustum, &view_aabb);
    uint32_t num_verts_in_frustum = 0;
    for (uint32_t i = 0; i < element->num_verts; i++) {
        RasVertex* vertex = &element->verts[i];
        RasPipelineVertex* pv = &render_state->pipeline_verts[i];

        RasFixed model_space_position[4];
        RasFixed view_space_position[4];
        RasFixed screen_space_vec[4];
        RasFixed projected_vec[4];

        core_vector3f_to_4x1(&vertex->position, model_space_position);
        mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
        core_4x1_to_vector3f(view_space_position, &pv->view_space_position);

        core_set_pv_clip_flags(frustum, clip_flags, pv);

        num_verts_in_frustum
            += pv->clip_flags == 0 ? 1 : 0;

        pv->color = vertex->color;
        pv->u = vertex->u;
        pv->v = vertex->v;

        render_state->num_pipeline_verts++;
    }

    render_state->num_visible_indexes = 0;
    uint32_t num_faces_visible = 0;
    uint32_t num_faces_in_frustum = 0;
    uint32_t num_faces_must_clip = 0;
    uint32_t* vi = &render_state->num_visible_indexes;
    for (uint32_t i = 0; i < element->num_indexes; i += 3) {
        RasPipelineVertex* pv1 = &render_state->pipeline_verts[element->indexes[i]];
        RasPipelineVertex* pv2 = &render_state->pipeline_verts[element->indexes[i + 1]];
        RasPipelineVertex* pv3 = &render_state->pipeline_verts[element->indexes[i + 2]];

        if (pv1->clip_flags & pv2->clip_flags & pv3->clip_flags) {
            continue; // face is all out
        }
        num_faces_in_frustum += 1;

        if (core_is_backface(render_state->pipeline_verts, &element->indexes[i])
            && render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON) {
            continue;
        }

        RasClipFlags face_clip_flags = pv1->clip_flags | pv2->clip_flags | pv3->clip_flags;
        num_faces_must_clip += face_clip_flags == 0 ? 0 : 1;

        // FIXME: first poly for testing
        if (i == 0 && face_clip_flags != 0) {
            core_clip_poly(frustum, face_clip_flags, render_state, &element->indexes[i]);
        } else {

            render_state->visible_indexes[*vi] = element->indexes[i];
            render_state->visible_indexes[*vi + 1] = element->indexes[i + 1];
            render_state->visible_indexes[*vi + 2] = element->indexes[i + 2];
            render_state->num_visible_indexes += 3;
            (*vi) += 3;
        }
        num_faces_visible += 1;
    }

    // Project to screen space
    for (uint32_t i = 0; i < render_state->num_pipeline_verts; i++) {
        RasFixed view_space_position[4];
        RasFixed screen_space_vec[4];
        RasFixed projected_vec[4];
        RasPipelineVertex* pv = &render_state->pipeline_verts[i];

        core_vector3f_to_4x1(&pv->view_space_position, view_space_position);
        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &pv->screen_space_position);

        ras_log_trace("pipeline screen space pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->screen_space_position));
    }

    RasPipelineVertex* pv = &render_state->pipeline_verts[0];
    ras_log_buffer(
        "pv 0: view space pos: %s\nscreen space pos: %s\n",
        repr_point3f(buffer, sizeof buffer, &pv->view_space_position),
        repr_vector4f(buffer2, sizeof buffer, &pv->screen_space_position));

    ras_log_buffer("Verts in frustum: %d\n", num_verts_in_frustum);
    ras_log_buffer(
        "Total faces: %d. In frustum: %d. Visible: %d. Must clip %d\n",
        element->num_indexes / 3,
        num_faces_in_frustum,
        num_faces_visible,
        num_faces_must_clip);
}

void core_get_element_aabb(RasPipelineElement* element, RasAABB* aabb)
{
    core_aabb_init(aabb);

    for (int i = 0; i < element->num_verts; i++) {
        RasVertex* element_vert = &element->verts[i];
        core_min_vector3f(&element->aabb.min, &element_vert->position, &element->aabb.min);
        core_max_vector3f(&element->aabb.max, &element_vert->position, &element->aabb.max);
    }

    char buffer[255];
    ras_log_trace("Model AABB min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_trace("Model AABB max: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.max));
}

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element)
{

    element->num_verts = group->num_verts;
    for (int i = 0; i < group->num_verts; i++) {
        RasVertex* element_vert = &element->verts[i];

        element_vert->position.x = group->verts[i].x;
        element_vert->position.y = group->verts[i].y;
        element_vert->position.z = group->verts[i].z;
    }

    core_get_element_aabb(element, &element->aabb);

    element->num_indexes = group->num_faces * 3;
    uint32_t* element_index = &element->indexes[0];
    for (int j = 0; j < group->num_faces; j++) {
        RasModelFace* face = &group->faces[j];
        for (int k = 0; k < RAS_MAX_MODEL_FACE_INDEXES; k++) {
            RasModelFaceIndex* face_index = &face->indexes[k];
            *element_index = face_index->vert_index;
            element_index++;
        }
    }
}
