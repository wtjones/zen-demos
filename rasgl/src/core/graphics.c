#include "rasgl/core/graphics.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/color.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/normals.h"
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

bool core_aabb_in_frustum(
    RasAABB* aabb,
    RasFrustum* frustum,
    bool use_far_plane,
    RasClipFlags* flags)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasClipFlags point_flags[RAS_MAX_AABB_POINTS];
    *flags = 0;
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        point_flags[i] = core_point_in_frustum_planes(frustum, &points[i]);
        *flags = *flags | point_flags[i];
    }

    // Check if all AABB points are outside a plane.
    int32_t num_frustum_planes = (use_far_plane == true ? FRUSTUM_PLANES : FRUSTUM_PLANES - 1);
    for (RasFrustumPlane fpi = 0; fpi < num_frustum_planes; fpi++) {

        RasClipFlags plane_flag = core_to_clip_flag(fpi);
        bool all_out = true;

        for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
            if (!(point_flags[i] & plane_flag)) {
                all_out = false;
                break;
            }
        }

        if (all_out) {
            return true;
        }
    }

    return false;
}

bool core_aabb_in_frustum_alt(RasAABB* aabb, RasFrustum* frustum, RasClipFlags* flags)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    RasClipFlags point_flags[RAS_MAX_AABB_POINTS];
    *flags = 0;
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
        point_flags[i] = core_point_in_frustum_planes(frustum, &points[i]);
        *flags = *flags | point_flags[i];
    }

    // Check if all AABB points are outside a plane.
    for (RasFrustumPlane plane = 0; plane < FRUSTUM_PLANES; plane++) {
        RasClipFlags plane_flag = core_to_clip_flag(plane);
        bool all_out = true;

        for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
            if (!(point_flags[i] & plane_flag)) {
                all_out = false;
                break;
            }
        }

        if (all_out) {
            return true;
        }
    }

    return false;
}

void core_renderstate_init(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
    state->num_visible_faces = 0;
    state->num_material_indexes = 0;
    state->num_meshes = 0;
    state->num_visible_meshes = 0;
    state->current_frame = 0;
    state->max_frames = UINT32_MAX;

    state->backface_culling_mode = RAS_BACKFACE_CULLING_ON;
    state->clipping_mode = RAS_CLIPPING_ON;
    state->clip_side_mode = RAS_CLIP_SIDE_DEFAULT;
    state->normal_mode = RAS_NORMAL_MODE_OFF;
    state->grid_mode = RAS_GRID_MODE_OFF;
    state->pipeline_mode = RAS_PIPELINE_MODE_DEFAULT;

    memset(state->material_indexes, 0, sizeof(state->material_indexes));
    memset(state->visible_indexes, 0, sizeof(state->visible_indexes));
    memset(state->visible_faces, 0, sizeof(state->visible_faces));
    memset(state->visible_meshes, 0, sizeof(state->visible_meshes));
};

void core_renderstate_scene_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_SCENE;
    state->layer_visible = true;
    state->projection_mode = RAS_PERSPECTIVE_MATRIX;
    state->polygon_mode = RAS_POLYGON_WIREFRAME;
    state->grid_mode = RAS_GRID_MODE_ORIGIN;
}

void core_renderstate_ui_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_UI;
    state->layer_visible = true;
    state->projection_mode = RAS_ORTHO_MATRIX;
    state->polygon_mode = RAS_POLYGON_BITMAP;
}

void core_renderstate_console_init(RenderState* state)
{
    core_renderstate_init(state);
    state->layer = RAS_LAYER_CONSOLE;
    state->layer_visible = false;
    state->projection_mode = RAS_ORTHO_MATRIX;
    state->polygon_mode = RAS_POLYGON_BITMAP;
}

void core_renderstates_init(RenderState states[])
{
    core_renderstate_scene_init(&states[RAS_LAYER_SCENE]);
    core_renderstate_ui_init(&states[RAS_LAYER_UI]);
    core_renderstate_console_init(&states[RAS_LAYER_CONSOLE]);
}

void core_renderstate_clear(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->num_visible_indexes = 0;
    state->num_material_indexes = 0;
    state->num_visible_faces = 0;
    memset(state->material_indexes, 0, sizeof(state->material_indexes));
    memset(state->visible_indexes, 0, sizeof(state->visible_indexes));
    memset(state->visible_faces, 0, sizeof(state->visible_faces));
    state->num_meshes = 0;
    memset(state->meshes, 0, sizeof(state->meshes));
}

void core_renderstates_clear(RenderState states[])
{
    for (size_t i = 0; i < RAS_LAYER_COUNT; i++) {
        core_renderstate_clear(&states[i]);
    }
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

bool core_is_backface(const RasVector4f* sv0, const RasVector4f* sv1, const RasVector4f* sv2)
{
    // norm1 = (1.x - 0.x) * (0.y - 2.y)
    // norm2 = (1.y - 0.y) * (0.x - 2.x)
    int64_t dx1 = (int64_t)(sv1->x - sv0->x);
    int64_t dy1 = (int64_t)(sv1->y - sv0->y);
    int64_t dx2 = (int64_t)(sv2->x - sv0->x);
    int64_t dy2 = (int64_t)(sv2->y - sv0->y);

    int64_t cross = mul_fixed_16_16_to_fixed_32_32(dx1, dy2)
        - mul_fixed_16_16_to_fixed_32_32(dy1, dx2);

    return cross < 0;
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
        pv->clip_flags |= pv->screen_space_position.x <= 0
            ? core_to_clip_flag(PLANE_LEFT)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_RIGHT)) {
        pv->clip_flags |= pv->screen_space_position.x >= right_x
            ? core_to_clip_flag(PLANE_RIGHT)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_TOP)) {
        pv->clip_flags |= pv->screen_space_position.y <= 0
            ? core_to_clip_flag(PLANE_TOP)
            : 0;
    }

    if (aabb_flags & core_to_clip_flag(PLANE_BOTTOM)) {
        pv->clip_flags |= pv->screen_space_position.x >= bottom_y
            ? core_to_clip_flag(PLANE_BOTTOM)
            : 0;
    }
}

bool core_get_line_plane_intersect(
    RasVector3f* v1, RasVector3f* v2, RasPlane* plane, RasVector3f* dest_vec)
{
    RasFixed pv_a_side = core_dot_product(v1, &plane->normal) + plane->distance;
    RasFixed pv_b_side = core_dot_product(v2, &plane->normal) + plane->distance;

    // Handle edge cases gracefully
    if (pv_a_side == 0) {
        *dest_vec = *v1;
        return true;
    }
    if (pv_b_side == 0) {
        *dest_vec = *v2;
        return true;
    }

    // For intersection to be valid, vertices must be on opposite sides
    if ((pv_a_side > 0 && pv_b_side > 0) || (pv_a_side < 0 && pv_b_side < 0)) {
        ras_log_buffer_warn_ex(
            RAS_EVENT_INVALID_MATH,
            "Warning: both vertices on same side of plane");

        return false;
    }

    RasFixed scale = div_fixed_16_16_by_fixed_16_16(-pv_a_side, pv_b_side - pv_a_side);

    RasVector3f* cur_vert = v1;
    RasVector3f* next_vert = v2;
    dest_vec->x = cur_vert->x + (mul_fixed_16_16_by_fixed_16_16(next_vert->x - cur_vert->x, scale));
    dest_vec->y = cur_vert->y + (mul_fixed_16_16_by_fixed_16_16(next_vert->y - cur_vert->y, scale));
    dest_vec->z = cur_vert->z + (mul_fixed_16_16_by_fixed_16_16(next_vert->z - cur_vert->z, scale));
    return true;
}

void core_clip_poly_plane(
    RasFrustum* frustum,
    RasFrustumPlane side,
    RasPipelineVertexBuffer* vertex_buffer,
    uint32_t indexes[3],
    int32_t material_index,
    RasPipelineFace* face)
{
    // FIXME: Happens when a triangle is rotating and clipping on the left side
    assert(vertex_buffer->num_verts < MAX_PIPELINE_VERTS);
    RasPlane* plane = &frustum->planes[side];
    RasPipelineVertex* pipeline_verts = &vertex_buffer->verts[0];

    RasPipelineVertex* pv_0 = &pipeline_verts[indexes[0]];
    RasPipelineVertex* pv_1 = &pipeline_verts[indexes[1]];
    RasPipelineVertex* pv_2 = &pipeline_verts[indexes[2]];

    RasPipelineVertex* cur_pv = &pipeline_verts[indexes[0]];
    RasVector3f* cur_vert = &cur_pv->view_space_position;
    RasPipelineVertex* next_pv = &pipeline_verts[indexes[1]];
    RasVector3f* next_vert = &next_pv->view_space_position;
    char buffer[255];

    int num_in = 0;
    int first_in = -1, second_in = -1;
    for (int i = 0; i < 3; i++) {
        RasPipelineVertex* pv = &pipeline_verts[indexes[i]];
        bool is_in = !(pv->clip_flags & core_to_clip_flag(side));
        num_in += is_in ? 1 : 0;
        first_in = (first_in == -1 && num_in == 1 && is_in) ? i : first_in;
        second_in = (second_in == -1 && num_in == 2 && is_in) ? i : second_in;
    }

    ras_log_buffer("cpp: num_in: %d\n", num_in);
    ras_log_buffer("cpp: first_in: %d, 2nd_in: %d\n", first_in, second_in);

    if (num_in == 1) {
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

        // Identify vertex A

        int index_a = first_in;
        int index_b = first_in == 2 ? 0 : first_in + 1;
        int index_c = first_in == 0 ? 2 : first_in - 1;

        ras_log_buffer("cpp: A = %d, B = %d, C = %d\n", index_a, index_b, index_c);

        int pv_a_index = indexes[index_a];
        int pv_b_index = indexes[index_b];
        int pv_c_index = indexes[index_c];

        RasPipelineVertex* pv_a = &pipeline_verts[indexes[index_a]];
        RasPipelineVertex* pv_b = &pipeline_verts[indexes[index_b]];
        RasPipelineVertex* pv_c = &pipeline_verts[indexes[index_c]];

        // Allocate vertices B' and C' on the pv array
        uint32_t pv_b_alt_index = vertex_buffer->num_verts++;
        RasPipelineVertex* pv_b_alt = &vertex_buffer->verts[pv_b_alt_index];
        uint32_t pv_c_alt_index = vertex_buffer->num_verts++;
        RasPipelineVertex* pv_c_alt = &vertex_buffer->verts[pv_c_alt_index];

        // FIME: need to set other values on vertex, like screen pos

        pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;
        pv_c_alt->aabb_clip_flags = pv_c->aabb_clip_flags;

        // Find B' to create side AB'
        core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_b->view_space_position,
            plane,
            &pv_b_alt->view_space_position);

        core_set_pv_clip_flags_alt(frustum, pv_b_alt->aabb_clip_flags, pv_b_alt);

        // Find C' to create side AC'
        core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_c_alt->view_space_position);

        core_set_pv_clip_flags_alt(frustum, pv_c_alt->aabb_clip_flags, pv_c_alt);

        ras_log_buffer("cpp: clip: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));

        // Connect A -> B'
        uint32_t* vi = &vertex_buffer->num_visible_indexes;
        vertex_buffer->visible_indexes[(*vi)++] = pv_a_index;
        vertex_buffer->visible_indexes[(*vi)++] = pv_b_alt_index;

        // Connect B' -> C' -> A
        vertex_buffer->visible_indexes[(*vi)++] = pv_c_alt_index;

        // The first face is the repurposed face. Point element indices to new pvs
        // so that the subsequent calls to this function clip correctly.
        indexes[0] = pv_a_index;
        indexes[1] = pv_b_alt_index;
        indexes[2] = pv_c_alt_index;

        // Set material index
        uint32_t* mi = &vertex_buffer->num_material_indexes;
        vertex_buffer->material_indexes[*mi] = material_index;
        (*mi)++;

    } else if (num_in == 2) {
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

        // Identify vertex A

        int index_a = -1, index_b = -1, index_c = -1;

        if (first_in == 0) {
            if (second_in == 1) {
                index_a = 1;
                index_b = 0;
                index_c = 2;
            } else {
                index_a = 0;
                index_b = 2;
                index_c = 1;
            }
        } else if (first_in == 1) {
            if (second_in == 2) {
                index_a = 2;
                index_b = 1;
                index_c = 0;
            } else {
                index_a = 1;
                index_b = 0;
                index_c = 2;
            }
        } else if (first_in == 2) {
            if (second_in == 0) {
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

        RasPipelineVertex* pv_a = &pipeline_verts[indexes[index_a]];
        RasPipelineVertex* pv_b = &pipeline_verts[indexes[index_b]];
        RasPipelineVertex* pv_c = &pipeline_verts[indexes[index_c]];

        // Allocate vertices A' and B' on the pv array
        uint32_t pv_a_alt_index = vertex_buffer->num_verts++;
        RasPipelineVertex* pv_a_alt = &vertex_buffer->verts[pv_a_alt_index];
        uint32_t pv_b_alt_index = vertex_buffer->num_verts++;
        RasPipelineVertex* pv_b_alt = &vertex_buffer->verts[pv_b_alt_index];

        pv_a_alt->aabb_clip_flags = pv_a->aabb_clip_flags;
        pv_b_alt->aabb_clip_flags = pv_b->aabb_clip_flags;

        // Find A' to create side AA'
        core_get_line_plane_intersect(
            &pv_a->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_a_alt->view_space_position);

        core_set_pv_clip_flags_alt(frustum, pv_a_alt->aabb_clip_flags, pv_a_alt);

        // Find B' to create side BB'
        core_get_line_plane_intersect(
            &pv_b->view_space_position,
            &pv_c->view_space_position,
            plane,
            &pv_b_alt->view_space_position);

        core_set_pv_clip_flags_alt(frustum, pv_b_alt->aabb_clip_flags, pv_b_alt);

        ras_log_buffer("cpp: clip: pv_a_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));
        ras_log_buffer("cpp: clip: pv_b_alt: %s\n", repr_point3f(buffer, sizeof buffer, &pv_b_alt->view_space_position));

        uint32_t* vi = &vertex_buffer->num_visible_indexes;

        // Connect A -> A' -> B
        vertex_buffer->visible_indexes[(*vi)++] = pv_a_index;
        vertex_buffer->visible_indexes[(*vi)++] = pv_a_alt_index;
        vertex_buffer->visible_indexes[(*vi)++] = pv_b_index;

        // Set material index
        uint32_t* mi = &vertex_buffer->num_material_indexes;
        vertex_buffer->material_indexes[*mi] = material_index;
        (*mi)++;

        // The first face is the repurposed face. Point element indices to new pvs
        // so that the subsequent calls to this function clip correctly.
        indexes[0] = pv_a_index;
        indexes[1] = pv_a_alt_index;
        indexes[2] = pv_b_index;

        // Connect A' -> B' -> B
        vertex_buffer->visible_indexes[(*vi)++] = pv_a_alt_index;
        vertex_buffer->visible_indexes[(*vi)++] = pv_b_alt_index;
        vertex_buffer->visible_indexes[(*vi)++] = pv_b_index;

        vertex_buffer->material_indexes[*mi] = material_index;
        (*mi)++;

        // Copy the original face to the new face
        uint32_t* num_dest_faces = &vertex_buffer->num_visible_faces;
        RasPipelineFace* new_face = &vertex_buffer->visible_faces[*num_dest_faces];
        (*num_dest_faces)++;
        memcpy(new_face, face, sizeof(RasPipelineFace));

        // Recurse to clip added face
        uint32_t new_face_indexes[3] = { pv_a_alt_index, pv_b_alt_index, pv_b_index };
        core_clip_poly(
            frustum, 0, vertex_buffer, new_face_indexes, material_index, new_face);
    } else {
        assert(true);
    }
}

void core_clip_poly(
    RasFrustum* frustum,
    RasClipFlags face_clip_flags,
    RasPipelineVertexBuffer* vertex_buffer,
    uint32_t in_indexes[3],
    int32_t material_index,
    RasPipelineFace* face)
{
    uint32_t indexes[3] = { in_indexes[0], in_indexes[1], in_indexes[2] };

    for (uint8_t i = 0; i < FRUSTUM_PLANES; i++) {

        // recalc flags as the face may point to new verts
        RasPipelineVertex* pv1 = &vertex_buffer->verts[indexes[0]];
        RasPipelineVertex* pv2 = &vertex_buffer->verts[indexes[1]];
        RasPipelineVertex* pv3 = &vertex_buffer->verts[indexes[2]];
        face_clip_flags = pv1->clip_flags | pv2->clip_flags | pv3->clip_flags;

        RasClipFlags plane_flag = core_to_clip_flag((RasFrustumPlane)i);
        if (face_clip_flags & plane_flag) {
            ras_log_buffer("PV clipping against plane %d\n", i);
            core_clip_poly_plane(
                frustum, (RasFrustumPlane)i, vertex_buffer, indexes, material_index, face);
        }
    }
}

void core_render_point(
    RenderState* render_state,
    RasVector4f* screen_space_position,
    int32_t material)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;

    if (*num_points >= MAX_RENDER_POINTS || *num_commands >= MAX_RENDER_COMMANDS) {
        ras_log_error("Render state full, cannot render more points.");
        assert(false);
        return;
    }
    screen_pos = &render_state->points[*num_points];
    screen_pos->x = FIXED_16_16_TO_INT_32(screen_space_position->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(screen_space_position->y);

    render_state->commands[*num_commands].num_points = 1;
    render_state->commands[*num_commands].point_indices[0] = *num_points;
    RasFixed shade_scale = float_to_fixed_16_16(3.5);

    RasFixed darken = mul_fixed_16_16_by_fixed_16_16(
        screen_space_position->z, shade_scale);
    uint8_t darken_8 = FIXED_16_16_TO_INT_32(darken) < (RAS_COLOR_RAMP_SIZE - 1)
        ? FIXED_16_16_TO_INT_32(darken)
        : 6;

    uint8_t shade = material + RAS_COLOR_RAMP_SIZE - 1 - darken_8;
    render_state->commands[*num_commands].color = shade;
    (*num_points)++;
    (*num_commands)++;
}

void core_render_line(RenderState* render_state, RasVector4f* p0, RasVector4f* p1)
{
    uint32_t* num_points = &render_state->num_points;
    uint32_t* num_commands = &render_state->num_commands;
    Point2i* screen_pos;

    screen_pos = &render_state->points[*num_points];
    screen_pos->x = FIXED_16_16_TO_INT_32(p0->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(p0->y);

    render_state->commands[*num_commands].num_points = 2;
    render_state->commands[*num_commands].point_indices[0] = *num_points;
    (*num_points)++;
    screen_pos++;

    screen_pos->x = FIXED_16_16_TO_INT_32(p1->x);
    screen_pos->y = FIXED_16_16_TO_INT_32(p1->y);
    render_state->commands[*num_commands].point_indices[1] = *num_points;
    (*num_points)++;
    (*num_commands)++;
}

void core_projected_to_screen_point(int32_t screen_width, int32_t screen_height, RasFixed projected_point[4], RasVector4f* screen_point)
{
    RasFixed half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    RasFixed half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width;
    screen_point->y = -mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height;
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

        core_render_point(render_state, &screen_space_position, RAS_COLOR_RAMP_OFFSET_RED);
        num_points++;
    }
    ras_log_buffer("AABB points rendered: %d\n", num_points);
}

/**
 * @brief Append object-level vertext buffer to render state array
 *
 * @param render_state
 * @param vert_buffer
 */
void core_append_vertex_buffer(
    RenderState* render_state,
    RasPipelineVertexBuffer* vert_buffer)
{
    uint32_t num_pipeline_verts_prev = render_state->num_pipeline_verts;

    if (render_state->num_pipeline_verts + vert_buffer->num_verts > MAX_PIPELINE_VERTS) {
        ras_log_error("Pipeline vertex buffer full, skipping append.");
        return;
    }

    memcpy(
        &render_state->pipeline_verts[render_state->num_pipeline_verts],
        vert_buffer->verts,
        sizeof(RasPipelineVertex) * vert_buffer->num_verts);

    render_state->num_pipeline_verts += vert_buffer->num_verts;

    // Add an offset to convert object indices from relative to absolute
    uint32_t* si = &render_state->num_visible_indexes;
    for (uint32_t i = 0; i < vert_buffer->num_visible_indexes; i++) {
        render_state->visible_indexes[*si] = num_pipeline_verts_prev + vert_buffer->visible_indexes[i];
        (*si)++;
    }

    si = &render_state->num_material_indexes;
    for (uint32_t i = 0; i < vert_buffer->num_material_indexes; i++) {
        render_state->material_indexes[*si]
            = vert_buffer->material_indexes[i];
        (*si)++;
    }

    memcpy(
        &render_state->visible_faces[render_state->num_visible_faces],
        vert_buffer->visible_faces,
        sizeof(RasPipelineFace) * vert_buffer->num_visible_faces);

    render_state->num_visible_faces += vert_buffer->num_visible_faces;
}

void core_light_poly(
    RasPipelineFace* face,
    RasVector3f* camera_pos,
    RasVector3f* light_pos)
{
    RasVector3f light_dir;
    core_sub_vector3f(light_pos, camera_pos, &light_dir);
    core_normalize(&light_dir);

    face->diffuse_intensity = core_dot_product(
        &face->view_space_normal, &light_dir);

    face->diffuse_intensity = face->diffuse_intensity < 0 ? 0 : face->diffuse_intensity;
    char buffer[255];
    ras_log_buffer_trace("diffuse_intensity: %s", repr_fixed_16_16(buffer, sizeof buffer, face->diffuse_intensity));
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
    static RasPipelineVertexBuffer vert_buffer;
    RasFixed model_view_matrix[4][4];
    RasFixed combined_matrix[4][4];
    RasFixed normal_mvt_matrix[4][4];
    RasFixed dest_vec[4];

    ras_log_buffer("core_draw_element ##################\n");

    // model -> view transform
    mat_mul_4x4_4x4(world_view_matrix, model_world_matrix, model_view_matrix);

    // Remove translation from model matrix to get normal matrix
    core_mat_normal_init(model_view_matrix, normal_mvt_matrix);
    ras_log_buffer("normal mvt: %s", repr_mat_4x4(buffer, sizeof buffer, normal_mvt_matrix));

    // model/view -> projection
    mat_mul_4x4_4x4(proj_matrix, model_view_matrix, combined_matrix);

    // translate AABB to view space
    RasAABB view_aabb;
    core_aabb_xform(&element->aabb, model_view_matrix, &view_aabb);

    ras_log_buffer("AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_buffer("AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.min));
    ras_log_buffer("AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.max));

    RasClipFlags aabb_clip_flags = 0;
    bool all_out = core_aabb_in_frustum_alt(&view_aabb, frustum, &aabb_clip_flags);

    ras_log_buffer("AABB flags: %hhu, all_out: %s\n", aabb_clip_flags, all_out ? "true" : "false");

    if (all_out) {
        return;
    }

    /**
     * @brief Transform pipeline verts
     *
     */
    vert_buffer.num_verts = 0;
    vert_buffer.num_visible_indexes = 0;
    memset(vert_buffer.visible_indexes, 0, sizeof(vert_buffer.visible_indexes));
    memset(vert_buffer.verts, 0, sizeof(vert_buffer.verts));

    core_render_aabb(render_state, proj_matrix, frustum, &view_aabb);
    uint32_t num_verts_in_frustum = 0;

    for (uint32_t i = 0; i < element->num_verts; i++) {
        RasVertex* vertex = &element->verts[i];
        RasPipelineVertex* pv = &vert_buffer.verts[i];

        RasFixed model_space_position[4];
        RasFixed view_space_position[4];
        RasFixed screen_space_vec[4];
        RasFixed projected_vec[4];

        core_vector3f_to_4x1(&vertex->position, model_space_position);
        mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
        core_4x1_to_vector3f(view_space_position, &pv->view_space_position);

        core_set_pv_clip_flags_alt(frustum, aabb_clip_flags, pv);
        pv->aabb_clip_flags = aabb_clip_flags;
        num_verts_in_frustum
            += pv->clip_flags == 0 ? 1 : 0;

        pv->color = vertex->color;
        pv->u = vertex->u;
        pv->v = vertex->v;

        // project to screen space
        core_vector3f_to_4x1(&pv->view_space_position, view_space_position);
        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &pv->screen_space_position);

        ras_log_buffer_trace("pipeline screen space pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->screen_space_position));

        vert_buffer.num_verts++;
    }

    /**
     * @brief Determine visible faces
     *
     */
    uint32_t num_visible_indexes_prev = render_state->num_visible_indexes;
    uint32_t num_faces_in_frustum = 0;
    uint32_t num_faces_must_clip = 0;
    uint32_t num_faces_excluded = 0;
    uint32_t* vi = &vert_buffer.num_visible_indexes;
    uint32_t num_projected_verts = vert_buffer.num_verts;
    uint32_t current_src_face_index = 0;
    uint32_t* num_dest_materials = &vert_buffer.num_material_indexes;
    *num_dest_materials = 0;
    uint32_t* num_dest_faces = &vert_buffer.num_visible_faces;
    *num_dest_faces = 0;

    for (uint32_t i = 0; i < element->num_indexes; i += 3) {
        RasPipelineVertex* pv1 = &vert_buffer.verts[element->indexes[i]];
        RasPipelineVertex* pv2 = &vert_buffer.verts[element->indexes[i + 1]];
        RasPipelineVertex* pv3 = &vert_buffer.verts[element->indexes[i + 2]];

        if (pv1->clip_flags & pv2->clip_flags & pv3->clip_flags) {
            current_src_face_index++;

            continue; // face is all out
        }
        num_faces_in_frustum += 1;
        bool is_backface = core_is_backface(
            &pv1->screen_space_position,
            &pv2->screen_space_position,
            &pv3->screen_space_position);
        bool is_culling = render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON;
        if (is_backface && is_culling) {
            current_src_face_index++;
            continue;
        }

        /**
         * @brief If dropped due to clipping exclusion, skip out to avoid
         * adding the face to the visible faces list.
         *
         */
        RasClipFlags face_clip_flags = pv1->clip_flags | pv2->clip_flags | pv3->clip_flags;

        if (face_clip_flags != 0 && render_state->clipping_mode == RAS_CLIPPING_EXCLUDE) {
            num_faces_excluded++;
            current_src_face_index++;
            continue;
        }

        /**
         * @brief Transform face normal to view space
         *
         */
        RasElementFace* src_face = &element->faces[current_src_face_index];
        RasPipelineFace* face = &vert_buffer.visible_faces[*num_dest_faces];
        face->normal = src_face->normal;
        face->material_index = src_face->material_index;

        RasFixed model_space_normal[4];
        RasFixed view_space_normal[4];

        core_vector3f_to_4x1(&face->normal, model_space_normal);
        mat_mul_4x4_4x1(normal_mvt_matrix, model_space_normal, view_space_normal);
        core_4x1_to_vector3f(view_space_normal, &face->view_space_normal);
        (*num_dest_faces)++;

        RasVector3f camera_pos = { 0, 0, 0 };
        RasVector3f light_pos = { 0, 0, RAS_FIXED_ONE };

        core_light_poly(
            face,
            &camera_pos,
            &light_pos);

        num_faces_must_clip += face_clip_flags == 0 ? 0 : 1;
        if (face_clip_flags != 0) {
            // Note: Legacy pipeline is deprecated. Clipping is broken.
            core_clip_poly(
                frustum,
                face_clip_flags,
                &vert_buffer,
                &element->indexes[i],
                element->material_indexes[current_src_face_index],
                face);
            current_src_face_index++;
            continue;
        }

        vert_buffer.visible_indexes[*vi] = element->indexes[i];
        vert_buffer.visible_indexes[*vi + 1] = element->indexes[i + 1];
        vert_buffer.visible_indexes[*vi + 2] = element->indexes[i + 2];
        vert_buffer.num_visible_indexes += 3;

        vert_buffer.material_indexes[*num_dest_materials]
            = element->material_indexes[current_src_face_index];
        (*num_dest_materials)++;

        current_src_face_index++;
    }

    // Project verts created from clipping
    for (uint32_t i = num_projected_verts; i < vert_buffer.num_verts; i++) {
        RasFixed view_space_position[4];
        RasFixed screen_space_vec[4];
        RasFixed projected_vec[4];
        RasPipelineVertex* pv = &vert_buffer.verts[i];

        // project to screen space
        core_vector3f_to_4x1(&pv->view_space_position, view_space_position);
        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &pv->screen_space_position);

        ras_log_buffer_trace("pipeline screen space pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->screen_space_position));
    }

    if (render_state->normal_mode != RAS_NORMAL_MODE_OFF) {
        draw_element_normals(render_state, &vert_buffer, model_view_matrix, proj_matrix);
    }

    core_append_vertex_buffer(render_state, &vert_buffer);
    RasPipelineVertex* pv = &vert_buffer.verts[0];

    ras_log_buffer(
        "pv 0: view space pos: %s\nscreen space pos: %s\n",
        repr_point3f(buffer, sizeof buffer, &pv->view_space_position),
        repr_vector4f(buffer2, sizeof buffer, &pv->screen_space_position));

    ras_log_buffer("Verts in frustum: %d\n", num_verts_in_frustum);
    uint32_t num_faces_visible = (render_state->num_visible_indexes - num_visible_indexes_prev) / 3;

    ras_log_buffer(
        "Faces:\n    In Model: %d. In frustum: %d. Visible: %d. Must clip %d. Dropped: %d. After clip: %d\n",
        element->num_indexes / 3,
        num_faces_in_frustum,
        num_faces_visible,
        num_faces_must_clip,
        num_faces_excluded,
        render_state->num_visible_indexes / 3);
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
    element->num_material_indexes = group->num_faces;
    element->num_faces = group->num_faces;
    uint32_t* element_index = &element->indexes[0];
    int32_t* material_index = &element->material_indexes[0];
    RasElementFace* dest_face = &element->faces[0];

    for (int j = 0; j < group->num_faces; j++) {
        RasModelFace* face = &group->faces[j];
        for (int k = 0; k < RAS_MAX_MODEL_FACE_INDEXES; k++) {
            RasModelFaceIndex* face_index = &face->indexes[k];
            *element_index = face_index->vert_index;
            element_index++;
        }

        *material_index = face->material_index;
        material_index++;

        dest_face->material_index = face->material_index;
        RasVector3f* src_normal = &group->normals[face->indexes[0].normal_index];
        dest_face->normal = *src_normal;
        dest_face++;
    }
}
