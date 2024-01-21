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

RasClipFlags core_aabb_in_frustum(RasAABB* aabb, RasFrustum* frustum)
{
    RasClipFlags result = 0;
    RasVector3f points[RAS_MAX_AABB_POINTS];
    core_aabb_to_points(aabb, points);

    for (RasFrustumPlane p = 0; p < 6; p++) {
        Plane* plane = &frustum->planes[p];

        bool outside = false;
        for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {

            outside = core_plane_vector_side(plane, &points[i]);
            if (outside) {
                break;
            }
        }
        result = result | outside << p;
    }
    return result;
}

void core_renderstate_init(RenderState* state)
{
    state->num_commands = 0;
    state->num_points = 0;
    state->num_pipeline_verts = 0;
    state->current_frame = 0;
    state->max_frames = UINT32_MAX;
    state->projection_mode = RAS_PERSPECTIVE_MATRIX;
    state->backface_culling_mode = RAS_BACKFACE_CULLING_ON;
};

void core_renderstate_clear(RenderState* state)
{
    state->num_visible_indexes = 0;
    state->num_commands = 0;
    state->num_points = 0;
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
    RasAABB* aabb)
{
    RasVector3f points[RAS_MAX_AABB_POINTS];
    core_aabb_to_points(aabb, points);

    for (int i = 0; i < RAS_MAX_AABB_POINTS; i++) {
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
    }
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

    core_render_aabb(render_state, proj_matrix, &view_aabb);

    RasClipFlags clip_flags = core_aabb_in_frustum(&view_aabb, frustum);

    ras_log_buffer("AABB side: %d\n", clip_flags);

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

        ras_log_trace("pipeline view space pos: %s\n", repr_point3f(buffer, sizeof buffer, &pv->view_space_position));

        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &pv->screen_space_position);

        ras_log_trace("pipeline screen space pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->screen_space_position));

        pv->color = vertex->color;
        pv->u = vertex->u;
        pv->v = vertex->v;

        render_state->num_pipeline_verts++;
    }

    render_state->num_visible_indexes = 0;
    uint32_t vi = 0;
    for (uint32_t i = 0; i < element->num_indexes; i += 3) {
        bool is_visible = (!core_is_backface(render_state->pipeline_verts, &element->indexes[i]))
            || render_state->backface_culling_mode == RAS_BACKFACE_CULLING_OFF;
        if (is_visible) {
            render_state->visible_indexes[vi] = element->indexes[i];
            render_state->visible_indexes[vi + 1] = element->indexes[i + 1];
            render_state->visible_indexes[vi + 2] = element->indexes[i + 2];
            render_state->num_visible_indexes += 3;
            vi += 3;
        }
    }
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
