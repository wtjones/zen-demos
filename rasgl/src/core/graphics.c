#include "rasgl/core/graphics.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"

void projected_to_screen_point(int32_t screen_width, int32_t screen_height, int32_t projected_point[4], Point2i* screen_point)
{

    int32_t half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    int32_t half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

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
    int32_t norm1 = mul_fixed_16_16_by_fixed_16_16(sv1->x - sv0->x, sv0->y - sv2->y);
    int32_t norm2 = mul_fixed_16_16_by_fixed_16_16(sv1->y - sv0->y, sv0->x - sv2->x);
    int32_t norm = norm1 - norm2;
    return (norm1 - norm2 > 0);
}

void core_projected_to_screen_point(int32_t screen_width, int32_t screen_height, int32_t projected_point[4], RasVector4f* screen_point)
{

    int32_t half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    int32_t half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width;
    screen_point->y = mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height;
    screen_point->z = projected_point[2];
    screen_point->w = projected_point[3];
}

void core_draw_element(
    RenderState* render_state,
    RasPipelineElement* element,
    int32_t model_world_matrix[4][4],
    int32_t world_view_matrix[4][4],
    int32_t proj_matrix[4][4])
{
    core_draw_elements(render_state,
        element->verts,
        element->num_verts,
        element->indexes,
        element->num_indexes,
        model_world_matrix,
        world_view_matrix,
        proj_matrix);
}

void core_draw_elements(
    RenderState* render_state,
    RasVertex* verts,
    uint32_t num_verts,
    uint32_t* indexes,
    uint32_t num_indexes,
    int32_t model_world_matrix[4][4],
    int32_t world_view_matrix[4][4],
    int32_t proj_matrix[4][4])
{
    char buffer[1000];
    int32_t model_view_matrix[4][4];
    int32_t combined_matrix[4][4];
    int32_t dest_vec[4];

    // model -> view transform
    mat_mul_4x4_4x4(world_view_matrix, model_world_matrix, model_view_matrix);

    // model/view -> projection
    mat_mul_4x4_4x4(proj_matrix, model_view_matrix, combined_matrix);

    for (uint32_t i = 0; i < num_verts; i++) {
        RasVertex* vertex = &verts[i];
        RasPipelineVertex* pv = &render_state->pipeline_verts[i];

        int32_t model_space_position[4];
        int32_t view_space_position[4];
        int32_t screen_space_vec[4];
        int32_t projected_vec[4];

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
    for (uint32_t i = 0; i < num_indexes; i += 3) {
        bool is_visible = (!core_is_backface(render_state->pipeline_verts, &indexes[i]))
            || render_state->backface_culling_mode == RAS_BACKFACE_CULLING_OFF;
        if (is_visible) {
            render_state->visible_indexes[vi] = indexes[i];
            render_state->visible_indexes[vi + 1] = indexes[i + 1];
            render_state->visible_indexes[vi + 2] = indexes[i + 2];
            render_state->num_visible_indexes += 3;
            vi += 3;
        }
    }
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

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element)
{
    element->aabb.min.x = FIXED_MAX;
    element->aabb.min.y = FIXED_MAX;
    element->aabb.min.z = FIXED_MAX;

    element->aabb.max.x = FIXED_MIN;
    element->aabb.max.y = FIXED_MIN;
    element->aabb.max.z = FIXED_MIN;

    element->num_verts = group->num_verts;
    for (int i = 0; i < group->num_verts; i++) {
        RasVertex* element_vert = &element->verts[i];

        element_vert->position.x = group->verts[i].x;
        element_vert->position.y = group->verts[i].y;
        element_vert->position.z = group->verts[i].z;

        core_min_vector3f(&element->aabb.min, &element_vert->position, &element->aabb.min);
        core_max_vector3f(&element->aabb.max, &element_vert->position, &element->aabb.max);
    }
    char buffer[255];
    ras_log_trace("Model AABB min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_trace("Model AABB max: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.max));

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
