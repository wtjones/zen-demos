#include "rasgl/core/graphics.h"
#include "rasgl/core/debug.h"

/**
 * project view point to screen
 *
 */
Point2f project_point(int32_t screen_width, int32_t screen_height, int32_t projection_ratio, Point3f view_point)
{
    char buffer[100];

    Point2f result;
    ras_log_trace("view_point: %s\n", repr_point3f(buffer, sizeof buffer, &view_point));

    // x = ((x / z * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_W / 2

    int32_t x_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.x, view_point.z);
    ras_log_trace("x_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, x_div_z));

    int32_t mul_ratio = mul_fixed_16_16_by_fixed_16_16(x_div_z, projection_ratio);
    ras_log_trace("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    int32_t screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    ras_log_trace("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    int32_t left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    int32_t right = INT_32_TO_FIXED_16_16(screen_width / 2);
    result.x = left + right;
    ras_log_trace("result.x = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    // y = ((y / z * -1.0 * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_H / 2
    int32_t y_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.y, view_point.z);
    ras_log_trace("y_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, y_div_z));

    mul_ratio = mul_fixed_16_16_by_fixed_16_16(y_div_z, projection_ratio);
    mul_ratio = mul_fixed_16_16_by_fixed_16_16(mul_ratio, -float_to_fixed_16_16(1.0));

    ras_log_trace("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    ras_log_trace("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    right = INT_32_TO_FIXED_16_16(screen_height / 2);
    result.y = left + right;
    ras_log_trace("result.y = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    return result;
}

void projected_to_screen_point(int32_t screen_width, int32_t screen_height, int32_t projected_point[4], Point2i* screen_point)
{

    int32_t half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    int32_t half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width);
    screen_point->y = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height);
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
        mat_mul_project(combined_matrix, view_space_position, projected_vec);

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

    // No clipping yet, so just copy the indexes.
    for (int i = 0; i < num_indexes; i++) {
        render_state->visible_indexes[i] = indexes[i];
    }
    render_state->num_visible_indexes = num_indexes;
}
