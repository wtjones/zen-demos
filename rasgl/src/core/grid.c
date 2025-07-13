#include "rasgl/core/grid.h"

void render_grid_points(
    RenderState* render_state,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4],
    RasVector3f* points,
    size_t count)
{
    RasFixed projected_vec[4];
    RasFixed view_space_position[4];
    RasFixed model_space_position[4];

    for (size_t i = 0; i < count; i++) {
        RasVector3f* point = &points[i];
        RasVector3f view_space_point;
        RasVector4f screen_space_position;
        Point2i* render_point;

        core_vector3f_to_4x1(point, model_space_position);
        mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
        core_4x1_to_vector3f(view_space_position, &view_space_point);

        // Screen space in NDC coords
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        core_render_point(render_state, &screen_space_position);
    }
}

void core_draw_grid(
    RenderState* render_state,
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{
    RasFixed model_view_matrix[4][4];
    RasFixed combined_matrix[4][4];
    RasFixed model_world_matrix[4][4];
    RasFixed model_space_position[4];
    RasFixed screen_space_vec[4];

    size_t points_count = 0;

    if ((render_state->grid_mode == RAS_GRID_MODE_OFF)) {
        return;
    }

    // Grid is world space.
    mat_set_identity_4x4(model_view_matrix);
    mat_set_identity_4x4(model_world_matrix);

    // model -> view transform
    mat_mul_4x4_4x4(world_view_matrix, model_world_matrix, model_view_matrix);

    if ((render_state->grid_mode & RAS_GRID_MODE_ORIGIN)) {

        static RasVector3f points[RAS_GRID_ORIGIN_POINTS] = {
            { .x = RAS_FIXED_ZERO,
                .y = RAS_FIXED_ZERO,
                .z = RAS_FIXED_ZERO }
        };

        for (size_t i = 1; i < RAS_GRID_ORIGIN_AXIS_POINTS + 1; i++) {
            RasVector3f* point_x = &points[i];
            RasVector3f* point_y = &points[i + RAS_GRID_ORIGIN_AXIS_POINTS];
            RasVector3f* point_z = &points[i + (2 * RAS_GRID_ORIGIN_AXIS_POINTS)];
            RasFixed major_axis = i * float_to_fixed_16_16(0.03);
            point_x->x = major_axis;
            point_x->y = RAS_FIXED_ZERO;
            point_x->z = RAS_FIXED_ZERO;
            point_y->x = RAS_FIXED_ZERO;
            point_y->y = major_axis;
            point_y->z = RAS_FIXED_ZERO;
            point_z->x = RAS_FIXED_ZERO;
            point_z->y = RAS_FIXED_ZERO;
            point_z->z = major_axis;
        }

        render_grid_points(
            render_state,
            model_view_matrix,
            proj_matrix,
            points,
            RAS_GRID_ORIGIN_POINTS);
    }

    if (!(render_state->grid_mode & RAS_GRID_MODE_GRID)) {
        return;
    }

    static RasVector3f grid_points[RAS_GRID_LINE_POINTS_COUNT];
    size_t count = 0;

    // Example: 5 x 5 x 5 grid is -2.0 to 2.0 on each axis
    RasFixed offset = -INT_32_TO_FIXED_16_16(RAS_GRID_LINES_AXIS_POINTS / 2);

    for (int32_t i = 0; i < RAS_GRID_LINES_AXIS_POINTS; i++) {
        RasFixed x = INT_32_TO_FIXED_16_16(i) + offset;

        for (int32_t j = 0; j < RAS_GRID_LINES_AXIS_POINTS; j++) {
            RasFixed y = INT_32_TO_FIXED_16_16(j) + offset;

            for (int32_t k = 0; k < RAS_GRID_LINES_AXIS_POINTS; k++) {
                RasFixed z = INT_32_TO_FIXED_16_16(k) + offset;
                grid_points[count].x = x;
                grid_points[count].y = y;
                grid_points[count].z = z;

                count++;
            }
        }
    }

    render_grid_points(
        render_state,
        model_view_matrix,
        proj_matrix,
        grid_points,
        count);
}
