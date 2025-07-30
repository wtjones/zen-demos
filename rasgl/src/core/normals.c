#include "rasgl/core/normals.h"
#include "rasgl/core/color.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/repr.h"

/**
 * @brief Render face normals as transformed with model.
 * For diagnostic purposes, as this transform is not intended for
 * normal calculations.
 *
 * @param render_state
 * @param vert_buffer
 * @param model_view_matrix
 * @param proj_matrix
 */
void draw_element_normals_faux(
    RenderState* render_state,
    RasPipelineVertexBuffer* vert_buffer,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{
    RasVector3f origin_vec = { 0, 0, 0 };
    RasFixed projected_origin[4];
    RasVector4f screen_origin;
    RasFixed model_space_position[4];
    RasFixed view_space_position[4];
    char buffer[255];

    // Project model origin for faux mode
    core_vector3f_to_4x1(&origin_vec, model_space_position);
    mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
    mat_mul_project(proj_matrix, view_space_position, projected_origin);

    core_projected_to_screen_point(
        render_state->screen_settings.screen_width,
        render_state->screen_settings.screen_height,
        projected_origin,
        &screen_origin);

    for (size_t i = 0; i < vert_buffer->num_visible_faces; i++) {
        RasPipelineFace* face = &vert_buffer->visible_faces[i];

        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&face->normal, model_space_position);
        mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        ras_log_buffer("d normal %s",
            repr_point3f(buffer, sizeof buffer, &face->normal));

        ras_log_buffer("d vs pos %s",
            repr_mat_4x1(buffer, sizeof buffer, view_space_position));
        ras_log_buffer("d projected %s",
            repr_mat_4x1(buffer, sizeof buffer, projected_vec));

        ras_log_buffer("d ss normal %s",
            repr_vector4f(buffer, sizeof buffer, &screen_space_position));

        core_render_point(render_state, &screen_space_position, RAS_COLOR_RAMP_OFFSET_RED);
        core_render_line(render_state, &screen_origin, &screen_space_position);
    }
}

/**
 * @brief Render face normals as transformed with model.
 * For diagnostic purposes, as this transform is not indeded for
 * normal calculations.
 *
 * @param render_state
 * @param vert_buffer
 * @param model_view_matrix
 * @param proj_matrix
 */
void draw_mesh_normals_faux(
    RenderState* render_state,
    RasPipelineMesh* mesh,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{
    RasVector3f origin_vec = { 0, 0, 0 };
    RasFixed projected_origin[4];
    RasVector4f screen_origin;
    RasFixed model_space_position[4];
    RasFixed view_space_position[4];
    char buffer[255];

    // Project model origin for faux mode
    core_vector3f_to_4x1(&origin_vec, model_space_position);
    mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
    mat_mul_project(proj_matrix, view_space_position, projected_origin);

    core_projected_to_screen_point(
        render_state->screen_settings.screen_width,
        render_state->screen_settings.screen_height,
        projected_origin,
        &screen_origin);

    for (size_t i = 0; i < mesh->num_visible_faces; i++) {
        RasPipelineFace* face = &mesh->visible_faces[i];

        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&face->normal, model_space_position);
        mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        ras_log_buffer("d normal %s",
            repr_point3f(buffer, sizeof buffer, &face->normal));

        ras_log_buffer("d vs pos %s",
            repr_mat_4x1(buffer, sizeof buffer, view_space_position));
        ras_log_buffer("d projected %s",
            repr_mat_4x1(buffer, sizeof buffer, projected_vec));

        ras_log_buffer("d ss normal %s",
            repr_vector4f(buffer, sizeof buffer, &screen_space_position));

        core_render_point(render_state, &screen_space_position, RAS_COLOR_RAMP_OFFSET_RED);
        core_render_line(render_state, &screen_origin, &screen_space_position);
    }
}

/**
 * @brief Render he ortho-normal transform of the face normals.
 * To ease visibility, the points are offset down the z-axis.
 *
 * @param render_state
 * @param vert_buffer
 * @param model_view_matrix
 * @param proj_matrix
 */
void draw_element_normals_ortho(
    RenderState* render_state,
    RasPipelineVertexBuffer* vert_buffer,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{

    RasFixed model_space_position[4];
    RasFixed ortho_origin_offset_z = -float_to_fixed_16_16(5.0);
    RasFixed view_space_position[4];
    RasFixed projected_origin[4];
    RasVector4f screen_ortho_origin;
    char buffer[255];

    view_space_position[0] = 0;
    view_space_position[1] = 0;
    view_space_position[2] = ortho_origin_offset_z;
    view_space_position[3] = RAS_FIXED_ONE;
    mat_mul_project(proj_matrix, view_space_position, projected_origin);

    core_projected_to_screen_point(
        render_state->screen_settings.screen_width,
        render_state->screen_settings.screen_height,
        projected_origin,
        &screen_ortho_origin);

    for (size_t i = 0; i < vert_buffer->num_visible_faces; i++) {
        RasPipelineFace* face = &vert_buffer->visible_faces[i];

        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&face->view_space_normal, view_space_position);
        view_space_position[2] += ortho_origin_offset_z;
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        ras_log_buffer("d normal %s",
            repr_point3f(buffer, sizeof buffer, &face->view_space_normal));

        ras_log_buffer("d vs pos %s",
            repr_mat_4x1(buffer, sizeof buffer, view_space_position));
        ras_log_buffer("d projected %s",
            repr_mat_4x1(buffer, sizeof buffer, projected_vec));

        ras_log_buffer("d ss normal %s",
            repr_vector4f(buffer, sizeof buffer, &screen_space_position));

        core_render_line(render_state, &screen_ortho_origin, &screen_space_position);
    }
}

/**
 * @brief Render he ortho-normal transform of the face normals.
 * To ease visibility, the points are offset down the z-axis.
 *
 * @param render_state
 * @param vert_buffer
 * @param model_view_matrix
 * @param proj_matrix
 */
void draw_mesh_normals_ortho(
    RenderState* render_state,
    RasPipelineMesh* mesh,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{

    RasFixed model_space_position[4];
    RasFixed ortho_origin_offset_z = -float_to_fixed_16_16(5.0);
    RasFixed view_space_position[4];
    RasFixed projected_origin[4];
    RasVector4f screen_ortho_origin;
    char buffer[255];

    view_space_position[0] = 0;
    view_space_position[1] = 0;
    view_space_position[2] = ortho_origin_offset_z;
    view_space_position[3] = RAS_FIXED_ONE;
    mat_mul_project(proj_matrix, view_space_position, projected_origin);

    core_projected_to_screen_point(
        render_state->screen_settings.screen_width,
        render_state->screen_settings.screen_height,
        projected_origin,
        &screen_ortho_origin);

    for (size_t i = 0; i < mesh->num_visible_faces; i++) {
        RasPipelineFace* face = &mesh->visible_faces[i];

        RasFixed projected_vec[4];
        RasVector4f screen_space_position;

        core_vector3f_to_4x1(&face->view_space_normal, view_space_position);
        view_space_position[2] += ortho_origin_offset_z;
        mat_mul_project(proj_matrix, view_space_position, projected_vec);

        core_projected_to_screen_point(
            render_state->screen_settings.screen_width,
            render_state->screen_settings.screen_height,
            projected_vec,
            &screen_space_position);

        ras_log_buffer("d normal %s",
            repr_point3f(buffer, sizeof buffer, &face->view_space_normal));

        ras_log_buffer("d vs pos %s",
            repr_mat_4x1(buffer, sizeof buffer, view_space_position));
        ras_log_buffer("d projected %s",
            repr_mat_4x1(buffer, sizeof buffer, projected_vec));

        ras_log_buffer("d ss normal %s",
            repr_vector4f(buffer, sizeof buffer, &screen_space_position));

        core_render_line(render_state, &screen_ortho_origin, &screen_space_position);
    }
}

void draw_element_normals(
    RenderState* render_state,
    RasPipelineVertexBuffer* vert_buffer,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{
    if (render_state->normal_mode == RAS_NORMAL_MODE_FAUX) {
        draw_element_normals_faux(render_state, vert_buffer, model_view_matrix, proj_matrix);
    } else if (render_state->normal_mode == RAS_NORMAL_MODE_ORTHO) {
        draw_element_normals_ortho(render_state, vert_buffer, model_view_matrix, proj_matrix);
    }
}

void core_draw_mesh_normals(
    RenderState* render_state,
    RasPipelineMesh* mesh,
    RasFixed model_view_matrix[4][4],
    RasFixed proj_matrix[4][4])
{
    if (render_state->normal_mode == RAS_NORMAL_MODE_FAUX) {
        draw_mesh_normals_faux(render_state, mesh, model_view_matrix, proj_matrix);
    } else if (render_state->normal_mode == RAS_NORMAL_MODE_ORTHO) {
        draw_mesh_normals_ortho(render_state, mesh, model_view_matrix, proj_matrix);
    }
}
