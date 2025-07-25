#include "rasgl/core/aabb.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/stages.h"

void core_renderdata_init(
    RasRenderData* render_data,
    RenderState* render_state,
    RasScene* scene,
    RasCamera* camera)
{
    render_data->render_state = render_state;
    render_data->scene = scene;
    render_data->camera = camera;
    mat_set_identity_4x4(render_data->world_view_matrix);
    mat_set_identity_4x4(render_data->projection_matrix);
    render_data->num_visible_objects = 0;
    render_data->vert_buffer.num_verts = 0;

    for (size_t i = 0; i < RAS_MAX_SCENE_OBJECTS; i++) {
        mat_set_identity_4x4(render_data->model_world_matrix[i]);
        mat_set_identity_4x4(render_data->model_view_matrix[i]);
        mat_set_identity_4x4(render_data->normal_mvt_matrix[i]);
        render_data->aabb_clip_flags[i] = 0;
    }
}

void* core_sg_setup(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    if (render_data == NULL || render_data->scene == NULL || render_data->camera == NULL) {
        ras_log_error("Invalid render data or scene/camera not set.");
        return NULL;
    }
    ras_camera_projection_init(render_data->camera, render_data->projection_matrix);
    mat_set_identity_4x4(render_data->world_view_matrix);
    ras_camera_world_view_init(render_data->camera, render_data->world_view_matrix);
    core_frustum_init(render_data->projection_matrix, &render_data->frustum);

    return render_data;
}

void* core_sg_xform_objects(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];
        RasVector3f* model_pos = &current_object->position;
        RasVector3f* model_rotation = &current_object->rotation;
        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[i];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[i];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[i];
        RasFixed model_world1[4][4];
        RasFixed model_world2[4][4];
        RasFixed model_world3[4][4];

        mat_set_identity_4x4(model_world1);

        core_translate_apply(model_world1, model_pos);

        // FIXME: Should rotate around model origin
        core_rotate_x_apply(model_world1, FIXED_16_16_TO_INT_32(model_rotation->x));
        mat_rotate_y(model_world1, FIXED_16_16_TO_INT_32(model_rotation->y), model_world2);
        mat_rotate_z(model_world2, FIXED_16_16_TO_INT_32(model_rotation->z), model_world_matrix);

        core_translate_apply(model_world_matrix, model_pos);

        mat_mul_4x4_4x4(
            render_data->world_view_matrix,
            model_world_matrix, model_view_matrix);
        ras_log_buffer("Model world matrix: %s", repr_mat_4x4(buffer, sizeof buffer, model_world_matrix));
        ras_log_buffer("Model view matrix: %s", repr_mat_4x4(buffer, sizeof buffer, model_view_matrix));
        core_mat_normal_init(model_view_matrix, normal_mvt_matrix);
        ras_log_buffer("normal mvt: %s", repr_mat_4x4(buffer, sizeof buffer, normal_mvt_matrix));
    }
}

/**
 * @brief Transforms the axis-aligned bounding boxes (AABBs) of all objects in the scene.
 * Sets clip flags and populates visible objects array.
 *
 * @param input
 * @return void*
 */
void* core_sg_xform_aabb(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];
        RasPipelineElement* element = current_object->element_ref;
        RasAABB* view_aabb = &render_data->aabbs[i];

        core_aabb_xform(&element->aabb, render_data->model_view_matrix[i], view_aabb);

        ras_log_buffer("AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
        ras_log_buffer("AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->min));
        ras_log_buffer("AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->max));

        bool all_out = core_aabb_in_frustum(view_aabb, &render_data->frustum, &render_data->aabb_clip_flags[i]);

        ras_log_buffer("AABB flags: %hhu, all_out: %s\n", render_data->aabb_clip_flags[i], all_out ? "true" : "false");
        if (!all_out) {
            render_data->visible_objects[render_data->num_visible_objects++] = i;
        }
    }
}

void* core_sg_render_aabb(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    RenderState* render_state = render_data->render_state;

    for (uint32_t i = 0; i < render_data->num_visible_objects; i++) {
        uint32_t object_index = render_data->visible_objects[i];
        RasAABB* view_aabb = &render_data->aabbs[object_index];
        core_render_aabb(
            render_state,
            render_data->projection_matrix,
            &render_data->frustum,
            view_aabb);
    }
}

void* core_sg_xform_object_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->num_visible_objects; i++) {
        uint32_t object_index = render_data->visible_objects[i];
        RasSceneObject* current_object = &render_data->scene->objects[object_index];
        RasPipelineElement* element = current_object->element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[object_index];

        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[i];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[i];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[i];

        render_data->num_verts_in_frustum[object_index] = 0;
        mesh->num_verts = element->num_verts;

        for (uint32_t i = 0; i < element->num_verts; i++) {
            RasVertex* vertex = &element->verts[i];
            RasPipelineVertex* pv = &mesh->verts[i];

            RasFixed model_space_position[4];
            RasFixed view_space_position[4];
            RasFixed screen_space_vec[4];
            RasFixed projected_vec[4];

            core_vector3f_to_4x1(&vertex->position, model_space_position);
            mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
            core_4x1_to_vector3f(view_space_position, &pv->view_space_position);

            core_set_pv_clip_flags(
                &render_data->frustum,
                render_data->aabb_clip_flags[object_index],
                pv);

            pv->aabb_clip_flags = render_data->aabb_clip_flags[object_index];

            render_data->num_verts_in_frustum[object_index] += pv->clip_flags == 0 ? 1 : 0;

            pv->color = vertex->color;
            pv->u = vertex->u;
            pv->v = vertex->v;
        }
        ras_log_debug("Transformed %d verts in frustum", render_data->num_verts_in_frustum[object_index]);
    }
}

void* core_sg_project_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->num_visible_objects; i++) {
        uint32_t object_index = render_data->visible_objects[i];

        RasPipelineMesh* mesh = &render_data->render_state->meshes[object_index];

        for (size_t j = 0; j < mesh->num_verts; j++) {
            RasPipelineVertex* pv = &mesh->verts[j];
            RasFixed view_space_position[4];
            RasFixed screen_space_vec[4];
            RasFixed projected_vec[4];

            // Project to screen space
            core_vector3f_to_4x1(&pv->view_space_position, view_space_position);
            // Screen space in NDC coords
            mat_mul_project(render_data->projection_matrix, view_space_position, projected_vec);

            core_projected_to_screen_point(
                render_data->render_state->screen_settings.screen_width,
                render_data->render_state->screen_settings.screen_height,
                projected_vec,
                &pv->screen_space_position);

            static char buffer[255];
            ras_log_buffer_trace("pipeline screen space pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->screen_space_position));
        }
    }
}
