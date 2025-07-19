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
    ras_log_info("Setting up render data for scene: %s", render_data->scene->name);
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
        RasAABB view_aabb;

        core_aabb_xform(&element->aabb, render_data->model_view_matrix[i], &view_aabb);

        ras_log_buffer("AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
        ras_log_buffer("AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.min));
        ras_log_buffer("AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb.max));

        bool all_out = core_aabb_in_frustum(&view_aabb, &render_data->frustum, &render_data->aabb_clip_flags[i]);

        ras_log_buffer("AABB flags: %hhu, all_out: %s\n", render_data->aabb_clip_flags[i], all_out ? "true" : "false");
        if (!all_out) {
            render_data->visible_objects[render_data->num_visible_objects++] = i;
        }
    }
}
