#include "rasgl/core/aabb.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/normals.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/stages.h"
#include "rasgl/core/tombmap.h"

void* core_sg_xform_tombmaps(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    RasSceneTombMap* tombmap = render_data->scene->num_tombmaps == 1
        ? &render_data->scene->tombmaps[0]
        : NULL;
    if (tombmap == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < tombmap->num_rooms; i++) {
        RasTombMapRoom* current_room = &tombmap->rooms[i];

        uint32_t mesh_index = current_room->mesh_index = render_data->render_state->num_meshes++;
        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

        // Gridmap isn't transformed from object space, so use default model_view_matrix.
        mat_mul_4x4_4x4(
            render_data->world_view_matrix,
            model_world_matrix, model_view_matrix);
        core_mat_normal_init(model_view_matrix, normal_mvt_matrix);

        core_tombmap_room_to_element_faces(
            current_room,
            render_data->camera,
            &current_room->element);
    }
}

void* core_sg_xform_tombmap_aabb(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    RasSceneTombMap* tombmap = render_data->scene->num_tombmaps == 1
        ? &render_data->scene->tombmaps[0]
        : NULL;
    if (tombmap == NULL) {
        return NULL;
    }

    size_t num_frustum_planes = 0;
    RasFrustumPlane* frustum_planes = get_side_mode_planes(
        render_data->render_state->clip_side_mode,
        &num_frustum_planes);
    bool use_far_plane = false;
    for (size_t i = 0; i < num_frustum_planes; i++) {
        if (frustum_planes[i] == PLANE_FAR) {
            use_far_plane = true;
            break;
        }
    }

    RasTombMapRoom* current_room = &tombmap->rooms[0];
    RasPipelineElement* element = &current_room->element;
    const uint32_t mesh_index = current_room->mesh_index;
    RasAABB* view_aabb = &render_data->aabbs[mesh_index];

    core_aabb_xform(&element->aabb, render_data->model_view_matrix[mesh_index], view_aabb);

    ras_log_buffer("Gridmap AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
    ras_log_buffer("Gridmap AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->min));
    ras_log_buffer("Gridmap AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->max));

    bool all_out = core_aabb_in_frustum(
        view_aabb,
        &render_data->frustum,
        use_far_plane,
        &render_data->aabb_clip_flags[mesh_index]);

    ras_log_buffer(
        "Tombmap AABB flags: %hhu, all_out: %s\n",
        render_data->aabb_clip_flags[mesh_index],
        all_out ? "true" : "false");
    if (!all_out) {

        render_data->mesh_elements[render_data->num_mesh_elements].mesh_index = mesh_index;
        render_data->mesh_elements[render_data->num_mesh_elements].element_ref = element;
        render_data->num_mesh_elements++;

        render_data->render_state->visible_meshes[render_data->render_state->num_visible_meshes] = mesh_index;
        render_data->render_state->num_visible_meshes++;
    }
}
