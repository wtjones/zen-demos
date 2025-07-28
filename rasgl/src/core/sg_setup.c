#include "rasgl/core/aabb.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/stages.h"

void core_pipeline_init(RasPipeline* pipeline)
{
    RasPipeline template = {
        .num_stages = 9,
        .stages = {
            { .name = "core_sg_setup", core_sg_setup },
            { .name = "core_sg_xform_objects", core_sg_xform_objects },
            { .name = "core_sg_xform_aabb", core_sg_xform_aabb },
            { .name = "core_sg_render_aabb", core_sg_render_aabb },
            { .name = "core_sg_xform_verts", core_sg_xform_verts },
            { .name = "core_sg_project_verts", core_sg_project_verts },
            { .name = "core_sg_visible_faces", core_sg_visible_faces },
            { .name = "core_sg_xform_normals", core_sg_xform_normals },
            { .name = "core_sg_lighting", core_sg_lighting } }
    };

    memcpy(pipeline, &template, sizeof(RasPipeline));
    ras_log_info("Pipeline initialized with %d stages", pipeline->num_stages);
}

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
    render_data->num_visible_meshes = 0;

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

        bool all_out = core_aabb_in_frustum(
            view_aabb,
            &render_data->frustum,
            &render_data->aabb_clip_flags[i]);

        ras_log_buffer("AABB flags: %hhu, all_out: %s\n", render_data->aabb_clip_flags[i], all_out ? "true" : "false");
        if (!all_out) {
            render_data->visible_objects[render_data->num_visible_objects++] = i;
            render_data->visible_meshes[render_data->num_visible_meshes].mesh_index = i;
            render_data->visible_meshes[render_data->num_visible_meshes].element_ref = element;
            render_data->num_visible_meshes++;
        }
    }
}

void* core_sg_render_aabb(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    RenderState* render_state = render_data->render_state;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasAABB* view_aabb = &render_data->aabbs[mesh_index];
        core_render_aabb(
            render_state,
            render_data->projection_matrix,
            &render_data->frustum,
            view_aabb);
    }
}

void* core_sg_xform_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasPipelineElement* element = render_data->visible_meshes[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[i];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[i];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[i];

        render_data->num_verts_in_frustum[mesh_index] = 0;
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
                render_data->aabb_clip_flags[mesh_index],
                pv);

            pv->aabb_clip_flags = render_data->aabb_clip_flags[mesh_index];

            render_data->num_verts_in_frustum[mesh_index] += pv->clip_flags == 0 ? 1 : 0;

            pv->color = vertex->color;
            pv->u = vertex->u;
            pv->v = vertex->v;
        }
        ras_log_debug(
            "Transformed %d verts in frustum",
            render_data->num_verts_in_frustum[mesh_index]);
    }
}

void* core_sg_project_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasPipelineElement* element = render_data->visible_meshes[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        for (size_t j = 0; j < mesh->num_verts; j++) {
            RasPipelineVertex* pv = &mesh->verts[j];
            RasFixed view_space_position[4];
            RasFixed screen_space_vec[4];
            RasFixed projected_vec[4];

            // Project to screen space
            core_vector3f_to_4x1(&pv->view_space_position, view_space_position);
            // Screen space in NDC coords
            mat_mul_project(
                render_data->projection_matrix,
                view_space_position,
                projected_vec);

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

void* core_sg_visible_faces(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasPipelineElement* element = render_data->visible_meshes[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        uint32_t* num_dest_faces = &mesh->num_visible_faces;
        uint32_t* vi = &mesh->num_visible_indexes;
        uint32_t* num_dest_materials = &mesh->num_material_indexes;
        *num_dest_materials = 0;

        render_data->num_faces_in_frustum[mesh_index] = 0;
        uint32_t current_src_face_index = 0;

        for (uint32_t i = 0; i < element->num_indexes; i += 3) {
            RasPipelineVertex* pv1 = &mesh->verts[element->indexes[i]];
            RasPipelineVertex* pv2 = &mesh->verts[element->indexes[i + 1]];
            RasPipelineVertex* pv3 = &mesh->verts[element->indexes[i + 2]];

            if (pv1->clip_flags & pv2->clip_flags & pv3->clip_flags) {

                current_src_face_index++;

                continue; // face is all out
            }
            render_data->num_faces_in_frustum[mesh_index] += 1;

            bool is_backface = core_is_backface(
                &pv1->screen_space_position,
                &pv2->screen_space_position,
                &pv3->screen_space_position);
            bool is_culling = render_data->render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON;
            if (is_backface && is_culling) {
                current_src_face_index++;
                continue;
            }

            mesh->visible_indexes[*vi] = element->indexes[i];
            mesh->visible_indexes[*vi + 1] = element->indexes[i + 1];
            mesh->visible_indexes[*vi + 2] = element->indexes[i + 2];
            (*vi) += 3;

            // copy material
            mesh->material_indexes[*num_dest_materials]
                = element->material_indexes[current_src_face_index];
            (*num_dest_materials)++;

            // Copy face
            mesh->visible_faces[*num_dest_faces].normal = element->faces[current_src_face_index].normal;
            mesh->visible_faces[*num_dest_faces].material_index = element->faces[current_src_face_index].material_index;
            (*num_dest_faces)++;
            current_src_face_index++;
        }

        ras_log_buffer(
            "Faces:\n    In Model: %d. In frustum: %d. Visible: %d.",
            element->num_indexes / 3,
            render_data->num_faces_in_frustum[mesh_index],
            mesh->num_visible_indexes / 3);
    }
}

void* core_sg_xform_normals(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[i];

        for (uint32_t j = 0; j < mesh->num_visible_faces; j++) {
            RasPipelineFace* face = &mesh->visible_faces[j];
            RasFixed model_space_normal[4];
            RasFixed view_space_normal[4];

            core_vector3f_to_4x1(&face->normal, model_space_normal);
            mat_mul_4x4_4x1(normal_mvt_matrix, model_space_normal, view_space_normal);
            core_4x1_to_vector3f(view_space_normal, &face->view_space_normal);
        }
    }
}

void* core_sg_lighting(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_visible_meshes; i++) {
        uint32_t mesh_index = render_data->visible_meshes[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[i];

        for (uint32_t j = 0; j < mesh->num_visible_faces; j++) {
            RasPipelineFace* face = &mesh->visible_faces[j];
            RasVector3f camera_pos = { 0, 0, 0 };
            RasVector3f light_pos = { 0, 0, RAS_FIXED_ONE };

            core_light_poly(
                face,
                &camera_pos,
                &light_pos);
        }
    }
}
