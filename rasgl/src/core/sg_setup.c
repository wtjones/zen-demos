#include "rasgl/core/aabb.h"
#include "rasgl/core/clip.h"
#include "rasgl/core/event.h"
#include "rasgl/core/grid.h"
#include "rasgl/core/gridmap.h"
#include "rasgl/core/normals.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/stages.h"
#include "rasgl/core/tombmap.h"

void core_pipeline_init(RasPipeline* pipeline)
{
    RasPipeline template = {
        .num_stages = 18,
        .stages = {
            { .name = "core_sg_setup", core_sg_setup },
            { .name = "core_sg_xform_gridmaps", core_sg_xform_gridmaps },
            { .name = "core_sg_xform_tombmaps", core_sg_xform_tombmaps },
            { .name = "core_sg_xform_objects", core_sg_xform_objects },
            { .name = "core_sg_xform_gridmap_aabb", core_sg_xform_gridmap_aabb },
            { .name = "core_sg_xform_tombmap_aabb", core_sg_xform_tombmap_aabb },
            { .name = "core_sg_xform_aabb", core_sg_xform_aabb },
            { .name = "core_sg_render_aabb", core_sg_render_aabb },
            { .name = "core_sg_xform_verts", core_sg_xform_verts },
            { .name = "core_sg_project_to_clip_space", core_sg_project_to_clip_space },
            { .name = "core_sg_clip_flag_verts", core_sg_clip_flag_verts },
            { .name = "core_sg_visible_faces", core_sg_visible_faces },
            { .name = "core_sg_project_to_screen_space", core_sg_project_to_screen_space },
            { .name = "core_sg_cull_backfaces", core_sg_cull_backfaces },
            { .name = "core_sg_xform_normals", core_sg_xform_normals },
            { .name = "core_sg_lighting", core_sg_lighting },
            { .name = "core_sg_draw_normals", core_sg_draw_normals },
            { .name = "core_sg_draw_grid", core_sg_draw_grid } }
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
    render_data->num_visible_gridmaps = 0;
    render_data->num_visible_objects = 0;
    render_data->num_mesh_elements = 0;
    render_data->render_state->num_visible_meshes = 0;

    for (size_t i = 0; i < RAS_MAX_MESHES; i++) {
        mat_set_identity_4x4(render_data->model_world_matrix[i]);
        mat_set_identity_4x4(render_data->model_view_matrix[i]);
        mat_set_identity_4x4(render_data->normal_mvt_matrix[i]);
        render_data->aabb_clip_flags[i] = 0;
    }

    render_state->num_meshes = 0;
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

void* core_sg_xform_gridmaps(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->scene->num_gridmaps; i++) {
        RasSceneGridMap* current_gridmap = &render_data->scene->gridmaps[i];

        uint32_t mesh_index = current_gridmap->mesh_index = render_data->render_state->num_meshes++;
        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

        // Gridmap isn't transformed from object space, so use default model_view_matrix.
        mat_mul_4x4_4x4(
            render_data->world_view_matrix,
            model_world_matrix, model_view_matrix);
        core_mat_normal_init(model_view_matrix, normal_mvt_matrix);

        core_gridmap_to_element_faces(
            current_gridmap,
            render_data->camera,
            &current_gridmap->element);
    }
}

void* core_sg_xform_objects(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];
        uint32_t mesh_index = current_object->mesh_index = render_data->render_state->num_meshes++;
        RasVector3f* model_pos = &current_object->position;
        RasVector3f* model_rotation = &current_object->rotation;
        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

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
        ras_log_buffer_trace("Model world matrix: %s", repr_mat_4x4(buffer, sizeof buffer, model_world_matrix));
        ras_log_buffer_trace("Model view matrix: %s", repr_mat_4x4(buffer, sizeof buffer, model_view_matrix));
        core_mat_normal_init(model_view_matrix, normal_mvt_matrix);
        ras_log_buffer_trace("normal mvt: %s", repr_mat_4x4(buffer, sizeof buffer, normal_mvt_matrix));
    }
}

void* core_sg_xform_gridmap_aabb(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

    if (render_data->scene->num_gridmaps == 0) {
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

    RasSceneGridMap* current_gridmap = &render_data->scene->gridmaps[0];
    RasPipelineElement* element = &current_gridmap->element;
    const uint32_t mesh_index = current_gridmap->mesh_index;
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
        "Gridmap AABB flags: %hhu, all_out: %s\n",
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

void* core_sg_xform_aabb(void* input)
{
    char buffer[1000];
    RasRenderData* render_data = (RasRenderData*)input;

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

    for (size_t i = 0; i < render_data->scene->num_objects; i++) {
        RasSceneObject* current_object = &render_data->scene->objects[i];
        RasPipelineElement* element = current_object->element_ref;
        const uint32_t mesh_index = current_object->mesh_index;
        RasAABB* view_aabb = &render_data->aabbs[mesh_index];

        core_aabb_xform(&element->aabb, render_data->model_view_matrix[mesh_index], view_aabb);

        ras_log_buffer("AABB orig min: %s\n", repr_point3f(buffer, sizeof buffer, &element->aabb.min));
        ras_log_buffer("AABB view min: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->min));
        ras_log_buffer("AABB view max: %s\n", repr_point3f(buffer, sizeof buffer, &view_aabb->max));

        bool all_out = core_aabb_in_frustum(
            view_aabb,
            &render_data->frustum,
            use_far_plane,
            &render_data->aabb_clip_flags[mesh_index]);

        ras_log_buffer("AABB flags: %hhu, all_out: %s\n", render_data->aabb_clip_flags[mesh_index], all_out ? "true" : "false");
        if (!all_out) {
            assert(render_data->num_visible_objects < RAS_MAX_SCENE_OBJECTS);
            assert(render_data->render_state->num_visible_meshes < RAS_MAX_MESHES);

            render_data->visible_objects[render_data->num_visible_objects++] = i;
            render_data->mesh_elements[render_data->num_mesh_elements].mesh_index = mesh_index;
            render_data->mesh_elements[render_data->num_mesh_elements].element_ref = element;
            render_data->num_mesh_elements++;

            render_data->render_state->visible_meshes[render_data->render_state->num_visible_meshes] = mesh_index;
            render_data->render_state->num_visible_meshes++;
        }
    }
}

void* core_sg_render_aabb(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    RenderState* render_state = render_data->render_state;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
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
    size_t xformed_verts = 0;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineElement* element = render_data->mesh_elements[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        RasFixed(*model_world_matrix)[4] = render_data->model_world_matrix[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

        render_data->num_verts_in_frustum[mesh_index] = 0;
        mesh->num_verts = element->num_verts;

        for (uint32_t j = 0; j < element->num_verts; j++) {
            RasVertex* vertex = &element->verts[j];
            RasPipelineVertex* pv = &mesh->verts[j];

            RasFixed model_space_position[4];
            RasFixed view_space_position[4];
            RasFixed screen_space_vec[4];
            RasFixed projected_vec[4];

            core_vector3f_to_4x1(&vertex->position, model_space_position);
            mat_mul_4x4_4x1(model_view_matrix, model_space_position, view_space_position);
            core_4x1_to_vector3f(view_space_position, &pv->view_space_position);

            pv->color = vertex->color;
            pv->u = vertex->u;
            pv->v = vertex->v;
        }
    }
    ras_log_buffer(
        "Transformed %d verts",
        xformed_verts);
}

void* core_sg_project_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
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

void* core_sg_project_to_clip_space(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        for (size_t j = 0; j < mesh->num_verts; j++) {
            RasPipelineVertex* pv = &mesh->verts[j];
            RasFixed view_space_position[4];
            RasFixed screen_space_vec[4];
            RasFixed projected_vec[4];

            core_view_space_to_clip_space(
                render_data->projection_matrix,
                &pv->view_space_position,
                &pv->clip_space_position);
        }
    }
}

void* core_sg_clip_flag_verts(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        render_data->num_verts_in_frustum[mesh_index] = 0;

        for (uint32_t j = 0; j < mesh->num_verts; j++) {

            RasPipelineVertex* pv = &mesh->verts[j];

            core_set_pv_clip_flags(
                pv);

            pv->aabb_clip_flags = render_data->aabb_clip_flags[mesh_index];

            render_data->num_verts_in_frustum[mesh_index] += pv->clip_flags == 0 ? 1 : 0;
        }
        ras_log_buffer(
            "Mesh id %d has %d verts in frustum",
            mesh_index,
            render_data->num_verts_in_frustum[mesh_index]);
    }
}

void* core_sg_project_to_screen_space(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];

        render_data->num_verts_in_frustum[mesh_index] = 0;

        for (uint32_t j = 0; j < mesh->num_verts; j++) {

            RasPipelineVertex* pv = &mesh->verts[j];

            if (pv->clip_space_position.w <= 0) {
                continue;
            }
            render_data->num_verts_in_frustum[mesh_index]++;
            RasFixed clip_space_position[4];
            RasFixed ndc_space_vec[4];

            core_vector4f_to_4x1(&pv->clip_space_position, clip_space_position);

            // Perform perspective divide to get NDC coords.
            core_clip_space_to_ndc(
                clip_space_position,
                ndc_space_vec);

            core_4x1_to_vector4f(ndc_space_vec, &pv->ndc_space_position);

            static char buffer[255];
            ras_log_buffer_trace("ndc pos: %s\n", repr_vector4f(buffer, sizeof buffer, &pv->ndc_space_position));

            core_projected_to_screen_point(
                render_data->render_state->screen_settings.screen_width,
                render_data->render_state->screen_settings.screen_height,
                ndc_space_vec,
                &pv->screen_space_position);
        }
    }
}

void* core_sg_visible_faces(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    size_t* num_faces_to_clip = &render_data->num_faces_to_clip;
    *num_faces_to_clip = 0;

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

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineElement* element = render_data->mesh_elements[i].element_ref;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        uint32_t* num_dest_faces = &mesh->num_visible_faces;
        uint32_t* vi = &mesh->num_visible_indexes;
        uint32_t* num_dest_materials = &mesh->num_material_indexes;
        *num_dest_materials = 0;

        render_data->num_faces_in_frustum[mesh_index] = 0;
        render_data->num_backfaces[mesh_index] = 0;
        uint32_t current_src_face_index = 0;
        uint16_t num_faces_excluded = 0;

        if (element->num_verts >= MAX_PIPELINE_VERTS) {
            ras_log_error("Reached max visible verts for mesh %d: %d",
                mesh_index,
                MAX_PIPELINE_VERTS);
            assert(false);
        }
        for (uint32_t i = 0; i < element->num_indexes; i += 3) {

            RasPipelineVertex* pv1 = &mesh->verts[element->indexes[i]];
            RasPipelineVertex* pv2 = &mesh->verts[element->indexes[i + 1]];
            RasPipelineVertex* pv3 = &mesh->verts[element->indexes[i + 2]];

            bool all_out = use_far_plane
                ? pv1->clip_flags & pv2->clip_flags & pv3->clip_flags
                : (pv1->clip_flags & ~core_to_clip_flag(PLANE_FAR))
                    & (pv2->clip_flags & ~core_to_clip_flag(PLANE_FAR))
                    & (pv3->clip_flags & ~core_to_clip_flag(PLANE_FAR));

            if (all_out) {
                current_src_face_index++;

                continue; // face is all out
            }
            render_data->num_faces_in_frustum[mesh_index] += 1;

            /**
             * @brief If dropped due to clipping exclusion, skip out to avoid
             * adding the face to the visible faces list.
             *
             */
            RasClipFlags face_clip_flags = pv1->clip_flags | pv2->clip_flags | pv3->clip_flags;
            bool clip_exclude = render_data->render_state->clipping_mode == RAS_CLIPPING_EXCLUDE;
            bool clip_on = render_data->render_state->clipping_mode == RAS_CLIPPING_ON;

            if (face_clip_flags != 0 && clip_exclude) {
                num_faces_excluded++;
                current_src_face_index++;
                continue;
            }

            if (face_clip_flags != 0 && clip_on) {
                ras_log_buffer_trace("scenario: face_clip_flags: %d\n", face_clip_flags);
                RasPipelineVertex* in_verts[3] = { pv1, pv2, pv3 };
                RasPipelineVertex out_verts[RAS_MAX_CLIP_OUT_VERTS];
                size_t num_out_verts = 0;
                (*num_faces_to_clip)++;

                core_clip_face(
                    render_data->render_state->clip_side_mode,
                    in_verts,
                    out_verts,
                    &num_out_verts,
                    RAS_MAX_CLIP_OUT_VERTS);

                static char buffer[255];
                ras_log_buffer_trace("clip2: num_out_verts: %d\n", num_out_verts);
                for (size_t j = 0; j < num_out_verts; j++) {
                    ras_log_buffer_trace("clip2: out_verts[%zu]: %s\n", j, repr_point3f(buffer, sizeof buffer, &out_verts[j].view_space_position));
                }

                if (mesh->num_verts + num_out_verts > MAX_PIPELINE_VERTS) {
                    ras_log_error("Reached max visible verts for mesh %d: %d",
                        mesh_index,
                        MAX_PIPELINE_VERTS);
                    assert(false);
                }

                // Copy out verts to mesh
                for (size_t j = 0; j < num_out_verts; j++) {

                    int32_t sx = FIXED_16_16_TO_INT_32(out_verts[j].screen_space_position.x);
                    int32_t sy = FIXED_16_16_TO_INT_32(out_verts[j].screen_space_position.y);

                    if (sx < 0
                        || sx > (int32_t)(render_data->render_state->screen_settings.screen_width - 1)
                        || sy < 0
                        || sy > (int32_t)(render_data->render_state->screen_settings.screen_height - 1)) {
                        ras_log_buffer_ex(
                            RAS_EVENT_RS_OOB,
                            "Vertex id: %d out of bounds: %s\nsx: %d, sy: %d",
                            j,
                            repr_vector4f(buffer, sizeof buffer, &out_verts[j].screen_space_position),
                            sx,
                            sy);
                    }
                    mesh->verts[mesh->num_verts] = out_verts[j];
                    mesh->visible_indexes[*vi] = mesh->num_verts;
                    mesh->num_verts++;
                    (*vi) += 1;
                }

                // Copy material indexes for each new face created from clipping
                assert(*num_dest_faces + num_out_verts / 3 < MAX_PIPELINE_FACES);
                assert(*num_dest_materials + num_out_verts / 3 < MAX_VISIBLE_INDEXES);
                for (size_t j = 0; j < num_out_verts / 3; j++) {
                    mesh->material_indexes[*num_dest_materials] = element->material_indexes[current_src_face_index];
                    mesh->visible_faces[*num_dest_faces].normal = element->faces[current_src_face_index].normal;
                    mesh->visible_faces[*num_dest_faces].material_index = element->faces[current_src_face_index].material_index;
                    (*num_dest_materials)++;
                    (*num_dest_faces)++;
                }

                current_src_face_index++;
                continue;
            }

            // The face will be added to:
            // - visible indexes
            // - visible faces
            // - material indexes
            mesh->visible_indexes[*vi] = element->indexes[i];
            mesh->visible_indexes[*vi + 1] = element->indexes[i + 1];
            mesh->visible_indexes[*vi + 2] = element->indexes[i + 2];
            (*vi) += 3;

            // copy material
            mesh->material_indexes[*num_dest_materials]
                = element->material_indexes[current_src_face_index];
            (*num_dest_materials)++;

            // Copy face
            RasPipelineFace* face = &mesh->visible_faces[*num_dest_faces];
            face->clip_flags = face_clip_flags;
            RasElementFace* src_face = &element->faces[current_src_face_index];
            face->normal = src_face->normal;
            face->material_index = src_face->material_index;
            face->outline_edges = src_face->outline_edges;
            face->outline_material_index = src_face->outline_material_index;
            (*num_dest_faces)++;
            current_src_face_index++;
        }

        ras_log_buffer_ex(
            RAS_EVENT_SG_SUMMARY,
            "Element faces:\n    In Model: %d. In frustum: %d. Visible: %d. Excluded: %d. To Clip: %d. Backfaces: %d.",
            element->num_indexes / 3,
            render_data->num_faces_in_frustum[mesh_index],
            mesh->num_visible_indexes / 3,
            num_faces_excluded,
            *num_faces_to_clip,
            render_data->num_backfaces[mesh_index]);
    }
}

void* core_sg_cull_backfaces(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    bool is_culling = render_data->render_state->backface_culling_mode == RAS_BACKFACE_CULLING_ON;

    if (!is_culling) {
        return NULL;
    }

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        uint32_t* vi = &mesh->num_visible_indexes;

        render_data->num_backfaces[mesh_index] = 0;
        size_t new_num_visible_indexes = 0;
        uint32_t* num_dest_materials = &mesh->num_material_indexes;
        *num_dest_materials = 0;
        uint32_t* num_dest_faces = &mesh->num_visible_faces;
        *num_dest_faces = 0;

        for (uint32_t i = 0; i < mesh->num_visible_indexes; i += 3) {
            RasPipelineVertex* pv1 = &mesh->verts[mesh->visible_indexes[i]];
            RasPipelineVertex* pv2 = &mesh->verts[mesh->visible_indexes[i + 1]];
            RasPipelineVertex* pv3 = &mesh->verts[mesh->visible_indexes[i + 2]];

            bool is_backface = core_is_backface(
                &pv1->ndc_space_position,
                &pv2->ndc_space_position,
                &pv3->ndc_space_position);

            if (is_backface) {
                render_data->num_backfaces[mesh_index] += 1;
            } else {
                mesh->visible_indexes[new_num_visible_indexes] = mesh->visible_indexes[i];
                mesh->visible_indexes[new_num_visible_indexes + 1] = mesh->visible_indexes[i + 1];
                mesh->visible_indexes[new_num_visible_indexes + 2] = mesh->visible_indexes[i + 2];
                new_num_visible_indexes += 3;

                mesh->material_indexes[*num_dest_materials]
                    = mesh->material_indexes[i / 3];
                (*num_dest_materials)++;

                RasPipelineFace* face = &mesh->visible_faces[*num_dest_faces];
                memcpy(face, &mesh->visible_faces[i / 3], sizeof(RasPipelineFace));
                (*num_dest_faces)++;
            }
        }
        mesh->num_visible_indexes = new_num_visible_indexes;
    }
}

void* core_sg_xform_normals(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

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

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        RasFixed(*normal_mvt_matrix)[4] = render_data->normal_mvt_matrix[mesh_index];

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

void* core_sg_draw_normals(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    for (uint32_t i = 0; i < render_data->num_mesh_elements; i++) {
        uint32_t mesh_index = render_data->mesh_elements[i].mesh_index;
        RasPipelineMesh* mesh = &render_data->render_state->meshes[mesh_index];
        RasFixed(*model_view_matrix)[4] = render_data->model_view_matrix[mesh_index];

        core_draw_mesh_normals(
            render_data->render_state,
            mesh,
            model_view_matrix,
            render_data->projection_matrix);
    }
}

void* core_sg_draw_grid(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;

    core_draw_grid(
        render_data->render_state,
        render_data->world_view_matrix,
        render_data->projection_matrix);
}
