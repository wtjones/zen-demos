#ifndef CORE_STAGES_H
#define CORE_STAGES_H

#include "camera.h"
#include "pipeline.h"
#include "scene.h"

typedef struct {
    RenderState* render_state;
    RasScene* scene;
    /**
     * @brief Should point to a camera in the scene.
     *
     */
    RasCamera* camera;
    RasFrustum frustum;
    RasFixed projection_matrix[4][4];
    RasFixed world_view_matrix[4][4];
    RasFixed model_world_matrix[RAS_MAX_SCENE_OBJECTS][4][4];
    RasFixed model_view_matrix[RAS_MAX_SCENE_OBJECTS][4][4];
    RasFixed normal_mvt_matrix[RAS_MAX_SCENE_OBJECTS][4][4];
    RasAABB aabbs[RAS_MAX_SCENE_OBJECTS];
    RasClipFlags aabb_clip_flags[RAS_MAX_SCENE_OBJECTS];
    /**
     * @brief Indexes of visible objects.
     * The indices match the render_state->meshes array.
     */
    uint32_t visible_objects[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_visible_objects;
    RasPipelineVertexBuffer vert_buffer; // needed?
    uint32_t num_verts_in_frustum[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_faces_in_frustum[RAS_MAX_SCENE_OBJECTS];

} RasRenderData;

void core_renderdata_init(
    RasRenderData* render_data,
    RenderState* render_state,
    RasScene* scene,
    RasCamera* camera);

void* core_sg_setup(void* input);
/**
 * @brief Transform object matrices
 *
 * @param input
 * @return void*
 */
void* core_sg_xform_objects(void* input);
void* core_sg_xform_aabb(void* input);
void* core_sg_render_aabb(void* input);
void* core_sg_xform_object_verts(void* input);
void* core_sg_project_verts(void* input);
/**
 * @brief Populate visible mesh faces (visible_indexes) by:
 * - backface culling faces
 * - clip flag evaluation of face vertices
 *
 * @param input
 * @return void*
 */
void* core_sg_visible_faces(void* input);
#endif
