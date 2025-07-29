#ifndef CORE_STAGES_H
#define CORE_STAGES_H

#include "camera.h"
#include "pipeline.h"
#include "scene.h"

typedef struct {
    uint32_t mesh_index;
    RasPipelineElement* element_ref;
} RasMeshElement;

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

    /**
     * @brief Represents visible meshes and related models.
     *
     */
    RasMeshElement mesh_elements[RAS_MAX_MESHES];
    size_t num_mesh_elements;

    RasPipelineVertexBuffer vert_buffer; // needed?
    uint32_t num_verts_in_frustum[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_faces_in_frustum[RAS_MAX_SCENE_OBJECTS];

} RasRenderData;

/**
 * @brief Initialize the non-platform specific pipeline.
 *
 * @param pipeline
 */
void core_pipeline_init(RasPipeline* pipeline);

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

/**
 * @brief Transforms the axis-aligned bounding boxes (AABBs) of all objects
 * in the scene.
 *
 * - Sets clip flags.
 * - Populates visible objects array as mapped to meshes array.
 * - Populates visible meshes array with a mapping to element.
 *      - This allows later stages to work without the context of objects.
 * @param input
 * @return void*
 */
void* core_sg_xform_aabb(void* input);
void* core_sg_render_aabb(void* input);
void* core_sg_xform_verts(void* input);
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

/**
 * @brief Transform face normals for lighting calculations.
 *
 * @param input
 * @return void*
 */
void* core_sg_xform_normals(void* input);

/**
 * @brief Calculate lighting for each visible face.
 *
 * @param input
 * @return void*
 */
void* core_sg_lighting(void* input);
#endif
