#ifndef CORE_STAGES_H
#define CORE_STAGES_H

#include "camera.h"
#include "pipeline.h"
#include "scene.h"

typedef struct {
    uint32_t mesh_index;
    RasPipelineElement* element_ref;
} RasMeshElement;

/**
 * @brief Mapping structure to identify faces to work in the pipeline.
 *
 */
typedef struct {
    uint32_t mesh_index;
    uint32_t face_index;
    RasClipFlags clip_flags;
} RasMeshFace;

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
    RasFixed model_world_matrix[RAS_MAX_MESHES][4][4];
    RasFixed model_view_matrix[RAS_MAX_MESHES][4][4];
    RasFixed normal_mvt_matrix[RAS_MAX_MESHES][4][4];

    /**
     * @brief Axis-aligned bounding boxes for each mesh.
     * The indices match the render_state->meshes array.
     */
    RasAABB aabbs[RAS_MAX_MESHES];
    RasClipFlags aabb_clip_flags[RAS_MAX_MESHES];

    /**
     * @brief Indices of visible gridmaps.
     * The indices match the render_state->meshes array.
     */
    uint32_t visible_gridmaps[RAS_MAX_SCENE_GRIDMAPS];
    uint32_t num_visible_gridmaps;

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

    RasMeshFace faces_to_clip[RAS_MAX_MESHES * 10];
    size_t num_faces_to_clip;

    RasPipelineVertexBuffer vert_buffer; // needed?
    uint32_t num_verts_in_frustum[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_faces_in_frustum[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_backfaces[RAS_MAX_SCENE_OBJECTS];

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

void* core_sg_xform_gridmaps(void* input);
void* core_sg_xform_tombmaps(void* input);
/**
 * @brief Transform object matrices
 *
 * @param input
 * @return void*
 */
void* core_sg_xform_objects(void* input);

void* core_sg_xform_gridmap_aabb(void* input);
void* core_sg_xform_tombmap_aabb(void* input);

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
void* core_sg_project_to_clip_space(void* input);
/**
 * @brief Populate visible mesh faces (visible_indexes) by:
 * - backface culling faces
 * - clip flag evaluation of face vertices
 *
 * @param input
 * @return void*
 */
/**
 * @brief Set clip flags of clip-space vertices.
 */
void* core_sg_clip_flag_verts(void* input);
void* core_sg_project_to_screen_space(void* input);
void* core_sg_visible_faces(void* input);
void* core_sg_cull_backfaces(void* input);

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

void* core_sg_draw_normals(void* input);
void* core_sg_draw_grid(void* input);

void* core_sg_clip_faces(void* input);
#endif
