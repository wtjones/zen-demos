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

    RasClipFlags aabb_clip_flags[RAS_MAX_SCENE_OBJECTS];
    uint32_t visible_objects[RAS_MAX_SCENE_OBJECTS];
    uint32_t num_visible_objects;
    RasPipelineVertexBuffer vert_buffer;

} RasRenderData;

void core_renderdata_init(
    RasRenderData* render_data,
    RenderState* render_state,
    RasScene* scene,
    RasCamera* camera);

void* core_sg_setup(void* input);
void* core_sg_xform_objects(void* input);
void* core_sg_xform_aabb(void* input);

#endif
