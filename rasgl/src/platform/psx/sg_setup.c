#include "rasgl/core/stages.h"
#include "stages.h"

void* psx_sg_setup(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    if (render_data == NULL || render_data->scene == NULL || render_data->camera == NULL) {
        ras_log_error("Invalid render data or scene/camera not set.");
        return NULL;
    }
    return render_data;
}

void* psx_sg_xform_objects(void* input)
{
    RasRenderData* render_data = (RasRenderData*)input;
    return render_data;
}

void psx_pipeline_init(RasPipeline* pipeline)
{
    pipeline->num_stages = 0;

    ADD_STAGE(pipeline, core_sg_setup);
    ADD_STAGE(pipeline, psx_sg_setup);

    ADD_STAGE(pipeline, core_sg_xform_tombmaps);

    ADD_STAGE(pipeline, core_sg_xform_objects);
    ADD_STAGE(pipeline, psx_sg_xform_objects);

    ADD_STAGE(pipeline, core_sg_xform_tombmap_aabb);
    ADD_STAGE(pipeline, core_sg_xform_aabb);
    ADD_STAGE(pipeline, core_sg_render_aabb);
    ADD_STAGE(pipeline, core_sg_xform_verts);
    ADD_STAGE(pipeline, core_sg_project_to_clip_space);
    ADD_STAGE(pipeline, core_sg_clip_flag_verts);
    ADD_STAGE(pipeline, core_sg_visible_faces);
    ADD_STAGE(pipeline, core_sg_project_to_screen_space);
    ADD_STAGE(pipeline, core_sg_cull_backfaces);
    ADD_STAGE(pipeline, core_sg_xform_normals);
    ADD_STAGE(pipeline, core_sg_lighting);
    ADD_STAGE(pipeline, core_sg_draw_normals);
    ADD_STAGE(pipeline, core_sg_draw_grid);

    ras_log_info("Pipeline initialized with %d stages", pipeline->num_stages);
}
