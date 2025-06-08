#include "rasgl/core/console.h"

RasResult core_draw_console(RenderState* state, RasFont* font)
{
    RasPipelineVertex *pv0, *pv1, *pv2, *pv3;
    uint32_t pv0_i, pv1_i, pv2_i, pv3_i;
    uint32_t* si;
    uint32_t material_index = 0;

    // Top left pos of current char
    int32_t cur_x = 0;
    int32_t cur_y = 0;

    // Construct the console background with two triangles
    //  p0-------p2
    //  |  t0  / |
    //  |    /   |
    //  |  /  t1 |
    //  p1-------p3

    pv0 = &state->pipeline_verts[state->num_pipeline_verts];
    pv0_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv1 = &state->pipeline_verts[state->num_pipeline_verts];
    pv1_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv2 = &state->pipeline_verts[state->num_pipeline_verts];
    pv2_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv3 = &state->pipeline_verts[state->num_pipeline_verts];
    pv3_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    Point2f top_left = { .x = RAS_FIXED_ZERO, .y = RAS_FIXED_ZERO };
    Point2f bottom_right = {
        .x = INT_32_TO_FIXED_16_16(state->screen_settings.screen_width) - RAS_FIXED_ONE,
        .y = INT_32_TO_FIXED_16_16(state->screen_settings.screen_height) - RAS_FIXED_ONE
    };

    pv0->screen_space_position.x = top_left.x;
    pv0->screen_space_position.y = top_left.y;
    pv1->screen_space_position.x = top_left.x;
    pv1->screen_space_position.y = bottom_right.y;
    pv2->screen_space_position.x = bottom_right.x;
    pv2->screen_space_position.y = top_left.y;
    pv3->screen_space_position.x = bottom_right.x;
    pv3->screen_space_position.y = bottom_right.y;

    // uv0 (0,0) ------ uv1 (1,0)
    //    |               |
    //    |               |
    // uv2 (0,1) ------ uv3 (1,1)

    pv0->u = 0;
    pv0->v = 0;
    pv1->u = 1;
    pv1->v = 0;
    pv2->u = 0;
    pv2->v = 1;
    pv3->u = 1;
    pv3->v = 1;

    RasPipelineVertex* pvs[4] = { pv0, pv1, pv2, pv3 };

    state->material_indexes[state->num_material_indexes] = 0x2592;
    state->material_indexes[state->num_material_indexes + 1] = 0x2592;
    state->num_material_indexes += 2;

    si = &state->num_visible_indexes;

    state->visible_indexes[(*si)++] = pv0_i;
    state->visible_indexes[(*si)++] = pv1_i;
    state->visible_indexes[(*si)++] = pv2_i;
    state->visible_indexes[(*si)++] = pv2_i;
    state->visible_indexes[(*si)++] = pv1_i;
    state->visible_indexes[(*si)++] = pv3_i;

    return RAS_RESULT_OK;
}
