#include "rasgl/core/text.h"
#include "rasgl/core/graphics.h"
#include <stdlib.h>

/**
 * @brief Initialize a delivered non asset-based font.
 *
 * Currently a placebo.
 *
 * @param font_id
 * @return RasFont*
 */
RasFont* core_get_font_system(RasSystemFont font_id)
{
    RasFont* font = malloc(sizeof(RasFont));
    if (!font) {
        return NULL;
    }
    return font;
}

void core_free_font(RasFont* font)
{
    free(font);
}

RasFixed core_get_font_width(RasFont* font)
{
    return font == NULL ? -1 : RAS_TEXT_LETTER_WIDTH;
}

RasFixed core_get_font_height(RasFont* font)
{
    return font == NULL ? -1 : RAS_TEXT_LETTER_HEIGHT;
}

/**
 * @brief Construct bitmap characters as two triangles per character.
 *
 * p0-------p2    p4-------p5
 * |  t0  / |     |  t2  / |
 * |    /   |     |    /   |
 * |  /  t1 |     |  /  t3 |
 * p1-------p3    p6-------p7
 *
 * @param state
 * @param font
 * @param text
 * @param pos
 * @param fg_color
 * @param bg_color
 * @return RasResult
 */
RasResult core_draw_text(
    RenderState* state,
    RasFont* font,
    const char* text,
    Point2f pos,
    uint8_t fg_color,
    int32_t bg_color)
{

    RasPipelineVertex *pv0, *pv1, *pv2, *pv3;
    uint32_t pv0_i, pv1_i, pv2_i, pv3_i;
    uint32_t* si;
    uint32_t material_index = 0;

    // Top left pos of current char
    int32_t cur_x = pos.x;
    int32_t cur_y = pos.y;

    for (size_t i = 0; i < strlen(text); i++) {

        state->material_indexes[state->num_material_indexes] = text[i];
        state->material_indexes[state->num_material_indexes + 1] = text[i];
        state->num_material_indexes += 2;

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

        pv0->screen_space_position.x = cur_x;
        pv0->screen_space_position.y = cur_y;
        pv1->screen_space_position.x = cur_x;
        pv1->screen_space_position.y = cur_y + RAS_TEXT_LETTER_HEIGHT - RAS_FIXED_ONE;
        pv2->screen_space_position.x = cur_x + RAS_TEXT_LETTER_WIDTH - RAS_FIXED_ONE;
        pv2->screen_space_position.y = cur_y;
        pv3->screen_space_position.x = cur_x + RAS_TEXT_LETTER_WIDTH - RAS_FIXED_ONE;
        pv3->screen_space_position.y = cur_y + RAS_TEXT_LETTER_HEIGHT - RAS_FIXED_ONE;

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

        si = &state->num_visible_indexes;

        state->visible_indexes[(*si)++] = pv0_i;
        state->visible_indexes[(*si)++] = pv1_i;
        state->visible_indexes[(*si)++] = pv2_i;
        state->visible_indexes[(*si)++] = pv2_i;
        state->visible_indexes[(*si)++] = pv1_i;
        state->visible_indexes[(*si)++] = pv3_i;

        cur_x += RAS_TEXT_LETTER_WIDTH + RAS_TEXT_LETTER_SPACING;
    }
    return RAS_RESULT_OK;
}
