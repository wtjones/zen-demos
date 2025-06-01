#include "rasgl/core/text.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/maths.h"
#include <stdarg.h>
#include <stdio.h>
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

RasFixed core_get_font_width(RasFont* font, const char* str)
{
    if (font == NULL) {
        return -1;
    }

    return strlen(str) * RAS_TEXT_LETTER_WIDTH
        + (strlen(str) == 0
                ? 0
                : RAS_TEXT_LETTER_SPACING * (strlen(str) - 1));
}

RasFixed core_get_font_height(RasFont* font)
{
    return font == NULL ? -1 : RAS_TEXT_LETTER_HEIGHT;
}

bool bitmap_in_rect(RasPipelineVertex* pvs[4], Point2f top_left, Point2f bottom_right)
{
    for (size_t i = 0; i < 4; i++) {
        Point2f p;
        core_vector4f_to_vector2f(&pvs[i]->screen_space_position, &p);

        if (!core_point_in_rect(&p, &top_left, &bottom_right)) {
            return false;
        }
    }
    return true;
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
    Point2f pos,
    const char* text)
{

    RasPipelineVertex *pv0, *pv1, *pv2, *pv3;
    uint32_t pv0_i, pv1_i, pv2_i, pv3_i;
    uint32_t* si;
    uint32_t material_index = 0;

    // Top left pos of current char
    int32_t cur_x = pos.x;
    int32_t cur_y = pos.y;

    for (size_t i = 0; i < strlen(text); i++) {

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

        Point2f top_left = { .x = RAS_FIXED_ZERO, .y = RAS_FIXED_ZERO };
        Point2f bottom_right = {
            .x = INT_32_TO_FIXED_16_16(state->screen_settings.screen_width) - RAS_FIXED_ONE,
            .y = INT_32_TO_FIXED_16_16(state->screen_settings.screen_height) - RAS_FIXED_ONE
        };

        RasPipelineVertex* pvs[4] = { pv0, pv1, pv2, pv3 };

        if (!bitmap_in_rect(pvs, top_left, bottom_right)) {
            cur_x += RAS_TEXT_LETTER_WIDTH + RAS_TEXT_LETTER_SPACING;
            ras_log_debug("Bitmap culled: %c", text[i]);
            continue;
        }

        state->material_indexes[state->num_material_indexes] = text[i];
        state->material_indexes[state->num_material_indexes + 1] = text[i];
        state->num_material_indexes += 2;

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

RasResult core_draw_textf(RenderState* state,
    RasFont* font,
    Point2f pos,
    const char* fmt,
    ...)
{

    char buffer[1000];
    va_list args;

    int result = 0;
    va_start(args, fmt);
    result = vsnprintf(buffer, sizeof buffer, fmt, args);
    va_end(args);

    if (result < 0 || result > strlen(buffer)) {
        ras_log_error("Bad result from vsnprintf(): %d", result);
        return RAS_RESULT_ERROR;
    }
    return core_draw_text(state, font, pos, buffer);
}
