#ifndef CORE_TEXT_H
#define CORE_TEXT_H

#define RAS_TEXT_LETTER_WIDTH INT_32_TO_FIXED_16_16(8)
#define RAS_TEXT_LETTER_HEIGHT INT_32_TO_FIXED_16_16(8)
#define RAS_TEXT_LETTER_SPACING INT_32_TO_FIXED_16_16(0)
#define RAS_TEXT_CHAR_MAX 1024

#include "debug.h"
#include "graphics.h"
#include "maths.h"

typedef enum RasSystemFont {
    RAS_SYSTEM_FONT_DEFAULT = 1
} RasSystemFont;

typedef struct RasFont {
    // Placeholder
    size_t size;
} RasFont;

RasFont* core_get_font_system(RasSystemFont font_id);
void core_free_font(RasFont* font);

/**
 * @brief Get width metric of longest tokenized line of str.
 *
 * @param font
 * @param str
 * @return RasFixed
 */
RasFixed core_get_font_width(RasFont* font, const char* str);
RasFixed core_get_font_height(RasFont* font);

RasResult core_draw_text(
    RenderState* state,
    RasFont* font,
    Point2f pos,
    const char* text);

RasResult core_draw_textf(RenderState* state,
    RasFont* font,
    Point2f pos,
    const char* fmt, ...);

#endif
