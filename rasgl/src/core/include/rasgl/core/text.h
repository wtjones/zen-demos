#ifndef CORE_TEXT_H
#define CORE_TEXT_H

#define RAS_TEXT_LETTER_WIDTH INT_32_TO_FIXED_16_16(8)
#define RAS_TEXT_LETTER_SPACING INT_32_TO_FIXED_16_16(2)

#include "debug.h"
#include "graphics.h"
#include "maths.h"

typedef enum RasSystemFont {
    RAS_SYSTEM_FONT_DEFAULT = 1
} RasSystemFont;

typedef struct RasFont {

} RasFont;

RasFont* core_get_font_system(RasSystemFont font_id);

RasResult core_draw_text(
    RenderState* state,
    RasFont* font,
    const char* text,
    Point2i pos,
    uint8_t fg_color,
    int32_t bg_color);

#endif
