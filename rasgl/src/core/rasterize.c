#include "rasgl/core/rasterize.h"
#include "rasgl/core/repr.h"
#include <assert.h>

size_t rasterize_tri(
    RasVector4f* pv[3], RasHorizontalLine* lines, size_t max_lines)
{
    size_t num_lines = 0;

    // sort the vertices by y (v0 = top, v2 = bottom)
    if (pv[0]->y > pv[1]->y) {
        RasVector4f* temp = pv[0];
        pv[0] = pv[1];
        pv[1] = temp;
    }
    if (pv[0]->y > pv[2]->y) {
        RasVector4f* temp = pv[0];
        pv[0] = pv[2];
        pv[2] = temp;
    }
    if (pv[1]->y > pv[2]->y) {
        RasVector4f* temp = pv[1];
        pv[1] = pv[2];
        pv[2] = temp;
    }

    // Get integer Y bounds
    // Use ceiling for top vertices to skip partial pixels
    int32_t y0 = RAS_FIXED_16_16_INT_32_CEIL(pv[0]->y);
    int32_t y1 = RAS_FIXED_16_16_INT_32_CEIL(pv[1]->y);
    int32_t y2 = RAS_FIXED_16_16_INT_32_CEIL(pv[2]->y);

    // Precompute edge slopes (dx/dy)
    RasFixed invslope01 = (pv[1]->y != pv[0]->y) ? div_fixed_16_16_by_fixed_16_16(pv[1]->x - pv[0]->x, pv[1]->y - pv[0]->y) : 0;
    RasFixed invslope12 = (pv[2]->y != pv[1]->y) ? div_fixed_16_16_by_fixed_16_16(pv[2]->x - pv[1]->x, pv[2]->y - pv[1]->y) : 0;
    RasFixed invslope02 = (pv[2]->y != pv[0]->y) ? div_fixed_16_16_by_fixed_16_16(pv[2]->x - pv[0]->x, pv[2]->y - pv[0]->y) : 0;

    // Rasterize top half (y0 to y1)
    for (int32_t y = y0; y < y1 && y < y2; y++) {
        if (num_lines >= max_lines)
            break;

        RasFixed y_fixed = INT_32_TO_FIXED_16_16(y);
        RasFixed x_left = pv[0]->x + mul_fixed_16_16_by_fixed_16_16(invslope02, y_fixed - pv[0]->y);
        RasFixed x_right = pv[0]->x + mul_fixed_16_16_by_fixed_16_16(invslope01, y_fixed - pv[0]->y);

        // Ensure left < right
        if (x_left > x_right) {
            RasFixed temp = x_left;
            x_left = x_right;
            x_right = temp;
        }

        lines[num_lines].left.x = FIXED_16_16_TO_INT_32_ROUND(x_left);
        lines[num_lines].left.y = y;
        lines[num_lines].right.x = FIXED_16_16_TO_INT_32_ROUND(x_right - 1);
        lines[num_lines].right.y = y;
        num_lines++;
    }

    // Rasterize bottom half (y1 to y2)
    for (int32_t y = y1; y < y2; y++) {
        if (num_lines >= max_lines)
            break;

        RasFixed y_fixed = INT_32_TO_FIXED_16_16(y);
        RasFixed x_left = pv[0]->x + mul_fixed_16_16_by_fixed_16_16(invslope02, y_fixed - pv[0]->y);
        RasFixed x_right = pv[1]->x + mul_fixed_16_16_by_fixed_16_16(invslope12, y_fixed - pv[1]->y);

        // Ensure left < right
        if (x_left > x_right) {
            RasFixed temp = x_left;
            x_left = x_right;
            x_right = temp;
        }

        lines[num_lines].left.x = FIXED_16_16_TO_INT_32_ROUND(x_left);
        lines[num_lines].left.y = y;
        lines[num_lines].right.x = FIXED_16_16_TO_INT_32_ROUND(x_right - 1);
        lines[num_lines].right.y = y;
        num_lines++;
    }

    return num_lines;
}
