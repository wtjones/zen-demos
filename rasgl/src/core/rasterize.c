#include "rasgl/core/rasterize.h"
#include "rasgl/core/repr.h"
#include <assert.h>

size_t core_interpolate(
    RasFixed i0, RasFixed d0,
    RasFixed i1, RasFixed d1,
    RasFixed dest[], size_t max_size)
{
#ifdef RAS_INTERPOLATE_TRACE
    char buffer1[255];
    char buffer2[255];
    ras_log_buffer_trace("Interpolating from %s to %s\n",
        repr_fixed_16_16(buffer1, 255, i0),
        repr_fixed_16_16(buffer2, 255, i1));
#endif

    size_t count = 0;
    if (i0 == i1) {
        dest[count++] = d0;
        return count;
    }

    RasFixed a = div_fixed_16_16_by_fixed_16_16(d1 - d0, i1 - i0);
#ifdef RAS_INTERPOLATE_TRACE
    char buffer[255];
    ras_log_buffer_trace("Div: %s\n", repr_fixed_16_16(buffer, 255, a));
#endif
    RasFixed d = d0;
    RasFixed step = (i0 < i1) ? RAS_FIXED_ONE : -RAS_FIXED_ONE;

    if (i0 < i1) {
        for (RasFixed i = i0; i <= i1; i += RAS_FIXED_ONE) {
            if (count >= max_size) {
                ras_log_flush();
                assert(count < max_size);
            }
            dest[count++] = d;
            d = d + a;
        }
    } else {
        for (RasFixed i = i0; i >= i1; i -= RAS_FIXED_ONE) {
            if (count >= max_size) {
                ras_log_flush();
                assert(count < max_size);
            }
            dest[count++] = d;
            d = d + a;
        }
    }

#ifdef RAS_INTERPOLATE_TRACE
    ras_log_buffer("Interpolate result...");
    for (size_t i = 0; i < count; i++) {
        char buffer3[255];
        ras_log_buffer_trace("Interp: %s", repr_fixed_16_16(buffer3, 255, dest[i]));
    }
#endif

    return count;
}

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
    int32_t y0 = (pv[0]->y + 65535) >> 16;
    int32_t y1 = (pv[1]->y + 65535) >> 16;
    int32_t y2 = (pv[2]->y + 65535) >> 16;

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

        lines[num_lines].left.x = x_left >> 16;
        lines[num_lines].left.y = y;
        lines[num_lines].right.x = x_right >> 16;
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

        lines[num_lines].left.x = x_left >> 16;
        lines[num_lines].left.y = y;
        lines[num_lines].right.x = x_right >> 16;
        lines[num_lines].right.y = y;
        num_lines++;
    }

    return num_lines;
}
