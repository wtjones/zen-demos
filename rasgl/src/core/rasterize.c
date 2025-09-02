#include "rasgl/core/rasterize.h"
#include "rasgl/core/repr.h"
#include <assert.h>

size_t core_interpolate(
    RasFixed i0, RasFixed d0,
    RasFixed i1, RasFixed d1,
    RasFixed dest[], size_t max_size)
{
    char buffer1[255];
    char buffer2[255];
    ras_log_buffer_trace("Interpolating from %s to %s\n",
        repr_fixed_16_16(buffer1, 255, i0),
        repr_fixed_16_16(buffer2, 255, i1));

    size_t count = 0;
    if (i0 == i1) {
        dest[count++] = d0;
        return count;
    }

    RasFixed a = div_fixed_16_16_by_fixed_16_16(d1 - d0, i1 - i0);
    char buffer[255];
    ras_log_buffer("Div: %s\n", repr_fixed_16_16(buffer, 255, a));

    RasFixed d = d0;
    RasFixed step = (i0 < i1) ? RAS_FIXED_ONE : -RAS_FIXED_ONE;

    if (i0 < i1) {
        for (RasFixed i = i0; i < i1; i += RAS_FIXED_ONE) {
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

    ras_log_buffer("Interpolate result...");
    for (size_t i = 0; i < count; i++) {
        char buffer3[255];
        ras_log_buffer_trace("Interp: %s", repr_fixed_16_16(buffer3, 255, dest[i]));
    }

    return count;
}

void rasterize_tri(
    RasVector4f* pv[3], RasHorizontalLine* lines, size_t* num_lines)
{
    *num_lines = 0;
    RasFixed x01[RAS_INTERPOLATE_MAX];
    RasFixed x12[RAS_INTERPOLATE_MAX];
    RasFixed x02[RAS_INTERPOLATE_MAX];
    // Concatination of the two shorter sides, x01 and x12
    RasFixed x012[RAS_INTERPOLATE_MAX];

    // sort the vertices by y
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

    size_t x01_count = core_interpolate(
        pv[0]->y, pv[0]->x, pv[1]->y, pv[1]->x,
        x01, RAS_INTERPOLATE_MAX);
    ras_log_buffer("count of x01: %d", x01_count);
    size_t x12_count = core_interpolate(
        pv[1]->y, pv[1]->x, pv[2]->y, pv[2]->x,
        x12, RAS_INTERPOLATE_MAX);
    ras_log_buffer("count of x12: %d", x12_count);
    size_t x02_count = core_interpolate(
        pv[0]->y, pv[0]->x, pv[2]->y, pv[2]->x,
        x02, RAS_INTERPOLATE_MAX);
    ras_log_buffer("count of x02: %d", x02_count);

    // Omit line where x01 and x12 meet are repeated
    size_t x012_count = 0;
    for (size_t i = 0; i < x01_count - 1; i++) {
        x012[x012_count++] = x01[i];
    }
    for (size_t i = 0; i < x12_count; i++) {
        x012[x012_count++] = x12[i];
    }
    ras_log_buffer("count of x012: %d", x012_count);

    size_t m = x02_count / 2;
    ras_log_buffer("m: %d", m);
    RasFixed *x_left, *x_right;
    if (x02[m] < x012[m]) {
        x_left = x02;
        x_right = x012;
    } else {
        x_left = x012;
        x_right = x02;
    }

    for (size_t i = 0; i < x012_count; i++) {
        lines[*num_lines].left.x = FIXED_16_16_TO_INT_32(x_left[i]);
        lines[*num_lines].left.y = FIXED_16_16_TO_INT_32(pv[0]->y) + i;
        lines[*num_lines].right.x = FIXED_16_16_TO_INT_32(x_right[i]);
        lines[*num_lines].right.y = FIXED_16_16_TO_INT_32(pv[0]->y) + i;
        (*num_lines)++;
    }
}
