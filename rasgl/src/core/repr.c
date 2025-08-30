#include "rasgl/core/repr.h"

static const char* g_repr_clipping_mode[] = {
    "RAS_CLIPPING_ON",
    "RAS_CLIPPING_OFF",
    "RAS_CLIPPING_EXCLUDE"
};

static const char* g_repr_normal_mode[] = {
    "RAS_NORMAL_MODE_OFF",
    "RAS_NORMAL_MODE_FAUX",
    "RAS_NORMAL_MODE_ORTHO"
};

static const char* g_repr_grid_mode[] = {
    "RAS_GRID_MODE_OFF",
    "RAS_GRID_MODE_ORIGIN",
    "RAS_GRID_MODE_GRID"
};

char* repr_point2i(char* buffer, size_t count, Point2i* p)
{
    snprintf(
        buffer,
        count,
        "[x: %d, y: %d]",
        p->x,
        p->y);
    return buffer;
}

char* repr_point2f(char* buffer, size_t count, Point2f* p)
{
    snprintf(
        buffer,
        count,
        "[x: %f, y: %f]",
        fixed_16_16_to_float(p->x),
        fixed_16_16_to_float(p->y));
    return buffer;
}

char* repr_point3f(char* buffer, size_t count, Point3f* p)
{
    snprintf(
        buffer,
        count,
        "[x: %f, y: %f, z: %f]",
        fixed_16_16_to_float(p->x),
        fixed_16_16_to_float(p->y),
        fixed_16_16_to_float(p->z));
    return buffer;
}

char* repr_vector4f(char* buffer, size_t count, RasVector4f* p)
{
    snprintf(
        buffer,
        count,
        "[x: %f, y: %f, z: %f, w: %f]",
        fixed_16_16_to_float(p->x),
        fixed_16_16_to_float(p->y),
        fixed_16_16_to_float(p->z),
        fixed_16_16_to_float(p->w));
    return buffer;
}

char* repr_fixed_16_16(char* buffer, size_t count, RasFixed f)
{
    snprintf(
        buffer,
        count,
        "[%f (%d)]",
        fixed_16_16_to_float(f),
        f);
    return buffer;
}
char* repr_mat_4x4(char* buffer, size_t count, RasFixed s1[4][4])
{
    char matrix_buffer[255];
    buffer[0] = '\0';
    strcat(buffer, "[");

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {

            if (j > 0) {
                strcat(buffer, ", ");
            } else {
                strcat(buffer, "\n[");
            }
            snprintf(
                matrix_buffer,
                sizeof matrix_buffer,
                "%12.5f",
                fixed_16_16_to_float(s1[i][j]));

            strcat(buffer, matrix_buffer);

            if (j == 3) {
                strcat(buffer, "]");
            }
        }
    }
    strcat(buffer, "]");
    return buffer;
}

char* repr_mat_4x1(char* buffer, size_t count, RasFixed s[4])
{
    char matrix_buffer[255];
    buffer[0] = '\0';
    strcat(buffer, "[");

    for (int i = 0; i < 4; i++) {
        strcat(buffer, "\n[");

        snprintf(
            matrix_buffer,
            sizeof matrix_buffer,
            "%12.5f",
            fixed_16_16_to_float(s[i]));

        strcat(buffer, matrix_buffer);

        if (i == 3) {
            strcat(buffer, "]");
        }
    }

    strcat(buffer, "]");
    return buffer;
}

const char* repr_clipping_mode(char* buffer, size_t count, RasClippingMode mode)
{
    return g_repr_clipping_mode[mode];
}

const char* repr_normal_mode(char* buffer, size_t count, RasNormalMode mode)
{
    return g_repr_normal_mode[mode];
}

const char* repr_grid_mode(char* buffer, size_t count, RasGridMode mode)
{
    buffer[0] = '\0';
    ras_log_info("grid debug: %d", mode);
    if (mode == RAS_GRID_MODE_OFF) {
        strcat(buffer, g_repr_grid_mode[mode]);
    }
    for (uint8_t i = 1; i < RAS_GRID_MODE_COUNT; i++) {
        strcat(buffer, (mode & i) && strlen(buffer) > 0 ? " | " : "");
        strcat(buffer, mode & i ? g_repr_grid_mode[mode & i] : "");
    }

    return buffer;
}
