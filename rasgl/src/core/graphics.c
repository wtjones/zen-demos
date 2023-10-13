#include "rasgl/core/graphics.h"
#include "rasgl/core/debug.h"

/**
 * project view point to screen
 *
 */
Point2f project_point(int32_t screen_width, int32_t screen_height, int32_t projection_ratio, Point3f view_point)
{
    char buffer[100];

    Point2f result;
    ras_log_trace("view_point: %s\n", repr_point3f(buffer, sizeof buffer, &view_point));

    // x = ((x / z * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_W / 2

    int32_t x_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.x, view_point.z);
    ras_log_trace("x_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, x_div_z));

    int32_t mul_ratio = mul_fixed_16_16_by_fixed_16_16(x_div_z, projection_ratio);
    ras_log_trace("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    int32_t screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    ras_log_trace("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    int32_t left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    int32_t right = INT_32_TO_FIXED_16_16(screen_width / 2);
    result.x = left + right;
    ras_log_trace("result.x = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    // y = ((y / z * -1.0 * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_H / 2
    int32_t y_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.y, view_point.z);
    ras_log_trace("y_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, y_div_z));

    mul_ratio = mul_fixed_16_16_by_fixed_16_16(y_div_z, projection_ratio);
    mul_ratio = mul_fixed_16_16_by_fixed_16_16(mul_ratio, -float_to_fixed_16_16(1.0));

    ras_log_trace("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    ras_log_trace("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    ras_log_trace("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    right = INT_32_TO_FIXED_16_16(screen_height / 2);
    result.y = left + right;
    ras_log_trace("result.y = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    return result;
}

void projected_to_screen_point(int32_t screen_width, int32_t screen_height, int32_t projected_point[4], Point2i* screen_point)
{

    int32_t half_screen_width = INT_32_TO_FIXED_16_16(screen_width / 2);
    int32_t half_screen_height = INT_32_TO_FIXED_16_16(screen_height / 2);

    screen_point->x = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_width, projected_point[0]) + half_screen_width);
    screen_point->y = FIXED_16_16_TO_INT_32(
        mul_fixed_16_16_by_fixed_16_16(half_screen_height, projected_point[1]) + half_screen_height);
}
