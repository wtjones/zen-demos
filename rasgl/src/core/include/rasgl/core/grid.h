#ifndef CORE_GRID_H
#define CORE_GRID_H

#include "debug.h"
#include "graphics.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define RAS_GRID_ORIGIN_AXIS_POINTS 5
#define RAS_GRID_ORIGIN_POINTS ((RAS_GRID_ORIGIN_AXIS_POINTS * 3) + 1)

#define RAS_GRID_LINES_AXIS_POINTS 13
#define RAS_GRID_LINE_POINTS_COUNT   \
    (RAS_GRID_LINES_AXIS_POINTS      \
        * RAS_GRID_LINES_AXIS_POINTS \
        * RAS_GRID_LINES_AXIS_POINTS)

void core_draw_grid(
    RenderState* render_state,
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4]);

#endif
