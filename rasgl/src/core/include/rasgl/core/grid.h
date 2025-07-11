#ifndef CORE_GRID_H
#define CORE_GRID_H

#include "debug.h"
#include "graphics.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define RAS_GRID_ORIGIN_AXIS_POINTS 9
#define RAS_GRID_ORIGIN_POINTS ((RAS_GRID_ORIGIN_AXIS_POINTS * 3) + 1)

void core_draw_grid(
    RenderState* render_state,
    RasFixed world_view_matrix[4][4],
    RasFixed proj_matrix[4][4]);

#endif
