#ifndef RASTERIZE_H
#define RASTERIZE_H

#include "graphics.h"
#include "maths.h"

#define RAS_INTERPOLATE_MAX 500

typedef struct {
    Point2i left;
    Point2i right;
} RasHorizontalLine;

size_t core_interpolate(
    RasFixed i0,
    RasFixed d0,
    RasFixed i1,
    RasFixed d1,
    RasFixed dest[],
    size_t max_size);

void rasterize_tri(
    RasVector4f* pv[3], RasHorizontalLine* lines, size_t* num_lines);

#endif
