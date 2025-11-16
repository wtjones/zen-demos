#ifndef RASTERIZE_H
#define RASTERIZE_H

#include "graphics.h"
#include "maths.h"

#define RAS_HORIZONTAL_LINE_MAX 4000

typedef struct {
    Point2i left;
    Point2i right;
} RasHorizontalLine;

/**
 * @brief Rasterizes a triangle into horizontal scanlines.
 * Splits the triangle into top-flat and bottom-flat parts.
 * A top-left fill rule is used to prevent overdraw.
 *
 * @param pv
 * @param lines Output array of horizontal lines.
 * @return size_t The number of horizontal lines generated.
 */
size_t rasterize_tri(
    RasVector4f* pv[3],
    RasHorizontalLine* lines,
    size_t max_lines);

#endif
