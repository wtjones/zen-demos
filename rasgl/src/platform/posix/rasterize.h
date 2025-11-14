#ifndef POSIX_RASTERIZE_H
#define POSIX_RASTERIZE_H

#include "rasgl/core/graphics.h"
#include <SDL2/SDL.h>

#define RAS_PLOT_PIXEL(surface, x, y, color)                                     \
    do {                                                                         \
        if ((x) >= 0 && (x) < (surface)->w && (y) >= 0 && (y) < (surface)->h) {  \
            ((uint8_t*)(surface)->pixels)[(y) * (surface)->pitch + (x)] = color; \
        }                                                                        \
    } while (0)

void ras_draw_line(
    SDL_Surface* surface, Point2i* p0, Point2i* p1, uint8_t color);

/**
 * @brief Draw a horizontal line with exclusive right end.
 *
 * @param surface
 * @param x0
 * @param x1
 * @param y
 * @param color
 */
void ras_draw_hline(SDL_Surface* surface, int32_t x0, int32_t x1, int32_t y, uint8_t color);

#endif
