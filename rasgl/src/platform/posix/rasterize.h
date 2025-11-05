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

void ras_draw_horizontal_line(
    SDL_Surface* surface, int x0, int x1, int y, uint8_t color);

void ras_draw_line(
    SDL_Surface* surface, Point2i* p0, Point2i* p1, uint8_t color);

#endif
