
#include "rasterize.h"

void ras_draw_horizontal_line(SDL_Surface* surface, int x0, int x1, int y, uint8_t color)
{
    // Ensure x0 <= x1
    if (x0 > x1) {
        int temp = x0;
        x0 = x1;
        x1 = temp;
    }

    // Clamp to screen bounds
    if (y < 0 || y >= surface->h)
        return;
    if (x1 < 0 || x0 >= surface->w)
        return;
    if (x0 < 0)
        x0 = 0;
    if (x1 > surface->w)
        x1 = surface->w;

    // Draw pixels from x0 to x1 (exclusive on right)
    for (int x = x0; x < x1; x++) {
        ((Uint8*)surface->pixels)[y * surface->pitch + x] = color;
    }
}

void ras_draw_line(SDL_Surface* surface, Point2i* p0, Point2i* p1, uint8_t color)
{
    int x0 = p0->x, y0 = p0->y;
    int x1 = p1->x, y1 = p1->y;
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = dx + dy, e2;

    while (1) {
        if (x0 >= 0 && x0 < surface->w && y0 >= 0 && y0 < surface->h) {
            ((Uint8*)surface->pixels)[y0 * surface->pitch + x0] = color;
        }
        if (x0 == x1 && y0 == y1)
            break;
        e2 = 2 * err;
        if (e2 >= dy) {
            err += dy;
            x0 += sx;
        }
        if (e2 <= dx) {
            err += dx;
            y0 += sy;
        }
    }
}
