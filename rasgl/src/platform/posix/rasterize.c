
#include "rasterize.h"

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
