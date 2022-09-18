#include <SDL2/SDL.h>
#define _USE_MATH_DEFINES
#include "matrix.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

const int MAX_SHAPE_VERTICES = 10;
const int NUM_OBJECTS = 3;
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

typedef struct Point2f {
    float x;
    float y;
} Point2f;

typedef struct Viewer {
    Point2f position;
} Viewer;

typedef struct Shape {
    int num_vertices;
    Point2f vertices[MAX_SHAPE_VERTICES];
} Shape;

typedef struct WorldObject {
    Point2f position;
    int angle;
    Shape* shape;
} WorldObject;

Shape square = {
    .num_vertices = 4,
    .vertices = {
        { .x = -12.0, .y = 12.0 },
        { .x = 12.0, .y = 12.0 },
        { .x = -12.0, .y = -12.0 },
        { .x = 12.0, .y = -12.0 } }
};

Shape triange = {
    .num_vertices = 3,
    .vertices = {
        { .x = 0.0, .y = 12.0 },
        { .x = -12.0, .y = -12.0 },
        { .x = 12.0, .y = -12.0 } }
};

WorldObject objects[NUM_OBJECTS] = {
    { .position = { .x = -50.0, .y = 0.0 },
        .angle = 40,
        .shape = &square },
    { .position = { .x = 50.0, .y = 0.0 },
        .angle = 90,
        .shape = &square },
    { .position = { .x = 0.0, .y = 100.0 },
        .angle = 90,
        .shape = &triange }
};

Viewer viewer = { .position = { .x = 0, .y = 0 } };

float cos_table[360];
float sin_table[360];

void init_math()
{

    for (int i = 0; i < 360; i++) {
        cos_table[i] = cos((float)i * M_PI / -180.0);
        sin_table[i] = sin((float)i * M_PI / -180.0);
    }
}

void update()
{
    for (int i = 0; i < NUM_OBJECTS; i++) {
        WorldObject* o = &objects[i];
        o->angle = (o->angle + 1) % 360;
    }
}

void transform_objects(
    WorldObject objects[NUM_OBJECTS],
    Point2f vertices_at_screen[MAX_SHAPE_VERTICES][NUM_OBJECTS])
{

    Point2f vertex_xformed;
    Point2f vertex_at_world;

    for (int i = 0; i < NUM_OBJECTS; i++) {

        WorldObject* o = &objects[i];

        float c = cos_table[o->angle];
        float s = sin_table[o->angle];

        for (int v = 0; v < o->shape->num_vertices; v++) {
            Point2f* source = &o->shape->vertices[v];

            vertex_xformed.x = c * source->x - s * source->y;
            vertex_xformed.y = c * source->y + s * source->x;

            vertex_at_world.x = vertex_xformed.x + o->position.x;
            vertex_at_world.y = vertex_xformed.y + o->position.y;

            vertices_at_screen[v][i].x = vertex_at_world.x + SCREEN_WIDTH / 2;

            vertices_at_screen[v][i].y = -vertex_at_world.y + SCREEN_HEIGHT / 2;
        }
    }
}

void render(
    SDL_Renderer* renderer,
    WorldObject objects[NUM_OBJECTS],
    Point2f vertices_at_screen[MAX_SHAPE_VERTICES][NUM_OBJECTS])
{

    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);

    for (int i = 0; i < NUM_OBJECTS; i++) {

        WorldObject* o = &objects[i];

        for (int v = 0; v < o->shape->num_vertices; v++) {
            Point2f* vert = &vertices_at_screen[v][i];
            SDL_RenderDrawPointF(renderer, vert->x, vert->y);
        }
    }
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderPresent(renderer);
}

int main()
{
    init_math();

    SDL_bool should_quit = SDL_FALSE;
    printf("Hello, World!2 \n");
    char str[80];

    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow("Hello World!", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    SDL_Renderer* renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED);
    Point2f vertices_at_screen[MAX_SHAPE_VERTICES][NUM_OBJECTS];

    int last_frame = SDL_GetTicks();
    while (!should_quit) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                should_quit = SDL_TRUE;
                break;
            case SDL_KEYUP:
                should_quit = event.key.keysym.scancode == SDL_SCANCODE_ESCAPE;
                should_quit = true;
            }
        }

        transform_objects(objects, vertices_at_screen);
        render(renderer, objects, vertices_at_screen);
        update();

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}