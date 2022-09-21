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
const int MAX_SHAPES = NUM_OBJECTS + 1;
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;
const int WORLD_WIDTH = 600;
const int WORLD_HEIGHT = 400;

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

typedef struct WorldBoundary {
    Point2f top_left;
    Point2f bottom_right;
    Shape* shape;
} WorldBoundary;

Shape square = {
    .num_vertices = 4,
    .vertices = {
        { .x = -12.0, .y = -12.0 },
        { .x = 12.0, .y = -12.0 },
        { .x = 12.0, .y = 12.0 },
        { .x = -12.0, .y = 12.0 } }
};

Shape boundary = {
    .num_vertices = 4,
    .vertices = {
        { .x = -WORLD_WIDTH / 2, .y = -WORLD_HEIGHT / 2 },
        { .x = WORLD_WIDTH / 2, .y = -WORLD_HEIGHT / 2 },
        { .x = WORLD_WIDTH / 2, .y = WORLD_HEIGHT / 2 },
        { .x = -WORLD_WIDTH / 2, .y = WORLD_HEIGHT / 2 } }
};

WorldBoundary world_boundary = {
    .shape = &boundary
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

void update(WorldObject objects[NUM_OBJECTS], Viewer* viewer, const Uint8* keys)
{
    for (int i = 0; i < NUM_OBJECTS; i++) {
        WorldObject* o = &objects[i];
        o->angle = (o->angle + 1) % 360;
    }

    viewer->position.x += keys[SDL_SCANCODE_RIGHT] == 1 ? 1 : 0;
    viewer->position.x -= keys[SDL_SCANCODE_LEFT] == 1 ? 1 : 0;
    viewer->position.y += keys[SDL_SCANCODE_DOWN] == 1 ? 1 : 0;
    viewer->position.y -= keys[SDL_SCANCODE_UP] == 1 ? 1 : 0;
}

void transform_objects(
    WorldObject objects[NUM_OBJECTS],
    Shape shapes_at_screen[MAX_SHAPES],
    int* count_shapes,
    Viewer* viewer)
{

    Point2f vertex_xformed;
    Point2f vertex_at_world;

    for (int i = 0; i < NUM_OBJECTS; i++) {

        WorldObject* o = &objects[i];

        float c = cos_table[o->angle];
        float s = sin_table[o->angle];

        Shape* src_shape = o->shape;
        Shape* dest_shape = &shapes_at_screen[*count_shapes];
        dest_shape->num_vertices = src_shape->num_vertices;

        for (int v = 0; v < o->shape->num_vertices; v++) {
            Point2f* source = &o->shape->vertices[v];

            // Rotate and translate to world coord
            float matrix[3][3] = {
                { c, -s, o->position.x },
                { s, c, o->position.y },
                { 0, 0, 1.0 }
            };

            float pos[3] = { source->x, source->y, 1 };
            float xform_result[3];
            mat_mul_3x3_3x1(matrix, pos, xform_result);

            float offset_x = (SCREEN_WIDTH / 2) - viewer->position.x;
            float offset_y = (SCREEN_HEIGHT / 2) - viewer->position.y;

            // Scale and translate to screen coord
            float scale_matrix[3][3] = {
                { 1.0, 0.0, offset_x },
                { 0.0, 1.0, offset_y },
                { 0.0, 0.0, 1.0 }
            };
            float scale_result[3];
            mat_mul_3x3_3x1(scale_matrix, xform_result, scale_result);

            dest_shape->vertices[v].x = scale_result[0];
            dest_shape->vertices[v].y = scale_result[1];
        }
        (*count_shapes)++;
    }
}

void transform_boundary(
    WorldBoundary* boundary,
    Shape shapes_at_screen[MAX_SHAPES],
    int* count_shapes,
    Viewer* viewer)
{
    Shape* src_shape = boundary->shape;
    Shape* dest_shape = &shapes_at_screen[*count_shapes];
    dest_shape->num_vertices = src_shape->num_vertices;

    for (int v = 0; v < src_shape->num_vertices; v++) {
        Point2f* source = &src_shape->vertices[v];

        float pos[3] = { source->x, source->y, 1 };
        float offset_x = (SCREEN_WIDTH / 2) - viewer->position.x;
        float offset_y = (SCREEN_HEIGHT / 2) - viewer->position.y;

        // Scale and translate to screen coord
        float scale_matrix[3][3] = {
            { 1.0, 0.0, offset_x },
            { 0.0, 1.0, offset_y },
            { 0.0, 0.0, 1.0 }
        };
        float scale_result[3];
        mat_mul_3x3_3x1(scale_matrix, pos, scale_result);

        dest_shape->vertices[v].x = scale_result[0];
        dest_shape->vertices[v].y = scale_result[1];
    }
    (*count_shapes)++;
}

void render(
    SDL_Renderer* renderer,
    Shape shapes_at_screen[MAX_SHAPES],
    int* count_shapes)
{

    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);

    SDL_FPoint line_points[MAX_SHAPE_VERTICES + 1];
    for (int i = 0; i < *count_shapes; i++) {
        Shape* shape = &shapes_at_screen[i];

        // map to SDL points
        for (int vi = 0; vi < shape->num_vertices; vi++) {
            line_points[vi].x = shape->vertices[vi].x;
            line_points[vi].y = shape->vertices[vi].y;
        }
        // close the loop
        line_points[shape->num_vertices].x = shape->vertices[0].x;
        line_points[shape->num_vertices].y = shape->vertices[0].y;

        SDL_RenderDrawLinesF(renderer, line_points, shape->num_vertices + 1);
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
    Shape shapes_at_screen[MAX_SHAPES];
    int count_shapes = 0;
    Viewer viewer = { .position = { .x = 0.0, .y = 0.0 } };

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
            }
        }
        int num_keys;
        const Uint8* keys = SDL_GetKeyboardState(&num_keys);

        count_shapes = 0;
        transform_objects(
            objects, shapes_at_screen, &count_shapes, &viewer);
        transform_boundary(
            &world_boundary, shapes_at_screen, &count_shapes, &viewer);
        render(renderer, shapes_at_screen, &count_shapes);
        update(objects, &viewer, keys);

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}