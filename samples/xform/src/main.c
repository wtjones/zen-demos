#include "maths.h"
#include "matrix.h"
#include <SDL2/SDL.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>

#define NUM_OBJECTS 3
#define MAX_SHAPES NUM_OBJECTS + 1
#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define WORLD_WIDTH 600
#define WORLD_HEIGHT 400
#define ZOOM_SPEED 0.04
#define ZOOM_MAX 8.0
#define ZOOM_MIN 0.2

typedef struct Viewer {
    Point3f position;
} Viewer;

typedef struct WorldObject {
    Point2f position;
    int angle;
    Shape* shape;
    Point2f vector;
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

void init_objects()
{
    for (int i = 0; i < NUM_OBJECTS; i++) {
        WorldObject* o = &objects[i];
        int angle = rand() % 360;
        Point2f origin = { 0.0, 0.0 };
        apply_unit_vector(&origin, angle, &o->vector);
    }
}

/**
 * @brief Determine if any points of a shape collide with a wall
 *
 * Assumes world coordinates
 * @param shape
 * @param boundary
 * @param side_p1 collision side
 * @param side_p2 collision side
 * @return true
 * @return false
 */
bool get_boundary_collision(
    Shape* shape,
    WorldBoundary* boundary,
    Point2f* side_p1,
    Point2f* side_p2)
{
    Point2f* boundary_point1;
    Point2f* boundary_point2;
    for (int ov = 0; ov < shape->num_vertices; ov++) {
        Point2f* shape_point = &shape->vertices[ov];

        if (point_in_polygon(shape_point, boundary->shape, side_p1, side_p2)) {
            return true;
        }
    }
    return false;
}

void update(WorldObject objects[NUM_OBJECTS], WorldBoundary* boundary, Viewer* viewer, const Uint8* keys)
{
    for (int i = 0; i < NUM_OBJECTS; i++) {
        WorldObject* o = &objects[i];
        o->angle = (o->angle + 1) % 360;

        float c = cos_table[o->angle];
        float s = sin_table[o->angle];

        // translate direction vector to world space
        Point2f vector = { o->position.x + o->vector.x, o->position.y + o->vector.y };

        Point2f delta = {
            (o->position.x - vector.x) / .5,
            (o->position.y - vector.y) / .5
        };
        Point2f new_position = { o->position.x + delta.x, o->position.y + delta.y };
        Point2f side_p1, side_p2;

        // Transform object shape to new pos world coords
        Shape world_shape = { .num_vertices = o->shape->num_vertices };

        for (int sv = 0; sv < o->shape->num_vertices; sv++) {

            Point2f* source = &o->shape->vertices[sv];
            Point2f dest;
            xform_to_world(&new_position, c, s, source, &dest);
            world_shape.vertices[sv].x = dest.x;
            world_shape.vertices[sv].y = dest.y;
        }

        if (get_boundary_collision(
                &world_shape, boundary, &side_p1, &side_p2)) {

            if (side_p1.x == side_p2.x) {
                o->vector.x -= o->vector.x * 2;
            } else {
                o->vector.y -= o->vector.y * 2;
            }
            // translate direction vector to world space
            vector.x = o->position.x + o->vector.x;
            vector.y = o->position.y + o->vector.y;

            delta.x = (o->position.x - vector.x) / .5;
            delta.y = (o->position.y - vector.y) / .5;

        } else {
        }

        o->position.y += delta.y;
        o->position.x += delta.x;
    }

    viewer->position.x += keys[SDL_SCANCODE_RIGHT] == 1 ? 1 : 0;
    viewer->position.x -= keys[SDL_SCANCODE_LEFT] == 1 ? 1 : 0;
    viewer->position.y += keys[SDL_SCANCODE_DOWN] == 1 ? 1 : 0;
    viewer->position.y -= keys[SDL_SCANCODE_UP] == 1 ? 1 : 0;
    viewer->position.z += (keys[SDL_SCANCODE_KP_PLUS] | keys[SDL_SCANCODE_EQUALS]) == 1
        ? viewer->position.z < ZOOM_MAX ? ZOOM_SPEED : 0
        : 0;
    viewer->position.z -= (keys[SDL_SCANCODE_KP_MINUS] | keys[SDL_SCANCODE_MINUS]) == 1
        ? viewer->position.z > ZOOM_MIN ? ZOOM_SPEED : 0
        : 0;
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
            Point2f world;
            xform_to_world(&o->position, c, s, source, &world);

            xform_to_screen(
                SCREEN_WIDTH,
                SCREEN_HEIGHT,
                &viewer->position,
                &world,
                &dest_shape->vertices[v]);
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

        xform_to_screen(
            SCREEN_WIDTH,
            SCREEN_HEIGHT,
            &viewer->position,
            source,
            &dest_shape->vertices[v]);
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
    srand(time(NULL));
    init_math_lookups();

    SDL_bool should_quit = SDL_FALSE;
    char str[80];

    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow("Transform Demo", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    SDL_Renderer* renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    Shape shapes_at_screen[MAX_SHAPES];
    int count_shapes = 0;
    Viewer viewer = { .position = { .x = 0.0, .y = 0.0, .z = 1.0 } };
    init_objects();

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
        update(objects, &world_boundary, &viewer, keys);

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
