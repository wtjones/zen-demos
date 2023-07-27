#include "rasgl/core/app.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>

ScreenSettings plat_settings = { .screen_width = 320, .screen_height = 240 };
SDL_Renderer* renderer;

RenderState state = { .num_commands = 0, .num_points = 0, .current_frame = 0 };
InputState plat_input_state;

void map_input()
{
    int num_keys;
    const Uint8* keys = SDL_GetKeyboardState(&num_keys);
    for (int i = 0; i < RAS_MAX_KEYS; i++) {
        plat_input_state.keys[i] = 0;
    }
    plat_input_state.keys[RAS_KEY_W] = keys[SDL_SCANCODE_W];
    plat_input_state.keys[RAS_KEY_A] = keys[SDL_SCANCODE_A];
    plat_input_state.keys[RAS_KEY_S] = keys[SDL_SCANCODE_S];
    plat_input_state.keys[RAS_KEY_D] = keys[SDL_SCANCODE_D];
    plat_input_state.keys[RAS_KEY_Q] = keys[SDL_SCANCODE_Q];
    plat_input_state.keys[RAS_KEY_E] = keys[SDL_SCANCODE_E];
    plat_input_state.keys[RAS_KEY_EQUALS] = keys[SDL_SCANCODE_EQUALS];
    plat_input_state.keys[RAS_KEY_MINUS] = keys[SDL_SCANCODE_MINUS];
    plat_input_state.keys[RAS_KEY_ESCAPE] = keys[SDL_SCANCODE_ESCAPE];
    plat_input_state.keys[RAS_KEY_RIGHT] = keys[SDL_SCANCODE_RIGHT];
    plat_input_state.keys[RAS_KEY_LEFT] = keys[SDL_SCANCODE_LEFT];
}

void render_state(RenderState* state)
{
    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];

        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);
            SDL_SetRenderDrawColor(renderer, 255, 0, 255, 255);
            SDL_RenderDrawPoint(renderer, point->x, point->y);
        } else if (command->num_points == 3) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            Point2i* point2 = &(state->points[command->point_indices[2]]);
            SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
            SDL_RenderDrawLine(renderer, point0->x, point0->y, point1->x, point1->y);
            SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
            SDL_RenderDrawLine(renderer, point1->x, point1->y, point2->x, point2->y);
            SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
            SDL_RenderDrawLine(renderer, point2->x, point2->y, point0->x, point0->y);
        }
    }
    state->current_frame++;
}

int main(int argc, const char** argv)
{
    SDL_bool should_quit = SDL_FALSE;
    printf("Hello, World!2 \n");
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow(
        "Hello World!",
        100,
        100,
        plat_settings.screen_width * 2,
        plat_settings.screen_height * 2,
        SDL_WINDOW_SHOWN);
    renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);

    SDL_RenderSetLogicalSize(renderer, 320, 240);
    ras_app_init(argc, argv, &plat_settings);

    int last_frame = SDL_GetTicks();
    while (!should_quit) {
        map_input();

        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                should_quit = SDL_TRUE;
                break;
            case SDL_KEYUP:
                should_quit = event.key.keysym.scancode == SDL_SCANCODE_ESCAPE;
                plat_input_state.keys[RAS_KEY_TAB] = event.key.keysym.scancode == SDL_SCANCODE_TAB ? 1 : 0;
            }
        }

        ras_app_update(&plat_input_state);
        ras_app_render(&state);
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
        SDL_RenderClear(renderer);
        render_state(&state);

        SDL_RenderPresent(renderer);

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
