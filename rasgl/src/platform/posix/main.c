#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include "rasgl/core/rasterize.h"
#include <SDL2/SDL.h>
#include <stdbool.h>

ScreenSettings plat_settings = { .screen_width = 320, .screen_height = 240 };
SDL_Renderer* renderer;

RenderState state;
InputState plat_input_state;

void map_input()
{
    int num_keys;
    const Uint8* keys = SDL_GetKeyboardState(&num_keys);
    for (int i = 0; i < RAS_MAX_KEYS; i++) {
        plat_input_state.keys[i] = 0;
    }
    plat_input_state.keys[RAS_KEY_UP] = keys[SDL_SCANCODE_UP];
    plat_input_state.keys[RAS_KEY_DOWN] = keys[SDL_SCANCODE_DOWN];
    plat_input_state.keys[RAS_KEY_LEFT] = keys[SDL_SCANCODE_LEFT];
    plat_input_state.keys[RAS_KEY_RIGHT] = keys[SDL_SCANCODE_RIGHT];
    plat_input_state.keys[RAS_KEY_W] = keys[SDL_SCANCODE_W];
    plat_input_state.keys[RAS_KEY_A] = keys[SDL_SCANCODE_A];
    plat_input_state.keys[RAS_KEY_S] = keys[SDL_SCANCODE_S];
    plat_input_state.keys[RAS_KEY_D] = keys[SDL_SCANCODE_D];
    plat_input_state.keys[RAS_KEY_Q] = keys[SDL_SCANCODE_Q];
    plat_input_state.keys[RAS_KEY_E] = keys[SDL_SCANCODE_E];
    plat_input_state.keys[RAS_KEY_P] = keys[SDL_SCANCODE_P];
    plat_input_state.keys[RAS_KEY_O] = keys[SDL_SCANCODE_O];
    plat_input_state.keys[RAS_KEY_B] = keys[SDL_SCANCODE_B];
    plat_input_state.keys[RAS_KEY_F] = keys[SDL_SCANCODE_F];
    plat_input_state.keys[RAS_KEY_EQUALS] = keys[SDL_SCANCODE_EQUALS];
    plat_input_state.keys[RAS_KEY_MINUS] = keys[SDL_SCANCODE_MINUS];
    plat_input_state.keys[RAS_KEY_ESCAPE] = keys[SDL_SCANCODE_ESCAPE];
    plat_input_state.keys[RAS_KEY_RIGHT] = keys[SDL_SCANCODE_RIGHT];
    plat_input_state.keys[RAS_KEY_LEFT] = keys[SDL_SCANCODE_LEFT];
    plat_input_state.keys[RAS_KEY_LEFTBRACKET] = keys[SDL_SCANCODE_LEFTBRACKET];
    plat_input_state.keys[RAS_KEY_RIGHTBRACKET] = keys[SDL_SCANCODE_RIGHTBRACKET];
    plat_input_state.keys[RAS_KEY_LSHIFT] = keys[SDL_SCANCODE_LSHIFT];
    plat_input_state.keys[RAS_KEY_LCTRL] = keys[SDL_SCANCODE_LCTRL];
}

void render_state(RenderState* state)
{
    int i = 0;

    if (state->polygon_mode == RAS_POLYGON_WIREFRAME) {
        RasVector4f* sv;

        while (i < state->num_visible_indexes) {
            RasPipelineVertex* pv0 = &state->pipeline_verts[state->visible_indexes[i++]];
            sv = &pv0->screen_space_position;
            Point2i point0 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv1 = &state->pipeline_verts[state->visible_indexes[i++]];
            sv = &pv1->screen_space_position;
            Point2i point1 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            RasPipelineVertex* pv2 = &state->pipeline_verts[state->visible_indexes[i++]];
            sv = &pv2->screen_space_position;
            Point2i point2 = {
                .x = FIXED_16_16_TO_INT_32(sv->x),
                .y = FIXED_16_16_TO_INT_32(sv->y)
            };

            SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
            SDL_RenderDrawLine(renderer, point0.x, point0.y, point1.x, point1.y);
            SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
            SDL_RenderDrawLine(renderer, point1.x, point1.y, point2.x, point2.y);
            SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
            SDL_RenderDrawLine(renderer, point2.x, point2.y, point0.x, point0.y);
        }
    } else {
        i = 0;
        RasVector4f* tri[3];
        RasHorizontalLine hlines[255];
        size_t num_hlines = 0;
        while (i < state->num_visible_indexes) {
            RasPipelineVertex* pv0 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[0] = &pv0->screen_space_position;

            RasPipelineVertex* pv1 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[1] = &pv1->screen_space_position;

            RasPipelineVertex* pv2 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[2] = &pv2->screen_space_position;
            rasterize_tri(tri, hlines, &num_hlines);

            SDL_SetRenderDrawColor(renderer, 0, 70, 100, 255);
            for (size_t j = 0; j < num_hlines; j++) {
                Point2i point0 = {
                    .x = hlines[j].left.x,
                    .y = hlines[j].left.y
                };
                Point2i point1 = {
                    .x = hlines[j].right.x,
                    .y = hlines[j].right.y
                };
                SDL_RenderDrawLine(renderer, point0.x, point0.y, point1.x, point1.y);
            }
        }
    }

    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];

        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);
            SDL_SetRenderDrawColor(renderer, 255, 0, 255, 255);
            SDL_RenderDrawPoint(renderer, point->x, point->y);
        } else if (command->num_points == 2) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
            SDL_RenderDrawLine(renderer, point0->x, point0->y, point1->x, point1->y);
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
    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(RAS_LOG_LEVEL_STRERR);
    log_set_quiet(false);

    ras_log_info("Starting SDL...");
    SDL_bool should_quit = SDL_FALSE;
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        ras_log_error("SDL error \n");
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

    RasResult result = ras_app_init(argc, argv, &plat_settings);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Error result from ras_app_init(), exiting...");

        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 1;
    }

    core_renderstate_init(&state);
    core_input_init(&plat_input_state);

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
                plat_input_state.keys[RAS_KEY_TAB] = event.key.keysym.scancode == SDL_SCANCODE_TAB
                    ? RAS_KEY_EVENT_UP
                    : RAS_KEY_EVENT_NONE;
                plat_input_state.keys[RAS_KEY_P] = event.key.keysym.scancode == SDL_SCANCODE_P
                    ? RAS_KEY_EVENT_UP
                    : RAS_KEY_EVENT_NONE;
                plat_input_state.keys[RAS_KEY_B] = event.key.keysym.scancode == SDL_SCANCODE_B
                    ? RAS_KEY_EVENT_UP
                    : RAS_KEY_EVENT_NONE;
                plat_input_state.keys[RAS_KEY_F] = event.key.keysym.scancode == SDL_SCANCODE_F
                    ? RAS_KEY_EVENT_UP
                    : RAS_KEY_EVENT_NONE;
                plat_input_state.keys[RAS_KEY_O] = event.key.keysym.scancode == SDL_SCANCODE_O
                    ? RAS_KEY_EVENT_UP
                    : RAS_KEY_EVENT_NONE;
            }
        }
        if (state.max_frames == UINT32_MAX || state.current_frame < state.max_frames) {
            core_renderstate_clear(&state);
            ras_core_update(&plat_input_state, &state);
            ras_app_update(&plat_input_state);
            state.screen_settings.screen_width = plat_settings.screen_width;
            state.screen_settings.screen_height = plat_settings.screen_height;
            ras_app_render(&state);
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
            SDL_RenderClear(renderer);
            render_state(&state);

            SDL_RenderPresent(renderer);
        }

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
