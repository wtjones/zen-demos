#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include "rasgl/core/rasterize.h"
#include "rasterize.h"
#include <SDL2/SDL.h>
#include <stdbool.h>

ScreenSettings plat_settings
    = { .screen_width = 320, .screen_height = 240 };
SDL_Renderer* renderer;
SDL_Surface* surface;

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
    plat_input_state.keys[RAS_KEY_C] = keys[SDL_SCANCODE_C];
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

uint8_t color_from_material(int32_t material)
{

    // TODO: Use calculated shade
    int8_t color = material == -1
        ? 7
        : (7 + (material * 8));
}

void render_state(RenderState* state)
{
    int i = 0;

    if (state->polygon_mode == RAS_POLYGON_WIREFRAME) {
        RasVector4f* sv;
        uint32_t material_index = 0;
        while (i < state->num_visible_indexes) {
            int32_t material = state->material_indexes[material_index];
            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }
            int8_t color = material == -1
                ? 7
                : (7 + (material * 8));
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

            ras_draw_line(surface, &point0, &point1, color);
            ras_draw_line(surface, &point1, &point2, color);
            ras_draw_line(surface, &point2, &point0, color);
            material_index++;
        }
    } else {
        i = 0;
        RasVector4f* tri[3];
        RasHorizontalLine hlines[255];
        size_t num_hlines = 0;
        uint32_t material_index = 0;

        while (i < state->num_visible_indexes) {
            int32_t material = state->material_indexes[material_index];

            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }

            // TODO: Use calculated shade
            int8_t color = material == -1
                ? 7
                : (7 + (material * 8));

            RasPipelineVertex* pv0 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[0] = &pv0->screen_space_position;

            RasPipelineVertex* pv1 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[1] = &pv1->screen_space_position;

            RasPipelineVertex* pv2 = &state->pipeline_verts[state->visible_indexes[i++]];
            tri[2] = &pv2->screen_space_position;
            rasterize_tri(tri, hlines, &num_hlines);

            for (size_t j = 0; j < num_hlines; j++) {
                Point2i point0 = {
                    .x = hlines[j].left.x,
                    .y = hlines[j].left.y
                };
                Point2i point1 = {
                    .x = hlines[j].right.x,
                    .y = hlines[j].right.y
                };
                ras_draw_line(surface, &point0, &point1, color);
            }
            material_index++;
        }
    }

    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];

        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);
            RAS_PLOT_PIXEL(surface, point->x, point->y, 1);

        } else if (command->num_points == 2) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            ras_draw_line(surface, point0, point1, 2);

        } else if (command->num_points == 3) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            Point2i* point2 = &(state->points[command->point_indices[2]]);

            ras_draw_line(surface, point0, point1, 0);
            ras_draw_line(surface, point1, point2, 1);
            ras_draw_line(surface, point2, point0, 2);
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

    surface = SDL_CreateRGBSurface(
        0,
        plat_settings.screen_width,
        plat_settings.screen_height,
        8, 0, 0, 0, 0);

    SDL_Color colors[256] = {
        // Grey ramp
        { 0x17, 0x18, 0x03, 255 }, // #171803
        { 0x1B, 0x25, 0x0A, 255 }, // #1B250A
        { 0x23, 0x35, 0x1A, 255 }, // #23351A
        { 0x31, 0x45, 0x30, 255 }, // #314530
        { 0x52, 0x66, 0x58, 255 }, // #526658
        { 0x84, 0x96, 0x8E, 255 }, // #84968E
        { 0xB6, 0xC3, 0xC2, 255 }, // #B6C3C2
        { 0xF1, 0xF1, 0xF1, 255 }, // #F1F1F1

        // Red ramp
        { 0x1F, 0x00, 0x00, 255 }, // Darkest red
        { 0x3F, 0x00, 0x00, 255 },
        { 0x5F, 0x00, 0x00, 255 },
        { 0x7F, 0x00, 0x00, 255 },
        { 0x9F, 0x00, 0x00, 255 },
        { 0xBF, 0x00, 0x00, 255 },
        { 0xDF, 0x00, 0x00, 255 },
        { 0xFF, 0x00, 0x00, 255 }, // Brightest red

        // Green ramp
        { 0x00, 0x1F, 0x00, 255 }, // Darkest green
        { 0x00, 0x3F, 0x00, 255 },
        { 0x00, 0x5F, 0x00, 255 },
        { 0x00, 0x7F, 0x00, 255 },
        { 0x00, 0x9F, 0x00, 255 },
        { 0x00, 0xBF, 0x00, 255 },
        { 0x00, 0xDF, 0x00, 255 },
        { 0x00, 0xFF, 0x00, 255 }, // Brightest green

        // Blue ramp
        { 0x00, 0x00, 0x1F, 255 }, // Darkest blue
        { 0x00, 0x00, 0x3F, 255 },
        { 0x00, 0x00, 0x5F, 255 },
        { 0x00, 0x00, 0x7F, 255 },
        { 0x00, 0x00, 0x9F, 255 },
        { 0x00, 0x00, 0xBF, 255 },
        { 0x00, 0x00, 0xDF, 255 },
        { 0x00, 0x00, 0xFF, 255 } // Brightest blue
    };

    if (SDL_SetPaletteColors(surface->format->palette, colors, 0, 256) != 0) {
        ras_log_error("SDL_SetPaletteColors failed: %s", SDL_GetError());
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 1;
    }

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
                plat_input_state.keys[RAS_KEY_C] = event.key.keysym.scancode == SDL_SCANCODE_C
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
            SDL_LockSurface(surface);

            ras_app_render(&state);

            Uint8* pixels = (Uint8*)surface->pixels;
            for (int i = 0; i < surface->w * surface->h; i++) {
                pixels[i] = 0;
            }
            render_state(&state);

            SDL_UnlockSurface(surface);

            SDL_Surface* rgb_surface = SDL_ConvertSurfaceFormat(surface, SDL_PIXELFORMAT_RGB888, 0);
            if (!rgb_surface) {
                printf("SDL_ConvertSurfaceFormat failed: %s\n", SDL_GetError());
                SDL_FreeSurface(surface);
                SDL_DestroyRenderer(renderer);
                SDL_DestroyWindow(win);
                SDL_Quit();
                return -1;
            }

            SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, rgb_surface);
            SDL_RenderClear(renderer);
            SDL_RenderCopy(renderer, texture, NULL, NULL);
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
