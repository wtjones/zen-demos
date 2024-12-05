#include "rasgl/core/app.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/input.h"
#include "rasgl/core/maths.h"
#include "rasgl/core/rasterize.h"
#include "rasgl/core/repr.h"
#include "rasterize.h"
#include <SDL2/SDL.h>
#include <stdbool.h>

ScreenSettings plat_settings
    = { .screen_width = 320, .screen_height = 240 };
SDL_Renderer* renderer;
/**
 * @brief Drawing surface with 8-bit color
 *
 */
SDL_Surface* surface;

RenderState state;
InputState plat_input_state;
int g_key_app_to_plat[RAS_KEY_COUNT];
/**
 * @brief Maintain a mapping of SDL scancodes to RasKey for key-up events
 *
 */
int g_key_plat_to_app[SDL_NUM_SCANCODES];

void input_init()
{
    g_key_app_to_plat[RAS_KEY_A] = SDL_SCANCODE_A;
    g_key_app_to_plat[RAS_KEY_B] = SDL_SCANCODE_B;
    g_key_app_to_plat[RAS_KEY_C] = SDL_SCANCODE_C;
    g_key_app_to_plat[RAS_KEY_D] = SDL_SCANCODE_D;
    g_key_app_to_plat[RAS_KEY_E] = SDL_SCANCODE_E;
    g_key_app_to_plat[RAS_KEY_F] = SDL_SCANCODE_F;
    g_key_app_to_plat[RAS_KEY_G] = SDL_SCANCODE_G;
    g_key_app_to_plat[RAS_KEY_H] = SDL_SCANCODE_H;
    g_key_app_to_plat[RAS_KEY_I] = SDL_SCANCODE_I;
    g_key_app_to_plat[RAS_KEY_J] = SDL_SCANCODE_J;
    g_key_app_to_plat[RAS_KEY_K] = SDL_SCANCODE_K;
    g_key_app_to_plat[RAS_KEY_L] = SDL_SCANCODE_L;
    g_key_app_to_plat[RAS_KEY_M] = SDL_SCANCODE_M;
    g_key_app_to_plat[RAS_KEY_N] = SDL_SCANCODE_N;
    g_key_app_to_plat[RAS_KEY_O] = SDL_SCANCODE_O;
    g_key_app_to_plat[RAS_KEY_P] = SDL_SCANCODE_P;
    g_key_app_to_plat[RAS_KEY_Q] = SDL_SCANCODE_Q;
    g_key_app_to_plat[RAS_KEY_R] = SDL_SCANCODE_R;
    g_key_app_to_plat[RAS_KEY_S] = SDL_SCANCODE_S;
    g_key_app_to_plat[RAS_KEY_T] = SDL_SCANCODE_T;
    g_key_app_to_plat[RAS_KEY_U] = SDL_SCANCODE_U;
    g_key_app_to_plat[RAS_KEY_V] = SDL_SCANCODE_V;
    g_key_app_to_plat[RAS_KEY_W] = SDL_SCANCODE_W;
    g_key_app_to_plat[RAS_KEY_X] = SDL_SCANCODE_X;
    g_key_app_to_plat[RAS_KEY_Y] = SDL_SCANCODE_Y;
    g_key_app_to_plat[RAS_KEY_Z] = SDL_SCANCODE_Z;
    g_key_app_to_plat[RAS_KEY_LCTRL] = SDL_SCANCODE_LCTRL;
    g_key_app_to_plat[RAS_KEY_RCTRL] = SDL_SCANCODE_RCTRL;
    g_key_app_to_plat[RAS_KEY_LSHIFT] = SDL_SCANCODE_LSHIFT;
    g_key_app_to_plat[RAS_KEY_RSHIFT] = SDL_SCANCODE_RSHIFT;
    g_key_app_to_plat[RAS_KEY_MINUS] = SDL_SCANCODE_MINUS;
    g_key_app_to_plat[RAS_KEY_EQUALS] = SDL_SCANCODE_EQUALS;
    g_key_app_to_plat[RAS_KEY_TAB] = SDL_SCANCODE_TAB;
    g_key_app_to_plat[RAS_KEY_ESCAPE] = SDL_SCANCODE_ESCAPE;
    g_key_app_to_plat[RAS_KEY_LEFTBRACKET] = SDL_SCANCODE_LEFTBRACKET;
    g_key_app_to_plat[RAS_KEY_RIGHTBRACKET] = SDL_SCANCODE_RIGHTBRACKET;
    g_key_app_to_plat[RAS_KEY_UP] = SDL_SCANCODE_UP;
    g_key_app_to_plat[RAS_KEY_DOWN] = SDL_SCANCODE_DOWN;
    g_key_app_to_plat[RAS_KEY_LEFT] = SDL_SCANCODE_LEFT;
    g_key_app_to_plat[RAS_KEY_RIGHT] = SDL_SCANCODE_RIGHT;
    g_key_app_to_plat[RAS_KEY_F1] = SDL_SCANCODE_F1;
    g_key_app_to_plat[RAS_KEY_F2] = SDL_SCANCODE_F2;
    g_key_app_to_plat[RAS_KEY_F3] = SDL_SCANCODE_F3;
    g_key_app_to_plat[RAS_KEY_F4] = SDL_SCANCODE_F4;
    g_key_app_to_plat[RAS_KEY_F5] = SDL_SCANCODE_F5;
    g_key_app_to_plat[RAS_KEY_F6] = SDL_SCANCODE_F6;
    g_key_app_to_plat[RAS_KEY_F7] = SDL_SCANCODE_F7;
    g_key_app_to_plat[RAS_KEY_F8] = SDL_SCANCODE_F8;
    g_key_app_to_plat[RAS_KEY_F9] = SDL_SCANCODE_F9;
    g_key_app_to_plat[RAS_KEY_F10] = SDL_SCANCODE_F10;
    g_key_app_to_plat[RAS_KEY_F11] = SDL_SCANCODE_F11;
    g_key_app_to_plat[RAS_KEY_F12] = SDL_SCANCODE_F12;

    for (int i = 0; i < SDL_NUM_SCANCODES; i++) {
        g_key_plat_to_app[i] = RAS_KEY_UNKNOWN;
    }

    for (int i = 0; i < RAS_KEY_COUNT; i++) {
        g_key_plat_to_app[g_key_app_to_plat[i]] = i;
    }
}

void map_input()
{
    int num_keys;
    const Uint8* keys = SDL_GetKeyboardState(&num_keys);

    for (int i = 0; i < RAS_KEY_COUNT; i++) {
        int plat_scancode = g_key_app_to_plat[i];
        plat_input_state.keys[i] = keys[plat_scancode] ? RAS_KEY_EVENT_DOWN
                                                       : RAS_KEY_EVENT_NONE;
    }
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

        RasPipelineFace* face = &state->visible_faces[0];

        while (i < state->num_visible_indexes) {
            int32_t material = state->material_indexes[material_index];

            if (material == -1) {
                ras_log_buffer("Face %d has material = %d", i / 3, material);
            }

            char buffer[255];
            RasFixed max_shade = INT_32_TO_FIXED_16_16(7);
            RasFixed shade_fixed = mul_fixed_16_16_by_fixed_16_16(face->diffuse_intensity, max_shade);
            ras_log_buffer("diffuse_intensity: %s", repr_fixed_16_16(buffer, sizeof buffer, face->diffuse_intensity));
            ras_log_buffer("shade_fixed: %s", repr_fixed_16_16(buffer, sizeof buffer, shade_fixed));
            int shade = FIXED_16_16_TO_INT_32(shade_fixed);
            shade = shade < 1 ? 1 : shade;
            assert(shade <= 7);

            int8_t color = material == -1
                ? shade
                : (shade + (material * 8));

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
            face++;
        }
    }

    for (size_t i = 0; i < state->num_commands; i++) {
        RenderCommand* command = &state->commands[i];

        if (command->num_points == 1) {
            Point2i* point = &(state->points[command->point_indices[0]]);
            RAS_PLOT_PIXEL(surface, point->x, point->y, 14);

        } else if (command->num_points == 2) {
            Point2i* point0 = &(state->points[command->point_indices[0]]);
            Point2i* point1 = &(state->points[command->point_indices[1]]);
            ras_draw_line(surface, point0, point1, 14);

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
    if (!surface) {
        printf("SDL_CreateRGBSurface failed: %s\n", SDL_GetError());
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return -1;
    }

    SDL_Surface* rgb_surface = SDL_ConvertSurfaceFormat(surface, SDL_PIXELFORMAT_RGB888, 0);
    if (!rgb_surface) {
        printf("SDL_ConvertSurfaceFormat failed: %s\n", SDL_GetError());
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return -1;
    }

    /**
     * @brief Texture to render 8bpp surface
     *
     */
    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, rgb_surface);
    if (!texture) {
        printf("SDL_CreateTextureFromSurface failed: %s\n", SDL_GetError());
        SDL_DestroyRenderer(renderer);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return -1;
    }

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
    input_init();
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
                int app_scancode = g_key_plat_to_app[event.key.keysym.scancode];
                if (app_scancode != RAS_KEY_UNKNOWN) {
                    plat_input_state.keys[app_scancode] = RAS_KEY_EVENT_UP;
                } else {
                    ras_log_warn("Unknown scancode: %d", event.key.keysym.scancode);
                }
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

            SDL_BlitSurface(surface, NULL, rgb_surface, NULL);

            SDL_UpdateTexture(texture, NULL, rgb_surface->pixels, rgb_surface->pitch);
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
