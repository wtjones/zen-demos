#include <SDL2/SDL.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdbool.h>
#include <stdio.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

float cos_table[360];
float sin_table[360];
int angle = 0;

float points[4][2];
float points_translated[4][2];
float points_tranformed[4][2];

void init_math()
{

    for (int i = 0; i < 360; i++) {
        cos_table[i] = cos((float)i * M_PI / -180.0);
        sin_table[i] = sin((float)i * M_PI / -180.0);
    }
}

int main()
{
    init_math();

    points[0][0] = 250;
    points[0][1] = 150;
    points[1][0] = 350;
    points[1][1] = 150;
    points[2][0] = 350;
    points[2][1] = 250;
    points[3][0] = 250;
    points[3][1] = 250;

    SDL_bool should_quit = SDL_FALSE;
    printf("Hello, World!2 \n");
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow("Hello World!", 100, 100, 640, 480, SDL_WINDOW_SHOWN);
    SDL_Renderer* renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED);

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

        float c = cos_table[angle];
        float s = sin_table[angle];

        for (int i = 0; i < 4; i++) {
            points_translated[i][0] = points[i][0] - 300;
            points_translated[i][1] = points[i][1] - 200;
        }

        for (int i = 0; i < 4; i++) {
            points_tranformed[i][0] = c * points_translated[i][0] - s * points_translated[i][1];
            points_tranformed[i][1] = c * points_translated[i][1] + s * points_translated[i][0];
        }

        for (int i = 0; i < 4; i++) {
            points_tranformed[i][0] = points_tranformed[i][0] + 300;
            points_tranformed[i][1] = points_tranformed[i][1] + 200;
        }

        SDL_RenderClear(renderer);
        SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
        SDL_RenderDrawPointsF(renderer, points_tranformed, 4);
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderPresent(renderer);
        angle = (angle + 1) % 360;
        char str[80];
        snprintf(str, sizeof(str), "Angle: %d\n", angle);
        printf(str);
        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}