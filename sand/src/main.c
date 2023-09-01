#include <SDL2/SDL.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define LOGICAL_WIDTH SCREEN_WIDTH / 4
#define LOGICAL_HEIGHT SCREEN_HEIGHT / 4

int main(int argc, char* argv[])
{
    srand(time(NULL));

    SDL_bool should_quit = SDL_FALSE;
    char str[80];

    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow("Transform Demo", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    SDL_Renderer* renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    SDL_RenderSetLogicalSize(renderer, LOGICAL_WIDTH, LOGICAL_HEIGHT);

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
    for (int i = 0; i < LOGICAL_WIDTH; ++i)
        SDL_RenderDrawPoint(renderer, i, i);
    SDL_RenderPresent(renderer);

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

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
