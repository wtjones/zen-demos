#include <stdio.h>
#include <SDL2/SDL.h>

int main()
{
    printf("Hello, World!2 \n");
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
    {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window *win = SDL_CreateWindow("Hello World!", 100, 100, 640, 480, SDL_WINDOW_SHOWN);
    SDL_Delay(1000);
    SDL_Quit();
    return 0;
}