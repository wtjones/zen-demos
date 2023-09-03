#include <SDL2/SDL.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define LOGICAL_WIDTH SCREEN_WIDTH / 4
#define LOGICAL_HEIGHT SCREEN_HEIGHT / 4
#define BOARD_WIDTH LOGICAL_WIDTH
#define BOARD_HEIGHT LOGICAL_HEIGHT
#define SIM_RATE 30

typedef enum CellType {
    EMPTY = 0,
    SAND = 1,
    WATER = 2,
    OOB = 3
} CellType;

int8_t board[LOGICAL_WIDTH * LOGICAL_HEIGHT];
int32_t iteration = 0;

bool is_oob(int32_t row, int32_t col)
{
    return (row < 0 || row >= BOARD_HEIGHT || col < 0 || col >= BOARD_WIDTH);
}
int8_t get_cell(int32_t row, int32_t col)
{
    if (is_oob(row, col)) {
        return OOB;
    }
    return board[BOARD_WIDTH * row + col];
}

void set_cell(int32_t row, int32_t col, int8_t cell)
{
    assert(!is_oob(row, col));
    board[BOARD_WIDTH * row + col] = cell;
}

void update()
{
    for (int row = BOARD_HEIGHT - 1; row >= 0; row--) {
        for (int col = BOARD_WIDTH - 1; col >= 0; col--) {
            int8_t cell = get_cell(row, col);

            if (cell == SAND) {
                if (get_cell(row + 1, col) == EMPTY) {
                    set_cell(row, col, EMPTY);
                    set_cell(row + 1, col, SAND);
                }
            }
        }
    }
}
void draw_board(SDL_Renderer* renderer)
{
    for (int row = 0; row < LOGICAL_HEIGHT; row++) {
        for (int col = 0; col < LOGICAL_WIDTH; col++) {
            int8_t cell = get_cell(row, col);

            if (cell != EMPTY) {
                if (cell == SAND) {
                    SDL_SetRenderDrawColor(renderer, 0, 150, 150, 255);
                } else {
                    SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
                }
                SDL_RenderDrawPoint(renderer, col, row);
            }
        }
    }
}

int main(int argc, char* argv[])
{
    srand(time(NULL));

    SDL_bool should_quit = SDL_FALSE;
    char str[80];

    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_Window* win = SDL_CreateWindow("Simulation", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    SDL_Renderer* renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    SDL_RenderSetLogicalSize(renderer, LOGICAL_WIDTH, LOGICAL_HEIGHT);

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
    SDL_RenderClear(renderer);

    printf("board rows: %d, cols: %d\n", BOARD_HEIGHT, BOARD_WIDTH);
    set_cell(0, BOARD_WIDTH / 2, SAND);

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
        printf("iteration: %d\n", iteration);

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
        SDL_RenderClear(renderer);
        update();
        draw_board(renderer);

        iteration++;

        SDL_RenderPresent(renderer);
        while (SDL_GetTicks() - last_frame < 1000 / SIM_RATE) {
        }
        last_frame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
