#include "Engine.h"
#include "Player.h"
#include "Vector2.h"
#include <SDL2/SDL.h>
#include <array>
#include <iostream>
#include <math.h>
#include <string>

#define _USE_MATH_DEFINES

#ifdef main
#    undef main
#endif

const int SCREEN_LOGICAL_WIDTH = 320;
const int SCREEN_LOGICAL_HEIGHT = 240;
const int SCREEN_WIDTH = SCREEN_LOGICAL_WIDTH * 4;
const int SCREEN_HEIGHT = SCREEN_LOGICAL_HEIGHT * 4;

const float WALK_SPEED = 0.1f;
const float TURN_SPEED = 2;
const bool USE_LIGHTING = false;

#define mapWidth 24
#define mapHeight 24

int worldMap[mapWidth][mapHeight] = {
    { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 0, 3, 0, 3, 0, 3, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 2, 2, 0, 2, 2, 0, 0, 0, 0, 3, 0, 3, 0, 3, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 4, 4, 4, 4, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 0, 4, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 0, 0, 0, 0, 3, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 0, 4, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 0, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
    { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
};

float sinTable[360];
float cosTable[360];

SDL_Window* sdlWindow = nullptr;
SDL_Renderer* sdlRenderer = nullptr;
SDL_Texture* sdlTexture = nullptr;

int main(int argc, char** argv)
{
    /*
    Engine *engine = new Engine();


    if (!engine->Init())
    {
        return 1;
    }
    engine->Run();


*/
    double posX = 22, posY = 12; //x and y start position
    double origDirX = -1, origDirY = 0;
    double dirX = -1, dirY = 0;              //initial direction vector
    double origPlaneX = 0, origPlaneY = .66; //0.66; //the 2d raycaster version of camera plane
    double planeX = 0, planeY = 0.66;        //the 2d raycaster version of camera plane

    double time = 0;    //time of current frame
    double oldTime = 0; //time of previous frame

    for (int i = 0; i < 360; i++) {
        cosTable[i] = cos((float)i * M_PI / -180.0);
        sinTable[i] = sin((float)i * M_PI / -180.0);
    }

    Player player;
    player.angle = 90;

    if (SDL_Init(SDL_INIT_EVERYTHING) == -1) {
        std::cout << SDL_GetError() << std::endl;
        return 1;
    }

    SDL_Surface* texture = nullptr;
    texture = SDL_LoadBMP("assets/wickedcrystal1.bmp");

    if (texture == nullptr) {
        const char* error = SDL_GetError();
        std::cout << SDL_GetError() << std::endl;
        return 2;
    }

    sdlWindow = SDL_CreateWindow("Lesson 2", SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);

    if (sdlWindow == nullptr) {
        std::cout << SDL_GetError() << std::endl;
        return 2;
    }
    sdlRenderer = SDL_CreateRenderer(sdlWindow, -1, 0);
    if (sdlRenderer == nullptr) {
        std::cout << SDL_GetError() << std::endl;
        return 3;
    }

    SDL_RenderSetLogicalSize(sdlRenderer, SCREEN_LOGICAL_WIDTH, SCREEN_LOGICAL_HEIGHT);

    sdlTexture = SDL_CreateTexture(sdlRenderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        SCREEN_LOGICAL_WIDTH, SCREEN_LOGICAL_HEIGHT);

    Uint32* myPixels; // maybe this is a surface->pixels, or a malloc()'d buffer, or whatever.
    int bufferSize = SCREEN_LOGICAL_WIDTH * SCREEN_LOGICAL_WIDTH * sizeof(Uint32);
    myPixels = (Uint32*)malloc(bufferSize);

    if (myPixels == nullptr) {
        SDL_Log("Nope!!!!!!!!!!!!!!!!!!");
        return 3;
    }
    memset(myPixels, 0, bufferSize);
    SDL_Color color;
    color.a = 255;
    color.r = 255;
    color.g = 0;
    color.b = 0;

    SDL_Point* stripPoints;
    stripPoints = (SDL_Point*)malloc(SCREEN_LOGICAL_HEIGHT * sizeof(SDL_Point));

    int ballx = 5;
    int bally = 5;

    SDL_Event e;
    bool quit = false;
    int lastFrame = SDL_GetTicks();
    while (!quit) {
        //Event polling
        while (SDL_PollEvent(&e)) {
            //If user closes he window
            if (e.type == SDL_QUIT)
                quit = true;
            //If user presses any key
            if (e.type == SDL_KEYDOWN) {
                if (e.key.keysym.sym == SDLK_ESCAPE)
                    quit = true;
            }
        }

        float move = 0;

        const Uint8* state = SDL_GetKeyboardState(NULL);
        if (state[SDL_SCANCODE_RIGHT]) {
            player.angle = (player.angle += TURN_SPEED) % 360;
            SDL_Log(std::to_string(player.angle).c_str());
        }
        if (state[SDL_SCANCODE_LEFT]) {
            ballx--;
            player.angle -= TURN_SPEED;
            if (player.angle < 0)
                player.angle = 359;

            SDL_Log(std::to_string(player.angle).c_str());
        }
        if (state[SDL_SCANCODE_UP]) {
            move = WALK_SPEED;
        }
        if (state[SDL_SCANCODE_DOWN]) {
            move = -WALK_SPEED;
        }

        dirX = cosTable[player.angle] * origDirX - sinTable[player.angle] * origDirY;
        dirY = sinTable[player.angle] * origDirX + cosTable[player.angle] * origDirY;
        planeX = cosTable[player.angle] * origPlaneX - sinTable[player.angle] * origPlaneY;
        planeY = sinTable[player.angle] * origPlaneX + cosTable[player.angle] * origPlaneY;

        if (move != 0) {
            posX += dirX * move;
            posY += dirY * move;
            SDL_Log((std::to_string(posX) + " " + std::to_string(posY)).c_str());
        }

        Vector2 direction;

        Uint32 px = posX + 5 * cosTable[player.angle];
        Uint32 py = posY - 5 * sinTable[player.angle];

        SDL_SetRenderDrawColor(sdlRenderer, 0, 0, 0, 255);
        SDL_RenderClear(sdlRenderer);

        // ray loop
        int w = SCREEN_LOGICAL_WIDTH;
        int h = SCREEN_LOGICAL_HEIGHT;
        for (int x = 0; x < w; x++) {
            //calculate ray position and direction
            double cameraX = 2 * x / double(w) - 1; //The x-coordinate in camera space. Gives a range of -1 to 1
            double rayPosX = posX;
            double rayPosY = posY;
            double rayDirX = dirX + planeX * cameraX;
            double rayDirY = dirY + planeY * cameraX;

            //which box of the map we're in
            int mapX = int(rayPosX);
            int mapY = int(rayPosY);

            //length of ray from current position to next x or y-side
            double sideDistX;
            double sideDistY;

            //length of ray from one x or y-side to next x or y-side
            double deltaDistX = sqrt(1 + (rayDirY * rayDirY) / (rayDirX * rayDirX));
            double deltaDistY = sqrt(1 + (rayDirX * rayDirX) / (rayDirY * rayDirY));
            double perpWallDist;

            //what direction to step in x or y-direction (either +1 or -1)
            int stepX;
            int stepY;

            int hit = 0; //was there a wall hit?
            int side;    //was a NS or a EW wall hit?

            //calculate step and initial sideDist
            if (rayDirX < 0) {
                stepX = -1;
                sideDistX = (rayPosX - mapX) * deltaDistX;
            } else {
                stepX = 1;
                sideDistX = (mapX + 1.0 - rayPosX) * deltaDistX;
            }
            if (rayDirY < 0) {
                stepY = -1;
                sideDistY = (rayPosY - mapY) * deltaDistY;
            } else {
                stepY = 1;
                sideDistY = (mapY + 1.0 - rayPosY) * deltaDistY;
            }

            //perform DDA
            while (hit == 0) {
                //jump to next map square, OR in x-direction, OR in y-direction
                if (sideDistX < sideDistY) {
                    sideDistX += deltaDistX;
                    mapX += stepX;
                    side = 0;
                } else {
                    sideDistY += deltaDistY;
                    mapY += stepY;
                    side = 1;
                }
                //Check if ray has hit a wall
                if (worldMap[mapX][mapY] > 0) {
                    hit = 1;
                }
            }

            //Calculate distance projected on camera direction (oblique distance will give fisheye effect!)
            if (side == 0)
                perpWallDist = fabs((mapX - rayPosX + (1 - stepX) / 2) / rayDirX);
            else
                perpWallDist = fabs((mapY - rayPosY + (1 - stepY) / 2) / rayDirY);

            //calculate value of wallX
            double wallX; //where exactly the wall was hit
            double hitPointX, hitPointY;
            if (side == 1) {
                hitPointX = rayPosX + ((mapY - rayPosY + (1 - stepY) / 2) / rayDirY) * rayDirX;
                hitPointY = mapY;
                wallX = hitPointX;
            } else {
                hitPointY = rayPosY + ((mapX - rayPosX + (1 - stepX) / 2) / rayDirX) * rayDirY;
                hitPointX = mapX;
                wallX = hitPointY;
            }
            wallX -= floor((wallX));

            double hitDistance = sqrt(
                (hitPointX - rayPosX) * (hitPointX - rayPosX) + (hitPointY - rayPosY) * (hitPointY - rayPosY));

            double lightDimmer = hitDistance * 3.0f;
            if (hitDistance < 5)
                lightDimmer = 0;

            //if (x == 160) SDL_Log(("rayDir X, Y: " + std::to_string(rayDirX) + " " + std::to_string(rayDirY)).c_str());
            if (x == 160)
                SDL_Log(("dist : " + std::to_string(hitDistance)).c_str());

            //Calculate height of line to draw on screen
            int lineHeight = abs(int(h / perpWallDist));

            //calculate lowest and highest pixel to fill in current strip
            int drawStart = -lineHeight / 2 + h / 2;
            if (drawStart < 0)
                drawStart = 0;
            int drawEnd = lineHeight / 2 + h / 2;
            if (drawEnd >= h)
                drawEnd = h - 1;

            int colorRed = 0, colorGreen = 0, colorBlue = 0;
            /*switch(worldMap[mapX][mapY])
            {
            case 1:
                colorRed = (side == 1) ? 127 : 255;
                break;
            case 2:
                colorGreen = (side == 1) ? 127 : 255;
                break;
            case 3:
                colorBlue = (side == 1) ? 127 : 255;
                break;
            case 4:
                colorRed=colorGreen=colorBlue=200;
                break;

            }*/

            switch (worldMap[mapX][mapY]) {
            case 1:
                colorRed = 255;
                break;
            case 2:
                colorGreen = 255;
                break;
            case 3:
                colorBlue = 255;
                break;
            case 4:
                colorRed = colorGreen = colorBlue = 200;
                break;
            }

            float stripSize = drawEnd - drawStart;

            for (int drawY = drawStart; drawY < drawEnd; drawY++) {
                float stripY = drawY - drawStart;
                int texY;

                int d = drawY * 2 - h + lineHeight;
                texY = ((d * texture->h) / lineHeight) / 2;

                int texX = (int)(wallX * texture->w);
                char* pixels;
                pixels = (char*)texture->pixels;
                Uint32 tc;

                int offset = (texY * texture->pitch) + (texX * texture->format->BytesPerPixel);
                Uint8 tr;
                Uint8 tg;
                Uint8 tb;
                Uint8 ta;

                tr = pixels[offset];
                tg = pixels[offset + 1];
                tb = pixels[offset + 2];

                if (USE_LIGHTING) {
                    Uint8 dim = (Uint8)lightDimmer;
                    //lightDimmer = 1.0f;

                    if ((int)tr - (int)dim < 0 || (int)tg - (int)dim < 0 || (int)tb - (int)dim < 0) {
                        tr = tg = tb = 0;
                    } else {

                        tr -= dim;
                        //if (tr < 0) tr = 0;
                        tg -= dim;
                        //if (tg < 0) tg = 0;
                        tb -= dim;
                        //if (tb < 0) tb = 0;
                    }
                }

                SDL_SetRenderDrawColor(sdlRenderer, tr, tg, tb, 255);
                SDL_RenderDrawPoint(sdlRenderer, x, drawY);
            }
        }

        SDL_SetRenderDrawColor(sdlRenderer, 255, 255, 255, 255);
        SDL_RenderDrawPoint(sdlRenderer, (Uint32)100, (Uint32)30);
        SDL_SetRenderDrawColor(sdlRenderer, 255, 0, 0, 255);
        SDL_RenderDrawPoint(sdlRenderer, (Uint32)(dirX * 10) + 100, (Uint32)(-dirY * 10) + 30);

        SDL_SetRenderDrawColor(sdlRenderer, 255, 255, 255, 255);
        SDL_RenderDrawPoint(sdlRenderer, (Uint32)150, (Uint32)30);
        SDL_SetRenderDrawColor(sdlRenderer, 0, 150, 0, 10);
        SDL_RenderDrawPoint(sdlRenderer, (Uint32)(planeX * 10) + 150, (Uint32)(-planeY * 10) + 30);

        // draw player overview
        Uint32 osdMapOffsetX = SCREEN_LOGICAL_WIDTH / 2;
        Uint32 osdMapOffsetY = SCREEN_LOGICAL_HEIGHT / 2;

        SDL_SetRenderDrawColor(sdlRenderer, 0, 255, 0, 255);
        SDL_RenderDrawPoint(sdlRenderer, osdMapOffsetX + (Uint32)posX, osdMapOffsetY + -(Uint32)posY);
        SDL_SetRenderDrawColor(sdlRenderer, 0, 0, 255, 255);
        SDL_RenderDrawPoint(sdlRenderer, osdMapOffsetX + (Uint32)px, osdMapOffsetY + -(Uint32)py);

        SDL_RenderPresent(sdlRenderer);

        int currentTicks = SDL_GetTicks();
        while (SDL_GetTicks() - lastFrame < 1000 / 60) {
        }
        lastFrame = SDL_GetTicks();
    }

    SDL_DestroyRenderer(sdlRenderer);
    SDL_DestroyWindow(sdlWindow);
    SDL_Quit();

    return 0;
}

void DrawPixel(Uint32* buffer, int x, int y, SDL_Color color)
{

    int bpp = 32;
    Uint32 rmask;
    Uint32 gmask;
    Uint32 bmask;
    Uint32 amask;
    bool ret = SDL_PixelFormatEnumToMasks(SDL_PIXELFORMAT_ARGB8888, &bpp, &rmask, &gmask, &bmask, &amask);

    int index = 0;

    Uint32 val = 0;
    val = (color.a & amask); // + (color.r & rmask) + (color.g & gmask) + (color.b & bmask);

    buffer[index] = 0xFFFF0000;
}