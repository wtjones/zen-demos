#pragma once

#include "SDL.h"
#include "Map.h"
#include "Player.h"
#include "RaycastRenderer.h"

class Engine
{
    //int worldMap[24][24];
    Map *worldMap;
    SDL_Window *sdlWindow;
    SDL_Renderer *sdlRenderer;
    SDL_Surface *texture;
    Player player;
    bool useLighting;
public:
    Engine(void);
    ~Engine(void);
    bool Init();
    bool LoadAssets();
    void Run();

};

