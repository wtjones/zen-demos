#include "Engine.h"


Engine::Engine(void)
{
    this->sdlWindow = nullptr;
    this->sdlRenderer = nullptr;
    this->texture = nullptr;
}


Engine::~Engine(void)
{
}


bool Engine::Init()
{
     if (SDL_Init(SDL_INIT_EVERYTHING) == -1)
     {
        SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 
                         "Error from SDL_Init()",
                         SDL_GetError(),
                         NULL);
        std::cout << SDL_GetError() << std::endl;
        return false;
    }
     return true;
}


bool Engine::LoadAssets()
{
    std::string fileName = "c:\\temp\\spinworld\\wall64.bmp";
    this->texture = SDL_LoadBMP(fileName.c_str());
    if (this->texture == NULL)
    {
        SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 
            ("Error from SDL_LoadBMP() while loading " + fileName).c_str(),
            SDL_GetError(),
            NULL);
        return false;
    }
    return true;
}


void Engine::Run()
{
    this->worldMap = new Map("map1.txt");

    int t = this->worldMap->GetWall(6, 4);


    if (!this->LoadAssets()) return;

    
    this->player.position.x = 22;
    this->player.position.y = 12;

    RaycastRenderer renderer;



}