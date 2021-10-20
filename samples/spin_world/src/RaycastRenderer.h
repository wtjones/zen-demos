#pragma once

#include "Player.h"
#include <SDL2/SDL.h>
#include <math.h>

class RaycastRenderer {

    float sinTable[360];
    float cosTable[360];

public:
    RaycastRenderer(void);
    ~RaycastRenderer(void);
    void Render(Player* player, SDL_Surface* texture);
};
