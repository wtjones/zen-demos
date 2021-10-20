#include "RaycastRenderer.h"


RaycastRenderer::RaycastRenderer(void)
{
    for (int i = 0; i < 360; i++)
    {
        this->cosTable[i] = cos((float)i * M_PI / -180.0);
        this->sinTable[i] = sin((float)i * M_PI / -180.0);
    }

}


RaycastRenderer::~RaycastRenderer(void)
{
}


void RaycastRenderer::Render(Player *player, SDL_Surface *texture)
{
    
    double posX = player->position.x, posY = player->position.y;  //x and y start position
    double origDirX = -1, origDirY = 0;
    double dirX = -1, dirY = 0; //initial direction vector
    double origPlaneX = 0, origPlaneY = .66;//0.66; //the 2d raycaster version of camera plane
    double planeX = 0, planeY = 0.66; //the 2d raycaster version of camera plane
}