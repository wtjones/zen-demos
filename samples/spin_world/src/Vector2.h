#pragma once
 
#include <math.h>
 
class Vector2
{
public:
    Vector2(void);
    Vector2(float x, float y);
    ~Vector2(void);
    float Length();
    Vector2 Normalize();
    float x,y;
};