#pragma once

#include "Vector2.h"

class Player
{
public:
	Player(void);
	~Player(void);

	Vector2 position;
    //float x;
	//float y;
	int angle;
    float walkSpeed;
    float turnSpeed;

};

