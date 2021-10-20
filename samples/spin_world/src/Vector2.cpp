#include "Vector2.h"
 
Vector2::Vector2(void)
{
}
 
Vector2::Vector2(float x, float y){
    this->x = x;
    this->y = y;
}
 
// Returns the length of the vector
float Vector2::Length(){
    return sqrt(x * x + y * y);
}
 
// Normalizes the vector
Vector2 Vector2::Normalize(){
    Vector2 vector;
    float length = this->Length();
 
    if(length != 0){
        vector.x = x/length;
        vector.y = y/length;
    }
 
    return vector;
}
 
Vector2::~Vector2(void)
{
}