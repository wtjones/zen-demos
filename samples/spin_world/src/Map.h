#pragma once

#include <memory>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

class Map
{
    std::unique_ptr<int[]> data;
    int x;
    int y;

public:
    Map(std::string filePath);
    ~Map(void);
    int GetWall(int x, int y);
};

