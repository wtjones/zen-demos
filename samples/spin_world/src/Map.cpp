#include "Map.h"



Map::Map(std::string filePath)
{
    

    std::ifstream inFile(filePath);
    
    //std::cout << "hi" << std::endl;
    


    if (inFile.is_open())
    {             
        // read file to temporary array of lines
        std::vector<std::string> mapLines;
        std::string line;
        while (getline(inFile, line))        
        {
            mapLines.push_back(line);
            this->x = line.length();           
        }
        this->y = mapLines.size();

        // With size known, allocate and set array;
        this->data = std::unique_ptr<int[]>(new int[this->x * this->y]);        
        int y = 0;
        for (auto row = mapLines.begin(); row != mapLines.end(); ++row)
        {
            
            for (int x = 0; x < (*row).length(); x++)
            {
                char c  = (*row)[x];
                int cellValue = std::atoi(&c);                          
                this->data[y * this->x + x] = cellValue;
            }
            y++;
        }

        int blah = this->data[0];
        //while (std::getline(inFile, line))
        //{

//        }
    }

}


Map::~Map(void)
{
}

int Map::GetWall(int x, int y)
{
    return this->data[y * this->x + x];
}