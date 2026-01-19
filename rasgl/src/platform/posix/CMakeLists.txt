find_package(SDL2 REQUIRED)

add_library(ras_sdl main.c rasterize.c)

target_include_directories(ras_sdl PUBLIC include ${SDL2_INCLUDE_DIRS})
target_link_libraries(ras_sdl PRIVATE m core hosted PUBLIC SDL2::SDL2)
