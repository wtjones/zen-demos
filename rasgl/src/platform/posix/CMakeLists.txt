find_package(SDL2 REQUIRED)

include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

add_library(ras_sdl main.c rasterize.c settings.c)

target_include_directories(ras_sdl PUBLIC include ${SDL2_INCLUDE_DIRS})
target_link_libraries(ras_sdl PRIVATE core hosted PUBLIC m SDL2::SDL2 larse::core)
