find_package(SDL2 REQUIRED)

add_library(ras_sdl main.c)

target_link_libraries(ras_sdl PRIVATE m core PUBLIC SDL2::SDL2)

target_include_directories(ras_sdl PUBLIC include ${SDL2_INCLUDE_DIRS})
