include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

find_package(SDL2 REQUIRED)

add_library(ras_sdl
    main.c
    rasterize.c
    settings.c)

target_include_directories(ras_sdl PUBLIC include ${SDL2_INCLUDE_DIRS})
target_link_libraries(ras_sdl PRIVATE m core hosted PUBLIC ${LARSE_LIB} SDL2::SDL2)
