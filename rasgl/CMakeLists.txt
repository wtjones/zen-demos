cmake_minimum_required(VERSION 3.21)

project(
    RasGL
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

# find_file(SDL2_INCLUDE_DIR NAME SDL.h HINTS SDL2)
#  find_library(SDL2_LIBRARY NAME SDL2)
# find_package(SDL2 REQUIRED)
# find_library(SDL2_LIBRARY NAME SDL2)


find_package(SDL2 REQUIRED)

add_executable(${PROJECT_NAME} src/main.c)

 #target_link_libraries(${PROJECT_NAME} SDL2::SDL2 SDL2::SDL2main)

 target_link_libraries(RasGL PRIVATE m PUBLIC SDL2::SDL2)
# target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARY})

 target_include_directories(RasGL PUBLIC ${SDL2_INCLUDE_DIRS})

# target_link_libraries(RasGL ${SDL2_LIBRARY})


