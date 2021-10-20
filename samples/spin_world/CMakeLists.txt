cmake_minimum_required(VERSION 3.21)

project(
    SpinWorld
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES CXX)


find_package(SDL2 REQUIRED)

add_executable(${PROJECT_NAME} src/main.cpp src/Vector2.cpp src/Player.cpp)

target_link_libraries(SpinWorld PUBLIC SDL2::SDL2)

target_include_directories(SpinWorld PUBLIC ${SDL2_INCLUDE_DIRS})
