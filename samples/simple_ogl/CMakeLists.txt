cmake_minimum_required(VERSION 3.21)

project(
    simple_ogl
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

include(CTest)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)
find_package(SDL2 REQUIRED)
find_package(OpenGL REQUIRED)


add_executable(${PROJECT_NAME}
    src/main.c)

target_link_libraries(simple_ogl PRIVATE m PUBLIC ${SDL2_LIBRARIES} ${OPENGL_LIBRARIES})


target_include_directories(simple_ogl PUBLIC ${SDL2_INCLUDE_DIRS})
target_include_directories(simple_ogl PUBLIC ${OPENGL_INCLUDE_DIR})
