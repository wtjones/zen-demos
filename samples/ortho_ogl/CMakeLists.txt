cmake_minimum_required(VERSION 3.10)
project(ortho_ogl C)

set(CMAKE_C_STANDARD 11)
if(POLICY CMP0072)
	cmake_policy(SET CMP0072 NEW)
endif()

set(OpenGL_GL_PREFERENCE GLVND)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GLFW3 REQUIRED glfw3)
find_package(OpenGL REQUIRED)

include_directories(${GLFW3_INCLUDE_DIRS})
link_directories(${GLFW3_LIBRARY_DIRS})

add_executable(ortho_ogl src/main.c)
target_link_libraries(ortho_ogl ${GLFW3_LIBRARIES} ${OPENGL_LIBRARIES})
