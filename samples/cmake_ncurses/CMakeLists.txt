cmake_minimum_required(VERSION 3.21)
project(cursed_tpl VERSION 1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

find_package(PkgConfig REQUIRED)
pkg_check_modules(NCURSESW REQUIRED ncursesw)


add_executable(${PROJECT_NAME}
    src/main.c)

target_include_directories(${PROJECT_NAME} PRIVATE ${NCURSESW_INCLUDE_DIRS})

target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall -O0 ${NCURSESW_CFLAGS_OTHER})

target_link_libraries(${PROJECT_NAME} PRIVATE ${NCURSESW_LIBRARIES})