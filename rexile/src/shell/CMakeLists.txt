add_executable(${PROJECT_NAME}
    src/main.c
    src/ux_args.c
    src/ux_draw.c
    src/ux_input.c)

target_link_libraries(rexile PRIVATE external rexile_core)

target_include_directories(rexile PRIVATE external PUBLIC include)

target_include_directories(${PROJECT_NAME} PRIVATE ${NCURSESW_INCLUDE_DIRS})

target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall -O0 ${NCURSESW_CFLAGS_OTHER})

target_link_libraries(${PROJECT_NAME} PRIVATE ${NCURSESW_LIBRARIES})
