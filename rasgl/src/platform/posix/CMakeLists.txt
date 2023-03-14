
find_package(SDL2 REQUIRED)

add_executable(ras_sdl
    main.c
    ../../demo/demo_main.c)

 target_link_libraries(ras_sdl PRIVATE m PUBLIC SDL2::SDL2)

 target_include_directories(ras_sdl PUBLIC ${SDL2_INCLUDE_DIRS})
