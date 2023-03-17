add_executable(demo
    demo_main.c)

target_link_libraries(demo PRIVATE raslib ${RAS_PLATFORM})

target_include_directories(demo PUBLIC include)
